{-# LANGUAGE BangPatterns #-}

{- | Game Boy Picture Processing Unit, covering both DMG and CGB.

The PPU owns its own VRAM (8 KiB on DMG, two banks on CGB, at
@0x8000-0x9FFF@), OAM (160 bytes at @0xFE00-0xFE9F@), and the register file
at @0xFF40-0xFF4B@ plus the CGB-only registers at @0xFF4F@ and
@0xFF68-0xFF6C@. The bus dispatches reads and writes for those ranges to
'read8' and 'write8'.

Every scanline is 456 dots, split between the modes:

> Mode 2 (OAM scan): dots 0..79                (80, fixed)
> Mode 3 (drawing) : dots 80..(80 + len - 1)   (len, variable; 172 minimum)
> Mode 0 (HBlank)  : the rest of the scanline  (456 - 80 - len)
> Mode 1 (VBlank)  : 10 lines * 456 dots = 4560 dots

Mode 3 is variable-length: 'mode3Length' adds the @SCX mod 8@ fine-scroll
discard and the 6-dot window-activation restart to the 172-dot base, and mode
0 absorbs the difference.

One scanline is not 456 dots, and it has no mode 2. The first line after LCDC bit
7 goes @0 -> 1@ reports mode 0 until drawing starts at dot 78, and runs 448 dots
overall; on DMG both figures gain one dot. 'ppuLcdOnFirstLine' tracks it. Running
that line at the full 456 leaves every later @LY@ edge, and the VBlank interrupt
with it, 8 T-cycles late for as long as the LCD stays on.

The mode the CPU reads back from STAT is not the mode the PPU is in: the register
bits lag the real mode, by 'statModeDelay' at most boundaries and by
'statVblankModeDelay' on entry to VBlank. 'visibleModeBits' applies that to reads
only, while 'computeStatLine' drives the interrupt from the real mode.

State is held in 'IORef's and 'IOVector's so reads and writes are O(1) and
the rendered framebuffer is updated in place.

What is implemented: the mode state machine and LY counter; background;
window, with a real window-line counter ('ppuWindowLine') rather than an
@LY - WY@ approximation; sprites with the DMG sort-by-X priority, 8x8 / 8x16
sizes, X/Y flip, OBP0/OBP1, and the BG-priority bit; BGP/OBP palette
transforms; the VBlank interrupt edge and all four STAT interrupt sources;
LCD-off freeze; and the CGB pipeline (VRAM banking, BG attributes, BG/OBJ
palette RAM, and OPRI sprite priority).

Not implemented: the per-object fetcher stall, so lines with sprites report
their sprite-free mode 3 length. That is what leaves mooneye
@acceptance/ppu/intr_2_mode0_timing_sprites@ pending.
-}
module Ocelot.Ppu (
    PpuState (..),
    PpuMode (..),
    CgbRenderMode (..),
    initialPpu,
    read8,
    write8,
    advance,
    framebuffer,
    framebufferRgb,
    copyFramebufferRgb,
    copyFramebufferRgbWithPitch,
    copyFramebufferRgba,
    framebufferRgbBytes,
    framebufferRgbaBytes,
    framebufferRgbaPtr,
    framebufferWidth,
    framebufferHeight,
    setCgbMode,
    setCgbRenderMode,
    FbTarget (..),
    setFbTarget,
    takePendingStatIrq,
    resyncMode3End,
    seedLcdc,
    accessedOamRow,
    triggerOamBug,
) where

import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int8)
import Data.Maybe (isJust)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

framebufferWidth, framebufferHeight :: Int
framebufferWidth = 160
framebufferHeight = 144

data PpuMode
    = ModeHBlank
    | ModeVBlank
    | ModeOamScan
    | ModeDrawing
    deriving (Eq, Show, Enum, Bounded)

{- | How to colorize a rendered scanline. Picked by the bus once at
startup based on (cart-CGB-flag, hardware-CGB-flag); the PPU reads
this in its RGB pass.
-}
data CgbRenderMode
    = RenderDmg
    | RenderCgbCompat
    | RenderCgbFull
    deriving (Eq, Show, Enum, Bounded)

{- | Which color framebuffer(s) the PPU populates during rendering.
Set once at startup via 'setFbTarget' to skip writing buffers that the
current frontend does not read, reducing per-scanline memory traffic.
-}
data FbTarget
    = -- | Write only 'ppuFbRgb' (RGB888). Used by the SDL desktop frontend.
      FbRgb
    | -- | Write only 'ppuFbRgba' (RGBA8888). Used by the web WASM frontend.
      -- Skips 'ppuFb' and 'ppuFbRgb' because the browser reads only RGBA.
      FbRgba
    | -- | Write both buffers. Default; used by tests and the terminal renderer.
      FbBoth
    deriving (Eq, Show)

data PpuState = PpuState
    { ppuLcdc :: !(IORef Word8)
    , ppuStat :: !(IORef Word8)
    , ppuLy :: !(IORef Word8)
    , ppuLyc :: !(IORef Word8)
    , ppuScy :: !(IORef Word8)
    , ppuScx :: !(IORef Word8)
    , ppuWy :: !(IORef Word8)
    , ppuWx :: !(IORef Word8)
    , ppuBgp :: !(IORef Word8)
    , ppuObp0 :: !(IORef Word8)
    , ppuObp1 :: !(IORef Word8)
    , ppuMode :: !(IORef PpuMode)
    , ppuDot :: !(IORef Int)
    , ppuMode3End :: !(IORef Int)
    -- ^ Dot at which mode 3 ends on the line currently being drawn, latched
    -- when the PPU leaves OAM scan. Mode 3 is not a fixed 172 dots: the
    -- fetcher throws away @SCX mod 8@ pixels at the left edge, activating the
    -- window restarts it, and each object stalls it. Mode 0 absorbs whatever
    -- mode 3 takes, so the scanline stays 456 dots either way. Derived state,
    -- recomputed every line; 'resyncMode3End' rebuilds it after a snapshot
    -- load so the restored line does not use the previous machine's value.
    , ppuLcdOnFirstLine :: !(IORef Bool)
    -- ^ Set while the PPU is on the first scanline after LCDC bit 7 went
    -- 0 -> 1, which hardware runs short and without a mode 2: drawing starts at
    -- 'lcdOnPreDrawDots' and the line lasts 'lcdOnLineDots'. Cleared when that
    -- line ends. Without it the whole PPU line phase sits 8 T-cycles late from
    -- the moment the LCD is enabled, which shifts every later LY edge and the
    -- VBlank IRQ with it.
    , ppuWindowLine :: !(IORef Int)
    -- ^ Internal window-line counter (\"WLY\"). Reset to 0 at the start
    -- of each frame and on LCD-off; increments by 1 only on lines where
    -- the window was actually rendered. The window's row in its tilemap
    -- is this counter, not @ly - wy@, which lets games disable\/move the
    -- window mid-frame and still get correct row addressing.
    , ppuVram :: !(IOVector Word8)
    -- ^ 16 KiB on hardware (two 8 KiB banks); DMG only ever uses bank 0.
    -- Address 0x8000-0x9FFF reads/writes the bank currently selected by
    -- 'ppuVbk'.
    , ppuOam :: !(IOVector Word8)
    , ppuFb :: !(IOVector Word8)
    -- ^ DMG palette-index framebuffer (160 * 144 bytes, values 0..3).
    -- Kept for tests and the terminal renderer; CGB rendering still
    -- writes meaningful values here for the BG layer.
    , ppuFbRgb :: !(IOVector Word8)
    -- ^ RGB888 color framebuffer (160 * 144 * 3 bytes). What the SDL
    -- frontend uses; populated by the same render pass that fills
    -- 'ppuFb'. DMG mode goes through the shade palette; CGB mode uses
    -- BG palette RAM for the BG layer and OBP0\/OBP1 for sprites.
    , ppuFbRgba :: !(VSM.IOVector Word8)
    -- ^ RGBA8888 color framebuffer (160 * 144 * 4 bytes). Backed by a
    -- storable (pinned) vector so that 'framebufferRgbaPtr' can return a
    -- stable 'Ptr' directly into this buffer — eliminating the copy that
    -- the web WASM frontend would otherwise need every frame.
    , ppuFbTarget :: !(IORef FbTarget)
    -- ^ Which frontend framebuffer(s) to populate. Set via 'setFbTarget'.
    , ppuCgbMode :: !(IORef Bool)
    -- ^ Whether the bus is running a CGB cart. Set once at startup
    -- via 'setCgbMode'; rendering reads this to pick the BG path.
    , ppuRenderMode :: !(IORef CgbRenderMode)
    -- ^ How to colorize each rendered scanline:
    --
    -- * 'RenderDmg' (DMG hardware running a DMG cart): hardcoded
    --   greenish-DMG shade palette.
    -- * 'RenderCgbCompat' (CGB hardware running a DMG cart): the
    --   CGB-compatibility auto-palette pre-loaded into CGB BG palette
    --   0 and OBJ palettes 0\/1, indexed by the DMG BGP\/OBP0\/OBP1
    --   shade.
    -- * 'RenderCgbFull' (CGB cart): full CGB pipeline (BG attribute
    --   palette + tile bank + flips, OBJ palette via OAM attr bits).
    , ppuVbk :: !(IORef Word8)
    -- ^ CGB VRAM bank select (0xFF4F). Bit 0 selects the active bank;
    -- DMG ignores writes (always reads as 0xFF).
    , ppuBcps :: !(IORef Word8)
    -- ^ CGB BG palette index register (0xFF68): bit 7 = auto-increment,
    -- bits 5-0 = byte offset into 'ppuBgPalRam' (0..63).
    , ppuOcps :: !(IORef Word8)
    -- ^ CGB OBJ palette index register (0xFF6A); same layout.
    , ppuBgPalRam :: !(IOVector Word8)
    -- ^ 64 bytes of BG palette memory: 8 palettes x 4 colors x 2 bytes
    -- (RGB555 little-endian). Read via 0xFF69, written via 0xFF69.
    , ppuObjPalRam :: !(IOVector Word8)
    -- ^ 64 bytes of OBJ palette memory; read/written via 0xFF6B.
    , ppuPrevStatLine :: !(IORef Bool)
    -- ^ Last sampled value of the OR'd STAT interrupt line. The STAT
    -- IRQ fires only on a low->high transition of this signal, so
    -- back-to-back enabled sources (e.g. mode 0 followed by mode 2 with
    -- both bits 3 and 5 set in STAT) raise IF bit 1 only once. See
    -- mooneye 'stat_irq_blocking'.
    , ppuPendingStatIrq :: !(IORef Bool)
    -- ^ Latched whenever a register write (STAT, LYC, LCDC bit 7)
    -- causes a STAT-line rising edge. The bus reads and clears this
    -- via 'takePendingStatIrq' after the write, propagating to IF.
    , ppuOpri :: !(IORef Word8)
    -- ^ CGB sprite-priority register at @0xFF6C@. Bit 0 = 0 selects
    -- OAM-index priority (CGB native); bit 0 = 1 selects leftmost-X
    -- priority (DMG behavior). Bits 1-7 read as 1 on real hardware.
    -- The CGB boot ROM seeds this from the cart's CGB-flag at startup
    -- (1 for unmodified DMG carts, 0 for CGB carts); games may
    -- overwrite it. We stub the boot logic by initializing the value
    -- in 'Bus.fromCartridgeOnHost' based on the render mode.
    }

initialPpu :: IO PpuState
initialPpu = do
    -- Hardware power-on: LCD off. Callers that want the post-boot handoff
    -- (LCDC=0x91, LCD on) go through 'seedLcdc', not 'write8': the handoff is
    -- not a guest-visible LCD enable and must not start the short first line.
    lcdc <- newIORef 0x00
    stat <- newIORef 0x00
    ly <- newIORef 0x00
    lyc <- newIORef 0x00
    scy <- newIORef 0x00
    scx <- newIORef 0x00
    wy <- newIORef 0x00
    wx <- newIORef 0x00
    -- Hardware power-on: palettes 0x00. Post-boot callers overwrite to
    -- BGP=0xFC, OBP0/OBP1=0xFF via Ppu.write8.
    bgp <- newIORef 0x00
    obp0 <- newIORef 0x00
    obp1 <- newIORef 0x00
    mode <- newIORef ModeOamScan
    dot <- newIORef 0
    mode3End <- newIORef (oamScanDots + mode3BaseDots)
    lcdOnFirstLine <- newIORef False
    windowLine <- newIORef 0
    vram <- MV.replicate 0x4000 0
    oam <- MV.replicate 0xA0 0
    fb <- MV.replicate (framebufferWidth * framebufferHeight) 0
    fbRgb <- MV.replicate (framebufferWidth * framebufferHeight * 3) 0
    fbRgba <- VSM.replicate (framebufferWidth * framebufferHeight * 4) 0
    let initAlpha !i
            | i >= framebufferWidth * framebufferHeight = pure ()
            | otherwise = do
                VSM.write fbRgba (i * 4 + 3) 255
                initAlpha (i + 1)
    initAlpha 0
    fbTarget <- newIORef FbBoth
    cgbMode <- newIORef False
    renderMode <- newIORef RenderDmg
    vbk <- newIORef 0
    bcps <- newIORef 0
    ocps <- newIORef 0
    bgPal <- MV.replicate 0x40 0xFF
    objPal <- MV.replicate 0x40 0xFF
    prevStatLine <- newIORef False
    pendingStatIrq <- newIORef False
    -- Default OPRI = 0 (OAM priority). The bus overrides this for
    -- DMG-on-CGB compat carts to match a CGB-boot-ROM-driven OPRI=1.
    opri <- newIORef 0x00
    pure
        PpuState
            { ppuLcdc = lcdc
            , ppuStat = stat
            , ppuLy = ly
            , ppuLyc = lyc
            , ppuScy = scy
            , ppuScx = scx
            , ppuWy = wy
            , ppuWx = wx
            , ppuBgp = bgp
            , ppuObp0 = obp0
            , ppuObp1 = obp1
            , ppuMode = mode
            , ppuDot = dot
            , ppuMode3End = mode3End
            , ppuLcdOnFirstLine = lcdOnFirstLine
            , ppuWindowLine = windowLine
            , ppuVram = vram
            , ppuOam = oam
            , ppuFb = fb
            , ppuFbRgb = fbRgb
            , ppuFbRgba = fbRgba
            , ppuFbTarget = fbTarget
            , ppuCgbMode = cgbMode
            , ppuRenderMode = renderMode
            , ppuVbk = vbk
            , ppuBcps = bcps
            , ppuOcps = ocps
            , ppuBgPalRam = bgPal
            , ppuObjPalRam = objPal
            , ppuOpri = opri
            , ppuPrevStatLine = prevStatLine
            , ppuPendingStatIrq = pendingStatIrq
            }

{- | Take a snapshot of the framebuffer as an immutable Vector. Used by the
terminal renderer in 'app/Main.hs'; safe because @ppuFb@ is not modified
concurrently with this call.
-}
framebuffer :: PpuState -> IO (Vector Word8)
framebuffer ps = V.freeze (ppuFb ps)

{- | Snapshot of the RGB framebuffer (160 * 144 * 3 bytes, R/G/B
interleaved). Populated by the same render pass that fills 'ppuFb';
DMG carts go through the standard shade palette, CGB carts use BG\/OBJ
palette RAM (BG only this slice; sprites are still DMG-style).
-}
framebufferRgb :: PpuState -> IO (Vector Word8)
framebufferRgb ps = V.freeze (ppuFbRgb ps)

-- | Copy the RGB framebuffer into a caller-provided buffer in RGB888 order.
copyFramebufferRgb :: Ptr Word8 -> PpuState -> IO ()
copyFramebufferRgb ptr ps = go 0
  where
    rgbBytes = framebufferWidth * framebufferHeight * 3
    go !i
        | i >= rgbBytes = pure ()
        | otherwise = do
            px <- MV.unsafeRead (ppuFbRgb ps) i
            pokeByteOff ptr i px
            go (i + 1)

-- | Copy the RGB framebuffer into a caller-provided buffer whose rows are separated by @pitch@ bytes.
copyFramebufferRgbWithPitch :: Ptr Word8 -> Int -> PpuState -> IO ()
copyFramebufferRgbWithPitch ptr pitch ps
    | pitch == rowBytes = copyFramebufferRgb ptr ps
    | otherwise = copyRows 0 0
  where
    rowBytes = framebufferWidth * 3
    totalRows = framebufferHeight
    copyRows !row !srcOff
        | row >= totalRows = pure ()
        | otherwise = do
            copyRow srcOff (row * pitch) 0
            copyRows (row + 1) (srcOff + rowBytes)

    copyRow !_ !_ !col | col >= rowBytes = pure ()
    copyRow !srcOff !dstOff !col = do
        px <- MV.unsafeRead (ppuFbRgb ps) (srcOff + col)
        pokeByteOff ptr (dstOff + col) px
        copyRow srcOff dstOff (col + 1)

-- | Copy the RGBA framebuffer into a caller-provided buffer in RGBA8888 order.
copyFramebufferRgba :: Ptr Word8 -> PpuState -> IO ()
copyFramebufferRgba dst ps =
    VSM.unsafeWith (ppuFbRgba ps) $ \src -> copyBytes dst src rgbaBytes
  where
    rgbaBytes = framebufferWidth * framebufferHeight * 4

{- | Return a stable 'Ptr' directly into the RGBA framebuffer. The pointer
is valid for the lifetime of the 'PpuState' because 'ppuFbRgba' is backed by
a pinned storable vector that never moves. Use only where the 'PpuState'
outlives the pointer (e.g. a WASM session that holds the machine alive).
-}
framebufferRgbaPtr :: PpuState -> Ptr Word8
framebufferRgbaPtr ps =
    unsafeForeignPtrToPtr . fst $ VSM.unsafeToForeignPtr0 (ppuFbRgba ps)

-- | Copy the RGB framebuffer into a packed strict 'ByteString' in RGB888 order.
framebufferRgbBytes :: PpuState -> IO ByteString
framebufferRgbBytes ps =
    BSI.create rgbBytes $ \ptr -> copyFramebufferRgb ptr ps
  where
    rgbBytes = framebufferWidth * framebufferHeight * 3

-- | Copy the RGB framebuffer into a packed strict 'ByteString' in RGBA8888 order.
framebufferRgbaBytes :: PpuState -> IO ByteString
framebufferRgbaBytes ps =
    BSI.create rgbaBytes $ \ptr -> copyFramebufferRgba ptr ps
  where
    rgbaBytes = framebufferWidth * framebufferHeight * 4

{- | Tell the PPU whether it's running a CGB cart (called once by the
bus at startup). Affects BG attribute fetching and the sprite-priority
rule; the higher-level color routing is controlled by 'setCgbRenderMode'.
-}
setCgbMode :: Bool -> PpuState -> IO ()
setCgbMode b ps = writeIORef (ppuCgbMode ps) b

-- | Pick the colorization path for rendered scanlines.
setCgbRenderMode :: CgbRenderMode -> PpuState -> IO ()
setCgbRenderMode m ps = writeIORef (ppuRenderMode ps) m

{- | Set which color framebuffer(s) the render loop writes. Call once after
'initialPpu', before the first frame runs. Frontends that read only one
format should call this so the PPU skips the unused writes each scanline.
-}
setFbTarget :: FbTarget -> PpuState -> IO ()
setFbTarget t ps = writeIORef (ppuFbTarget ps) t

{- | Standard DMG shade palette mapped to the SDL frontend's
greenish-DMG colors. Used when converting palette indices to RGB.
-}
dmgShadeRgb :: Word8 -> (Word8, Word8, Word8)
dmgShadeRgb 0 = (0xE0, 0xF8, 0xD0)
dmgShadeRgb 1 = (0x88, 0xC0, 0x70)
dmgShadeRgb 2 = (0x34, 0x68, 0x56)
dmgShadeRgb _ = (0x08, 0x18, 0x20)

{- | Decode a CGB RGB555 word (low byte first): bits 0-4 R, 5-9 G,
10-14 B. We scale 5-bit channels to 8-bit by replicating the high bits
into the low ones (i.e. @c8 = (c5 \<\< 3) | (c5 \>\> 2)@), which gives
the standard 0..255 range.
-}
rgb555ToRgb888 :: Word8 -> Word8 -> (Word8, Word8, Word8)
rgb555ToRgb888 lo hi =
    let w = fromIntegral lo .|. (fromIntegral hi `shiftL` 8) :: Int
        r5 = w .&. 0x1F
        g5 = (w `shiftR` 5) .&. 0x1F
        b5 = (w `shiftR` 10) .&. 0x1F
        scale c = fromIntegral ((c `shiftL` 3) .|. (c `shiftR` 2)) :: Word8
     in (scale r5, scale g5, scale b5)

----------------------------------------------------------------------
-- Register I/O
----------------------------------------------------------------------

read8 :: Word16 -> PpuState -> IO Word8
read8 addr ps
    | addr <= 0x9FFF = do
        bank <- vramBankIndex ps
        MV.read (ppuVram ps) (bank + (fromIntegral addr .&. 0x1FFF))
    | addr >= 0xFE00 && addr <= 0xFE9F =
        MV.read (ppuOam ps) (fromIntegral addr .&. 0xFF)
    | addr == 0xFF40 = readIORef (ppuLcdc ps)
    | addr == 0xFF41 = do
        stat <- readIORef (ppuStat ps)
        bits <- visibleModeBits ps
        match <- lycMatches ps
        let lyMatch = if match then 0x04 else 0
        pure ((stat .&. 0x78) .|. bits .|. lyMatch .|. 0x80)
    | addr == 0xFF42 = readIORef (ppuScy ps)
    | addr == 0xFF43 = readIORef (ppuScx ps)
    | addr == 0xFF44 = visibleLy ps
    | addr == 0xFF45 = readIORef (ppuLyc ps)
    | addr == 0xFF47 = readIORef (ppuBgp ps)
    | addr == 0xFF48 = readIORef (ppuObp0 ps)
    | addr == 0xFF49 = readIORef (ppuObp1 ps)
    | addr == 0xFF4A = readIORef (ppuWy ps)
    | addr == 0xFF4B = readIORef (ppuWx ps)
    | addr == 0xFF4F = (.|. 0xFE) <$> readIORef (ppuVbk ps)
    -- BCPS/OCPS: bit 7 = auto-increment, bits 0-5 = palette index, bit 6
    -- is unused and reads as 1 on real hardware (matches SameBoy
    -- 'GB_IO_BGPI/OBPI' read path).
    | addr == 0xFF68 = (.|. 0x40) <$> readIORef (ppuBcps ps)
    | addr == 0xFF69 = do
        ix <- readIORef (ppuBcps ps)
        MV.read (ppuBgPalRam ps) (fromIntegral (ix .&. 0x3F))
    | addr == 0xFF6A = (.|. 0x40) <$> readIORef (ppuOcps ps)
    | addr == 0xFF6B = do
        ix <- readIORef (ppuOcps ps)
        MV.read (ppuObjPalRam ps) (fromIntegral (ix .&. 0x3F))
    -- OPRI: bit 0 readable, bits 1-7 read as 1 (matches SameBoy
    -- 'memory.c:635': @io_registers[OPRI] | 0xFE@).
    | addr == 0xFF6C = (.|. 0xFE) <$> readIORef (ppuOpri ps)
    | otherwise = pure 0xFF

write8 :: Word16 -> Word8 -> PpuState -> IO ()
write8 addr !v ps
    | addr <= 0x9FFF = do
        bank <- vramBankIndex ps
        MV.write (ppuVram ps) (bank + (fromIntegral addr .&. 0x1FFF)) v
    | addr >= 0xFE00 && addr <= 0xFE9F =
        MV.write (ppuOam ps) (fromIntegral addr .&. 0xFF) v
    | addr == 0xFF40 = handleLcdcWrite v ps
    | addr == 0xFF41 = do
        modifyIORef' (ppuStat ps) (\s -> (v .&. 0x78) .|. (s .&. 0x07))
        sampleStatLine ps
    | addr == 0xFF42 = writeIORef (ppuScy ps) v
    | addr == 0xFF43 = writeIORef (ppuScx ps) v
    | addr == 0xFF44 = pure () -- LY is read-only
    | addr == 0xFF45 = do
        writeIORef (ppuLyc ps) v
        sampleStatLine ps
    | addr == 0xFF47 = writeIORef (ppuBgp ps) v
    | addr == 0xFF48 = writeIORef (ppuObp0 ps) v
    | addr == 0xFF49 = writeIORef (ppuObp1 ps) v
    | addr == 0xFF4A = writeIORef (ppuWy ps) v
    | addr == 0xFF4B = writeIORef (ppuWx ps) v
    | addr == 0xFF4F = writeIORef (ppuVbk ps) (v .&. 0x01)
    | addr == 0xFF68 = writeIORef (ppuBcps ps) v
    | addr == 0xFF69 = writePaletteByte ps ppuBcps ppuBgPalRam v
    | addr == 0xFF6A = writeIORef (ppuOcps ps) v
    | addr == 0xFF6B = writePaletteByte ps ppuOcps ppuObjPalRam v
    -- Only bit 0 of OPRI is meaningful; ignore the rest.
    | addr == 0xFF6C = writeIORef (ppuOpri ps) (v .&. 0x01)
    | otherwise = pure ()

-- | Byte offset of the active VRAM bank (0 or 0x2000).
vramBankIndex :: PpuState -> IO Int
vramBankIndex ps = do
    b <- readIORef (ppuVbk ps)
    pure (if testBit b 0 then 0x2000 else 0)

{- | Common implementation of writes through the BCPS\/OCPS auto-increment
register: write the byte at the current low-6-bit index, then if bit 7 of
the index register is set, advance the index (wrapping inside its low 6
bits but leaving bit 7 alone).
-}
writePaletteByte ::
    PpuState ->
    (PpuState -> IORef Word8) ->
    (PpuState -> IOVector Word8) ->
    Word8 ->
    IO ()
writePaletteByte ps idxSel ramSel v = do
    ix <- readIORef (idxSel ps)
    MV.write (ramSel ps) (fromIntegral (ix .&. 0x3F)) v
    when (testBit ix 7) $
        let next = (ix .&. 0xC0) .|. ((ix + 1) .&. 0x3F)
         in writeIORef (idxSel ps) next

handleLcdcWrite :: Word8 -> PpuState -> IO ()
handleLcdcWrite v ps = do
    !prev <- readIORef (ppuLcdc ps)
    writeIORef (ppuLcdc ps) v
    -- LCD turning off freezes LY at 0 in Mode 0 and resets WLY. The STAT
    -- line is gated low while the LCD is off, so the edge detector also
    -- resets to avoid a stale rising-edge when the LCD comes back on.
    unless (testBit v 7) $ do
        writeIORef (ppuLy ps) 0
        writeIORef (ppuMode ps) ModeHBlank
        writeIORef (ppuDot ps) 0
        writeIORef (ppuWindowLine ps) 0
        writeIORef (ppuPrevStatLine ps) False
        writeIORef (ppuLcdOnFirstLine ps) False
    -- LCD turning back on: real hardware starts a fresh frame at mode 2
    -- (OAM scan), LY=0, dot=0. Without this reset the PPU resumes from
    -- 'ModeHBlank' (where 'unless (testBit v 7)' just put it during the
    -- preceding LCD-off), which leaves the first 456 dots after re-enable
    -- as one elongated HBlank: real CGB games that toggle LCD on/off per
    -- frame end up with their first scanline never re-entering OAM scan,
    -- which serialises into "BG never renders" on the very first frame.
    --
    -- That first line is also special, and 'ppuLcdOnFirstLine' marks it: it runs
    -- 'lcdOnLineDots' rather than 456, and it has no mode 2 at all. Hardware
    -- reports mode 0 with OAM and VRAM unblocked until drawing starts at
    -- 'lcdOnPreDrawDots', so the mode here is 'ModeHBlank' rather than
    -- 'ModeOamScan'. Using mode 2 fabricates an OAM-source STAT interrupt that
    -- hardware never raises, and blocks OAM reads that hardware allows.
    when (not (testBit prev 7) && testBit v 7) $ do
        writeIORef (ppuMode ps) ModeHBlank
        writeIORef (ppuLy ps) 0
        writeIORef (ppuDot ps) 0
        writeIORef (ppuWindowLine ps) 0
        writeIORef (ppuLcdOnFirstLine ps) True
    sampleStatLine ps

{- | Seed LCDC the way the boot ROM left it, skipping the side effects of a
guest-visible @0 -> 1@ enable.

The post-boot handoff in "Ocelot.Bus" jumps straight to the state the boot ROM
produced, and by then the LCD has been on for most of a frame drawing the logo.
Routing that seed through 'write8' would look like a fresh enable and start the
machine on the short 448-dot line, which shifts the whole first frame and breaks
mooneye @acceptance/boot_hwio-dmgABCmgb@. The mode, LY, dot, and window-line
values the enable path would write are already what 'initialPpu' set, so only
the STAT sample is still needed here.
-}
seedLcdc :: Word8 -> PpuState -> IO ()
seedLcdc v ps = do
    writeIORef (ppuLcdc ps) v
    sampleStatLine ps

----------------------------------------------------------------------
-- DMG OAM bug
----------------------------------------------------------------------

{- | Byte offset of the OAM row the PPU is currently scanning, or @-1@ when there is
no such row.

SameBoy advances @accessed_oam_row@ once per *pair* of objects during mode 2, setting
it to @(index & ~1) * 4 + 8@. Across the 80-dot scan that is row 8 for the first four
dots and 8 bytes more every four dots after, so the last row it can name is 152, OAM's
twentieth and final eight-byte row, reached at dot 72.

Two consequences fall out of that formula, and together they are what make the
corruption window 76 dots rather than the full 80:

* Row 0 is never scanned, so the first row is 8. The row's first word decays towards
  the two rows above it, which is why there has to be a row above it at all.
* The last four dots (76-79) compute row 160, past the end of OAM. They name no row,
  so nothing can be corrupted there, and this returns @-1@ as it does outside mode 2.

blargg @oam_bug\/4-scanline_timing@ measures both edges directly: a trigger one M-cycle
before the window must not corrupt, the next 19 must, and the one after must not.
-}
accessedOamRow :: PpuState -> IO Int
accessedOamRow ps = do
    lcdc <- readIORef (ppuLcdc ps)
    mode <- readIORef (ppuMode ps)
    if not (testBit lcdc 7) || mode /= ModeOamScan
        then pure (-1)
        else do
            dot <- readIORef (ppuDot ps)
            let !row = 8 * (1 + (dot `div` 4))
            pure (if row > 152 then -1 else row)

{- | SameBoy's @bitwise_glitch@: how the scanned row's first word decays when the
CPU touches the OAM address range mid-scan.
-}
oamBugGlitch :: Word16 -> Word16 -> Word16 -> Word16
oamBugGlitch a b c = ((a `xor` c) .&. (b `xor` c)) `xor` c

{- | Apply the DMG OAM bug for a CPU access to @addr@.

A CPU access anywhere in @0xFE00-0xFEFF@ while the PPU is scanning OAM corrupts the
row being scanned, even though the access itself reads @0xFF@. The row's first word
is glitched against the two rows above it and bytes 2..7 are copied down from the
previous row. CGB has no OAM bug, and row 0 has nothing above it to decay towards.

This is SameBoy's @GB_trigger_oam_bug@ write-side pattern. It is wired in two places:
'Ocelot.Bus' calls it for CPU reads and writes anywhere in @0xFE00-0xFEFF@, and
'Ocelot.Cpu.Execute' calls it through @Bus.triggerOamBug@ for the address-bus
instructions that never issue a bus access at all (16-bit @INC@\/@DEC@ and @PUSH@).
Together those pass blargg @oam_bug@ 2-causes, 4-scanline_timing, and 5-timing_bug
while keeping 3-non_causes and 6-timing_no_bug green.

Still failing: @7-timing_effect@ and @8-instr_effect@. Reads need
@GB_trigger_oam_bug_read@'s separate secondary\/tertiary\/quaternary corruption
patterns, which are unimplemented, so a read's effect is currently modelled with the
write pattern. That is the likely cause of both.
-}
triggerOamBug :: Word16 -> PpuState -> IO ()
triggerOamBug addr ps
    | addr < 0xFE00 || addr > 0xFEFF = pure ()
    | otherwise = do
        cgb <- readIORef (ppuCgbMode ps)
        if cgb
            then pure ()
            else do
                row <- accessedOamRow ps
                when (row >= 8) $ do
                    let oam = ppuOam ps
                        wordAt i = do
                            !lo <- MV.read oam i
                            !hi <- MV.read oam (i + 1)
                            pure (fromIntegral lo .|. (fromIntegral hi `shiftL` 8) :: Word16)
                    !cur <- wordAt row
                    !prev <- wordAt (row - 8)
                    !mid <- wordAt (row - 4)
                    let !glitched = oamBugGlitch cur prev mid
                    MV.write oam row (fromIntegral (glitched .&. 0xFF))
                    MV.write oam (row + 1) (fromIntegral (glitched `shiftR` 8))
                    mapM_
                        (\i -> MV.read oam (row - 8 + i) >>= MV.write oam (row + i))
                        [2 .. 7]

{- | Dot on line 153 at which the LY register stops reporting 153 and reads 0.

Line 153 barely reports itself. SameBoy writes @LY = 153@ two dots in and @LY = 0@
six dots after that, so for roughly 448 of the line's 456 dots a read of @0xFF44@
returns 0 while the PPU is still on line 153. Holding 153 for the whole line is
what made every blargg APU subtest and mooneye @acceptance\/oam_dma_start@ diverge:
they sync on LY at the frame wrap and read 153 where hardware reads 0.
-}
lyLine153ClearDot :: Int
lyLine153ClearDot = 8

{- | LY as the CPU reads it at @0xFF44@.

Only line 153 differs from the internal counter, per 'lyLine153ClearDot'.
'ppuLy' stays the line counter the state machine advances and compares.
-}
visibleLy :: PpuState -> IO Word8
{-# INLINE visibleLy #-}
visibleLy ps = do
    ly <- readIORef (ppuLy ps)
    if ly /= 153
        then pure ly
        else do
            dot <- readIORef (ppuDot ps)
            pure (if dot < lyLine153ClearDot then 153 else 0)

{- | The value the PPU compares against LYC, or 'Nothing' when no match is
possible.

The comparison does not read LY. SameBoy keeps a separate @ly_for_comparison@ and
both the STAT bit-2 flag and the LYC interrupt source run off it. Reading the sleep
sequence in @display.c@ dot by dot, on a visible line it is not simply "suppressed
for a while" -- it still holds the *previous* line number until the register is
written:

> dots 0-2  previous line   (the -1 store has not happened yet)
> dot  3    none            (@ly_for_comparison = current_line ? -1 : 0@, and LY is
>                            written in the same breath)
> dots 4+   current line    (one more 1-dot sleep, then the real value)

A VBlank line stores -1 before its first sleep instead, so it is genuinely
suppressed from dot 0 until dot 4. Line 0 stores 0 rather than -1, so it never has
a no-match dot.

Getting this shape wrong matters in both directions: treating dots 0-2 as "no
match" loses a real match against the previous line, and treating dot 3 as a match
invents one.
-}
lycCompareValue :: PpuState -> IO (Maybe Word8)
{-# INLINE lycCompareValue #-}
lycCompareValue ps = do
    ly <- readIORef (ppuLy ps)
    dot <- readIORef (ppuDot ps)
    pure $
        if ly >= 144
            then if dot < 4 then Nothing else Just ly
            else
                if dot < 3
                    then Just (if ly == 0 then 153 else ly - 1)
                    else
                        if dot < 4
                            then if ly == 0 then Just 0 else Nothing
                            else Just ly

{- | Dots within the current line at which 'lycCompareValue' changes, and so at
which the STAT line has to be re-sampled.
-}
lycEventDots :: PpuState -> IO [Int]
{-# INLINE lycEventDots #-}
lycEventDots ps = do
    ly <- readIORef (ppuLy ps)
    pure (if ly >= 144 then [4] else [3, 4])

-- | The next dot on this line at which 'lycCompareValue' changes, if any is left.
nextLycEventDot :: PpuState -> IO (Maybe Int)
{-# INLINE nextLycEventDot #-}
nextLycEventDot ps = do
    dots <- lycEventDots ps
    dot <- readIORef (ppuDot ps)
    pure $ case filter (> dot) dots of
        (d : _) -> Just d
        [] -> Nothing

{- | Whether LY compares equal to LYC as the STAT register reports it, honouring
'lycCompareDelay'.

This feeds the register view only. 'computeStatLine' deliberately keeps the raw
compare, for the reason recorded there: the STAT line is sampled at mode
transitions rather than per dot, so suppressing the match here would not delay the
rising edge by a dot, it would defer it to the next transition.
-}
lycMatches :: PpuState -> IO Bool
{-# INLINE lycMatches #-}
lycMatches ps = do
    mv <- lycCompareValue ps
    case mv of
        Nothing -> pure False
        Just v -> do
            lyc <- readIORef (ppuLyc ps)
            pure (v == lyc)

{- | Dots by which the STAT mode bits lag the PPU's actual mode at most boundaries.

SameBoy carries this as a standing note in @display.c@: \"It seems that the STAT
register's mode bits are always late by 4 T-cycles.\" A differential trace agrees:
at the dot-80 mode 2 -> 3 boundary Ocelot reported mode 3 while SameBoy still read
mode 2, switching a few dots later.

It is not uniform across boundaries, despite SameBoy's wording: entry to VBlank
uses 'statVblankModeDelay', and the mode 2 -> 3 report is still about a dot off
against SameBoy (see @tools\/README.md@). Lowering this to 3 is worse, not better.
-}
statModeDelay :: Int
statModeDelay = 4

{- | The same lag on entry to VBlank, which is a dot longer.

SameBoy's line-144 path sleeps 2, then 2, then 1 before @STAT |= 1@, so mode 1
becomes visible 5 dots into the line rather than 4. The four boundaries the mode
bits cross do not all share one offset.
-}
statVblankModeDelay :: Int
statVblankModeDelay = 5

{- | The mode bits as the CPU sees them in STAT, which lag 'ppuMode' by
'statModeDelay' dots, or 'statVblankModeDelay' on entry to VBlank.

Only this register view is delayed. 'computeStatLine' keeps using the real mode,
mirroring SameBoy's separate @mode_for_interrupt@, so interrupt timing is
untouched.

Rather than storing the previous mode, this reconstructs it from the dot at which
the current mode began, which the state machine already determines.

With the LCD off the mode bits read 0 regardless of the internal mode, matching
SameBoy's @GB_lcd_off@ (@STAT &= ~3@). That is not the same as reporting the
internal mode: 'initialPpu' powers on with the LCD off but 'ppuMode' at
'ModeOamScan', so reading the internal mode there would report mode 2 on every
boot-ROM machine before the guest ever enables the LCD.
-}
visibleModeBits :: PpuState -> IO Word8
{-# INLINE visibleModeBits #-}
visibleModeBits ps = do
    lcdc <- readIORef (ppuLcdc ps)
    mode <- readIORef (ppuMode ps)
    if not (testBit lcdc 7)
        then pure 0
        else do
            dot <- readIORef (ppuDot ps)
            case mode of
                ModeOamScan -> do
                    -- Line 0 follows VBlank; every other OAM scan follows an HBlank.
                    ly <- readIORef (ppuLy ps)
                    let prev = if ly == 0 then ModeVBlank else ModeHBlank
                    pure (modeBits (if dot < statModeDelay then prev else mode))
                ModeDrawing -> do
                    -- Through 'oamScanDotsFor', not a local copy of its arithmetic: the state machine
                    -- decides where mode 3 begins, and this has to report the same dot it does.
                    start <- oamScanDotsFor ps
                    firstLine <- readIORef (ppuLcdOnFirstLine ps)
                    -- The enable line reaches mode 3 from its mode-0 window, not from a mode 2.
                    let prev = if firstLine then ModeHBlank else ModeOamScan
                    pure (modeBits (if dot - start < statModeDelay then prev else mode))
                ModeHBlank -> do
                    preDraw <- inLcdOnPreDrawWindow ps
                    if preDraw
                        then pure (modeBits mode) -- pre-draw window: no predecessor to hold
                        else do
                            end <- readIORef (ppuMode3End ps)
                            pure (modeBits (if dot - end < statModeDelay then ModeDrawing else mode))
                ModeVBlank -> do
                    ly <- readIORef (ppuLy ps)
                    let entering = ly == 144 && dot < statVblankModeDelay
                    pure (modeBits (if entering then ModeHBlank else mode))

modeBits :: PpuMode -> Word8
modeBits ModeHBlank = 0
modeBits ModeVBlank = 1
modeBits ModeOamScan = 2
modeBits ModeDrawing = 3

----------------------------------------------------------------------
-- Mode advance
----------------------------------------------------------------------

{- | Advance the PPU by N M-cycles (4N T-cycles). Returns a bitmask of
pending interrupts: bit 0 = VBlank, bit 1 = LCD STAT. Frozen when LCD is off.
-}
advance :: Int -> PpuState -> IO Word8
advance mCycles ps = do
    lcdc <- readIORef (ppuLcdc ps)
    if not (testBit lcdc 7)
        then pure 0
        else stepDots (mCycles * 4) ps 0

stepDots :: Int -> PpuState -> Word8 -> IO Word8
stepDots 0 _ !flags = pure flags
stepDots !n !ps !flags = do
    mode <- readIORef (ppuMode ps)
    dot <- readIORef (ppuDot ps)
    !next <- boundaryFor mode ps
    let !toNext = next - dot
        !consume = min n toNext
        !dot' = dot + consume
    if dot' < next
        then do
            writeIORef (ppuDot ps) dot'
            pure flags
        else do
            !newFlags <- transition mode ps
            stepDots (n - consume) ps (flags .|. newFlags)

-- | Dots of OAM scan (mode 2) at the head of every visible scanline.
oamScanDots :: Int
oamScanDots = 80

-- | Total dots per scanline, in every mode.
scanlineDots :: Int
scanlineDots = 456

{- | Dots after an LCD enable at which mode 3 starts on that first scanline.

Hardware runs no mode 2 there at all. SameBoy holds the STAT mode bits at 0 for
@MODE2_LENGTH - 4@ (76) dots, sleeps 2 more, and only then sets mode 3, so drawing
begins at dot 78. Using 76 here failed blargg @oam_bug\/1-lcd_sync@, which passes at
78; mooneye @acceptance\/ppu\/lcdon_timing-GS@ fails either way and wants the STAT
report no later than its own +76, which no single value of this constant has yet
satisfied. See the open item in @tools\/README.md@ before changing it.
-}
lcdOnPreDrawDots :: Int
lcdOnPreDrawDots = 78

{- | Total dots of the first scanline after an LCD enable, before the DMG extra.
Eight short of a normal line, which is the accounted-but-never-slept @+= 8@ in
SameBoy's first-line path.
-}
lcdOnLineDots :: Int
lcdOnLineDots = 448

{- | DMG spends one further dot before the post-enable scanline starts at all
(SameBoy's @if (!GB_is_cgb(gb)) GB_SLEEP(display, 23, 1)@). It sits outside that
line's own accounting, so it pushes out both the mode 3 start and the line end.
-}
lcdOnDmgExtraDots :: Int
lcdOnDmgExtraDots = 1

-- | The DMG-only extra dot, or zero on CGB.
lcdOnExtraDots :: PpuState -> IO Int
{-# INLINE lcdOnExtraDots #-}
lcdOnExtraDots ps = do
    cgb <- readIORef (ppuCgbMode ps)
    pure (if cgb then 0 else lcdOnDmgExtraDots)

{- | Dot at which mode 3 begins on the line currently being scanned.

On a normal line that is the end of OAM scan, 'oamScanDots'. The first line after
an LCD enable has no mode 2 at all, and there mode 3 begins at 'lcdOnPreDrawDots'
(plus the DMG extra) with the line reporting mode 0 up to that point.
-}
oamScanDotsFor :: PpuState -> IO Int
{-# INLINE oamScanDotsFor #-}
oamScanDotsFor ps = do
    short <- readIORef (ppuLcdOnFirstLine ps)
    if not short
        then pure oamScanDots
        else do
            extra <- lcdOnExtraDots ps
            pure (lcdOnPreDrawDots + extra)

{- | Total dots on the line currently being scanned. The first line after an LCD
enable runs 'lcdOnLineDots' (plus the DMG extra); every other line runs the full
'scanlineDots'.
-}
scanlineDotsFor :: PpuState -> IO Int
{-# INLINE scanlineDotsFor #-}
scanlineDotsFor ps = do
    short <- readIORef (ppuLcdOnFirstLine ps)
    if not short
        then pure scanlineDots
        else do
            extra <- lcdOnExtraDots ps
            pure (lcdOnLineDots + extra)

{- | Mode 3 with no penalties: 12 dots of initial fetch plus 160 pixels.
Everything that stalls the fetcher is added on top by 'mode3Length'.
-}
mode3BaseDots :: Int
mode3BaseDots = 172

{- | Dot at which the current mode ends. Only mode 3 varies, and its end is
latched per line into 'ppuMode3End' when the PPU leaves OAM scan; mode 0 then
simply runs from there to the end of the scanline.
-}

{- | Whether the dot walk is short of this line's LYC-compare event.

The PPU does things partway through a line, not only at mode boundaries, and
'stepDots' has to stop for them or they are invisible. This is the first such
event: 'lycCompareDelay' dots in, @ly_for_comparison@ becomes the line number, so
the LYC STAT source can go high there. Without a stop, 'statEdge' would not run
again until the next mode boundary and the rising edge would land up to 80 dots
late.
-}
atLycCompareDot :: PpuState -> IO Bool
{-# INLINE atLycCompareDot #-}
atLycCompareDot ps = isJust <$> nextLycEventDot ps

{- | Dot at which the current mode ends, or at which the next sub-line event
happens, whichever comes first. 'transition' dispatches on the same predicates.
-}
boundaryFor :: PpuMode -> PpuState -> IO Int
{-# INLINE boundaryFor #-}
boundaryFor ModeDrawing ps = readIORef (ppuMode3End ps)
boundaryFor ModeOamScan ps = do
    next <- nextLycEventDot ps
    case next of
        Just d -> pure d
        Nothing -> oamScanDotsFor ps
boundaryFor ModeHBlank ps = do
    preDraw <- inLcdOnPreDrawWindow ps
    if preDraw then oamScanDotsFor ps else scanlineDotsFor ps
boundaryFor ModeVBlank ps = do
    next <- nextLycEventDot ps
    case next of
        Just d -> pure d
        Nothing -> scanlineDotsFor ps

{- | Whether the PPU is in the mode-0-looking window that opens the first
scanline after the LCD is enabled, before drawing starts.

The mode alone cannot answer this, because that line passes through 'ModeHBlank'
twice: once for this window at the head of the line, and again for the real
HBlank after mode 3. The dot position is what separates them, so 'boundaryFor'
and 'transition' both ask through here to stay in agreement.
-}
inLcdOnPreDrawWindow :: PpuState -> IO Bool
{-# INLINE inLcdOnPreDrawWindow #-}
inLcdOnPreDrawWindow ps = do
    short <- readIORef (ppuLcdOnFirstLine ps)
    if not short
        then pure False
        else do
            dot <- readIORef (ppuDot ps)
            oamEnd <- oamScanDotsFor ps
            pure (dot < oamEnd)

{- | How long mode 3 runs on the line that is about to be drawn.

Three things stall the pixel fetcher, per Pandocs:

* The first @SCX mod 8@ pixels are fetched and discarded, so a non-zero
  fine scroll costs that many dots.
* Activating the window mid-line aborts and restarts the fetcher, costing
  6 dots on the line the window first appears.

Object penalties are not modelled yet, so lines with sprites still report
their sprite-free length; that is what leaves mooneye's
@intr_2_mode0_timing_sprites@ pending.

The result is clamped so it can never land before the end of OAM scan or
past the end of the scanline, which keeps 'stepDots' monotonic even if a
corrupt snapshot restores nonsense.
-}
mode3Length :: PpuState -> IO Int
mode3Length ps = do
    lcdc <- readIORef (ppuLcdc ps)
    scx <- readIORef (ppuScx ps)
    ly <- readIORef (ppuLy ps)
    wy <- readIORef (ppuWy ps)
    wx <- readIORef (ppuWx ps)
    cgb <- readIORef (ppuCgbMode ps)
    let !fineScroll = fromIntegral scx .&. 7
        -- Mirrors 'renderLine': on CGB the BG layer is always active because
        -- LCDC bit 0 means "master priority" there rather than "BG enable".
        bgActive = cgb || testBit lcdc 0
        windowHere =
            testBit lcdc 5
                && bgActive
                && ly >= wy
                && fromIntegral wx <= (166 :: Int)
        !windowPenalty = if windowHere then 6 else 0
        !len = mode3BaseDots + fineScroll + windowPenalty
    -- Clamp against this line's own length, which is shorter on the first line
    -- after the LCD is enabled.
    lineDots <- scanlineDotsFor ps
    start <- oamScanDotsFor ps
    pure (min (lineDots - start - 1) len)

{- | Recompute the latched mode 3 end from the current registers.

'ppuMode3End' is derived state that the mode state machine refreshes once
per line. A snapshot restores the registers but not the latch, so call this
after a load to stop the restored line from running on the previous
machine's value.
-}
resyncMode3End :: PpuState -> IO ()
resyncMode3End ps = do
    len <- mode3Length ps
    start <- oamScanDotsFor ps
    writeIORef (ppuMode3End ps) (start + len)

{- | Transition out of the current mode at its boundary. Returns a bitmask:
bit 0 = VBlank entry, bit 1 = STAT (rising edge of the OR'd STAT line),
bit 2 = HBlank entry (used by the bus to step HDMA, not an interrupt).

Mode and LY are updated first, then the STAT line is sampled and edge-
detected against 'ppuPrevStatLine'. This correctly handles back-to-back
enabled sources (e.g. mode 0 -> mode 2 with both STAT bits set): the
line stays high through the boundary and no second IRQ fires.
-}
transition :: PpuMode -> PpuState -> IO Word8
transition mode ps = case mode of
    ModeOamScan -> do
        atLyc <- atLycCompareDot ps
        if atLyc then lycCompareEvent ps else oamScanEnd ps
    ModeDrawing -> do
        renderLine ps
        writeIORef (ppuMode ps) ModeHBlank
        end <- readIORef (ppuMode3End ps)
        writeIORef (ppuDot ps) end
        s <- statEdge ps
        pure (s .|. 0x04) -- Bit 2: HBlank entered (consumed by Bus for HDMA).
    ModeHBlank -> do
        preDraw <- inLcdOnPreDrawWindow ps
        if preDraw then lcdOnPreDrawEnd ps else hblankLineEnd ps
    ModeVBlank -> do
        atLyc <- atLycCompareDot ps
        if atLyc then lycCompareEvent ps else vblankLineEnd ps

{- | The LYC-compare event partway into a line: @ly_for_comparison@ takes the line
number, so re-sample the STAT line here rather than waiting for the next mode
boundary. Stays in the same mode, only the dot moves.
-}
lycCompareEvent :: PpuState -> IO Word8
lycCompareEvent ps = do
    next <- nextLycEventDot ps
    mapM_ (writeIORef (ppuDot ps)) next
    statEdge ps

-- | End of mode 2: latch mode 3's length and start drawing.
oamScanEnd :: PpuState -> IO Word8
oamScanEnd ps = do
    -- Latch this line's mode 3 length now: the registers it depends on
    -- (SCX, WY/WX, LCDC) are sampled at the start of drawing, so a
    -- mid-line write must not retroactively move the mode 0 boundary.
    resyncMode3End ps
    writeIORef (ppuMode ps) ModeDrawing
    -- 'oamScanDotsFor', not the 'oamScanDots' constant, so this stays correct if
    -- the caller is ever reached with the enable-line latch set. On the enable
    -- line itself the PPU leaves the mode-0 window through 'lcdOnPreDrawEnd'
    -- instead, so in practice this is always the plain 80.
    start <- oamScanDotsFor ps
    writeIORef (ppuDot ps) start
    statEdge ps

-- | End of a VBlank scanline: advance LY, wrapping to a new frame after line 153.
vblankLineEnd :: PpuState -> IO Word8
vblankLineEnd ps = do
    ly <- readIORef (ppuLy ps)
    let ly' = ly + 1
    if ly' == 154
        then do
            writeIORef (ppuMode ps) ModeOamScan
            writeIORef (ppuLy ps) 0
            writeIORef (ppuDot ps) 0
            writeIORef (ppuWindowLine ps) 0 -- New frame resets WLY.
            statEdge ps
        else do
            writeIORef (ppuLy ps) ly'
            writeIORef (ppuDot ps) 0
            statEdge ps

{- | End of the mode-0-looking window that opens the first scanline after the LCD
is enabled. Hardware skips mode 2 entirely on this line, so this goes straight to
mode 3 rather than advancing to the next line.
-}
lcdOnPreDrawEnd :: PpuState -> IO Word8
lcdOnPreDrawEnd ps = do
    -- Latch mode 3's length for this line exactly as leaving mode 2 would.
    resyncMode3End ps
    writeIORef (ppuMode ps) ModeDrawing
    oamEnd <- oamScanDotsFor ps
    writeIORef (ppuDot ps) oamEnd
    statEdge ps

-- | End of a visible scanline: advance LY, entering VBlank after line 143.
hblankLineEnd :: PpuState -> IO Word8
hblankLineEnd ps = do
    -- The short first-line-after-LCD-on applies to this line only.
    writeIORef (ppuLcdOnFirstLine ps) False
    ly <- readIORef (ppuLy ps)
    let ly' = ly + 1
    if ly' == 144
        then do
            writeIORef (ppuMode ps) ModeVBlank
            writeIORef (ppuLy ps) 144
            writeIORef (ppuDot ps) 0
            s <- statEdge ps
            pure (0x01 .|. s)
        else do
            writeIORef (ppuMode ps) ModeOamScan
            writeIORef (ppuLy ps) ly'
            writeIORef (ppuDot ps) 0
            statEdge ps

{- | Compute the OR of all enabled STAT interrupt sources and update the
edge-detector. Returns @0x02@ on a low->high transition of the OR'd
line (indicating the bus should set IF bit 1), otherwise @0@.
-}
statEdge :: PpuState -> IO Word8
statEdge ps = do
    new <- computeStatLine ps
    prev <- readIORef (ppuPrevStatLine ps)
    writeIORef (ppuPrevStatLine ps) new
    pure (if new && not prev then 0x02 else 0)

{- | Sample the STAT line after a register write (STAT, LYC, or LCDC).
If the line just went low->high, latch a pending IRQ for the bus to
consume via 'takePendingStatIrq'. Without this, edges driven by direct
register writes (e.g. enabling STAT bit 6 while LY already equals LYC)
would never reach the IF flag, since the only other edge-detection
path is the per-mode-transition 'statEdge' inside 'transition'.
-}
sampleStatLine :: PpuState -> IO ()
sampleStatLine ps = do
    edge <- statEdge ps
    when (edge /= 0) (writeIORef (ppuPendingStatIrq ps) True)

{- | Read and clear the pending-STAT-IRQ flag. Called by the bus right
after each PPU register write that might have driven a rising edge.
-}
takePendingStatIrq :: PpuState -> IO Bool
takePendingStatIrq ps = do
    p <- readIORef (ppuPendingStatIrq ps)
    when p (writeIORef (ppuPendingStatIrq ps) False)
    pure p

{- | The current value of the OR'd STAT interrupt request line. Held low
while the LCD is off (LCDC bit 7 clear).
-}
computeStatLine :: PpuState -> IO Bool
computeStatLine ps = do
    lcdc <- readIORef (ppuLcdc ps)
    if not (testBit lcdc 7)
        then pure False
        else do
            mode <- readIORef (ppuMode ps)
            stat <- readIORef (ppuStat ps)
            ly <- readIORef (ppuLy ps)
            -- Honours the LYC suppression window. Safe now that 'atLycCompareDot'
            -- makes 'stepDots' stop at the compare dot, so the rising edge is seen
            -- there rather than deferred to the next mode boundary.
            match <- lycMatches ps
            -- The OAM-scan STAT source (bit 5) is also asserted on the
            -- first scanline of VBlank (LY=144), per the documented DMG
            -- quirk. Subsequent VBlank lines (145-153) only see bit 4.
            let modeSrc = case mode of
                    ModeHBlank -> testBit stat 3
                    ModeVBlank ->
                        testBit stat 4 || (ly == 144 && testBit stat 5)
                    ModeOamScan -> testBit stat 5
                    ModeDrawing -> False
                lycSrc = testBit stat 6 && match
            pure (modeSrc || lycSrc)

----------------------------------------------------------------------
-- Line renderer (BG + window + sprites)
----------------------------------------------------------------------

data LineRenderContext = LineRenderContext
    { lineLy :: !Int
    , lineCgb :: !Bool
    , lineBgActive :: !Bool
    , lineWinEnabled :: !Bool
    , lineBgp :: !Word8
    , lineWly :: !Int
    , lineWy :: !Int
    , lineWx :: !Int
    , lineScy :: !Int
    , lineScx :: !Int
    , lineBgMapBase :: !Int
    , lineWindowMapBase :: !Int
    , lineUnsignedTiles :: !Bool
    , lineRenderMode :: !CgbRenderMode
    }

data SpriteLineContext = SpriteLineContext
    { spriteLineMasterOn :: !Bool
    , spriteLineHeight :: !Int
    , spriteLineObp0 :: !Word8
    , spriteLineObp1 :: !Word8
    , spriteLineCandidates :: ![Sprite]
    }

renderLine :: PpuState -> IO ()
renderLine ps = do
    ly <- readIORef (ppuLy ps)
    lcdc <- readIORef (ppuLcdc ps)
    cgb <- readIORef (ppuCgbMode ps)
    let lyI = fromIntegral ly :: Int
        bgWinEnabled = testBit lcdc 0
        -- On CGB, LCDC bit 0 has a different meaning (master priority);
        -- treat the BG layer as always enabled in CGB mode.
        bgActive = cgb || bgWinEnabled
        winEnabled = testBit lcdc 5 && bgActive
        spritesEnabled = testBit lcdc 1
        unsignedTiles = testBit lcdc 4
        bgMapBase = if testBit lcdc 3 then 0x1C00 else 0x1800
        windowMapBase = if testBit lcdc 6 then 0x1C00 else 0x1800
    bgp <- readIORef (ppuBgp ps)
    scy <- fromIntegral <$> readIORef (ppuScy ps) :: IO Int
    scx <- fromIntegral <$> readIORef (ppuScx ps) :: IO Int
    -- Snapshot WLY for this line so mid-line increments don't leak into
    -- the same scanline's pixel addressing.
    wly <- readIORef (ppuWindowLine ps)
    wy <- fromIntegral <$> readIORef (ppuWy ps) :: IO Int
    wx <- fromIntegral <$> readIORef (ppuWx ps) :: IO Int
    renderMode <- readIORef (ppuRenderMode ps)
    let windowOnThisLine = winEnabled && lyI >= wy && wx <= 166
        ctx =
            LineRenderContext
                { lineLy = lyI
                , lineCgb = cgb
                , lineBgActive = bgActive
                , lineWinEnabled = winEnabled
                , lineBgp = bgp
                , lineWly = wly
                , lineWy = wy
                , lineWx = wx
                , lineScy = scy
                , lineScx = scx
                , lineBgMapBase = bgMapBase
                , lineWindowMapBase = windowMapBase
                , lineUnsignedTiles = unsignedTiles
                , lineRenderMode = renderMode
                }
    spriteCtx <-
        if spritesEnabled
            then Just <$> prepareSpriteLine ps cgb lyI lcdc
            else pure Nothing
    renderPixelsForLine ps ctx spriteCtx
    -- Increment WLY if the window was actually drawn this line.
    when windowOnThisLine (writeIORef (ppuWindowLine ps) (wly + 1))

renderPixelsForLine :: PpuState -> LineRenderContext -> Maybe SpriteLineContext -> IO ()
renderPixelsForLine ps ctx mSpriteCtx = do
    !target <- readIORef (ppuFbTarget ps)
    go target 0
  where
    fbBase = lineLy ctx * framebufferWidth
    rgbBase = lineLy ctx * framebufferWidth * 3
    rgbaBase = lineLy ctx * framebufferWidth * 4

    go !target !x
        | x >= framebufferWidth = pure ()
        | otherwise = do
            (!bgIdx, !bgAttr) <- bgPixelAt ps ctx x
            let !bgShade = paletteApply (lineBgp ctx) bgIdx
            (!finalShade, mHit) <- case mSpriteCtx of
                Just spriteCtx ->
                    resolveSpritePixel ps ctx spriteCtx x bgIdx bgAttr bgShade
                Nothing -> pure (bgShade, Nothing)
            unless (target == FbRgba) $
                MV.write (ppuFb ps) (fbBase + x) finalShade
            rgb <- pixelRgb ps (lineRenderMode ctx) bgIdx bgAttr finalShade mHit
            writeRgbPixel target (rgbBase + x * 3) (rgbaBase + x * 4) rgb
            go target (x + 1)

    writeRgbPixel FbRgb rgbOff _ (r, g, b) = do
        MV.write (ppuFbRgb ps) rgbOff r
        MV.write (ppuFbRgb ps) (rgbOff + 1) g
        MV.write (ppuFbRgb ps) (rgbOff + 2) b
    writeRgbPixel FbRgba _ rgbaOff (r, g, b) = do
        VSM.write (ppuFbRgba ps) rgbaOff r
        VSM.write (ppuFbRgba ps) (rgbaOff + 1) g
        VSM.write (ppuFbRgba ps) (rgbaOff + 2) b
        VSM.write (ppuFbRgba ps) (rgbaOff + 3) 255
    writeRgbPixel FbBoth rgbOff rgbaOff (r, g, b) = do
        MV.write (ppuFbRgb ps) rgbOff r
        MV.write (ppuFbRgb ps) (rgbOff + 1) g
        MV.write (ppuFbRgb ps) (rgbOff + 2) b
        VSM.write (ppuFbRgba ps) rgbaOff r
        VSM.write (ppuFbRgba ps) (rgbaOff + 1) g
        VSM.write (ppuFbRgba ps) (rgbaOff + 2) b
        VSM.write (ppuFbRgba ps) (rgbaOff + 3) 255

bgPixelAt :: PpuState -> LineRenderContext -> Int -> IO (Word8, Word8)
bgPixelAt ps ctx x
    | not (lineBgActive ctx) = pure (0, 0)
    | inWindow =
        tilePixelAt
            ps
            (lineCgb ctx)
            (lineUnsignedTiles ctx)
            (lineWindowMapBase ctx)
            (x + 7 - lineWx ctx)
            (lineWly ctx)
    | otherwise =
        tilePixelAt
            ps
            (lineCgb ctx)
            (lineUnsignedTiles ctx)
            (lineBgMapBase ctx)
            ((lineScx ctx + x) .&. 0xFF)
            ((lineScy ctx + lineLy ctx) .&. 0xFF)
  where
    inWindow =
        lineWinEnabled ctx
            && lineLy ctx >= lineWy ctx
            && (x + 7) >= lineWx ctx

tilePixelAt :: PpuState -> Bool -> Bool -> Int -> Int -> Int -> IO (Word8, Word8)
tilePixelAt ps cgb unsigned mapBase col row = do
    let !tileX = col `shiftR` 3
        !tileY = row `shiftR` 3
        !mapIdx = mapBase + tileY * 32 + tileX
        vram = ppuVram ps
    tileNum <- MV.read vram mapIdx
    attr <-
        if cgb
            then MV.read vram (0x2000 + mapIdx)
            else pure 0
    let hflip = cgb && testBit attr 5
        vflip = cgb && testBit attr 6
        tileBank = if cgb && testBit attr 3 then 0x2000 else 0
        rowInTile0 = row .&. 7
        rowInTile = if vflip then 7 - rowInTile0 else rowInTile0
        !tileBase =
            if unsigned
                then fromIntegral tileNum * 16
                else 0x1000 + fromIntegral (fromIntegral tileNum :: Int8) * 16
        !rowOff = tileBank + tileBase + rowInTile * 2
    byteLow <- MV.read vram rowOff
    byteHigh <- MV.read vram (rowOff + 1)
    let colInTile0 = col .&. 7
        colInTile = if hflip then colInTile0 else 7 - colInTile0
        idx =
            (if testBit byteHigh colInTile then 2 else 0)
                + (if testBit byteLow colInTile then 1 else 0)
    pure (idx, attr)

pixelRgb ::
    PpuState ->
    CgbRenderMode ->
    Word8 ->
    Word8 ->
    Word8 ->
    Maybe (Sprite, Word8) ->
    IO (Word8, Word8, Word8)
pixelRgb ps mode bgIdx bgAttr finalShade mHit = case mode of
    RenderDmg -> pure (dmgShadeRgb finalShade)
    RenderCgbCompat ->
        case mHit of
            Just (s, _) ->
                let pal = if testBit (spriteAttr s) 4 then 1 else 0
                 in cgbPalRgb (ppuObjPalRam ps) pal finalShade
            Nothing -> cgbPalRgb (ppuBgPalRam ps) 0 finalShade
    RenderCgbFull ->
        case mHit of
            Just (s, sIdx) -> cgbObjRgb ps (spriteAttr s) sIdx
            Nothing -> cgbBgRgb ps bgAttr bgIdx

{- | Look up a CGB palette color from a palette RAM IOVector by
(palette index 0..7, color index 0..3).
-}
cgbPalRgb :: IOVector Word8 -> Int -> Word8 -> IO (Word8, Word8, Word8)
cgbPalRgb pal palIdx colorIdx = do
    let off = palIdx * 8 + fromIntegral colorIdx * 2
    lo <- MV.read pal off
    hi <- MV.read pal (off + 1)
    pure (rgb555ToRgb888 lo hi)

-- | Look up a BG pixel's CGB color from its attribute byte and color index.
cgbBgRgb :: PpuState -> Word8 -> Word8 -> IO (Word8, Word8, Word8)
cgbBgRgb ps attr colorIdx = do
    let pal = fromIntegral (attr .&. 0x07) :: Int
        off = pal * 8 + fromIntegral colorIdx * 2
    lo <- MV.read (ppuBgPalRam ps) off
    hi <- MV.read (ppuBgPalRam ps) (off + 1)
    pure (rgb555ToRgb888 lo hi)

{- | Look up a sprite pixel's CGB color from its OAM attribute byte and
color index. Bits 0..2 of @attr@ select OBJ palette 0..7.
-}
cgbObjRgb :: PpuState -> Word8 -> Word8 -> IO (Word8, Word8, Word8)
cgbObjRgb ps attr colorIdx = do
    let pal = fromIntegral (attr .&. 0x07) :: Int
        off = pal * 8 + fromIntegral colorIdx * 2
    lo <- MV.read (ppuObjPalRam ps) off
    hi <- MV.read (ppuObjPalRam ps) (off + 1)
    pure (rgb555ToRgb888 lo hi)

-- | Apply a DMG palette register to a 2-bit color index.
paletteApply :: Word8 -> Word8 -> Word8
paletteApply pal idx = (pal `shiftR` (fromIntegral idx `shiftL` 1)) .&. 0x03

----------------------------------------------------------------------
-- Sprite overlay
----------------------------------------------------------------------

data Sprite = Sprite
    { spriteY :: !Int
    , spriteX :: !Int
    , spriteTile :: !Word8
    , spriteAttr :: !Word8
    , spriteOam :: !Int
    }

prepareSpriteLine :: PpuState -> Bool -> Int -> Word8 -> IO SpriteLineContext
prepareSpriteLine ps cgb ly lcdc = do
    let height = if testBit lcdc 2 then 16 else 8
        masterOn = testBit lcdc 0
    candidates <- readVisibleSprites (ppuOam ps) ly height
    opri <- readIORef (ppuOpri ps)
    obp0 <- readIORef (ppuObp0 ps)
    obp1 <- readIORef (ppuObp1 ps)
    let xOrder = not cgb || testBit opri 0
        sorted = if xOrder then stableSortByX candidates else candidates
    pure
        SpriteLineContext
            { spriteLineMasterOn = masterOn
            , spriteLineHeight = height
            , spriteLineObp0 = obp0
            , spriteLineObp1 = obp1
            , spriteLineCandidates = sorted
            }

readVisibleSprites :: IOVector Word8 -> Int -> Int -> IO [Sprite]
readVisibleSprites oam ly height = go (0 :: Int) (0 :: Int) []
  where
    go !i !found !acc
        | i >= 40 || found >= 10 = pure (reverse acc)
        | otherwise = do
            y <- MV.read oam (i * 4)
            x <- MV.read oam (i * 4 + 1)
            t <- MV.read oam (i * 4 + 2)
            a <- MV.read oam (i * 4 + 3)
            let sprite =
                    Sprite
                        { spriteY = fromIntegral y - 16
                        , spriteX = fromIntegral x - 8
                        , spriteTile = t
                        , spriteAttr = a
                        , spriteOam = i
                        }
            if overlapsLine ly height sprite
                then go (i + 1) (found + 1) (sprite : acc)
                else go (i + 1) found acc

{- | Overlay sprites on top of the background. Per pixel returns:

* The shade to write to the palette-index framebuffer.
* 'Just (sprite, colorIndex)' when a sprite pixel won, so the RGB pass
  can look up the CGB OBJ palette; 'Nothing' when the BG won.

CGB priority arbitration considers three sources:

* LCDC bit 0: when 0 in CGB mode, the master \"BG\/Window has no
  priority\" override forces OBJ to win (subject to BG transparency).
* BG attribute bit 7: per-tile \"BG over OBJ\" flag.
* OAM attribute bit 7: per-sprite \"behind BG colors 1-3\" flag.

Object wins iff master-priority-off, or BG is transparent, or neither
of the BG\/OBJ priority bits is set.
-}
resolveSpritePixel ::
    PpuState ->
    LineRenderContext ->
    SpriteLineContext ->
    Int ->
    Word8 ->
    Word8 ->
    Word8 ->
    IO (Word8, Maybe (Sprite, Word8))
resolveSpritePixel ps ctx spriteCtx x bgIdx bgAttr bgShade = do
    hit <-
        foreMostHit
            (spriteLineHeight spriteCtx)
            (spriteLineCandidates spriteCtx)
            x
    case hit of
        Nothing -> pure (bgShade, Nothing)
        Just (s, sIdx) ->
            let objPriority = testBit (spriteAttr s) 7
                bgPriority = lineCgb ctx && testBit bgAttr 7
                bgOpaque = bgIdx > 0
                masterOff = lineCgb ctx && not (spriteLineMasterOn spriteCtx)
                objWins =
                    masterOff
                        || not bgOpaque
                        || not (objPriority || bgPriority)
             in if objWins
                    then
                        let pal =
                                if testBit (spriteAttr s) 4
                                    then spriteLineObp1 spriteCtx
                                    else spriteLineObp0 spriteCtx
                         in pure (paletteApply pal sIdx, Just (s, sIdx))
                    else pure (bgShade, Nothing)
  where
    foreMostHit :: Int -> [Sprite] -> Int -> IO (Maybe (Sprite, Word8))
    foreMostHit _ [] _ = pure Nothing
    foreMostHit height (s : rest) px
        | px < spriteX s || px >= spriteX s + 8 = foreMostHit height rest px
        | otherwise = do
            mIdx <- spritePixelIdx (ppuVram ps) (lineCgb ctx) (lineLy ctx) height s px
            case mIdx of
                Just idx | idx /= 0 -> pure (Just (s, idx))
                _ -> foreMostHit height rest px

overlapsLine :: Int -> Int -> Sprite -> Bool
overlapsLine ly height s =
    ly >= spriteY s && ly < spriteY s + height

{- | Stable sort by sprite X coordinate. Leftmost sprite wins; equal-X
sprites keep their original OAM order (lower index earlier). Uses a
simple insertion sort: the list holds at most 10 elements so merge sort's
O(n log n) intermediate allocation is more expensive than O(n^2) here.
-}
stableSortByX :: [Sprite] -> [Sprite]
stableSortByX = foldr insertByX []

insertByX :: Sprite -> [Sprite] -> [Sprite]
{-# INLINE insertByX #-}
insertByX x [] = [x]
insertByX x (y : ys)
    | spriteX x <= spriteX y = x : y : ys
    | otherwise = y : insertByX x ys

spritePixelIdx :: IOVector Word8 -> Bool -> Int -> Int -> Sprite -> Int -> IO (Maybe Word8)
spritePixelIdx vram cgb ly height s x
    | x < spriteX s || x >= spriteX s + 8 = pure Nothing
    | otherwise = do
        let !attr = spriteAttr s
            !xFlip = testBit attr 5
            !yFlip = testBit attr 6
            !xInSprite = x - spriteX s
            !yInSprite = ly - spriteY s
            !xPx = if xFlip then 7 - xInSprite else xInSprite
            !yPx = if yFlip then height - 1 - yInSprite else yInSprite
            !tileBaseIdx =
                if height == 16
                    then
                        (fromIntegral (spriteTile s .&. 0xFE) :: Int)
                            + (if yPx >= 8 then 1 else 0)
                    else fromIntegral (spriteTile s) :: Int
            -- CGB OAM attribute bit 3 selects the VRAM bank for sprite tile data.
            !tileBank = if cgb && testBit attr 3 then 0x2000 else 0
            !yInTile = yPx .&. 7
            !rowOff = tileBank + tileBaseIdx * 16 + yInTile * 2
        byteLow <- MV.read vram rowOff
        byteHigh <- MV.read vram (rowOff + 1)
        let !bit = 7 - xPx
            !idx =
                (if testBit byteHigh bit then 2 else 0)
                    + (if testBit byteLow bit then 1 else 0)
        pure (Just idx)
