{-# LANGUAGE BangPatterns #-}

{- | Game Boy Picture Processing Unit (DMG only).

The PPU owns its own VRAM (8 KiB at @0x8000-0x9FFF@), OAM (160 bytes at
@0xFE00-0xFE9F@), and the register file at @0xFF40-0xFF4B@. The bus
dispatches reads and writes for those ranges to 'read8' and 'write8'.

Mode timing follows the standard scanline structure:

> Mode 2 (OAM scan): T-cycles   0..79  (80)
> Mode 3 (drawing) : T-cycles  80..251 (172)
> Mode 0 (HBlank)  : T-cycles 252..455 (204)
> Mode 1 (VBlank)  : 10 lines * 456 T-cycles = 4560 T-cycles

State is held in 'IORef's and 'IOVector's so reads and writes are O(1) and
the rendered framebuffer is updated in place.

What is implemented: the mode state machine and LY counter; background;
window (LY-WY approximation); sprites with the DMG sort-by-X priority,
8x8 / 8x16 sizes, X/Y flip, OBP0/OBP1, and BG-priority bit; BGP/OBP palette
transforms; VBlank interrupt edge; LCD-off freeze.

Not implemented: STAT-source interrupts, mode-3 timing variability, the
proper window line counter, the OAM DMA delay (the bus copies instantly).
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
    framebufferWidth,
    framebufferHeight,
    setCgbMode,
    setCgbRenderMode,
    FbTarget (..),
    setFbTarget,
    takePendingStatIrq,
) where

import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int8)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)
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
Set once at startup via 'setFbTarget' to skip writing the buffer
that the current frontend does not read, reducing per-scanline
memory traffic.
-}
data FbTarget
    = -- | Write only 'ppuFbRgb' (RGB888). Used by the SDL desktop frontend.
      FbRgb
    | -- | Write only 'ppuFbRgba' (RGBA8888). Used by the web WASM frontend.
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
    , ppuFbRgba :: !(IOVector Word8)
    -- ^ RGBA8888 color framebuffer (160 * 144 * 4 bytes). Kept so the
    -- web frontend can copy a presentation-ready buffer without
    -- rebuilding alpha-expanded pixels every frame.
    , ppuFbTarget :: !(IORef FbTarget)
    -- ^ Which color framebuffer(s) to populate. Set via 'setFbTarget'.
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
    -- Hardware power-on: LCD off. Callers that want the post-boot
    -- handoff (LCDC=0x91, LCD on) set it explicitly via 'write8'.
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
    windowLine <- newIORef 0
    vram <- MV.replicate 0x4000 0
    oam <- MV.replicate 0xA0 0
    fb <- MV.replicate (framebufferWidth * framebufferHeight) 0
    fbRgb <- MV.replicate (framebufferWidth * framebufferHeight * 3) 0
    fbRgba <- MV.replicate (framebufferWidth * framebufferHeight * 4) 0
    let initAlpha !i
            | i >= framebufferWidth * framebufferHeight = pure ()
            | otherwise = do
                MV.write fbRgba (i * 4 + 3) 255
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

-- | Copy the RGB framebuffer into a caller-provided buffer in RGBA8888 order.
copyFramebufferRgba :: Ptr Word8 -> PpuState -> IO ()
copyFramebufferRgba ptr ps = go 0
  where
    rgbaBytes = framebufferWidth * framebufferHeight * 4
    go !i
        | i >= rgbaBytes = pure ()
        | otherwise = do
            px <- MV.unsafeRead (ppuFbRgba ps) i
            pokeByteOff ptr i px
            go (i + 1)

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
        mode <- readIORef (ppuMode ps)
        ly <- readIORef (ppuLy ps)
        lyc <- readIORef (ppuLyc ps)
        let lyMatch = if ly == lyc then 0x04 else 0
        pure ((stat .&. 0x78) .|. modeBits mode .|. lyMatch .|. 0x80)
    | addr == 0xFF42 = readIORef (ppuScy ps)
    | addr == 0xFF43 = readIORef (ppuScx ps)
    | addr == 0xFF44 = readIORef (ppuLy ps)
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
    -- LCD turning back on: real hardware starts a fresh frame at mode 2
    -- (OAM scan), LY=0, dot=0. Without this reset the PPU resumes from
    -- 'ModeHBlank' (where 'unless (testBit v 7)' just put it during the
    -- preceding LCD-off), which leaves the first 456 dots after re-enable
    -- as one elongated HBlank: real CGB games that toggle LCD on/off per
    -- frame end up with their first scanline never re-entering OAM scan,
    -- which serialises into "BG never renders" on the very first frame.
    when (not (testBit prev 7) && testBit v 7) $ do
        writeIORef (ppuMode ps) ModeOamScan
        writeIORef (ppuLy ps) 0
        writeIORef (ppuDot ps) 0
        writeIORef (ppuWindowLine ps) 0
    sampleStatLine ps

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
    let !next = nextBoundary mode
        !toNext = next - dot
        !consume = min n toNext
        !dot' = dot + consume
    if dot' < next
        then do
            writeIORef (ppuDot ps) dot'
            pure flags
        else do
            !newFlags <- transition mode ps
            stepDots (n - consume) ps (flags .|. newFlags)

nextBoundary :: PpuMode -> Int
nextBoundary ModeOamScan = 80
nextBoundary ModeDrawing = 252
nextBoundary ModeHBlank = 456
nextBoundary ModeVBlank = 456

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
        writeIORef (ppuMode ps) ModeDrawing
        writeIORef (ppuDot ps) 80
        statEdge ps
    ModeDrawing -> do
        renderLine ps
        writeIORef (ppuMode ps) ModeHBlank
        writeIORef (ppuDot ps) 252
        s <- statEdge ps
        pure (s .|. 0x04) -- Bit 2: HBlank entered (consumed by Bus for HDMA).
    ModeHBlank -> do
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
    ModeVBlank -> do
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
            lyc <- readIORef (ppuLyc ps)
            -- The OAM-scan STAT source (bit 5) is also asserted on the
            -- first scanline of VBlank (LY=144), per the documented DMG
            -- quirk. Subsequent VBlank lines (145-153) only see bit 4.
            let modeSrc = case mode of
                    ModeHBlank -> testBit stat 3
                    ModeVBlank ->
                        testBit stat 4 || (ly == 144 && testBit stat 5)
                    ModeOamScan -> testBit stat 5
                    ModeDrawing -> False
                lycSrc = testBit stat 6 && ly == lyc
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
            MV.write (ppuFb ps) (fbBase + x) finalShade
            rgb <- pixelRgb ps (lineRenderMode ctx) bgIdx bgAttr finalShade mHit
            writeRgbPixel target (rgbBase + x * 3) (rgbaBase + x * 4) rgb
            go target (x + 1)

    writeRgbPixel FbRgb rgbOff _ (r, g, b) = do
        MV.write (ppuFbRgb ps) rgbOff r
        MV.write (ppuFbRgb ps) (rgbOff + 1) g
        MV.write (ppuFbRgb ps) (rgbOff + 2) b
    writeRgbPixel FbRgba _ rgbaOff (r, g, b) = do
        MV.write (ppuFbRgba ps) rgbaOff r
        MV.write (ppuFbRgba ps) (rgbaOff + 1) g
        MV.write (ppuFbRgba ps) (rgbaOff + 2) b
        MV.write (ppuFbRgba ps) (rgbaOff + 3) 255
    writeRgbPixel FbBoth rgbOff rgbaOff (r, g, b) = do
        MV.write (ppuFbRgb ps) rgbOff r
        MV.write (ppuFbRgb ps) (rgbOff + 1) g
        MV.write (ppuFbRgb ps) (rgbOff + 2) b
        MV.write (ppuFbRgba ps) rgbaOff r
        MV.write (ppuFbRgba ps) (rgbaOff + 1) g
        MV.write (ppuFbRgba ps) (rgbaOff + 2) b
        MV.write (ppuFbRgba ps) (rgbaOff + 3) 255

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
