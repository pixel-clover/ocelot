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
    initialPpu,
    read8,
    write8,
    advance,
    framebuffer,
    framebufferRgb,
    framebufferWidth,
    framebufferHeight,
    setCgbMode,
) where

import Control.Monad (when)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int8)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)

framebufferWidth, framebufferHeight :: Int
framebufferWidth = 160
framebufferHeight = 144

data PpuMode
    = ModeHBlank
    | ModeVBlank
    | ModeOamScan
    | ModeDrawing
    deriving (Eq, Show, Enum, Bounded)

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
    , ppuCgbMode :: !(IORef Bool)
    -- ^ Whether the bus is running a CGB cart. Set once at startup
    -- via 'setCgbMode'; rendering reads this to pick the BG path.
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
    }

initialPpu :: IO PpuState
initialPpu = do
    lcdc <- newIORef 0x91
    stat <- newIORef 0x00
    ly <- newIORef 0x00
    lyc <- newIORef 0x00
    scy <- newIORef 0x00
    scx <- newIORef 0x00
    wy <- newIORef 0x00
    wx <- newIORef 0x00
    bgp <- newIORef 0xFC
    obp0 <- newIORef 0xFF
    obp1 <- newIORef 0xFF
    mode <- newIORef ModeOamScan
    dot <- newIORef 0
    vram <- MV.replicate 0x4000 0
    oam <- MV.replicate 0xA0 0
    fb <- MV.replicate (framebufferWidth * framebufferHeight) 0
    fbRgb <- MV.replicate (framebufferWidth * framebufferHeight * 3) 0
    cgbMode <- newIORef False
    vbk <- newIORef 0
    bcps <- newIORef 0
    ocps <- newIORef 0
    bgPal <- MV.replicate 0x40 0xFF
    objPal <- MV.replicate 0x40 0xFF
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
            , ppuVram = vram
            , ppuOam = oam
            , ppuFb = fb
            , ppuFbRgb = fbRgb
            , ppuCgbMode = cgbMode
            , ppuVbk = vbk
            , ppuBcps = bcps
            , ppuOcps = ocps
            , ppuBgPalRam = bgPal
            , ppuObjPalRam = objPal
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

{- | Tell the PPU whether it's running a CGB cart (called once by the
bus at startup). Affects BG rendering only.
-}
setCgbMode :: Bool -> PpuState -> IO ()
setCgbMode b ps = writeIORef (ppuCgbMode ps) b

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
        MV.read (ppuVram ps) (bank + fromIntegral (addr - 0x8000))
    | addr >= 0xFE00 && addr <= 0xFE9F =
        MV.read (ppuOam ps) (fromIntegral (addr - 0xFE00))
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
    | addr == 0xFF4F = (\x -> x .|. 0xFE) <$> readIORef (ppuVbk ps)
    | addr == 0xFF68 = readIORef (ppuBcps ps)
    | addr == 0xFF69 = do
        ix <- readIORef (ppuBcps ps)
        MV.read (ppuBgPalRam ps) (fromIntegral (ix .&. 0x3F))
    | addr == 0xFF6A = readIORef (ppuOcps ps)
    | addr == 0xFF6B = do
        ix <- readIORef (ppuOcps ps)
        MV.read (ppuObjPalRam ps) (fromIntegral (ix .&. 0x3F))
    | otherwise = pure 0xFF

write8 :: Word16 -> Word8 -> PpuState -> IO ()
write8 addr !v ps
    | addr <= 0x9FFF = do
        bank <- vramBankIndex ps
        MV.write (ppuVram ps) (bank + fromIntegral (addr - 0x8000)) v
    | addr >= 0xFE00 && addr <= 0xFE9F =
        MV.write (ppuOam ps) (fromIntegral (addr - 0xFE00)) v
    | addr == 0xFF40 = handleLcdcWrite v ps
    | addr == 0xFF41 =
        modifyIORef' (ppuStat ps) (\s -> (v .&. 0x78) .|. (s .&. 0x07))
    | addr == 0xFF42 = writeIORef (ppuScy ps) v
    | addr == 0xFF43 = writeIORef (ppuScx ps) v
    | addr == 0xFF44 = pure () -- LY is read-only
    | addr == 0xFF45 = writeIORef (ppuLyc ps) v
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
    writeIORef (ppuLcdc ps) v
    -- LCD turning off freezes LY at 0 in Mode 0.
    if not (testBit v 7)
        then do
            writeIORef (ppuLy ps) 0
            writeIORef (ppuMode ps) ModeHBlank
            writeIORef (ppuDot ps) 0
        else pure ()

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
bit 0 = VBlank entry, bit 1 = STAT (one of LYC=LY match, mode 0/1/2 entry,
gated by STAT bits 3-6).
-}
transition :: PpuMode -> PpuState -> IO Word8
transition mode ps = case mode of
    ModeOamScan -> do
        writeIORef (ppuMode ps) ModeDrawing
        writeIORef (ppuDot ps) 80
        pure 0 -- Mode 3 has no STAT source.
    ModeDrawing -> do
        renderLine ps
        writeIORef (ppuMode ps) ModeHBlank
        writeIORef (ppuDot ps) 252
        statForMode ps 3 -- Mode 0 (HBlank) STAT enable
    ModeHBlank -> do
        ly <- readIORef (ppuLy ps)
        let ly' = ly + 1
        if ly' == 144
            then do
                writeIORef (ppuMode ps) ModeVBlank
                writeIORef (ppuLy ps) 144
                writeIORef (ppuDot ps) 0
                stat1 <- statForMode ps 4 -- Mode 1 (VBlank) STAT enable
                lyc <- statForLyc ps
                pure (0x01 .|. stat1 .|. lyc)
            else do
                writeIORef (ppuMode ps) ModeOamScan
                writeIORef (ppuLy ps) ly'
                writeIORef (ppuDot ps) 0
                stat2 <- statForMode ps 5 -- Mode 2 (OAM scan) STAT enable
                lyc <- statForLyc ps
                pure (stat2 .|. lyc)
    ModeVBlank -> do
        ly <- readIORef (ppuLy ps)
        let ly' = ly + 1
        if ly' == 154
            then do
                writeIORef (ppuMode ps) ModeOamScan
                writeIORef (ppuLy ps) 0
                writeIORef (ppuDot ps) 0
                stat2 <- statForMode ps 5
                lyc <- statForLyc ps
                pure (stat2 .|. lyc)
            else do
                writeIORef (ppuLy ps) ly'
                writeIORef (ppuDot ps) 0
                statForLyc ps

{- | If the given STAT register bit is set, return @0x02@ (the IF STAT bit);
otherwise @0@.
-}
statForMode :: PpuState -> Int -> IO Word8
statForMode ps b = do
    stat <- readIORef (ppuStat ps)
    pure (if testBit stat b then 0x02 else 0)

-- | If LY matches LYC and STAT bit 6 is set, return @0x02@; otherwise @0@.
statForLyc :: PpuState -> IO Word8
statForLyc ps = do
    ly <- readIORef (ppuLy ps)
    lyc <- readIORef (ppuLyc ps)
    stat <- readIORef (ppuStat ps)
    pure (if ly == lyc && testBit stat 6 then 0x02 else 0)

----------------------------------------------------------------------
-- Line renderer (BG + window + sprites)
----------------------------------------------------------------------

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
    bgp <- readIORef (ppuBgp ps)
    -- Per-pixel BG info: (color index 0..3, optional CGB attribute byte).
    bgPixels <-
        if not bgActive
            then pure (replicate framebufferWidth (0 :: Word8, Nothing))
            else
                mapM
                    (bgOrWindowPixel ps cgb lyI winEnabled)
                    [0 .. framebufferWidth - 1]
    let bgIdxes = map fst bgPixels
        bgShades = map (paletteApply bgp) bgIdxes
    finalShades <-
        if spritesEnabled
            then overlaySprites ps lyI lcdc bgIdxes bgShades
            else pure bgShades
    let fbBase = lyI * framebufferWidth
    mapM_
        (\(i, sh) -> MV.write (ppuFb ps) (fbBase + i) sh)
        (zip [0 ..] finalShades)
    -- Mirror the line into the RGB framebuffer.
    writeRgbLine ps cgb lyI bgPixels finalShades

{- | Write one rendered scanline into 'ppuFbRgb'. In DMG mode the shade
palette converts each pixel; in CGB mode BG pixels go through the BG
palette RAM and sprite-overlaid pixels fall back to the DMG sprite
palette this slice.
-}
writeRgbLine :: PpuState -> Bool -> Int -> [(Word8, Maybe Word8)] -> [Word8] -> IO ()
writeRgbLine ps cgb lyI bgPixels finalShades = do
    let baseRgb = lyI * framebufferWidth * 3
    if not cgb
        then
            mapM_
                ( \(i, sh) ->
                    let (r, g, b) = dmgShadeRgb sh
                        off = baseRgb + i * 3
                     in do
                            MV.write (ppuFbRgb ps) off r
                            MV.write (ppuFbRgb ps) (off + 1) g
                            MV.write (ppuFbRgb ps) (off + 2) b
                )
                (zip [0 :: Int ..] finalShades)
        else
            mapM_
                ( \(i, (idx, mAttr), sh) -> do
                    rgb <- case mAttr of
                        Just attr -> cgbBgRgb ps attr idx
                        -- Sprite-painted pixel or BG-disabled fallback;
                        -- use DMG shade for now.
                        Nothing -> pure (dmgShadeRgb sh)
                    let (r, g, b) = rgb
                        off = baseRgb + i * 3
                    MV.write (ppuFbRgb ps) off r
                    MV.write (ppuFbRgb ps) (off + 1) g
                    MV.write (ppuFbRgb ps) (off + 2) b
                )
                (zip3 [0 :: Int ..] bgPixels finalShades)

-- | Look up a BG pixel's CGB color from its attribute byte and color index.
cgbBgRgb :: PpuState -> Word8 -> Word8 -> IO (Word8, Word8, Word8)
cgbBgRgb ps attr colorIdx = do
    let pal = fromIntegral (attr .&. 0x07) :: Int
        off = pal * 8 + fromIntegral colorIdx * 2
    lo <- MV.read (ppuBgPalRam ps) off
    hi <- MV.read (ppuBgPalRam ps) (off + 1)
    pure (rgb555ToRgb888 lo hi)

{- | Compute one BG\/Window pixel: (color index 0..3, optional CGB
attribute byte). The attribute byte is 'Nothing' in DMG mode and
'Just' the bank-1 byte in CGB mode.
-}
bgOrWindowPixel :: PpuState -> Bool -> Int -> Bool -> Int -> IO (Word8, Maybe Word8)
bgOrWindowPixel ps cgb ly winEnabled x = do
    wy <- fromIntegral <$> readIORef (ppuWy ps)
    wx <- fromIntegral <$> readIORef (ppuWx ps)
    let inWindow = winEnabled && ly >= wy && (x + 7) >= wx
    if inWindow
        then tilePixelCgb ps cgb (windowMapBase ps) (x + 7 - wx) (ly - wy)
        else do
            lcdc <- readIORef (ppuLcdc ps)
            scy <- fromIntegral <$> readIORef (ppuScy ps)
            scx <- fromIntegral <$> readIORef (ppuScx ps)
            let mapBase = if testBit lcdc 3 then 0x1C00 else 0x1800
                col = (scx + x) .&. 0xFF
                row = (scy + ly) .&. 0xFF
            tilePixelCgb ps cgb (pure mapBase) col row

windowMapBase :: PpuState -> IO Int
windowMapBase ps = do
    lcdc <- readIORef (ppuLcdc ps)
    pure (if testBit lcdc 6 then 0x1C00 else 0x1800)

{- | Sample one BG/Window pixel from the tile maps. In CGB mode the
attribute byte at the same map index in VRAM bank 1 controls the tile
data bank, palette, and per-tile flips; the returned pair carries that
attribute through to the RGB pass. DMG behaves identically to the
previous implementation and returns 'Nothing' for the attribute.
-}
tilePixelCgb :: PpuState -> Bool -> IO Int -> Int -> Int -> IO (Word8, Maybe Word8)
tilePixelCgb ps cgb mkMapBase col row = do
    lcdc <- readIORef (ppuLcdc ps)
    mapBase <- mkMapBase
    let unsigned = testBit lcdc 4
        !tileX = col `shiftR` 3
        !tileY = row `shiftR` 3
        !mapIdx = mapBase + tileY * 32 + tileX
        vram = ppuVram ps
    tileNum <- MV.read vram mapIdx
    -- CGB attribute byte: same offset, but in VRAM bank 1.
    attr <-
        if cgb
            then Just <$> MV.read vram (0x2000 + mapIdx)
            else pure Nothing
    let hflip = maybe False (`testBit` 5) attr
        vflip = maybe False (`testBit` 6) attr
        tileBank = case attr of
            Just a | testBit a 3 -> 0x2000
            _ -> 0
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

readOamSprites :: IOVector Word8 -> IO [Sprite]
readOamSprites oam =
    mapM
        ( \i -> do
            y <- MV.read oam (i * 4)
            x <- MV.read oam (i * 4 + 1)
            t <- MV.read oam (i * 4 + 2)
            a <- MV.read oam (i * 4 + 3)
            pure
                Sprite
                    { spriteY = fromIntegral y - 16
                    , spriteX = fromIntegral x - 8
                    , spriteTile = t
                    , spriteAttr = a
                    , spriteOam = i
                    }
        )
        [0 .. 39]

overlaySprites ::
    PpuState ->
    Int ->
    Word8 ->
    [Word8] ->
    [Word8] ->
    IO [Word8]
overlaySprites ps ly lcdc bgIdxes bgShades = do
    let height = if testBit lcdc 2 then 16 else 8
    sprs <- readOamSprites (ppuOam ps)
    let candidates = take 10 (filter (overlaps ly height) sprs)
        sorted = stableSortByX candidates
    obp0 <- readIORef (ppuObp0 ps)
    obp1 <- readIORef (ppuObp1 ps)
    mapM
        (pixelWith ly height sorted obp0 obp1)
        (zip3 [0 ..] bgIdxes bgShades)
  where
    overlaps :: Int -> Int -> Sprite -> Bool
    overlaps lyI height s =
        lyI >= spriteY s && lyI < spriteY s + height

    pixelWith ::
        Int ->
        Int ->
        [Sprite] ->
        Word8 ->
        Word8 ->
        (Int, Word8, Word8) ->
        IO Word8
    pixelWith lyI height sorted obp0 obp1 (x, bgI, bgS) = do
        hit <- foreMostHit lyI height sorted x
        case hit of
            Nothing -> pure bgS
            Just (s, sIdx) ->
                let priorityBit = testBit (spriteAttr s) 7
                    bgOpaque = bgI > 0
                 in if priorityBit && bgOpaque
                        then pure bgS
                        else
                            let pal =
                                    if testBit (spriteAttr s) 4
                                        then obp1
                                        else obp0
                             in pure (paletteApply pal sIdx)

    foreMostHit :: Int -> Int -> [Sprite] -> Int -> IO (Maybe (Sprite, Word8))
    foreMostHit _ _ [] _ = pure Nothing
    foreMostHit lyI height (s : rest) x
        | x < spriteX s || x >= spriteX s + 8 = foreMostHit lyI height rest x
        | otherwise = do
            mIdx <- spritePixelIdx (ppuVram ps) lyI height s x
            case mIdx of
                Just idx | idx /= 0 -> pure (Just (s, idx))
                _ -> foreMostHit lyI height rest x

stableSortByX :: [Sprite] -> [Sprite]
stableSortByX [] = []
stableSortByX (x : xs) =
    let (lt, ge) = span (\y -> spriteX y < spriteX x) xs
     in stableSortByX lt ++ [x] ++ stableSortByX ge

spritePixelIdx :: IOVector Word8 -> Int -> Int -> Sprite -> Int -> IO (Maybe Word8)
spritePixelIdx vram ly height s x
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
            !yInTile = yPx .&. 7
            !rowOff = tileBaseIdx * 16 + yInTile * 2
        byteLow <- MV.read vram rowOff
        byteHigh <- MV.read vram (rowOff + 1)
        let !bit = 7 - xPx
            !idx =
                (if testBit byteHigh bit then 2 else 0)
                    + (if testBit byteLow bit then 1 else 0)
        pure (Just idx)
