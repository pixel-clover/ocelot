{-# LANGUAGE BangPatterns #-}

{- | Game Boy Audio Processing Unit (DMG only).

The APU has four channels:

* Channel 1: square wave with frequency sweep, length, and envelope.
* Channel 2: square wave with length and envelope (no sweep).
* Channel 3: 32-step 4-bit waveform (16 bytes of wave RAM at @0xFF30-0xFF3F@).
* Channel 4: pseudo-random noise generator (15-bit LFSR).

A 512 Hz frame sequencer drives length (256 Hz), envelope (64 Hz), and sweep
(128 Hz). The mixer sums the four channels into stereo, scaled by the panning
byte (NR51) and master volume (NR50). NR52 power-off zeros all channel state
except wave RAM and length counters.

State is kept in a single 'IORef' over a pure 'ApuInternal' record. Wave RAM
is also part of that record (only 16 bytes; updates copy cheaply). The
emulator calls 'advance' to tick the APU and 'drainSamples' to read
accumulated stereo samples in @S16@ format.

What is /not/ modeled:

* The DAC's high-pass filter (output is direct).
* Length-counter \"obscure\" behavior on writes during specific frame
  sequencer steps.
* NR12 \"zombie mode\" envelope tweaks.
-}
module Ocelot.Apu (
    ApuState,
    initial,
    read8,
    write8,
    advance,
    drainSamples,
    sampleRate,
    dumpState,
    loadState,
) where

import Data.Bits (clearBit, complement, setBit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word16, Word8)
import qualified Ocelot.Snapshot.Binary as Snap

-- | Sample rate at which the APU emits stereo samples to the queue.
sampleRate :: Int
sampleRate = 48000

-- | Game Boy CPU T-cycle rate.
gbTCycleRate :: Int
gbTCycleRate = 4194304

-- | Frame sequencer steps once every 8192 T-cycles (~512 Hz).
frameSequencerPeriod :: Int
frameSequencerPeriod = 8192

----------------------------------------------------------------------
-- State types
----------------------------------------------------------------------

data ApuState = ApuState
    { apuRef :: !(IORef ApuInternal)
    , apuSamples :: !(IORef [Int16])
    -- ^ Pending stereo samples interleaved L,R, newest first.
    -- Drained via 'drainSamples'.
    }

data ApuInternal = ApuInternal
    { apuPower :: !Bool
    , apuFrameStep :: !Int
    , apuFrameTimer :: !Int
    , apuSampleAcc :: !Int
    -- ^ Sample-rate accumulator. Increments by 'sampleRate' per T-cycle;
    -- when it reaches 'gbTCycleRate' a sample is emitted.
    , apuVolL :: !Word8
    , apuVolR :: !Word8
    , apuPanning :: !Word8
    , apuCh1 :: !Square
    , apuCh2 :: !Square
    , apuCh3 :: !Wave
    , apuCh4 :: !Noise
    , apuWaveRam :: !(Vector Word8)
    , apuNr10 :: !Word8
    , apuNr30 :: !Word8
    -- ^ Cached register bytes for read-back of unused bits.
    }

{- | Square channel (used for ch1 and ch2). The sweep fields are only used by
ch1; ch2's sweep period is always 0 and is ignored.
-}
data Square = Square
    { sqEnabled :: !Bool
    , sqDacOn :: !Bool
    , sqFreq :: !Int
    -- ^ 11-bit frequency value (raw register; period = (2048 - freq) * 4 T-cycles)
    , sqDuty :: !Int
    , sqLength :: !Int
    , sqLengthEn :: !Bool
    , sqVolume :: !Int
    , sqEnvUp :: !Bool
    , sqEnvPeriod :: !Int
    , sqEnvTimer :: !Int
    , sqEnvInitial :: !Int
    -- ^ Volume to load on trigger (NR12 high nibble).
    , sqSweepPeriod :: !Int
    , sqSweepNegate :: !Bool
    , sqSweepShift :: !Int
    , sqSweepTimer :: !Int
    , sqSweepShadow :: !Int
    , sqSweepEnabled :: !Bool
    , sqFreqTimer :: !Int
    , sqDutyPos :: !Int
    }
    deriving (Eq, Show)

initialSquare :: Square
initialSquare =
    Square
        { sqEnabled = False
        , sqDacOn = False
        , sqFreq = 0
        , sqDuty = 0
        , sqLength = 0
        , sqLengthEn = False
        , sqVolume = 0
        , sqEnvUp = False
        , sqEnvPeriod = 0
        , sqEnvTimer = 0
        , sqEnvInitial = 0
        , sqSweepPeriod = 0
        , sqSweepNegate = False
        , sqSweepShift = 0
        , sqSweepTimer = 0
        , sqSweepShadow = 0
        , sqSweepEnabled = False
        , sqFreqTimer = 1
        , sqDutyPos = 0
        }

data Wave = Wave
    { wvEnabled :: !Bool
    , wvDacOn :: !Bool
    , wvFreq :: !Int
    , wvVolumeShift :: !Int
    -- ^ 0=mute, 1=100%, 2=50%, 3=25%
    , wvLength :: !Int
    , wvLengthEn :: !Bool
    , wvFreqTimer :: !Int
    , wvPos :: !Int
    -- ^ 0..31 (each step is one 4-bit sample)
    }
    deriving (Eq, Show)

initialWave :: Wave
initialWave =
    Wave
        { wvEnabled = False
        , wvDacOn = False
        , wvFreq = 0
        , wvVolumeShift = 0
        , wvLength = 0
        , wvLengthEn = False
        , wvFreqTimer = 1
        , wvPos = 0
        }

data Noise = Noise
    { noEnabled :: !Bool
    , noDacOn :: !Bool
    , noLength :: !Int
    , noLengthEn :: !Bool
    , noVolume :: !Int
    , noEnvUp :: !Bool
    , noEnvPeriod :: !Int
    , noEnvTimer :: !Int
    , noEnvInitial :: !Int
    , noClockShift :: !Int
    , noWidthMode7 :: !Bool
    -- ^ True = 7-bit LFSR, False = 15-bit
    , noDivisorCode :: !Int
    , noFreqTimer :: !Int
    , noLfsr :: !Int
    -- ^ 15-bit shift register, init to 0x7FFF on trigger.
    }
    deriving (Eq, Show)

initialNoise :: Noise
initialNoise =
    Noise
        { noEnabled = False
        , noDacOn = False
        , noLength = 0
        , noLengthEn = False
        , noVolume = 0
        , noEnvUp = False
        , noEnvPeriod = 0
        , noEnvTimer = 0
        , noEnvInitial = 0
        , noClockShift = 0
        , noWidthMode7 = False
        , noDivisorCode = 0
        , noFreqTimer = 1
        , noLfsr = 0x7FFF
        }

initialApuInternal :: ApuInternal
initialApuInternal =
    ApuInternal
        { apuPower = True
        , apuFrameStep = 0
        , apuFrameTimer = frameSequencerPeriod
        , apuSampleAcc = 0
        , apuVolL = 7
        , apuVolR = 7
        , apuPanning = 0xF3
        , apuCh1 = initialSquare
        , apuCh2 = initialSquare
        , apuCh3 = initialWave
        , apuCh4 = initialNoise
        , apuWaveRam = V.replicate 16 0
        , apuNr10 = 0x80
        , apuNr30 = 0x7F
        }

initial :: IO ApuState
initial = do
    ref <- newIORef initialApuInternal
    samples <- newIORef []
    pure (ApuState ref samples)

{- | Read all queued samples in chronological order (oldest first) and clear
the queue.
-}
drainSamples :: ApuState -> IO [Int16]
drainSamples apu = do
    xs <- readIORef (apuSamples apu)
    writeIORef (apuSamples apu) []
    pure (reverse xs)

{- | Encode the APU's full internal state to a flat byte string. The
sample queue is intentionally not snapshotted (it's drained per frame
and would only carry stale audio).
-}
dumpState :: ApuState -> IO ByteString
dumpState apu = do
    s <- readIORef (apuRef apu)
    pure (BL.toStrict (BB.toLazyByteString (encodeApu s)))

-- | Restore the APU from a 'dumpState' blob. The sample queue is cleared.
loadState :: ByteString -> ApuState -> IO ()
loadState bs apu = do
    let s = Snap.runCursor decodeApu bs
    writeIORef (apuRef apu) s
    writeIORef (apuSamples apu) []

encodeApu :: ApuInternal -> BB.Builder
encodeApu s =
    Snap.putBool (apuPower s)
        <> Snap.putU32 (fromIntegral (apuFrameStep s))
        <> Snap.putU32 (fromIntegral (apuFrameTimer s))
        <> Snap.putU32 (fromIntegral (apuSampleAcc s))
        <> Snap.putU8 (apuVolL s)
        <> Snap.putU8 (apuVolR s)
        <> Snap.putU8 (apuPanning s)
        <> Snap.putU8 (apuNr10 s)
        <> Snap.putU8 (apuNr30 s)
        <> encodeSquare (apuCh1 s)
        <> encodeSquare (apuCh2 s)
        <> encodeWave (apuCh3 s)
        <> encodeNoise (apuCh4 s)
        <> Snap.putBlob (BS.pack (V.toList (apuWaveRam s)))

decodeApu :: Snap.Cursor ApuInternal
decodeApu = do
    power <- Snap.getBool
    fstep <- fromIntegral <$> Snap.getU32
    ftimer <- fromIntegral <$> Snap.getU32
    sacc <- fromIntegral <$> Snap.getU32
    volL <- Snap.getU8
    volR <- Snap.getU8
    pan <- Snap.getU8
    nr10 <- Snap.getU8
    nr30 <- Snap.getU8
    ch1 <- decodeSquare
    ch2 <- decodeSquare
    ch3 <- decodeWave
    ch4 <- decodeNoise
    wave <- Snap.getBlob
    pure
        ApuInternal
            { apuPower = power
            , apuFrameStep = fstep
            , apuFrameTimer = ftimer
            , apuSampleAcc = sacc
            , apuVolL = volL
            , apuVolR = volR
            , apuPanning = pan
            , apuCh1 = ch1
            , apuCh2 = ch2
            , apuCh3 = ch3
            , apuCh4 = ch4
            , apuWaveRam = V.fromList (BS.unpack wave)
            , apuNr10 = nr10
            , apuNr30 = nr30
            }

encodeSquare :: Square -> BB.Builder
encodeSquare q =
    Snap.putBool (sqEnabled q)
        <> Snap.putBool (sqDacOn q)
        <> Snap.putU16 (fromIntegral (sqFreq q))
        <> Snap.putU8 (fromIntegral (sqDuty q))
        <> Snap.putU16 (fromIntegral (sqLength q))
        <> Snap.putBool (sqLengthEn q)
        <> Snap.putU8 (fromIntegral (sqVolume q))
        <> Snap.putBool (sqEnvUp q)
        <> Snap.putU8 (fromIntegral (sqEnvPeriod q))
        <> Snap.putU16 (fromIntegral (sqEnvTimer q))
        <> Snap.putU8 (fromIntegral (sqEnvInitial q))
        <> Snap.putU8 (fromIntegral (sqSweepPeriod q))
        <> Snap.putBool (sqSweepNegate q)
        <> Snap.putU8 (fromIntegral (sqSweepShift q))
        <> Snap.putU8 (fromIntegral (sqSweepTimer q))
        <> Snap.putU16 (fromIntegral (sqSweepShadow q))
        <> Snap.putBool (sqSweepEnabled q)
        <> Snap.putU16 (fromIntegral (sqFreqTimer q))
        <> Snap.putU8 (fromIntegral (sqDutyPos q))

decodeSquare :: Snap.Cursor Square
decodeSquare = do
    en <- Snap.getBool
    dac <- Snap.getBool
    freq <- fromIntegral <$> Snap.getU16
    duty <- fromIntegral <$> Snap.getU8
    len <- fromIntegral <$> Snap.getU16
    lenEn <- Snap.getBool
    vol <- fromIntegral <$> Snap.getU8
    envUp <- Snap.getBool
    envP <- fromIntegral <$> Snap.getU8
    envT <- fromIntegral <$> Snap.getU16
    envI <- fromIntegral <$> Snap.getU8
    swP <- fromIntegral <$> Snap.getU8
    swN <- Snap.getBool
    swSh <- fromIntegral <$> Snap.getU8
    swT <- fromIntegral <$> Snap.getU8
    swSha <- fromIntegral <$> Snap.getU16
    swEn <- Snap.getBool
    fT <- fromIntegral <$> Snap.getU16
    dPos <- fromIntegral <$> Snap.getU8
    pure
        Square
            { sqEnabled = en
            , sqDacOn = dac
            , sqFreq = freq
            , sqDuty = duty
            , sqLength = len
            , sqLengthEn = lenEn
            , sqVolume = vol
            , sqEnvUp = envUp
            , sqEnvPeriod = envP
            , sqEnvTimer = envT
            , sqEnvInitial = envI
            , sqSweepPeriod = swP
            , sqSweepNegate = swN
            , sqSweepShift = swSh
            , sqSweepTimer = swT
            , sqSweepShadow = swSha
            , sqSweepEnabled = swEn
            , sqFreqTimer = fT
            , sqDutyPos = dPos
            }

encodeWave :: Wave -> BB.Builder
encodeWave w =
    Snap.putBool (wvEnabled w)
        <> Snap.putBool (wvDacOn w)
        <> Snap.putU16 (fromIntegral (wvFreq w))
        <> Snap.putU8 (fromIntegral (wvVolumeShift w))
        <> Snap.putU16 (fromIntegral (wvLength w))
        <> Snap.putBool (wvLengthEn w)
        <> Snap.putU16 (fromIntegral (wvFreqTimer w))
        <> Snap.putU8 (fromIntegral (wvPos w))

decodeWave :: Snap.Cursor Wave
decodeWave = do
    en <- Snap.getBool
    dac <- Snap.getBool
    freq <- fromIntegral <$> Snap.getU16
    volSh <- fromIntegral <$> Snap.getU8
    len <- fromIntegral <$> Snap.getU16
    lenEn <- Snap.getBool
    fT <- fromIntegral <$> Snap.getU16
    pos <- fromIntegral <$> Snap.getU8
    pure
        Wave
            { wvEnabled = en
            , wvDacOn = dac
            , wvFreq = freq
            , wvVolumeShift = volSh
            , wvLength = len
            , wvLengthEn = lenEn
            , wvFreqTimer = fT
            , wvPos = pos
            }

encodeNoise :: Noise -> BB.Builder
encodeNoise n =
    Snap.putBool (noEnabled n)
        <> Snap.putBool (noDacOn n)
        <> Snap.putU16 (fromIntegral (noLength n))
        <> Snap.putBool (noLengthEn n)
        <> Snap.putU8 (fromIntegral (noVolume n))
        <> Snap.putBool (noEnvUp n)
        <> Snap.putU8 (fromIntegral (noEnvPeriod n))
        <> Snap.putU16 (fromIntegral (noEnvTimer n))
        <> Snap.putU8 (fromIntegral (noEnvInitial n))
        <> Snap.putU8 (fromIntegral (noClockShift n))
        <> Snap.putBool (noWidthMode7 n)
        <> Snap.putU8 (fromIntegral (noDivisorCode n))
        <> Snap.putU16 (fromIntegral (noFreqTimer n))
        <> Snap.putU16 (fromIntegral (noLfsr n))

decodeNoise :: Snap.Cursor Noise
decodeNoise = do
    en <- Snap.getBool
    dac <- Snap.getBool
    len <- fromIntegral <$> Snap.getU16
    lenEn <- Snap.getBool
    vol <- fromIntegral <$> Snap.getU8
    envUp <- Snap.getBool
    envP <- fromIntegral <$> Snap.getU8
    envT <- fromIntegral <$> Snap.getU16
    envI <- fromIntegral <$> Snap.getU8
    cs <- fromIntegral <$> Snap.getU8
    w7 <- Snap.getBool
    dc <- fromIntegral <$> Snap.getU8
    fT <- fromIntegral <$> Snap.getU16
    lfsr <- fromIntegral <$> Snap.getU16
    pure
        Noise
            { noEnabled = en
            , noDacOn = dac
            , noLength = len
            , noLengthEn = lenEn
            , noVolume = vol
            , noEnvUp = envUp
            , noEnvPeriod = envP
            , noEnvTimer = envT
            , noEnvInitial = envI
            , noClockShift = cs
            , noWidthMode7 = w7
            , noDivisorCode = dc
            , noFreqTimer = fT
            , noLfsr = lfsr
            }

----------------------------------------------------------------------
-- Register I/O
----------------------------------------------------------------------

-- | Duty-cycle waveforms: 4 patterns of 8 steps each.
dutyTable :: Int -> Int -> Bool
dutyTable 0 i = i == 7
dutyTable 1 i = i == 0 || i == 7
dutyTable 2 i = i == 0 || (i >= 5 && i <= 7)
dutyTable 3 i = i /= 0 && i /= 7
dutyTable _ _ = False

read8 :: Word16 -> ApuState -> IO Word8
read8 addr apu = do
    s <- readIORef (apuRef apu)
    pure (readRegister addr s)

readRegister :: Word16 -> ApuInternal -> Word8
readRegister addr s = case addr of
    0xFF10 -> apuNr10 s .|. 0x80 -- bit 7 always reads 1
    0xFF11 -> (fromIntegral (sqDuty (apuCh1 s)) `shiftL` 6) .|. 0x3F
    0xFF12 -> sqEnvelopeByte (apuCh1 s)
    0xFF13 -> 0xFF -- write-only
    0xFF14 -> (if sqLengthEn (apuCh1 s) then 0x40 else 0) .|. 0xBF
    0xFF15 -> 0xFF
    0xFF16 -> (fromIntegral (sqDuty (apuCh2 s)) `shiftL` 6) .|. 0x3F
    0xFF17 -> sqEnvelopeByte (apuCh2 s)
    0xFF18 -> 0xFF
    0xFF19 -> (if sqLengthEn (apuCh2 s) then 0x40 else 0) .|. 0xBF
    0xFF1A -> apuNr30 s .|. 0x7F
    0xFF1B -> 0xFF
    0xFF1C -> (fromIntegral (wvVolumeShift (apuCh3 s)) `shiftL` 5) .|. 0x9F
    0xFF1D -> 0xFF
    0xFF1E -> (if wvLengthEn (apuCh3 s) then 0x40 else 0) .|. 0xBF
    0xFF1F -> 0xFF
    0xFF20 -> 0xFF
    0xFF21 -> noEnvelopeByte (apuCh4 s)
    0xFF22 -> noPolyByte (apuCh4 s)
    0xFF23 -> (if noLengthEn (apuCh4 s) then 0x40 else 0) .|. 0xBF
    0xFF24 -> (apuVolL s `shiftL` 4) .|. apuVolR s
    0xFF25 -> apuPanning s
    0xFF26 -> nr52Byte s
    a
        | a >= 0xFF30 && a <= 0xFF3F ->
            apuWaveRam s V.! fromIntegral (a - 0xFF30)
    _ -> 0xFF

sqEnvelopeByte :: Square -> Word8
sqEnvelopeByte sq =
    (fromIntegral (sqEnvInitial sq) `shiftL` 4)
        .|. (if sqEnvUp sq then 0x08 else 0)
        .|. fromIntegral (sqEnvPeriod sq)

noEnvelopeByte :: Noise -> Word8
noEnvelopeByte n =
    (fromIntegral (noEnvInitial n) `shiftL` 4)
        .|. (if noEnvUp n then 0x08 else 0)
        .|. fromIntegral (noEnvPeriod n)

noPolyByte :: Noise -> Word8
noPolyByte n =
    (fromIntegral (noClockShift n) `shiftL` 4)
        .|. (if noWidthMode7 n then 0x08 else 0)
        .|. fromIntegral (noDivisorCode n)

nr52Byte :: ApuInternal -> Word8
nr52Byte s =
    (if apuPower s then 0x80 else 0)
        .|. 0x70 -- unused bits read 1
        .|. (if sqEnabled (apuCh1 s) then 0x01 else 0)
        .|. (if sqEnabled (apuCh2 s) then 0x02 else 0)
        .|. (if wvEnabled (apuCh3 s) then 0x04 else 0)
        .|. (if noEnabled (apuCh4 s) then 0x08 else 0)

write8 :: Word16 -> Word8 -> ApuState -> IO ()
write8 addr !v apu = modifyIORef' (apuRef apu) (writeRegister addr v)

writeRegister :: Word16 -> Word8 -> ApuInternal -> ApuInternal
writeRegister addr v s
    | addr == 0xFF26 = handleNr52 v s
    | addr >= 0xFF30 && addr <= 0xFF3F =
        s{apuWaveRam = apuWaveRam s V.// [(fromIntegral (addr - 0xFF30), v)]}
    | not (apuPower s) && addr /= 0xFF11 && addr /= 0xFF16 && addr /= 0xFF1B && addr /= 0xFF20 = s
    -- When powered off, only length-counter writes pass through (DMG).
    | otherwise = case addr of
        0xFF10 -> writeNr10 v s
        0xFF11 -> writeNr11 v s
        0xFF12 -> writeNr12 v s
        0xFF13 -> writeNr13 v s
        0xFF14 -> writeNr14 v s
        0xFF16 -> writeNr21 v s
        0xFF17 -> writeNr22 v s
        0xFF18 -> writeNr23 v s
        0xFF19 -> writeNr24 v s
        0xFF1A -> writeNr30 v s
        0xFF1B -> writeNr31 v s
        0xFF1C -> writeNr32 v s
        0xFF1D -> writeNr33 v s
        0xFF1E -> writeNr34 v s
        0xFF20 -> writeNr41 v s
        0xFF21 -> writeNr42 v s
        0xFF22 -> writeNr43 v s
        0xFF23 -> writeNr44 v s
        0xFF24 -> s{apuVolR = v .&. 0x07, apuVolL = (v `shiftR` 4) .&. 0x07}
        0xFF25 -> s{apuPanning = v}
        _ -> s

handleNr52 :: Word8 -> ApuInternal -> ApuInternal
handleNr52 v s
    | testBit v 7 = s{apuPower = True}
    | otherwise =
        -- Powering off: clear all registers (channels + mixer); preserve
        -- wave RAM and length counters per DMG behavior.
        let !ch1' = (apuCh1 s){sqEnabled = False, sqDacOn = False}
            !ch2' = (apuCh2 s){sqEnabled = False, sqDacOn = False}
            !ch3' = (apuCh3 s){wvEnabled = False, wvDacOn = False}
            !ch4' = (apuCh4 s){noEnabled = False, noDacOn = False}
         in s
                { apuPower = False
                , apuCh1 = clearSquare ch1'
                , apuCh2 = clearSquare ch2'
                , apuCh3 = clearWave ch3'
                , apuCh4 = clearNoise ch4'
                , apuVolL = 0
                , apuVolR = 0
                , apuPanning = 0
                , apuNr10 = 0
                , apuNr30 = 0
                }

clearSquare :: Square -> Square
clearSquare sq =
    sq
        { sqDuty = 0
        , sqVolume = 0
        , sqEnvUp = False
        , sqEnvPeriod = 0
        , sqEnvInitial = 0
        , sqSweepPeriod = 0
        , sqSweepNegate = False
        , sqSweepShift = 0
        , sqFreq = 0
        }

clearWave :: Wave -> Wave
clearWave w = w{wvVolumeShift = 0, wvFreq = 0}

clearNoise :: Noise -> Noise
clearNoise n =
    n
        { noVolume = 0
        , noEnvUp = False
        , noEnvPeriod = 0
        , noEnvInitial = 0
        , noClockShift = 0
        , noWidthMode7 = False
        , noDivisorCode = 0
        }

writeNr10 :: Word8 -> ApuInternal -> ApuInternal
writeNr10 v s =
    let ch = apuCh1 s
        !ch' =
            ch
                { sqSweepPeriod = fromIntegral ((v `shiftR` 4) .&. 0x07)
                , sqSweepNegate = testBit v 3
                , sqSweepShift = fromIntegral (v .&. 0x07)
                }
     in s{apuCh1 = ch', apuNr10 = v}

writeNr11 :: Word8 -> ApuInternal -> ApuInternal
writeNr11 v s =
    let ch = apuCh1 s
        !ch' =
            ch
                { sqDuty = fromIntegral (v `shiftR` 6)
                , sqLength = 64 - fromIntegral (v .&. 0x3F)
                }
     in s{apuCh1 = ch'}

writeNr12 :: Word8 -> ApuInternal -> ApuInternal
writeNr12 v s =
    let ch = apuCh1 s
        !envInit = fromIntegral ((v `shiftR` 4) .&. 0x0F)
        !envUp = testBit v 3
        !ch' =
            ch
                { sqEnvInitial = envInit
                , sqEnvUp = envUp
                , sqEnvPeriod = fromIntegral (v .&. 0x07)
                , sqDacOn = (v .&. 0xF8) /= 0
                , sqEnabled = sqEnabled ch && (v .&. 0xF8) /= 0
                }
     in s{apuCh1 = ch'}

writeNr13 :: Word8 -> ApuInternal -> ApuInternal
writeNr13 v s =
    let ch = apuCh1 s
        !freq = (sqFreq ch .&. 0x700) .|. fromIntegral v
     in s{apuCh1 = ch{sqFreq = freq}}

writeNr14 :: Word8 -> ApuInternal -> ApuInternal
writeNr14 v s =
    let ch = apuCh1 s
        !freq = (sqFreq ch .&. 0xFF) .|. (fromIntegral (v .&. 0x07) `shiftL` 8)
        !lengthEn = testBit v 6
        !trigger = testBit v 7
        !ch1 = ch{sqFreq = freq, sqLengthEn = lengthEn}
        !ch2 = if trigger then triggerSquare ch1 True else ch1
     in s{apuCh1 = ch2}

triggerSquare :: Square -> Bool -> Square
triggerSquare ch hasSweep =
    let !lengthRel = if sqLength ch == 0 then 64 else sqLength ch
        !period = (2048 - sqFreq ch) * 4
        !ch1 =
            ch
                { sqEnabled = sqDacOn ch
                , sqLength = lengthRel
                , sqVolume = sqEnvInitial ch
                , sqEnvTimer = if sqEnvPeriod ch == 0 then 8 else sqEnvPeriod ch
                , sqFreqTimer = period
                , sqDutyPos = 0
                }
        !ch2 =
            if hasSweep
                then
                    let !swEn = sqSweepPeriod ch1 /= 0 || sqSweepShift ch1 /= 0
                        !swTimer =
                            if sqSweepPeriod ch1 == 0 then 8 else sqSweepPeriod ch1
                     in ch1
                            { sqSweepShadow = sqFreq ch1
                            , sqSweepTimer = swTimer
                            , sqSweepEnabled = swEn
                            }
                else ch1
     in ch2

writeNr21 :: Word8 -> ApuInternal -> ApuInternal
writeNr21 v s =
    let ch = apuCh2 s
        !ch' =
            ch
                { sqDuty = fromIntegral (v `shiftR` 6)
                , sqLength = 64 - fromIntegral (v .&. 0x3F)
                }
     in s{apuCh2 = ch'}

writeNr22 :: Word8 -> ApuInternal -> ApuInternal
writeNr22 v s =
    let ch = apuCh2 s
        !ch' =
            ch
                { sqEnvInitial = fromIntegral ((v `shiftR` 4) .&. 0x0F)
                , sqEnvUp = testBit v 3
                , sqEnvPeriod = fromIntegral (v .&. 0x07)
                , sqDacOn = (v .&. 0xF8) /= 0
                , sqEnabled = sqEnabled ch && (v .&. 0xF8) /= 0
                }
     in s{apuCh2 = ch'}

writeNr23 :: Word8 -> ApuInternal -> ApuInternal
writeNr23 v s =
    let ch = apuCh2 s
        !freq = (sqFreq ch .&. 0x700) .|. fromIntegral v
     in s{apuCh2 = ch{sqFreq = freq}}

writeNr24 :: Word8 -> ApuInternal -> ApuInternal
writeNr24 v s =
    let ch = apuCh2 s
        !freq = (sqFreq ch .&. 0xFF) .|. (fromIntegral (v .&. 0x07) `shiftL` 8)
        !lengthEn = testBit v 6
        !trigger = testBit v 7
        !ch1 = ch{sqFreq = freq, sqLengthEn = lengthEn}
        !ch2 = if trigger then triggerSquare ch1 False else ch1
     in s{apuCh2 = ch2}

writeNr30 :: Word8 -> ApuInternal -> ApuInternal
writeNr30 v s =
    let ch = apuCh3 s
        !dac = testBit v 7
        !ch' = ch{wvDacOn = dac, wvEnabled = wvEnabled ch && dac}
     in s{apuCh3 = ch', apuNr30 = v}

writeNr31 :: Word8 -> ApuInternal -> ApuInternal
writeNr31 v s =
    let ch = apuCh3 s
     in s{apuCh3 = ch{wvLength = 256 - fromIntegral v}}

writeNr32 :: Word8 -> ApuInternal -> ApuInternal
writeNr32 v s =
    let ch = apuCh3 s
        !shft = case (v `shiftR` 5) .&. 0x03 of
            0 -> 4 -- mute (we'll right-shift by 4 to zero the 4-bit sample)
            1 -> 0 -- 100%
            2 -> 1 -- 50%
            _ -> 2 -- 25%
     in s{apuCh3 = ch{wvVolumeShift = shft}}

writeNr33 :: Word8 -> ApuInternal -> ApuInternal
writeNr33 v s =
    let ch = apuCh3 s
        !freq = (wvFreq ch .&. 0x700) .|. fromIntegral v
     in s{apuCh3 = ch{wvFreq = freq}}

writeNr34 :: Word8 -> ApuInternal -> ApuInternal
writeNr34 v s =
    let ch = apuCh3 s
        !freq = (wvFreq ch .&. 0xFF) .|. (fromIntegral (v .&. 0x07) `shiftL` 8)
        !lengthEn = testBit v 6
        !trigger = testBit v 7
        !ch1 = ch{wvFreq = freq, wvLengthEn = lengthEn}
        !ch2 =
            if trigger
                then
                    ch1
                        { wvEnabled = wvDacOn ch1
                        , wvLength = if wvLength ch1 == 0 then 256 else wvLength ch1
                        , wvFreqTimer = (2048 - freq) * 2
                        , wvPos = 0
                        }
                else ch1
     in s{apuCh3 = ch2}

writeNr41 :: Word8 -> ApuInternal -> ApuInternal
writeNr41 v s =
    let ch = apuCh4 s
     in s{apuCh4 = ch{noLength = 64 - fromIntegral (v .&. 0x3F)}}

writeNr42 :: Word8 -> ApuInternal -> ApuInternal
writeNr42 v s =
    let ch = apuCh4 s
        !ch' =
            ch
                { noEnvInitial = fromIntegral ((v `shiftR` 4) .&. 0x0F)
                , noEnvUp = testBit v 3
                , noEnvPeriod = fromIntegral (v .&. 0x07)
                , noDacOn = (v .&. 0xF8) /= 0
                , noEnabled = noEnabled ch && (v .&. 0xF8) /= 0
                }
     in s{apuCh4 = ch'}

writeNr43 :: Word8 -> ApuInternal -> ApuInternal
writeNr43 v s =
    let ch = apuCh4 s
        !ch' =
            ch
                { noClockShift = fromIntegral ((v `shiftR` 4) .&. 0x0F)
                , noWidthMode7 = testBit v 3
                , noDivisorCode = fromIntegral (v .&. 0x07)
                }
     in s{apuCh4 = ch'}

writeNr44 :: Word8 -> ApuInternal -> ApuInternal
writeNr44 v s =
    let ch = apuCh4 s
        !lengthEn = testBit v 6
        !trigger = testBit v 7
        !ch1 = ch{noLengthEn = lengthEn}
        !ch2 =
            if trigger
                then
                    ch1
                        { noEnabled = noDacOn ch1
                        , noLength = if noLength ch1 == 0 then 64 else noLength ch1
                        , noVolume = noEnvInitial ch1
                        , noEnvTimer =
                            if noEnvPeriod ch1 == 0 then 8 else noEnvPeriod ch1
                        , noFreqTimer = noiseTimerPeriod ch1
                        , noLfsr = 0x7FFF
                        }
                else ch1
     in s{apuCh4 = ch2}

noiseTimerPeriod :: Noise -> Int
noiseTimerPeriod n =
    let divisor = case noDivisorCode n of
            0 -> 8
            d -> d * 16
     in divisor `shiftL` noClockShift n

----------------------------------------------------------------------
-- Per-T-cycle advance
----------------------------------------------------------------------

advance :: Int -> ApuState -> IO ()
advance mCycles apu = do
    s0 <- readIORef (apuRef apu)
    let !totalT = mCycles * 4
        (s1, emitted) = stepCycles totalT s0
    writeIORef (apuRef apu) s1
    case emitted of
        [] -> pure ()
        _ -> modifyIORef' (apuSamples apu) (emitted ++)

stepCycles :: Int -> ApuInternal -> (ApuInternal, [Int16])
stepCycles totalT s0 = go totalT s0 []
  where
    go !remaining !s !acc
        | remaining <= 0 = (s, acc)
        | otherwise =
            let !s1 = tickChannels 1 s
                -- Frame timer
                !ft = apuFrameTimer s1 - 1
                !s2 =
                    if ft <= 0
                        then
                            let !step = (apuFrameStep s1 + 1) `mod` 8
                                !s2a =
                                    s1
                                        { apuFrameStep = step
                                        , apuFrameTimer = frameSequencerPeriod
                                        }
                             in stepFrame (apuFrameStep s1) s2a
                        else s1{apuFrameTimer = ft}
                -- Sample accumulator
                !sa = apuSampleAcc s2 + sampleRate
                (!s3, !sampMaybe) =
                    if sa >= gbTCycleRate
                        then
                            let !s3a = s2{apuSampleAcc = sa - gbTCycleRate}
                                (!l, !r) = mixSample s3a
                             in (s3a, Just (l, r))
                        else (s2{apuSampleAcc = sa}, Nothing)
                !acc' = case sampMaybe of
                    Nothing -> acc
                    Just (l, r) -> r : l : acc
             in go (remaining - 1) s3 acc'

-- | Tick each channel's frequency timer by one T-cycle.
tickChannels :: Int -> ApuInternal -> ApuInternal
tickChannels !t s =
    s
        { apuCh1 = tickSquare t (apuCh1 s)
        , apuCh2 = tickSquare t (apuCh2 s)
        , apuCh3 = tickWave t (apuCh3 s)
        , apuCh4 = tickNoise t (apuCh4 s)
        }

tickSquare :: Int -> Square -> Square
tickSquare !t sq =
    let !timer = sqFreqTimer sq - t
     in if timer > 0
            then sq{sqFreqTimer = timer}
            else
                let !period = max 1 ((2048 - sqFreq sq) * 4)
                    !pos' = (sqDutyPos sq + 1) `mod` 8
                 in sq{sqFreqTimer = period, sqDutyPos = pos'}

tickWave :: Int -> Wave -> Wave
tickWave !t w =
    let !timer = wvFreqTimer w - t
     in if timer > 0
            then w{wvFreqTimer = timer}
            else
                let !period = max 1 ((2048 - wvFreq w) * 2)
                    !pos' = (wvPos w + 1) `mod` 32
                 in w{wvFreqTimer = period, wvPos = pos'}

tickNoise :: Int -> Noise -> Noise
tickNoise !t n =
    let !timer = noFreqTimer n - t
     in if timer > 0
            then n{noFreqTimer = timer}
            else
                let !period = max 1 (noiseTimerPeriod n)
                    -- Advance LFSR.
                    !lfsr = noLfsr n
                    !bit01 = (lfsr `xor` (lfsr `shiftR` 1)) .&. 0x01
                    !lfsr' = (lfsr `shiftR` 1) .|. (bit01 `shiftL` 14)
                    !lfsr''
                        | noWidthMode7 n =
                            (lfsr' .&. complement 0x40) .|. (bit01 `shiftL` 6)
                        | otherwise = lfsr'
                 in n{noFreqTimer = period, noLfsr = lfsr''}

----------------------------------------------------------------------
-- Frame sequencer
----------------------------------------------------------------------

stepFrame :: Int -> ApuInternal -> ApuInternal
stepFrame step s = case step of
    0 -> tickLengths s
    1 -> s
    2 -> tickSweep (tickLengths s)
    3 -> s
    4 -> tickLengths s
    5 -> s
    6 -> tickSweep (tickLengths s)
    7 -> tickEnvelopes s
    _ -> s

tickLengths :: ApuInternal -> ApuInternal
tickLengths s =
    s
        { apuCh1 = tickLengthSq (apuCh1 s)
        , apuCh2 = tickLengthSq (apuCh2 s)
        , apuCh3 = tickLengthWave (apuCh3 s)
        , apuCh4 = tickLengthNoise (apuCh4 s)
        }

tickLengthSq :: Square -> Square
tickLengthSq sq
    | sqLengthEn sq && sqLength sq > 0 =
        let !l' = sqLength sq - 1
         in sq{sqLength = l', sqEnabled = sqEnabled sq && l' > 0}
    | otherwise = sq

tickLengthWave :: Wave -> Wave
tickLengthWave w
    | wvLengthEn w && wvLength w > 0 =
        let !l' = wvLength w - 1
         in w{wvLength = l', wvEnabled = wvEnabled w && l' > 0}
    | otherwise = w

tickLengthNoise :: Noise -> Noise
tickLengthNoise n
    | noLengthEn n && noLength n > 0 =
        let !l' = noLength n - 1
         in n{noLength = l', noEnabled = noEnabled n && l' > 0}
    | otherwise = n

tickEnvelopes :: ApuInternal -> ApuInternal
tickEnvelopes s =
    s
        { apuCh1 = tickEnvSq (apuCh1 s)
        , apuCh2 = tickEnvSq (apuCh2 s)
        , apuCh4 = tickEnvNoise (apuCh4 s)
        }

tickEnvSq :: Square -> Square
tickEnvSq sq
    | sqEnvPeriod sq == 0 = sq
    | otherwise =
        let !timer = sqEnvTimer sq - 1
         in if timer > 0
                then sq{sqEnvTimer = timer}
                else
                    let !v = sqVolume sq
                        !v' =
                            if sqEnvUp sq
                                then min 15 (v + 1)
                                else max 0 (v - 1)
                     in sq{sqEnvTimer = sqEnvPeriod sq, sqVolume = v'}

tickEnvNoise :: Noise -> Noise
tickEnvNoise n
    | noEnvPeriod n == 0 = n
    | otherwise =
        let !timer = noEnvTimer n - 1
         in if timer > 0
                then n{noEnvTimer = timer}
                else
                    let !v = noVolume n
                        !v' =
                            if noEnvUp n
                                then min 15 (v + 1)
                                else max 0 (v - 1)
                     in n{noEnvTimer = noEnvPeriod n, noVolume = v'}

tickSweep :: ApuInternal -> ApuInternal
tickSweep s
    | not (sqSweepEnabled (apuCh1 s)) = s
    | otherwise =
        let ch = apuCh1 s
            !timer = sqSweepTimer ch - 1
         in if timer > 0
                then s{apuCh1 = ch{sqSweepTimer = timer}}
                else
                    let !period =
                            if sqSweepPeriod ch == 0 then 8 else sqSweepPeriod ch
                        !shadow = sqSweepShadow ch
                        !delta = shadow `shiftR` sqSweepShift ch
                        !newF =
                            if sqSweepNegate ch
                                then shadow - delta
                                else shadow + delta
                        !ch' =
                            if newF > 2047 || newF < 0
                                then ch{sqEnabled = False, sqSweepEnabled = False}
                                else
                                    if sqSweepShift ch > 0
                                        then
                                            ch
                                                { sqSweepShadow = newF
                                                , sqFreq = newF
                                                , sqSweepTimer = period
                                                }
                                        else ch{sqSweepTimer = period}
                     in s{apuCh1 = ch'}

----------------------------------------------------------------------
-- Mixer
----------------------------------------------------------------------

-- | Compute one stereo sample (Int16 each) from the current channel state.
mixSample :: ApuInternal -> (Int16, Int16)
mixSample s =
    let !c1 = squareSample (apuCh1 s)
        !c2 = squareSample (apuCh2 s)
        !c3 = waveSample (apuCh3 s) (apuWaveRam s)
        !c4 = noiseSample (apuCh4 s)
        !pan = apuPanning s
        leftSum =
            (if testBit pan 7 then c4 else 0)
                + (if testBit pan 6 then c3 else 0)
                + (if testBit pan 5 then c2 else 0)
                + (if testBit pan 4 then c1 else 0)
        rightSum =
            (if testBit pan 3 then c4 else 0)
                + (if testBit pan 2 then c3 else 0)
                + (if testBit pan 1 then c2 else 0)
                + (if testBit pan 0 then c1 else 0)
        -- Each channel sample is in [-15..15]; sum is [-60..60].
        -- Master volume is 0..7; effective gain is (volL+1)/8 (and same for R).
        !leftScaled = leftSum * (fromIntegral (apuVolL s) + 1)
        !rightScaled = rightSum * (fromIntegral (apuVolR s) + 1)
        -- Convert to Int16 with headroom.
        !lFinal = clampI16 (leftScaled * 64)
        !rFinal = clampI16 (rightScaled * 64)
     in (lFinal, rFinal)

clampI16 :: Int -> Int16
clampI16 x
    | x > 32767 = 32767
    | x < -32768 = -32768
    | otherwise = fromIntegral x

squareSample :: Square -> Int
squareSample sq
    | not (sqEnabled sq) || not (sqDacOn sq) = 0
    | otherwise =
        let !high = dutyTable (sqDuty sq) (sqDutyPos sq)
            !v = sqVolume sq
         in if high then v else -v

waveSample :: Wave -> Vector Word8 -> Int
waveSample w wave
    | not (wvEnabled w) || not (wvDacOn w) = 0
    | otherwise =
        let !pos = wvPos w
            !byte = wave V.! (pos `shiftR` 1)
            !nibble =
                if pos .&. 1 == 0
                    then byte `shiftR` 4
                    else byte .&. 0x0F
            !shifted = fromIntegral nibble `shiftR` wvVolumeShift w
         in shifted - 8

noiseSample :: Noise -> Int
noiseSample n
    | not (noEnabled n) || not (noDacOn n) = 0
    | otherwise =
        let !out = if testBit (noLfsr n) 0 then -1 else 1
         in noVolume n * out
