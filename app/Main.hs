{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Codec.Archive.Zip as Zip
import Control.Exception (IOException, finally, try)
import Control.Monad (when)
import Data.Bits (testBit, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as V
import Data.Word (Word16, Word8)
import qualified Frontend.Sdl as Sdl
import Numeric (showHex)
import Ocelot (
    CartridgeError,
    Header (..),
    cartridgeHasBattery,
    extractSave,
    loadRom,
    loadSave,
    parseHeader,
    version,
 )
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (Cartridge)
import qualified Ocelot.Cartridge as Cartridge
import qualified Ocelot.Cpu.Decode as Decode
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Cpu.Registers (regPC)
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), getCpu, getCpuRegs, machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import Options.Applicative
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)

stepCap :: Int
stepCap = 10000000

data Command
    = Play !PlayOpts
    | Headless !FilePath
    | AudioTest
    | Info !FilePath

data PlayOpts = PlayOpts
    { playRomPath :: !(Maybe FilePath)
    , playBootRom :: !(Maybe FilePath)
    , playScale :: !Int
    }

main :: IO ()
main = do
    cmd <- execParser cliInfo
    runCommand cmd

runCommand :: Command -> IO ()
runCommand (Play opts) = playRom opts
runCommand (Headless path) = describeRom path
runCommand AudioTest = Sdl.audioTest
runCommand (Info path) = infoRom path

cliInfo :: ParserInfo Command
cliInfo =
    info
        (helper <*> versionFlag <*> commandParser)
        ( fullDesc
            <> header (T.unpack version <> " - Gameboy (DMG) and Gameboy Color (CGB) emulator in Haskell")
            <> footer
                ( "SDL key bindings (play): Z=A, X=B, Enter=Start, RShift=Select, "
                    <> "Arrows=D-pad, Space=pause, F1=help overlay, .=frame step, Tab=fast-fwd (held), "
                    <> "R=reset, F5=save state, F7=load state, F12=screenshot, Escape=quit."
                )
        )

versionFlag :: Parser (a -> a)
versionFlag =
    infoOption
        (T.unpack version)
        (long "version" <> short 'V' <> help "Print the version and exit")

commandParser :: Parser Command
commandParser =
    hsubparser
        ( command
            "play"
            ( info
                (Play <$> playOptsParser)
                ( progDesc
                    ( "Run the ROM in the SDL frontend (default mode). Pass "
                        <> "--boot-rom to start from a DMG/CGB boot ROM instead "
                        <> "of the post-boot register state."
                    )
                )
            )
            <> command
                "headless"
                ( info
                    (Headless <$> romArg)
                    ( progDesc
                        ( "Step the CPU for a fixed number of instructions and dump the "
                            <> "final state (registers, serial output, disassembly, memory hex "
                            <> "dump, VRAM tile preview, framebuffer) to the terminal."
                        )
                    )
                )
            <> command
                "audio-test"
                ( info
                    (pure AudioTest)
                    ( progDesc
                        ( "Play a 440 Hz sine tone for 2 seconds via SDL. No ROM needed; "
                            <> "verifies the SDL audio path."
                        )
                    )
                )
            <> command
                "info"
                ( info
                    (Info <$> romArg)
                    (progDesc "Print the ROM's cartridge header and exit.")
                )
        )

romArg :: Parser FilePath
romArg = strArgument (metavar "ROM-PATH" <> help "Path to a .gb or .gbc ROM file.")

playOptsParser :: Parser PlayOpts
playOptsParser =
    PlayOpts
        <$> optional romArg
        <*> optional
            ( strOption
                ( long "boot-rom"
                    <> short 'b'
                    <> metavar "BOOT-ROM-PATH"
                    <> help
                        ( "Optional DMG/CGB boot ROM. Loaded into the boot "
                            <> "window at 0x0000-0x00FF (and 0x0200-0x08FF on CGB) "
                            <> "until the ROM writes 0xFF50."
                        )
                )
            )
        <*> option
            (auto >>= \n -> if n >= 1 && n <= 5 then pure n else readerError "scale must be 1–5")
            ( long "scale"
                <> short 's'
                <> metavar "N"
                <> value 4
                <> showDefault
                <> help "Integer display scale factor (1–5). Window is 160N × 144N pixels."
            )

infoRom :: FilePath -> IO ()
infoRom path = do
    TIO.putStrLn version
    bytes <- readRomBytes path
    case parseHeader bytes of
        Left err -> die ("invalid cartridge header: " <> show err)
        Right hdr -> printHeader hdr

playRom :: PlayOpts -> IO ()
playRom opts = do
    mPath <- case playRomPath opts of
        Just p -> pure (Just p)
        Nothing -> Sdl.startupScreen (playScale opts)
    mapM_ (loadAndPlay opts) mPath

loadAndPlay :: PlayOpts -> FilePath -> IO ()
loadAndPlay opts path = do
    bytes <- readRomBytes path
    bootBytes <- traverse readRomBytes (playBootRom opts)
    case parseHeader bytes of
        Left err -> die ("invalid cartridge header: " <> show err)
        Right hdr -> do
            printHeader hdr
            case bootBytes of
                Just _ -> putStrLn ("boot:     loaded " <> fromMaybe "" (playBootRom opts))
                Nothing -> pure ()
            result <- loadRom bytes
            case result of
                Right cart -> do
                    let savePath = path <> ".sav"
                        battery = cartridgeHasBattery cart
                    when battery (loadSaveIfExists savePath cart)
                    openNew <-
                        Sdl.play path cart bootBytes (hdrTitle hdr) (playScale opts)
                            `finally` when battery (writeSave savePath cart)
                    when openNew $ do
                        mPath' <- Sdl.startupScreen (playScale opts)
                        mapM_ (loadAndPlay opts) mPath'
                Left err -> printNotSupported err

loadSaveIfExists :: FilePath -> Cartridge.Cartridge -> IO ()
loadSaveIfExists path cart = do
    r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    case r of
        Right bs -> do
            putStrLn ("save:     loading " <> path)
            loadSave bs cart
        Left _ -> putStrLn ("save:     no existing " <> path <> " (will create on exit)")

writeSave :: FilePath -> Cartridge.Cartridge -> IO ()
writeSave path cart = do
    bs <- extractSave cart
    BS.writeFile path bs
    putStrLn ("save:     wrote " <> path)

describeRom :: FilePath -> IO ()
describeRom path = do
    bytes <- readRomBytes path
    case parseHeader bytes of
        Left err -> die ("invalid cartridge header: " <> show err)
        Right hdr -> do
            printHeader hdr
            result <- loadRom bytes
            case result of
                Right cart -> runHeadless cart
                Left err -> printNotSupported err

readRomBytes :: FilePath -> IO BS.ByteString
readRomBytes path = do
    r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    case r of
        Left e -> die ("could not read ROM file " <> path <> ": " <> show e)
        Right bytes ->
            if map toLower (takeExtension path) == ".zip"
                then extractRomFromZip path bytes
                else pure bytes

romExtensions :: [String]
romExtensions = [".gb", ".gbc", ".sgb"]

extractRomFromZip :: FilePath -> BS.ByteString -> IO BS.ByteString
extractRomFromZip path bytes = do
    let archive = Zip.toArchive (BL.fromStrict bytes)
        isRom e = map toLower (takeExtension (Zip.eRelativePath e)) `elem` romExtensions
    case filter isRom (Zip.zEntries archive) of
        [] -> die ("no .gb, .gbc, or .sgb ROM found inside " <> path)
        (entry : _) -> do
            putStrLn ("zip:      extracting " <> Zip.eRelativePath entry)
            pure (BL.toStrict (Zip.fromEntry entry))

printHeader :: Header -> IO ()
printHeader h = do
    TIO.putStrLn ("title:    " <> hdrTitle h)
    putStrLn ("mbc:      " <> show (hdrMbcKind h))
    putStrLn ("rom:      " <> show (hdrRomBytes h `div` 1024) <> " KiB")
    putStrLn ("ram:      " <> show (hdrRamBytes h `div` 1024) <> " KiB")
    putStrLn ("cgb flag: " <> show (hdrCgbFlag h))
    putStrLn ("sgb flag: " <> show (hdrSgbFlag h))

printNotSupported :: CartridgeError -> IO ()
printNotSupported err =
    putStrLn ("status:   not yet supported (" <> show err <> ")")

runHeadless :: Cartridge -> IO ()
runHeadless cart = do
    putStrLn ("status:   stepping for up to " <> show stepCap <> " instructions...")
    m0 <- machineFromCartridge cart
    n <- runFor stepCap m0
    cpu <- getCpu m0
    pc <- regPC <$> getCpuRegs m0
    serial <- Bus.drainSerial (machineBus m0)
    fb <- Ppu.framebuffer (Bus.busPpu (machineBus m0))
    putStrLn ("steps:    " <> show n)
    putStrLn ("halted:   " <> show (cpuHalted cpu))
    putStrLn ("PC:       0x" <> hex16 pc)
    case serial of
        [] -> putStrLn "serial:   (no output)"
        bs -> do
            putStr "serial:   "
            BS.putStr (BS.pack bs)
            putStrLn ""
    putStrLn ""
    putStrLn "disassembly around PC:"
    disassembleAround pc m0
    putStrLn ""
    putStrLn "memory hex dump near PC (96 bytes):"
    hexDump pc 96 m0
    putStrLn ""
    putStrLn "VRAM tile preview (first 16 tiles):"
    vramPreview 16 m0
    putStrLn ""
    putStrLn "framebuffer (160x144, sampled to 80x72):"
    renderFramebuffer fb

{- | Print up to 8 instructions starting at the supplied PC, walking through the CPU's bus so MBC
banking and IO reads are honored. Useful when a headless run halts at an unexpected location.
-}
disassembleAround :: Word16 -> Machine -> IO ()
disassembleAround pc0 m = go (8 :: Int) pc0
  where
    go n pc
        | n <= 0 = pure ()
        | otherwise = do
            b0 <- Bus.read8 pc (machineBus m)
            b1 <- Bus.read8 (pc + 1) (machineBus m)
            b2 <- Bus.read8 (pc + 2) (machineBus m)
            let d = Decode.decode b0 b1 b2
                bytes =
                    take
                        (Decode.dLen d)
                        ["0x" <> hex8 b | b <- [b0, b1, b2]]
            putStrLn $
                "  0x"
                    <> hex16 pc
                    <> "  "
                    <> padTo 18 (unwords bytes)
                    <> show (Decode.dInstr d)
            go (n - 1) (pc + fromIntegral (Decode.dLen d))

padTo :: Int -> String -> String
padTo n s = s <> replicate (max 0 (n - length s)) ' '

hex8 :: Word8 -> String
hex8 b =
    let s = showHex b ""
        pad = replicate (2 - length s) '0'
     in pad <> s

{- | Print @n@ bytes of memory starting at @addr@, walking through the
bus so MBC banking and IO reads are honored. 16 bytes per row, lower
nibble of @addr@ aligned for readability.
-}
hexDump :: Word16 -> Int -> Machine -> IO ()
hexDump addr n m = mapM_ rowAt [start, start + 16 .. end - 1]
  where
    start = addr .&. 0xFFF0
    end = start + fromIntegral (((n + 15) `div` 16) * 16)
    rowAt base = do
        bytes <- mapM (\off -> Bus.read8 (base + off) (machineBus m)) [0 .. 15]
        let cells = unwords [hex8 b | b <- bytes]
            ascii = map asciiByte bytes
        putStrLn ("  0x" <> hex16 base <> "  " <> cells <> "  " <> ascii)

asciiByte :: Word8 -> Char
asciiByte b
    | b >= 0x20 && b < 0x7F = toEnum (fromIntegral b)
    | otherwise = '.'

{- | Print the first @n@ tiles from VRAM bank 0 as 8x8 block-character
art. Each tile takes 16 bytes (2bpp interleaved); palette indices are
mapped to the same Unicode shade ramp the framebuffer renderer uses.
-}
vramPreview :: Int -> Machine -> IO ()
vramPreview n m = mapM_ tileAt [0 .. n - 1]
  where
    tileAt i = do
        let base = 0x8000 + fromIntegral i * 16
        bytes <-
            mapM
                (\off -> Bus.read8 (base + fromIntegral off) (machineBus m))
                [0 .. 15 :: Int]
        putStrLn ("tile " <> show i <> ":")
        mapM_ (printTileRow bytes) [0 .. 7]
    printTileRow bytes y =
        let !lo = bytes !! (y * 2)
            !hi = bytes !! (y * 2 + 1)
            row =
                [ shadeChar
                    ( (if testBit hi (7 - x) then 2 else 0)
                        + (if testBit lo (7 - x) then 1 else 0)
                    )
                | x <- [0 .. 7]
                ]
         in putStrLn ("  " <> row)

renderFramebuffer :: V.Vector Word8 -> IO ()
renderFramebuffer fb =
    mapM_
        (\y -> putStrLn [shadeChar (fb V.! (y * 160 + x)) | x <- [0, 2 .. 158]])
        [0, 2 .. 142]

shadeChar :: Word8 -> Char
shadeChar 0 = ' '
shadeChar 1 = '\x2591'
shadeChar 2 = '\x2592'
shadeChar 3 = '\x2588'
shadeChar _ = '?'

hex16 :: (Integral a) => a -> String
hex16 n =
    let s = showHex n ""
        pad = replicate (4 - length s) '0'
     in pad <> s

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
