{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

{- HLINT ignore "Use camelCase" -}

import Control.Exception (SomeException, displayException, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import qualified Ocelot as Public
import qualified Ocelot.Joypad as Joypad
import qualified Ocelot.Snapshot as Snapshot
import qualified Ocelot.Web as Web
import System.IO.Unsafe (unsafePerformIO)

data SessionHandle = SessionHandle
    { shSession :: !Web.WebSession
    , shTitlePtr :: !(Ptr Word8)
    , shTitleLen :: !Int
    , shFramebufferPtr :: !(Ptr Word8)
    , shAudioPtr :: !(Ptr Int16)
    , shAudioCap :: !Int
    , shAudioLen :: !(IORef Int)
    , shStateBuffer :: !(IORef (Ptr Word8, Int))
    , shSaveBuffer :: !(IORef (Ptr Word8, Int))
    }

data Runtime = Runtime
    { rtNextSessionId :: !Int
    , rtSessions :: !(IntMap.IntMap SessionHandle)
    }

framebufferBytes :: Int
framebufferBytes = Web.framebufferWidth * Web.framebufferHeight * 4

audioCapacitySamples :: Int
audioCapacitySamples = 32768 * 2

{-# NOINLINE runtimeRef #-}
runtimeRef :: IORef Runtime
runtimeRef =
    unsafePerformIO $
        newIORef
            Runtime
                { rtNextSessionId = 1
                , rtSessions = IntMap.empty
                }

{-# NOINLINE lastErrorRef #-}
lastErrorRef :: IORef (Ptr Word8, Int)
lastErrorRef = unsafePerformIO (newIORef (nullPtr, 0))

{-# NOINLINE versionBuffer #-}
versionBuffer :: (Ptr Word8, Int)
versionBuffer =
    unsafePerformIO $
        allocBufferFromByteString (TE.encodeUtf8 Public.version)

main :: IO ()
main = pure ()

allocBufferFromByteString :: BS.ByteString -> IO (Ptr Word8, Int)
allocBufferFromByteString bs
    | BS.null bs = pure (nullPtr, 0)
    | otherwise = BS.useAsCStringLen bs $ \(src, len) -> do
        dst <- mallocBytes len
        copyBytes dst (castPtr src) len
        pure (castPtr dst, len)

replaceBuffer :: IORef (Ptr Word8, Int) -> BS.ByteString -> IO ()
replaceBuffer ref bs = do
    (oldPtr, _) <- readIORef ref
    when (oldPtr /= nullPtr) (free oldPtr)
    pair <- allocBufferFromByteString bs
    writeIORef ref pair

freeBufferRef :: IORef (Ptr Word8, Int) -> IO ()
freeBufferRef ref = do
    (ptr, _) <- readIORef ref
    when (ptr /= nullPtr) (free ptr)
    writeIORef ref (nullPtr, 0)

setLastError :: String -> IO ()
setLastError = replaceBuffer lastErrorRef . TE.encodeUtf8 . T.pack

clearLastError :: IO ()
clearLastError = replaceBuffer lastErrorRef BS.empty

lookupSession :: CInt -> IO (Maybe SessionHandle)
lookupSession sid = IntMap.lookup (fromIntegral sid) . rtSessions <$> readIORef runtimeRef

withSession :: CInt -> (SessionHandle -> IO a) -> IO (Maybe a)
withSession sid action = do
    found <- lookupSession sid
    case found of
        Just handle -> Just <$> action handle
        Nothing -> do
            setLastError "Invalid Ocelot web session"
            pure Nothing

insertSession :: SessionHandle -> IO CInt
insertSession handle = do
    runtime <- readIORef runtimeRef
    let sid = rtNextSessionId runtime
    writeIORef
        runtimeRef
        runtime
            { rtNextSessionId = sid + 1
            , rtSessions = IntMap.insert sid handle (rtSessions runtime)
            }
    pure (fromIntegral sid)

destroyHandle :: SessionHandle -> IO ()
destroyHandle handle = do
    when (shTitlePtr handle /= nullPtr) (free (shTitlePtr handle))
    when (shFramebufferPtr handle /= nullPtr) (free (shFramebufferPtr handle))
    when (shAudioPtr handle /= nullPtr) (free (shAudioPtr handle))
    freeBufferRef (shStateBuffer handle)
    freeBufferRef (shSaveBuffer handle)

copyFramebuffer :: SessionHandle -> IO ()
copyFramebuffer handle =
    Web.copyFramebufferRgba (shFramebufferPtr handle) (shSession handle)

drainAudioIntoHandle :: SessionHandle -> IO ()
drainAudioIntoHandle handle = do
    currentLen <- readIORef (shAudioLen handle)
    let room = shAudioCap handle - currentLen
    written <- Web.drainAudioSamplesInto (shAudioPtr handle `plusInt16Ptr` currentLen) room (shSession handle)
    writeIORef (shAudioLen handle) (currentLen + written)

plusInt16Ptr :: Ptr Int16 -> Int -> Ptr Int16
plusInt16Ptr ptr offset = ptr `plusPtrBytes` (offset * 2)

plusPtrBytes :: Ptr a -> Int -> Ptr a
plusPtrBytes ptr bytes = castPtr (castPtr ptr `plusPtr` bytes)

normalizeButton :: CInt -> Maybe Joypad.Button
normalizeButton code = case fromIntegral code :: Int of
    0 -> Just Joypad.ButtonUp
    1 -> Just Joypad.ButtonDown
    2 -> Just Joypad.ButtonLeft
    3 -> Just Joypad.ButtonRight
    4 -> Just Joypad.ButtonA
    5 -> Just Joypad.ButtonB
    6 -> Just Joypad.ButtonStart
    7 -> Just Joypad.ButtonSelect
    _ -> Nothing

makeHandle :: Web.WebSession -> IO SessionHandle
makeHandle session = do
    let titleBytes = TE.encodeUtf8 (Web.sessionTitle session)
    (titlePtr, titleLen) <- allocBufferFromByteString titleBytes
    framebufferPtr <- mallocBytes framebufferBytes
    audioPtr <- mallocBytes (audioCapacitySamples * 2)
    audioLen <- newIORef 0
    stateBuffer <- newIORef (nullPtr, 0)
    saveBuffer <- newIORef (nullPtr, 0)
    pure
        SessionHandle
            { shSession = session
            , shTitlePtr = titlePtr
            , shTitleLen = titleLen
            , shFramebufferPtr = framebufferPtr
            , shAudioPtr = audioPtr
            , shAudioCap = audioCapacitySamples
            , shAudioLen = audioLen
            , shStateBuffer = stateBuffer
            , shSaveBuffer = saveBuffer
            }

ocelot_alloc :: CSize -> IO (Ptr Word8)
ocelot_alloc size
    | size == 0 = pure nullPtr
    | otherwise = castPtr <$> mallocBytes (fromIntegral size)

ocelot_free :: Ptr Word8 -> CSize -> IO ()
ocelot_free ptr _
    | ptr == nullPtr = pure ()
    | otherwise = free ptr

ocelot_create :: Ptr Word8 -> CSize -> IO CInt
ocelot_create ptr len = do
    result <- try createSession :: IO (Either SomeException CInt)
    case result of
        Right sid
            | sid /= 0 -> clearLastError >> pure sid
            | otherwise -> pure 0
        Left err -> setLastError (displayException err) >> pure 0
  where
    createSession = do
        romBytes <- BS.packCStringLen (castPtr ptr, fromIntegral len)
        loaded <- Web.loadSession romBytes
        case loaded of
            Left err -> setLastError (show err) >> pure 0
            Right session -> do
                Web.setFbTargetRgba session
                insertSession =<< makeHandle session

ocelot_destroy :: CInt -> IO ()
ocelot_destroy sid = do
    runtime <- readIORef runtimeRef
    case IntMap.lookup (fromIntegral sid) (rtSessions runtime) of
        Nothing -> pure ()
        Just handle -> do
            destroyHandle handle
            writeIORef
                runtimeRef
                runtime{rtSessions = IntMap.delete (fromIntegral sid) (rtSessions runtime)}

ocelot_run_frame :: CInt -> IO CInt
ocelot_run_frame sid = do
    result <- withSession sid $ \handle -> do
        runResult <- try (Web.runFrame (shSession handle)) :: IO (Either SomeException ())
        case runResult of
            Left err -> setLastError (displayException err) >> pure 0
            Right () -> do
                copyFramebuffer handle
                drainAudioIntoHandle handle
                clearLastError
                pure 1
    pure (fromMaybe 0 result)

ocelot_set_button :: CInt -> CInt -> CInt -> IO ()
ocelot_set_button sid buttonCode down =
    case normalizeButton buttonCode of
        Nothing -> setLastError "Unknown joypad button code"
        Just button -> do
            _ <-
                withSession sid $ \handle -> do
                    Web.setButton button (down /= 0) (shSession handle)
                    clearLastError
            pure ()

ocelot_framebuffer_ptr :: CInt -> IO (Ptr Word8)
ocelot_framebuffer_ptr sid =
    maybe nullPtr shFramebufferPtr <$> lookupSession sid

ocelot_framebuffer_len :: CInt -> IO CSize
ocelot_framebuffer_len sid =
    maybe 0 (const (fromIntegral framebufferBytes)) <$> lookupSession sid

ocelot_audio_buffer_ptr :: CInt -> IO (Ptr Int16)
ocelot_audio_buffer_ptr sid =
    maybe nullPtr shAudioPtr <$> lookupSession sid

ocelot_audio_buffer_len :: CInt -> IO CSize
ocelot_audio_buffer_len sid = do
    found <- lookupSession sid
    maybe (pure 0) (fmap fromIntegral . readIORef . shAudioLen) found

ocelot_clear_audio_buffer :: CInt -> IO ()
ocelot_clear_audio_buffer sid = do
    _ <- withSession sid (\handle -> writeIORef (shAudioLen handle) 0)
    pure ()

ocelot_save_state :: CInt -> IO CInt
ocelot_save_state sid = do
    result <- withSession sid $ \handle -> do
        saved <- try (Web.saveState (shSession handle)) :: IO (Either SomeException BS.ByteString)
        case saved of
            Left err -> setLastError (displayException err) >> pure 0
            Right blob -> replaceBuffer (shStateBuffer handle) blob >> clearLastError >> pure 1
    pure (fromMaybe 0 result)

ocelot_save_state_ptr :: CInt -> IO (Ptr Word8)
ocelot_save_state_ptr sid = do
    found <- lookupSession sid
    case found of
        Nothing -> pure nullPtr
        Just handle -> fst <$> readIORef (shStateBuffer handle)

ocelot_save_state_len :: CInt -> IO CSize
ocelot_save_state_len sid = do
    found <- lookupSession sid
    case found of
        Nothing -> pure 0
        Just handle -> fromIntegral . snd <$> readIORef (shStateBuffer handle)

ocelot_load_state :: CInt -> Ptr Word8 -> CSize -> IO CInt
ocelot_load_state sid ptr len = do
    result <- withSession sid $ \handle -> do
        blob <- BS.packCStringLen (castPtr ptr, fromIntegral len)
        loaded <- Web.loadState blob (shSession handle)
        case loaded of
            Left err -> setLastError (show err) >> pure 0
            Right () -> clearLastError >> pure 1
    pure (fromMaybe 0 result)

ocelot_extract_save :: CInt -> IO CInt
ocelot_extract_save sid = do
    result <- withSession sid $ \handle -> do
        blob <- Web.extractSaveData (shSession handle)
        replaceBuffer (shSaveBuffer handle) blob
        clearLastError
        pure 1
    pure (fromMaybe 0 result)

ocelot_save_buffer_ptr :: CInt -> IO (Ptr Word8)
ocelot_save_buffer_ptr sid = do
    found <- lookupSession sid
    case found of
        Nothing -> pure nullPtr
        Just handle -> fst <$> readIORef (shSaveBuffer handle)

ocelot_save_buffer_len :: CInt -> IO CSize
ocelot_save_buffer_len sid = do
    found <- lookupSession sid
    case found of
        Nothing -> pure 0
        Just handle -> fromIntegral . snd <$> readIORef (shSaveBuffer handle)

ocelot_load_save :: CInt -> Ptr Word8 -> CSize -> IO CInt
ocelot_load_save sid ptr len = do
    result <- withSession sid $ \handle -> do
        blob <- BS.packCStringLen (castPtr ptr, fromIntegral len)
        Web.loadSaveData blob (shSession handle)
        clearLastError
        pure 1
    pure (fromMaybe 0 result)

ocelot_rom_title_ptr :: CInt -> IO (Ptr Word8)
ocelot_rom_title_ptr sid = maybe nullPtr shTitlePtr <$> lookupSession sid

ocelot_rom_title_len :: CInt -> IO CSize
ocelot_rom_title_len sid = maybe 0 (fromIntegral . shTitleLen) <$> lookupSession sid

ocelot_cartridge_has_battery :: CInt -> IO CInt
ocelot_cartridge_has_battery sid =
    maybe 0 (\h -> if Web.sessionHasBattery (shSession h) then 1 else 0) <$> lookupSession sid

ocelot_is_cgb :: CInt -> IO CInt
ocelot_is_cgb sid =
    maybe 0 (\h -> if Web.sessionIsCgb (shSession h) then 1 else 0) <$> lookupSession sid

ocelot_version_ptr :: IO (Ptr Word8)
ocelot_version_ptr = pure (fst versionBuffer)

ocelot_version_len :: IO CSize
ocelot_version_len = pure (fromIntegral (snd versionBuffer))

ocelot_snapshot_version :: IO CInt
ocelot_snapshot_version = pure (fromIntegral Snapshot.currentVersion)

ocelot_last_error_ptr :: IO (Ptr Word8)
ocelot_last_error_ptr = fst <$> readIORef lastErrorRef

ocelot_last_error_len :: IO CSize
ocelot_last_error_len = fromIntegral . snd <$> readIORef lastErrorRef

ocelot_framebuffer_width :: IO CInt
ocelot_framebuffer_width = pure (fromIntegral Web.framebufferWidth)

ocelot_framebuffer_height :: IO CInt
ocelot_framebuffer_height = pure (fromIntegral Web.framebufferHeight)

ocelot_audio_sample_rate :: IO CInt
ocelot_audio_sample_rate = pure (fromIntegral Web.audioSampleRate)

ocelot_button_up, ocelot_button_down, ocelot_button_left, ocelot_button_right :: IO CInt
ocelot_button_up = pure 0
ocelot_button_down = pure 1
ocelot_button_left = pure 2
ocelot_button_right = pure 3

ocelot_button_a, ocelot_button_b, ocelot_button_start, ocelot_button_select :: IO CInt
ocelot_button_a = pure 4
ocelot_button_b = pure 5
ocelot_button_start = pure 6
ocelot_button_select = pure 7

foreign export ccall ocelot_alloc :: CSize -> IO (Ptr Word8)
foreign export ccall ocelot_free :: Ptr Word8 -> CSize -> IO ()
foreign export ccall ocelot_create :: Ptr Word8 -> CSize -> IO CInt
foreign export ccall ocelot_destroy :: CInt -> IO ()
foreign export ccall ocelot_run_frame :: CInt -> IO CInt
foreign export ccall ocelot_set_button :: CInt -> CInt -> CInt -> IO ()
foreign export ccall ocelot_framebuffer_ptr :: CInt -> IO (Ptr Word8)
foreign export ccall ocelot_framebuffer_len :: CInt -> IO CSize
foreign export ccall ocelot_audio_buffer_ptr :: CInt -> IO (Ptr Int16)
foreign export ccall ocelot_audio_buffer_len :: CInt -> IO CSize
foreign export ccall ocelot_clear_audio_buffer :: CInt -> IO ()
foreign export ccall ocelot_save_state :: CInt -> IO CInt
foreign export ccall ocelot_save_state_ptr :: CInt -> IO (Ptr Word8)
foreign export ccall ocelot_save_state_len :: CInt -> IO CSize
foreign export ccall ocelot_load_state :: CInt -> Ptr Word8 -> CSize -> IO CInt
foreign export ccall ocelot_extract_save :: CInt -> IO CInt
foreign export ccall ocelot_save_buffer_ptr :: CInt -> IO (Ptr Word8)
foreign export ccall ocelot_save_buffer_len :: CInt -> IO CSize
foreign export ccall ocelot_load_save :: CInt -> Ptr Word8 -> CSize -> IO CInt
foreign export ccall ocelot_rom_title_ptr :: CInt -> IO (Ptr Word8)
foreign export ccall ocelot_rom_title_len :: CInt -> IO CSize
foreign export ccall ocelot_cartridge_has_battery :: CInt -> IO CInt
foreign export ccall ocelot_is_cgb :: CInt -> IO CInt
foreign export ccall ocelot_version_ptr :: IO (Ptr Word8)
foreign export ccall ocelot_version_len :: IO CSize
foreign export ccall ocelot_snapshot_version :: IO CInt
foreign export ccall ocelot_last_error_ptr :: IO (Ptr Word8)
foreign export ccall ocelot_last_error_len :: IO CSize
foreign export ccall ocelot_framebuffer_width :: IO CInt
foreign export ccall ocelot_framebuffer_height :: IO CInt
foreign export ccall ocelot_audio_sample_rate :: IO CInt
foreign export ccall ocelot_button_up :: IO CInt
foreign export ccall ocelot_button_down :: IO CInt
foreign export ccall ocelot_button_left :: IO CInt
foreign export ccall ocelot_button_right :: IO CInt
foreign export ccall ocelot_button_a :: IO CInt
foreign export ccall ocelot_button_b :: IO CInt
foreign export ccall ocelot_button_start :: IO CInt
foreign export ccall ocelot_button_select :: IO CInt
