{-# LANGUAGE BangPatterns #-}

{- | Game Boy joypad register at @0xFF00@.

The register works as a 4x2 matrix: writes select either the action-button
row (bit 5 low) or the direction-button row (bit 4 low), and reads expose the
four selected buttons in bits 0..3 with @0 = pressed@. Bits 6..7 always read
as 1.

The frontend (terminal or SDL) calls 'setButton' to push physical input. The
bus calls 'readP1' / 'writeP1' for memory-mapped access. State is kept in
'IORef's so the frontend's input thread and the emulation loop can share
ownership without a State monad.
-}
module Ocelot.Joypad (
    JoypadState,
    Button (..),
    initial,
    setButton,
    readP1,
    writeP1,
    takeIrqPending,
    isPressed,
    dumpState,
    loadState,
) where

import Control.Monad (unless, when)
import Data.Bits (shiftL, testBit, (.&.), (.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)

data Button
    = ButtonA
    | ButtonB
    | ButtonStart
    | ButtonSelect
    | ButtonUp
    | ButtonDown
    | ButtonLeft
    | ButtonRight
    deriving (Eq, Ord, Show, Bounded, Enum)

data JoypadState = JoypadState
    { jpButtons :: !(IORef (Set Button))
    , jpRowSelect :: !(IORef Word8)
    -- ^ The byte last written to @0xFF00@. Bits 4 (action row) and 5
    -- (direction row) are active-low.
    , jpIrqPending :: !(IORef Bool)
    -- ^ Set by 'setButton' on a falling edge of any selected-row button.
    -- Consumed by 'takeIrqPending' on each bus advance.
    }

{- | All buttons released, both rows enabled (action and direction). Real
hardware powers up with @P1=0xCF@ (bits 4-5 low, all four button bits high).
-}
initial :: IO JoypadState
initial = do
    btns <- newIORef Set.empty
    sel <- newIORef 0x00
    irq <- newIORef False
    pure (JoypadState btns sel irq)

{- | Set a button as pressed ('True') or released ('False'). Called by the
frontend on every key state change.

If the press transitions a selected-row button bit from 1 to 0 (the SM83's
joypad-interrupt condition), @jpIrqPending@ is latched so the next bus
advance raises @IF@ bit 4.
-}
setButton :: Button -> Bool -> JoypadState -> IO ()
setButton b !pressed jp = do
    btns <- readIORef (jpButtons jp)
    let !already = Set.member b btns
    if pressed
        then do
            modifyIORef' (jpButtons jp) (Set.insert b)
            -- Falling edge candidate: button newly pressed AND its row is selected.
            unless already $ do
                sel <- readIORef (jpRowSelect jp)
                let action = b == ButtonA || b == ButtonB || b == ButtonSelect || b == ButtonStart
                    actionRow = not (testBit sel 5)
                    directionRow = not (testBit sel 4)
                    edge = (action && actionRow) || (not action && directionRow)
                when edge (writeIORef (jpIrqPending jp) True)
        else modifyIORef' (jpButtons jp) (Set.delete b)

{- | Read and clear the pending-IRQ latch. Called by 'Ocelot.Bus.advance' so
that a falling edge raises @IF@ bit 4 exactly once.
-}
takeIrqPending :: JoypadState -> IO Bool
takeIrqPending jp = do
    p <- readIORef (jpIrqPending jp)
    when p (writeIORef (jpIrqPending jp) False)
    pure p

-- | Whether a button is currently pressed (helper for tests).
isPressed :: Button -> JoypadState -> IO Bool
isPressed b jp = Set.member b <$> readIORef (jpButtons jp)

{- | Snapshot the joypad to a 3-byte blob: row-select, button bitmask,
IRQ-pending. Bit positions in the bitmask follow the 'Button' 'Enum'
order (A=0, B=1, ... Right=7).
-}
dumpState :: JoypadState -> IO (Word8, Word8, Bool)
dumpState jp = do
    btns <- readIORef (jpButtons jp)
    sel <- readIORef (jpRowSelect jp)
    irq <- readIORef (jpIrqPending jp)
    let bit b = 1 `shiftL` fromEnum b
        mask =
            foldr
                (\b acc -> if Set.member b btns then acc .|. bit b else acc)
                (0x00 :: Word8)
                [minBound .. maxBound]
    pure (sel, mask, irq)

-- | Restore the joypad from a 'dumpState' triple.
loadState :: (Word8, Word8, Bool) -> JoypadState -> IO ()
loadState (sel, mask, irq) jp = do
    let btns = Set.fromList [b | b <- [minBound .. maxBound], testBit mask (fromEnum b)]
    writeIORef (jpButtons jp) btns
    writeIORef (jpRowSelect jp) sel
    writeIORef (jpIrqPending jp) irq

{- | Read the @P1@ register. Top 2 bits always 1; bits 4-5 reflect the last
write (active-low row select); bits 0-3 expose the four buttons of the
selected row(s), with @0 = pressed@.
-}
readP1 :: JoypadState -> IO Word8
readP1 jp = do
    sel <- readIORef (jpRowSelect jp)
    btns <- readIORef (jpButtons jp)
    let actionRow = not (testBit sel 5)
        directionRow = not (testBit sel 4)
        bit p mask = if Set.member p btns then 0 else mask
        actionBits =
            bit ButtonA 0x01
                .|. bit ButtonB 0x02
                .|. bit ButtonSelect 0x04
                .|. bit ButtonStart 0x08
        directionBits =
            bit ButtonRight 0x01
                .|. bit ButtonLeft 0x02
                .|. bit ButtonUp 0x04
                .|. bit ButtonDown 0x08
        lowNibble
            | actionRow && directionRow = actionBits .&. directionBits
            | actionRow = actionBits
            | directionRow = directionBits
            | otherwise = 0x0F
    pure (0xC0 .|. (sel .&. 0x30) .|. lowNibble)

{- | Write the @P1@ register. Only bits 4-5 are writable; the rest are
ignored on real hardware.
-}
writeP1 :: Word8 -> JoypadState -> IO ()
writeP1 v jp = writeIORef (jpRowSelect jp) (v .&. 0x30)
