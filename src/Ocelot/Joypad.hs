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
import Data.Bits (complement, shiftL, testBit, (.&.), (.|.))
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

The joypad interrupt fires on a 1->0 transition of any bit in the
low nibble of @P1@. We compute the low nibble before and after the
press to check for an actual falling edge, which correctly handles
the case where both rows are selected and the button being pressed
shares a column with an already-pressed button in the other row (the
two rows AND together, so the bit was already low and pressing the
new button does not constitute a new falling edge).
-}
setButton :: Button -> Bool -> JoypadState -> IO ()
setButton b !pressed jp = do
    btns <- readIORef (jpButtons jp)
    let !already = Set.member b btns
    if pressed
        then unless already $ do
            sel <- readIORef (jpRowSelect jp)
            let !oldLow = lowNibble sel btns
                !newBtns = Set.insert b btns
                !newLow = lowNibble sel newBtns
            writeIORef (jpButtons jp) newBtns
            when ((oldLow .&. complement newLow) /= 0) $
                writeIORef (jpIrqPending jp) True
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
    pure (0xC0 .|. (sel .&. 0x30) .|. lowNibble sel btns)

{- | Write the @P1@ register. Only bits 4-5 are writable; the rest are
ignored on real hardware.

A row-select change can drive a previously-unseen pressed button bit
from 1 to 0 (e.g. the game switches from the action row to the
direction row while @Up@ is held). That counts as a joypad-IRQ
falling edge, so this helper compares the low-nibble before and after
the change and latches @jpIrqPending@ on any 1->0 transition (matches
SameBoy 'GB_update_joyp' line 136 / 152).
-}
writeP1 :: Word8 -> JoypadState -> IO ()
writeP1 v jp = do
    btns <- readIORef (jpButtons jp)
    oldSel <- readIORef (jpRowSelect jp)
    let !oldLow = lowNibble oldSel btns
        !newSel = v .&. 0x30
        !newLow = lowNibble newSel btns
    writeIORef (jpRowSelect jp) newSel
    when ((oldLow .&. complement newLow) /= 0) $
        writeIORef (jpIrqPending jp) True

{- | Compute the active-low bottom nibble of @P1@ for a given row-select
byte and pressed-button set. Lifted out of 'readP1' so 'writeP1' can
reuse it for edge detection without duplicating the row-select logic.
-}
lowNibble :: Word8 -> Set Button -> Word8
lowNibble sel btns =
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
     in case (actionRow, directionRow) of
            (True, True) -> actionBits .&. directionBits
            (True, False) -> actionBits
            (False, True) -> directionBits
            (False, False) -> 0x0F
