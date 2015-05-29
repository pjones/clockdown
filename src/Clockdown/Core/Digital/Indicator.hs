{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | A single seven-segment indicator for a digital clock.
module Clockdown.Core.Digital.Indicator
       ( Indicator
       , Segment (..)
       , indicator
       , testSegment
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Bits
import Data.Word

--------------------------------------------------------------------------------
-- | An indicator (seven-segment display) is made up of segments
-- which "light up" in different arrangements in order to display a
-- single digit.
data Indicator = Indicator Word8 deriving (Show)

--------------------------------------------------------------------------------
-- | A segment represents a single bar (light) on a seven-segment
-- display.  The segments are given names from characters A through G.
--
-- @
--       -- A --
--      |       |
--      F       B
--      |       |
--       -- G --
--      |       |
--      E       C
--      |       |
--       -- D --
-- @
data Segment = A | B | C | D | E | F | G deriving (Eq, Enum, Bounded, Show)

--------------------------------------------------------------------------------
-- | Create an indicator that represents the given integer.  Only the numbers
-- 0 through 9 can be displayed.  Numbers outside that range will be
-- drawn as an underscore.
indicator :: Int -> Indicator
indicator n = Indicator $
  case n of
    0 -> 0x3f; 1 -> 0x06
    2 -> 0x5b; 3 -> 0x4f
    4 -> 0x66; 5 -> 0x6d
    6 -> 0x7d; 7 -> 0x07
    8 -> 0x7f; 9 -> 0x6f
    _ -> 0x08

--------------------------------------------------------------------------------
-- | Test to see if an indicator should be lit up.
testSegment :: Indicator -> Segment -> Bool
testSegment (Indicator bits) = testBit bits . fromEnum
