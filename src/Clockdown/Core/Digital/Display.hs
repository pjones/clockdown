{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | A digital display containing four seven-segment indicators.
module Clockdown.Core.Digital.Display
       ( Display (..)
       , digitalClock
       , countDown
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Time.LocalTime

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Digital.Indicator

--------------------------------------------------------------------------------
-- | Indicators are numbered from left to right.
data Display = Display
  { indicator0 :: Indicator
  , indicator1 :: Indicator
  , indicator2 :: Indicator
  , indicator3 :: Indicator
  }

--------------------------------------------------------------------------------
-- | Create a display suitable for showing a clock consisting of hours
-- and minutes.  The indicators will read from left to right: @H H M M@.
digitalClock :: TimeOfDay -> Display
digitalClock t = display (todHour t) (todMin t)

--------------------------------------------------------------------------------
-- | Create a display to show the number of seconds remaining in a
-- count down timer.  When there are more than sixth minutes remaining
-- the display will show hours and minutes.  Otherwise it will show
-- minutes and seconds.
countDown :: Int -> Display
countDown n = if hh > 0 then display hh mm else display mm ss
  where
    hh = n `div` 3600
    mm = (n `div` 60) - (hh * 60)
    ss = n - (hh * 3600) - (mm * 60)

--------------------------------------------------------------------------------
-- | Generic display creator.
display :: Int -> Int -> Display
display l r = Display (indicator lTens) (indicator lOnes)
                      (indicator rTens) (indicator rOnes)
  where
    (lTens, lOnes) = splitNumber l
    (rTens, rOnes) = splitNumber r

--------------------------------------------------------------------------------
-- | Split a number so that it can be displayed on two indicators.
splitNumber :: Int -> (Int, Int)
splitNumber n = (n `div` 10, n `mod` 10)
