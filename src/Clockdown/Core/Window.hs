{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Window
       ( Window
       , newClockWindow
       , newCountDownWindow
       , windowTick
       , windowDigitalDisplay
       , windowProperties
       , windowSucc
       , windowPred
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Time

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Clock
import Clockdown.Core.Countdown
import qualified Clockdown.Core.Digital.Display as Digital
import Clockdown.Core.Properties

--------------------------------------------------------------------------------
-- | A type to hold the information about what should be displayed in
-- a window.
data Window = ClockWin Clock
            | CountdownWin Countdown

--------------------------------------------------------------------------------
newClockWindow :: UTCTime -> Clock -> Window
newClockWindow t c = windowTick t (ClockWin c)

--------------------------------------------------------------------------------
newCountDownWindow :: UTCTime -> Countdown -> Window
newCountDownWindow t c = windowTick t $ CountdownWin (countDownStart t c)

--------------------------------------------------------------------------------
windowTick :: UTCTime -> Window -> Window
windowTick _ (ClockWin c) = ClockWin c -- No ticking necessary.
windowTick t (CountdownWin c) = CountdownWin (countDownTick t c)

--------------------------------------------------------------------------------
-- | Convert a window into a digital display.
windowDigitalDisplay :: Window -> UTCTime -> Digital.Display
windowDigitalDisplay (ClockWin c)     = clockDigitalDisplay c
windowDigitalDisplay (CountdownWin c) = countDownDigitalDisplay c

--------------------------------------------------------------------------------
-- | Get the display properties for a window.
windowProperties :: Window -> Properties
windowProperties (ClockWin c)     = clockProps c
windowProperties (CountdownWin c) = countProps c

--------------------------------------------------------------------------------
-- | Move the time shown in a window forward by some amount.
windowSucc :: UTCTime -> Window -> Window
windowSucc _ (ClockWin c)     = ClockWin (clockSucc c)
windowSucc t (CountdownWin c) = CountdownWin (countDownSucc t c)

--------------------------------------------------------------------------------
-- | Move the time show in a window backward by some amount.
windowPred :: UTCTime -> Window -> Window
windowPred _ (ClockWin c)     = ClockWin (clockPred c)
windowPred t (CountdownWin c) = CountdownWin (countDownPred t c)
