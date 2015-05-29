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
       , makeClock
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
import qualified Clockdown.Core.Digital.Display as Digital
import Clockdown.Core.Properties

--------------------------------------------------------------------------------
-- | A type to hold the information about what should be displayed in
-- a window.
data Window = ClockWin Clock

--------------------------------------------------------------------------------
makeClock :: Properties -> TimeZone -> Window
makeClock p = ClockWin . Clock p

--------------------------------------------------------------------------------
-- | Convert a window into a digital display.
windowDigitalDisplay :: Window -> UTCTime -> Digital.Display
windowDigitalDisplay (ClockWin c) = clockDigitalDisplay c

--------------------------------------------------------------------------------
-- | Get the display properties for a window.
windowProperties :: Window -> Properties
windowProperties (ClockWin c) = clockProps c

--------------------------------------------------------------------------------
-- | Move the time shown in a window forward by some amount.
windowSucc :: Window -> Window
windowSucc (ClockWin c) = ClockWin (clockForward c)

--------------------------------------------------------------------------------
-- | Move the time show in a window backward by some amount.
windowPred :: Window -> Window
windowPred (ClockWin c) = ClockWin (clockBackward c)
