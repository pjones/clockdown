{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Clock
       ( Clock (..)
       , clockDigitalDisplay
       , clockForward
       , clockBackward
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Time

--------------------------------------------------------------------------------
-- Local imports:
import qualified Clockdown.Core.Digital.Display as Digital
import Clockdown.Core.Properties

--------------------------------------------------------------------------------
data Clock = Clock
  { clockProps    :: Properties
  , clockTimeZone :: TimeZone
  }

--------------------------------------------------------------------------------
clockDigitalDisplay :: Clock -> UTCTime -> Digital.Display
clockDigitalDisplay c t = Digital.digitalClock (localTimeOfDay time)
  where time = utcToLocalTime (clockTimeZone c) t

--------------------------------------------------------------------------------
-- | Move a clock forward one hour.
clockForward :: Clock -> Clock
clockForward = modifyClockTZ (+60)

--------------------------------------------------------------------------------
-- | Move a clock backward one hour.
clockBackward :: Clock -> Clock
clockBackward = modifyClockTZ (subtract 60)

--------------------------------------------------------------------------------
-- | Helper function to alter a clock's time zone.
modifyClockTZ :: (Int -> Int) -> Clock -> Clock
modifyClockTZ f c = c {clockTimeZone = newTZ}
  where
    newMin = f (timeZoneMinutes $ clockTimeZone c)
    newTZ = (clockTimeZone c) {timeZoneMinutes = newMin}
