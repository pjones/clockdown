{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Countdown
       ( Countdown (..)
       , countDownDigitalDisplay
       , countDownSucc
       , countDownPred
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Time

--------------------------------------------------------------------------------
-- Local imports:
import qualified Clockdown.Core.Digital.Display as Digital
import Clockdown.Core.Properties

--------------------------------------------------------------------------------
data Countdown = Countdown
  { countProps :: Properties
  , countEnd   :: UTCTime
  }

--------------------------------------------------------------------------------
countDownDigitalDisplay :: Countdown -> UTCTime -> Digital.Display
countDownDigitalDisplay c t = Digital.digitalCountDown secs
  where secs = max 0 (truncate $ diffUTCTime (countEnd c) t)

--------------------------------------------------------------------------------
-- | Move a countdown forward one minute.
countDownSucc :: Countdown -> Countdown
countDownSucc = undefined

--------------------------------------------------------------------------------
-- | Move a countdown backward one minute.
countDownPred :: Countdown -> Countdown
countDownPred = undefined
