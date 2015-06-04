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
       , countDownStart
       , countDownTick
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
import Clockdown.Core.Color

--------------------------------------------------------------------------------
data Countdown = Countdown
  { countProps       :: Properties    -- ^ Properties.
  , countDoneColor   :: Maybe Color   -- ^ Main color when done.
  , countColorChange :: Int           -- ^ When to change colors.
  , countDuration    :: Int           -- ^ Number of seconds.

  -- The following fields are not user settable.
  , countEnd         :: Maybe UTCTime -- ^ Only set after countdown is running.
  , countOrigColor   :: Maybe Color   -- ^ Original property color.
  }

--------------------------------------------------------------------------------
countDownStart :: UTCTime -> Countdown -> Countdown
countDownStart t c = c { countEnd = Just endTime }
  where
    endTime :: UTCTime
    endTime = addUTCTime (fromInteger . toInteger $ countDuration c) t

--------------------------------------------------------------------------------
-- | Update a countdown.  May change colors.  The code is screaming
-- out for refactoring via lens.  Either way, I'd like to change this
-- code so that it actually just fires off an action and then create
-- an action called "ChangeColor".
countDownTick :: UTCTime -> Countdown -> Countdown
countDownTick t c
  | countDownSecondsLeft c t <= countColorChange c = changeColor
  | otherwise                                      = restoreColor

  where
    changeColor :: Countdown
    changeColor = case (countDoneColor c, countOrigColor c) of
                    -- We can change the color.
                    (Just color, Nothing) -> c { countProps     = newColor color
                                               , countOrigColor = Just oldColor
                                               }
                    -- Leave things alone.
                    (_,          _)       -> c

    restoreColor :: Countdown
    restoreColor = case countOrigColor c of
                     -- The color was changed, restore it.
                     Just color -> c { countProps     = newColor color
                                     , countOrigColor = Nothing
                                     }

                     -- Original color still intact.
                     Nothing    -> c

    newColor :: Color -> Properties
    newColor color = (countProps c) {propColor = color}

    oldColor :: Color
    oldColor = propColor (countProps c)

--------------------------------------------------------------------------------
countDownDigitalDisplay :: Countdown -> UTCTime -> Digital.Display
countDownDigitalDisplay c t = Digital.digitalCountDown (countDownSecondsLeft c t)

--------------------------------------------------------------------------------
countDownSecondsLeft :: Countdown -> UTCTime -> Int
countDownSecondsLeft c t =
  case countEnd c of
    Just end -> max 0 (truncate $ diffUTCTime end t)
    Nothing  -> 0

--------------------------------------------------------------------------------
-- | Add one minute to the countdown.
countDownSucc :: UTCTime -> Countdown -> Countdown
countDownSucc t c = countDownTick t $ c { countEnd = newEnd }
  where
    newEnd :: Maybe UTCTime
    newEnd = case countEnd c of
               Nothing -> Nothing
               Just t'
                 | countDownSecondsLeft c t <= 0 -> Just (addUTCTime 60 t)
                 | otherwise                     -> Just (addUTCTime 60 t')

--------------------------------------------------------------------------------
-- | Remove one minute from the countdown.
countDownPred :: UTCTime -> Countdown -> Countdown
countDownPred t c = countDownTick t $ c { countEnd = newEnd }
  where
    newEnd :: Maybe UTCTime
    newEnd = case countEnd c of
               Nothing -> Nothing
               Just t'
                 | countDownSecondsLeft c t > 0 -> Just (addUTCTime (-60) t')
                 | otherwise                    -> Just t'
