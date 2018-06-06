{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Config
       ( Config (..)
       , defConfig
       , startingClock
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time
import System.Console.Byline.Color

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Action
import Clockdown.Core.Binding
import Clockdown.Core.Clock
import Clockdown.Core.Countdown
import Clockdown.Core.Properties

--------------------------------------------------------------------------------
data Config = Config
  { configKeys          :: KeyMap Action
  , configClocks        :: Map Text Clock
  , configCountdowns    :: Map Text Countdown
  , configStartingClock :: Text
  }

--------------------------------------------------------------------------------
defConfig :: (MonadIO m) => m Config
defConfig = do
  tz <- liftIO getCurrentTimeZone

  return Config { configKeys          = defaultKeys
                , configClocks        = Map.fromList [ defaultClock tz  ]
                , configCountdowns    = Map.fromList [ defaultCountdown ]
                , configStartingClock = defaultClockName
                }

--------------------------------------------------------------------------------
startingClock :: (MonadIO m) => Config -> m Clock
startingClock c =
  case Map.lookup (configStartingClock c) (configClocks c) of
    Nothing    -> return . snd . defaultClock =<< liftIO getCurrentTimeZone
    Just clock -> return clock

--------------------------------------------------------------------------------
defaultKeys :: KeyMap Action
defaultKeys =
  makeKeyMap [ (([],      Escape),      Quit)
             , (([],      RawKey 'q'),  CloseWindowOrQuit)
             , (([],      RawKey '\t'), NextWindow)
             , (([Shift], RawKey '\t'), PrevWindow)
             , (([],      RawKey '='),  TimeSucc)
             , (([],      RawKey '+'),  TimeSucc)
             , (([],      RawKey '-'),  TimePred)
             , (([],      RawKey '_'),  TimeSucc)
             , (([],      RawKey '1'),  NewCountdown defaultCountdownName)
             ]

--------------------------------------------------------------------------------
defaultClockName :: Text
defaultClockName = "local"

--------------------------------------------------------------------------------
defaultClock :: TimeZone -> (Text, Clock)
defaultClock tz = (defaultClockName, clock)
  where
    clock :: Clock
    clock =  Clock props tz

    props :: Properties
    props =  Properties { propName         = defaultClockName
                        , propColor        = blue
                        , propMessage      = Just "%b. %d, %Y"
                        , propMessageColor = Nothing
                        , propTimeLocale   = defaultTimeLocale
                        }

--------------------------------------------------------------------------------
defaultCountdownName :: Text
defaultCountdownName = "ten"

--------------------------------------------------------------------------------
defaultCountdown :: (Text, Countdown)
defaultCountdown = (defaultCountdownName, countdown)
  where
    countdown :: Countdown
    countdown =  Countdown { countProps       = props
                           , countDoneColor   = Just red
                           , countColorChange = 120
                           , countDuration    = 600
                           , countEnd         = Nothing
                           , countOrigColor   = Nothing
                           }

    props :: Properties
    props =  Properties { propName         = defaultCountdownName
                        , propColor        = green
                        , propMessage      = Just "Break Time!"
                        , propMessageColor = Nothing
                        , propTimeLocale   = defaultTimeLocale
                        }
