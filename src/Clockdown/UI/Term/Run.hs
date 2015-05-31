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
module Clockdown.UI.Term.Run
       ( run
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Time
import Graphics.Vty

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Properties
import Clockdown.Core.Window
import Clockdown.UI.Common.Action
import Clockdown.UI.Term.Draw

--------------------------------------------------------------------------------
tickThread :: Chan Action -> IO ()
tickThread channel = forever $ do
  now <- getCurrentTime
  writeChan channel (Tick now)
  threadDelay 1000000

--------------------------------------------------------------------------------
drawThread :: Vty -> Chan Action -> IO ()
drawThread vty channel = forever $ do
  tz  <- getCurrentTimeZone
  action <- readChan channel

  case action of
    Tick t -> do
      let clock   = makeClock (Properties "foo") tz
          display = windowDigitalDisplay clock t
      region <- displayBounds (outputIface vty)
      update vty $ picForImage (centerImage region $ drawDisplay display)

--------------------------------------------------------------------------------
-- | Temporary function for testing the drawing functions.
run :: IO ()
run = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  channel <- newChan
  ticker <- async (tickThread channel)

  withAsync (drawThread vty channel) $ \_ -> do
    go vty
    shutdown vty
    cancel ticker

  where
    go vty = do
      e <- nextEvent vty

      case e of
        EvKey KEsc []        -> return ()
        _                    -> go vty
