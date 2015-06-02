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
import Clockdown.Core.Clockdown
import Clockdown.Core.Countdown
import Clockdown.Core.Properties
import qualified Clockdown.Core.Stack as S
import Clockdown.Core.Window
import Clockdown.UI.Common.Action
import Clockdown.UI.Common.Dispatch
import Clockdown.UI.Term.Draw

--------------------------------------------------------------------------------
tickThread :: Chan Action -> IO ()
tickThread channel = forever $ do
  now <- getCurrentTime
  writeChan channel (Tick now)
  threadDelay 1000000

--------------------------------------------------------------------------------
drawThread :: Vty -> S.Stack Window -> Chan Action -> IO ()
drawThread vty wins channel = runClockdown vty wins $ forever $ do
  action <- liftIO (readChan channel)
  (mtick, window) <- dispatch action
  tick <- maybe (liftIO getCurrentTime) return mtick
  liftIO (draw $ windowDigitalDisplay window tick)

  where draw d = do
          region <- displayBounds (outputIface vty)
          update vty $ picForImage (centerImage region $ drawDisplay d)

--------------------------------------------------------------------------------
-- | Temporary function for testing the drawing functions.
run :: IO ()
run = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  channel <- newChan
  ticker <- async (tickThread channel)
  tz  <- getCurrentTimeZone

  withAsync (drawThread vty (wins tz) channel) $ \_ -> do
    go channel vty
    shutdown vty
    cancel ticker

  where
    wins tz = S.stack (makeClock (Properties "foo") tz)

    go chan vty = do
      e <- nextEvent vty

      case e of
        EvKey KEsc []        -> return ()
        EvKey (KChar '1') [] -> do
          now <- getCurrentTime
          let end = addUTCTime 90 now
          let w = CountdownWin $ Countdown (Properties "foo") end
          writeChan chan (NewWindow w)
          go chan vty
        _                    -> go chan vty
