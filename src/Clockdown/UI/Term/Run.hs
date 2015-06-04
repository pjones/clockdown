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
import Graphics.Vty hiding (Config)

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Action
import Clockdown.Core.Clockdown
import Clockdown.Core.Config
import Clockdown.Core.Dispatch
import Clockdown.Core.Stack
import Clockdown.Core.Window
import Clockdown.UI.Term.Binding
import Clockdown.UI.Term.Draw

--------------------------------------------------------------------------------
data Env = Env
  { vty     :: Vty
  , channel :: Chan (UTCTime, Action)
  }

--------------------------------------------------------------------------------
tickThread :: Clockdown Env IO ()
tickThread =
  do chan <- asks (channel . private)
     forever (liftIO $ go chan)
  where
    go chan = do
      now <- getCurrentTime
      writeChan chan (now, Tick)
      threadDelay 1000000

--------------------------------------------------------------------------------
drawThread :: Clockdown Env IO ()
drawThread = forever $ do
  env    <- asks private
  tick   <- dispatch =<< liftIO (readChan $ channel env)
  window <- gets focus
  liftIO $  draw (vty env) tick window

  where
    draw :: Vty -> UTCTime -> Window -> IO ()
    draw v t w = do
      region <- displayBounds (outputIface v)
      update v (picForImage $ drawWindow t w region)

--------------------------------------------------------------------------------
eventThread :: Clockdown Env IO ()
eventThread = asks config >>= \c -> forever $ do
  e <- liftIO . nextEvent =<< asks (vty . private)
  maybe (return ()) send (eventToAction e c)

  where
    send :: Action -> Clockdown Env IO ()
    send action =
      do now <- liftIO getCurrentTime
         chan <- asks (channel . private)
         liftIO (writeChan chan (now, action))

--------------------------------------------------------------------------------
-- | Temporary function for testing the drawing functions.
run :: IO ()
run = do
  vty'     <- mkVty =<< standardIOConfig
  channel' <- newChan
  cfg      <- defConfig
  now      <- getCurrentTime
  windows  <- (stack . newClockWindow now) <$> startingClock cfg

  let env       = Env vty' channel'
      clockdown = void . runClockdown env cfg windows

  threads <- sequence [ async (clockdown tickThread)
                      , async (clockdown drawThread)
                      , async (clockdown eventThread)
                      ]

  _ <- waitAnyCatchCancel threads
  shutdown vty'
