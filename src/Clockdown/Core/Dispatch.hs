{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Dispatch
       ( dispatch
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Action
import Clockdown.Core.Clockdown
import Clockdown.Core.Config
import Clockdown.Core.Stack
import Clockdown.Core.Window

--------------------------------------------------------------------------------
dispatch :: (Monad m) => (UTCTime, Action) -> Clockdown r m UTCTime
dispatch (t, a) = do
  windows <- get

  case a of
    Tick ->
      -- Update all windows with the current tick.
      put $ fmap (windowTick t) windows

    PrevWindow ->
      put (focusLeft windows)

    NextWindow ->
      put (focusRight windows)

    NewCountdown name ->
      countdown t name

    Quit ->
      return ()

  return t

--------------------------------------------------------------------------------
countdown :: (Monad m) => UTCTime -> Text -> Clockdown r m ()
countdown tick name = newWin name configCountdowns (newCountDownWindow tick)

--------------------------------------------------------------------------------
newWin :: (Monad m) => Text -> (Config -> Map Text a) -> (a -> Window) -> Clockdown r m ()
newWin t m f = do
  c <- asks config

  case Map.lookup t (m c) of
    Nothing -> return ()
    Just a  -> modify (push (f a))
