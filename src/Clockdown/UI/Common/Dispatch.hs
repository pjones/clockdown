{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.UI.Common.Dispatch
       ( dispatch
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Time (UTCTime)

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Clockdown
import Clockdown.Core.Stack
import Clockdown.Core.Window
import Clockdown.UI.Common.Action

--------------------------------------------------------------------------------
dispatch :: (Monad m) => Action -> Clockdown r m (Maybe UTCTime)
dispatch a = do
  windows <- get

  case a of
    Tick t -> do
      -- Update all windows, then return the main window.
      put $ fmap (windowTick t) windows
      return (Just t)

    NextWindow -> do
      put (focusRight windows)
      return Nothing

    NewWindow w -> do
      put (push w windows)
      return Nothing

    Quit -> return Nothing
