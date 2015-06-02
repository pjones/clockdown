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
import qualified Clockdown.Core.Stack as S
import Clockdown.Core.Window
import Clockdown.UI.Common.Action

--------------------------------------------------------------------------------
dispatch :: (Monad m) => Action -> Clockdown r m (Maybe UTCTime, Window)
dispatch a = do
  windows <- get

  case a of
    Tick t -> do
      -- Update all windows, then return the main window.
      let windows' = fmap (windowTick t) windows
      put windows'
      return (Just t,  S.head windows')

    NewWindow w -> do
      let windows' = S.push w windows
      put windows'
      return (Nothing, w)

    Quit -> return (Nothing, S.head windows)
