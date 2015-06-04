{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Action
       ( Action (..)
       , parseAction
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Actions which can be triggered by the system or the user.
data Action = Tick
              -- ^ Update the clock.

            | NewCountdown Text
              -- ^ Add a countdown window to the end of the window
              -- list and then focus it.

            | PrevWindow
              -- ^ Focus the previous window.

            | NextWindow
              -- ^ Focus the next window.

            | TimeSucc
              -- ^ Update the time in the focused window.

            | TimePred
              -- ^ Update the time in the focused window.

            | Quit
              -- ^ Quit the application.

            deriving (Eq)

--------------------------------------------------------------------------------
parseAction :: Text -> Either String Action
parseAction = undefined
