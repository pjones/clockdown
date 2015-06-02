{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.UI.Common.Action
       ( Action (..)
       , parseAction
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Text (Text)
import Data.Time

--------------------------------------------------------------------------------
-- | Actions which can be triggered by the system or the user.
data Action = Tick UTCTime
              -- ^ Update the clock.

            | Quit
              -- ^ Quit the application.
            deriving (Show, Read)

--------------------------------------------------------------------------------
parseAction :: Text -> Either String Action
parseAction = undefined
