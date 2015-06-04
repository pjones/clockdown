{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Because I suck at naming things.
module Clockdown.Core.Properties
       ( Properties (..)
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Text (Text)
import Data.Time

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Color

--------------------------------------------------------------------------------
data Properties = Properties
  { propName         :: Text        -- ^ The name of the window.
  , propColor        :: Color       -- ^ The main color.
  , propMessage      :: Maybe Text  -- ^ An optional message to display.
  , propMessageColor :: Maybe Color -- ^ Color of the optional message.
  , propTimeLocale   :: TimeLocale  -- ^ Message locale.
  }
