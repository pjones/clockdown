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
-- | Functions and data types for key bindings.
module Clockdown.UI.Common.Binding
       ( KeyCode     (..)
       , KeyModifier (..)
       , KeyMap
       , parseKeys
       , processKey
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word

--------------------------------------------------------------------------------
data KeyCode = RawKey Char      -- ^ A normal character key.
             | Escape           -- ^ The escape key.
             deriving (Eq, Ord)

--------------------------------------------------------------------------------
data KeyModifier = Shift        -- ^ The shift key.
                 | Control      -- ^ The control key.
                 | Meta         -- ^ The meta/alt/option key.
                 deriving (Eq, Enum)

--------------------------------------------------------------------------------
newtype KeyMap a = KeyMap {unMap :: Map (Word8, KeyCode) a}

--------------------------------------------------------------------------------
instance Functor KeyMap where
  fmap f (KeyMap m) = KeyMap (Map.map f m)

--------------------------------------------------------------------------------
parseKeys :: [(Text, a)] -> Either String (KeyMap a)
parseKeys = undefined

--------------------------------------------------------------------------------
processKey :: [KeyModifier] -> KeyCode -> KeyMap a -> Maybe a
processKey mods key = Map.lookup (modsToWord mods, key) . unMap

--------------------------------------------------------------------------------
modsToWord :: [KeyModifier] -> Word8
modsToWord = foldl (.|.) 0 . map (setBit 0 . fromEnum)
