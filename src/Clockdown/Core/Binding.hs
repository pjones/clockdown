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
module Clockdown.Core.Binding
       ( KeyCode     (..)
       , KeyModifier (..)
       , KeyMap
       , makeKeyMap
       , parseKeys
       , processBinding
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word

--------------------------------------------------------------------------------
-- | Keys.
data KeyCode = RawKey Char      -- ^ A normal character key.
             | Escape           -- ^ The escape key.
             | Backspace        -- ^ The backspace key.
             | FKey Int         -- ^ One of the F keys.
             deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- | Modifier keys.
data KeyModifier = Shift        -- ^ The shift key.
                 | Control      -- ^ The control key.
                 | Meta         -- ^ The meta/alt/option key.
                 deriving (Eq, Enum)

--------------------------------------------------------------------------------
-- | A @KeyMap@ allows you to connect key events to some other type.
newtype KeyMap a = KeyMap {unMap :: Map (Word8, KeyCode) a}

--------------------------------------------------------------------------------
instance Functor KeyMap where
  fmap f (KeyMap m) = KeyMap (Map.map f m)

--------------------------------------------------------------------------------
-- | Build a 'KeyMap' by hand using an association list.
makeKeyMap :: [(([KeyModifier], KeyCode), a)] -> KeyMap a
makeKeyMap alist = KeyMap (Map.fromList $ list alist)
  where
    list :: [(([KeyModifier], KeyCode), a)] -> [((Word8, KeyCode), a)]
    list = map (\((ms, k), a) -> ((modsToWord ms, k), a))

--------------------------------------------------------------------------------
parseKeys :: [(Text, a)] -> Either String (KeyMap a)
parseKeys = undefined

--------------------------------------------------------------------------------
processBinding :: KeyMap a -> ([KeyModifier], KeyCode) -> Maybe a
processBinding m (mods, key) = (Map.lookup (modsToWord mods, key) . unMap) m

--------------------------------------------------------------------------------
modsToWord :: [KeyModifier] -> Word8
modsToWord = foldl (.|.) 0 . map (setBit 0 . fromEnum)
