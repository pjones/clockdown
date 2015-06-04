{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.UI.Term.Binding
       ( fixupKeys
       , convertVtyEvent
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import qualified Graphics.Vty as V

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Binding

--------------------------------------------------------------------------------
-- | Fix some Vty key events so that they report the correct keys.
fixupKeys :: V.Event -> V.Event
fixupKeys (V.EvKey V.KBackTab []) = V.EvKey (V.KChar '\t') [V.MShift]
fixupKeys e                       = e

--------------------------------------------------------------------------------
-- | Convert Vty events into Clockdown bindings.
convertVtyEvent :: V.Event -> Maybe ([KeyModifier], KeyCode)
convertVtyEvent (V.EvMouse{})    = Nothing
convertVtyEvent (V.EvResize{})   = Nothing
convertVtyEvent (V.EvKey key ms) = (,) (map convertMod ms) <$> convertKey key

--------------------------------------------------------------------------------
convertMod :: V.Modifier -> KeyModifier
convertMod V.MShift = Shift
convertMod V.MCtrl  = Control
convertMod V.MMeta  = Meta
convertMod V.MAlt   = Meta

--------------------------------------------------------------------------------
convertKey :: V.Key -> Maybe KeyCode
convertKey (V.KChar c) = Just (RawKey c)
convertKey V.KEsc      = Just Escape
convertKey V.KBS       = Just Backspace
convertKey (V.KFun n)  = Just (FKey n)
convertKey _           = Nothing
