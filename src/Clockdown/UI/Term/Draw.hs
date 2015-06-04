{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Functions for creating Vty images to display a clock/timer.
module Clockdown.UI.Term.Draw
       ( drawWindow
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Graphics.Vty
import Graphics.Vty.Prelude

--------------------------------------------------------------------------------
-- Local imports:
import qualified Clockdown.Core.Color as C
import Clockdown.Core.Digital.Display
import Clockdown.Core.Digital.Indicator
import Clockdown.Core.Properties
import Clockdown.Core.Window

--------------------------------------------------------------------------------
drawWindow :: UTCTime -> Window -> DisplayRegion -> Image
drawWindow t w r = vertCat [ timeDigits, message ]
  where
    display    = windowDigitalDisplay w t
    props      = windowProperties w
    timeDigits = centerImage r $ drawDisplay props display
    message    = centerImage (regionWidth r, 1) $ drawMessage props t

--------------------------------------------------------------------------------
-- | Draw a single indicator into a Vty image.
drawIndicator :: Properties -> Indicator -> Image
drawIndicator props ssd =
  withBorder [ paintA F A B
             , paintB F B
             , paintA F G B
             , paintB E C
             , paintA E D C
             ]
  where
    on  = defAttr `withBackColor` vtyColor (propColor props)
    off = defAttr

    whichAttr b = if b then on else off
    paint n b = string (whichAttr b) $ replicate n ' '

    paintA a b c = if ssd `hasSeg` b
                      then paint 6 True
                      else paintB a c

    paintB a b = horizCat [ paint 2 (ssd `hasSeg` a)
                          , paint 2 False
                          , paint 2 (ssd `hasSeg` b)
                          ]

--------------------------------------------------------------------------------
-- | Draw the time separator into a Vty image.
drawSep :: Properties -> Image
drawSep props = withBorder [ string defAttr "  "
                           , string (defAttr `withBackColor` c) "  "
                           , string defAttr "  "
                           , string (defAttr `withBackColor` c) "  "
                           , string defAttr "  "
                           ]
  where
    c = vtyColor (propColor props)

--------------------------------------------------------------------------------
drawMessage :: Properties -> UTCTime -> Image
drawMessage p t =
  case propMessage p of
    Nothing  -> emptyImage
    Just msg -> string (defAttr `withForeColor` color) (format msg)

  where
    format :: Text -> String
    format s = formatTime (propTimeLocale p) (Text.unpack s) t

    color :: Color
    color = vtyColor $ fromMaybe (propColor p) (propMessageColor p)

--------------------------------------------------------------------------------
-- | Draw an entire display into a Vty image.
drawDisplay :: Properties -> Display -> Image
drawDisplay p d = horizCat [ drawIndicator p (indicator0 d)
                           , drawIndicator p (indicator1 d)
                           , drawSep p
                           , drawIndicator p (indicator2 d)
                           , drawIndicator p (indicator3 d)
                           ]

--------------------------------------------------------------------------------
vtyColor :: C.Color -> Color
vtyColor c@(C.ColorCode _) = ISOColor (C.color8   c)
vtyColor c@(C.ColorRGB  _) = Color240 (C.color256 c)

--------------------------------------------------------------------------------
-- | Add a border around the given images.
withBorder :: [Image] -> Image
withBorder []         = emptyImage
withBorder imgs@(x:_) =
  vertCat [ hBorder
          , vertCat (map vBorder imgs)
          , hBorder
          ]
  where
    hBorder = string defAttr $ replicate (imageWidth x) ' '
    vBorder img = horizCat [char defAttr ' ', img, char defAttr ' ']

--------------------------------------------------------------------------------
-- | Center the given image on the current Vty display.
centerImage :: DisplayRegion -> Image -> Image
centerImage display image = translate x y image
  where
    x = (regionWidth  display `div` 2) - (imageWidth  image `div` 2)
    y = (regionHeight display `div` 2) - (imageHeight image `div` 2)
