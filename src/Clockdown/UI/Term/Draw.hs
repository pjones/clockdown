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
-- | Functions for creating Vty images to display a clock/timer.
module Clockdown.UI.Term.Draw
       ( run
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Time
import Graphics.Vty
import Graphics.Vty.Prelude

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Digital.Display
import Clockdown.Core.Digital.Indicator
import Clockdown.Core.Properties
import Clockdown.Core.Window

--------------------------------------------------------------------------------
-- | Draw a single indicator into a Vty image.
drawIndicator :: Indicator -> Image
drawIndicator ssd =
  withBorder [ paintA F A B
             , paintB F B
             , paintA F G B
             , paintB E C
             , paintA E D C
             ]
  where
    on  = defAttr `withBackColor` blue
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
drawSep :: Image
drawSep = withBorder [ string defAttr "  "
                     , string (defAttr `withBackColor` blue) "  "
                     , string defAttr "  "
                     , string (defAttr `withBackColor` blue) "  "
                     , string defAttr "  "
                     ]

--------------------------------------------------------------------------------
-- | Draw an entire display into a Vty image.
drawDisplay :: Display -> Image
drawDisplay d = horizCat [ drawIndicator (indicator0 d)
                         , drawIndicator (indicator1 d)
                         , drawSep
                         , drawIndicator (indicator2 d)
                         , drawIndicator (indicator3 d)
                         ]

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

--------------------------------------------------------------------------------
-- | Temporary function for testing the drawing functions.
run :: IO ()
run = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  tz  <- getCurrentTimeZone
  go vty $ makeClock (Properties "foo") tz
  shutdown vty

  where
    go vty clock = do
      now <- getCurrentTime
      region <- displayBounds (outputIface vty)

      let display = windowDigitalDisplay clock now
      update vty $ picForImage (centerImage region $ drawDisplay display)

      e <- nextEvent vty
      case e of
        EvKey KEsc []        -> return ()
        EvKey (KChar '+') [] -> go vty (windowSucc clock)
        EvKey (KChar '=') [] -> go vty (windowSucc clock)
        EvKey (KChar '-') [] -> go vty (windowPred clock)
        _                    -> go vty clock
