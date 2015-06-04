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
-- | Functions for working with colors.  Reuses the @Color@ type from
-- the @Byline@ package.
module Clockdown.Core.Color
       ( Color (..)
       , parseColor
       , color8
       , color256
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import qualified Data.Colour.SRGB as C
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import System.Console.Byline.Color
import System.Console.Byline.Internal.Color

--------------------------------------------------------------------------------
-- | Parse a @Text@ value containing either one of the 8 ANSI color
-- names or a standard RGB color in hex notation.
parseColor :: Text -> Either String Color
parseColor input =
  case Map.lookup input ansiColors of
    Just c  -> Right c
    Nothing -> if length readColor == 1 && null (snd (head readColor))
                  then Right $ toColor (fst . head $ readColor)
                  else Left ("invalid color: " ++ Text.unpack input)
  where
    readColor :: [(C.Colour Double, String)]
    readColor = C.sRGB24reads (Text.unpack input)

    toColor :: C.Colour Double -> Color
    toColor = ColorRGB . toRGB

    toRGB :: C.Colour Double -> (Word8, Word8, Word8)
    toRGB c = let x = C.toSRGB24 c in ( C.channelRed   x
                                      , C.channelGreen x
                                      , C.channelBlue  x
                                      )

--------------------------------------------------------------------------------
-- | Returns a ANSI color code (0-7) for the given color.
color8 :: Color -> Word8
color8 = fromInteger . toInteger . fromEnum . colorAsANSI

--------------------------------------------------------------------------------
-- | Returns an index into the 256 color table for terminals.
color256 :: Color -> Word8
color256 c@(ColorCode _) = color8 c
color256 (ColorRGB c)    = nearestColor c term256Locations

--------------------------------------------------------------------------------
-- | Map color names to the 8 standard ANSI colors.
ansiColors :: Map Text Color
ansiColors = Map.fromList [ ("black",   black)
                          , ("red",     red)
                          , ("green",   green)
                          , ("yellow",  yellow)
                          , ("blue",    blue)
                          , ("magenta", magenta)
                          , ("cyan",    cyan)
                          , ("white",   white)
                          ]
