{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import qualified IndicatorTest as DI
import Test.Tasty

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [ DI.tests ]

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
