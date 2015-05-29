{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module IndicatorTest (tests) where

--------------------------------------------------------------------------------
import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Clockdown.Core.Digital.Indicator

--------------------------------------------------------------------------------
segmentsTest :: Assertion
segmentsTest =
  forM_ testCases $ \(digit, segs) -> do
    let table = zip segs (repeat ())
    mapM_ (check digit table) [A .. G]

  where
    check :: Int -> [(Segment, ())] -> Segment -> Assertion
    check digit table seg =
      assertBool ("Segment: " ++ show seg ++ " from " ++ show digit) $
        case lookup seg table of
          Nothing -> not (indicator digit `testSegment` seg)
          Just _  -> indicator digit `testSegment` seg

    testCases :: [(Int, [Segment])]
    testCases = [ (0,  [A, B, C, D, E, F   ])
                , (1,  [   B, C            ])
                , (2,  [A, B,    D, E,    G])
                , (3,  [A, B, C, D,       G])
                , (4,  [   B, C,       F, G])
                , (5,  [A,    C, D,    F, G])
                , (6,  [A,    C, D, E, F, G])
                , (7,  [A, B, C            ])
                , (8,  [A, B, C, D, E, F, G])
                , (9,  [A, B, C, D,    F, G])
                , (42, [         D         ])
                ]


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "IndicatorTest"
  [ testCase "Correct segments light up" segmentsTest
  ]
