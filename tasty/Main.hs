module Main where

import qualified Hasql.CursorQuery.Sessions as C
import qualified Hasql.Session as A
import qualified Main.CursorQueries as D
import qualified Main.IO as F
import qualified Main.Statements as E
import Rebase.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

main :: IO ()
main =
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "All tests"
        [ testCase "Amount" $
            do
              (count1, count2) <- F.session ((,) <$> A.statement () E.countPGType <*> C.cursorQuery () D.countPGType)
              assertEqual (show (count1, count2)) count1 count2,
          testCase "Equality" $
            do
              (result1, result2) <- F.session ((,) <$> A.statement () E.slectOIDAndTypeName <*> C.cursorQuery () D.slectOIDAndTypeName)
              assertEqual (show (result1, result2)) result1 result2
        ]
