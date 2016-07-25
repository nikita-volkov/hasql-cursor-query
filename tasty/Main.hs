module Main where

import Rebase.Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import qualified Hasql.Session as A
import qualified Hasql.CursorQuery.Sessions as C
import qualified Main.CursorQueries as D
import qualified Main.Queries as E
import qualified Main.IO as F


main :: IO ()
main =
  defaultMain $
  localOption (NumThreads 1) $
  testGroup "All tests"
  [
    testCase "Amount" $
    do
      (count1, count2) <- F.session ((,) <$> A.query () E.countPGType <*> C.cursorQuery () D.countPGType)
      assertEqual (show (count1, count2)) count1 count2
    ,
    testCase "Equality" $
    do
      (result1, result2) <- F.session ((,) <$> A.query () E.slectOIDAndTypeName <*> C.cursorQuery () D.slectOIDAndTypeName)
      assertEqual (show (result1, result2)) result1 result2
  ]
