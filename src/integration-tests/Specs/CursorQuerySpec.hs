module Specs.CursorQuerySpec where

import Hasql.CursorQuery.Sessions qualified as CursorQuery
import Hasql.Session qualified as Session
import Helpers.CursorQueries qualified as CursorQueries
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Counts the same amount of rows as a plain aggregate query" \scopeParams ->
    Scripts.onConnection scopeParams \connection -> do
      count1 <- Scripts.session connection (Session.statement () Statements.countPGType)
      count2 <- Scripts.session connection (CursorQuery.cursorQuery () CursorQueries.countPGType)
      count2 `shouldBe` count1

  it "Selects the same rows as a plain select query" \scopeParams ->
    Scripts.onConnection scopeParams \connection -> do
      result1 <- Scripts.session connection (Session.statement () Statements.selectOidAndTypeName)
      result2 <- Scripts.session connection (CursorQuery.cursorQuery () CursorQueries.selectOidAndTypeName)
      result2 `shouldBe` result1
