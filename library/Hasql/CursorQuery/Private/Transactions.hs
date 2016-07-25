module Hasql.CursorQuery.Private.Transactions
where

import Hasql.CursorQuery.Private.Prelude
import qualified Hasql.CursorQuery.Private.Specs as B
import qualified Hasql.CursorQuery.Private.CursorTransactions as C
import qualified Hasql.Transaction as A
import qualified Hasql.CursorTransaction.Transactions as D


-- |
-- Executes CursorQuery in Transaction provided the parameters.
cursorQuery :: params -> B.CursorQuery params result -> A.Transaction result
cursorQuery params cursorQuery =
  D.cursorTransaction (C.cursorQuery params cursorQuery)
