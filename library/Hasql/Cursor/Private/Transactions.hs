module Hasql.Cursor.Private.Transactions
where

import Hasql.Cursor.Private.Prelude
import qualified Hasql.Cursor.Private.Specs as B
import qualified Hasql.Cursor.Private.CursorTransactions as C
import qualified Hasql.Transaction as A
import qualified Hasql.CursorTransaction.Transactions as D


-- |
-- Executes CursorQuery in Transaction provided the parameters.
cursorQuery :: params -> B.CursorQuery params result -> A.Transaction result
cursorQuery params cursorQuery =
  D.cursorTransaction (C.cursorQuery params cursorQuery)
