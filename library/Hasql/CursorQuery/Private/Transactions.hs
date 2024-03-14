module Hasql.CursorQuery.Private.Transactions where

import qualified Hasql.CursorQuery.Private.CursorQuery as B
import qualified Hasql.CursorQuery.Private.CursorTransactions as C
import qualified Hasql.CursorTransaction.Transactions as D
import qualified Hasql.Transaction as A

-- |
-- Executes CursorQuery in Transaction provided the parameters.
cursorQuery :: params -> B.CursorQuery params result -> A.Transaction result
cursorQuery params cursorQuery =
  D.cursorTransaction (C.cursorQuery params cursorQuery)
