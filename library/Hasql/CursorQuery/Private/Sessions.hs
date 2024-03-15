module Hasql.CursorQuery.Private.Sessions where

import qualified Hasql.CursorQuery.Private.CursorQuery as B
import qualified Hasql.CursorQuery.Private.CursorTransactions as C
import qualified Hasql.CursorTransaction.Sessions as A
import qualified Hasql.Session as D

-- |
-- Executes CursorQuery in Session provided the parameters.
--
-- During the execution it establishes a Read transaction with the ReadCommitted isolation level.
cursorQuery :: params -> B.CursorQuery params result -> D.Session result
cursorQuery params cursorQuery =
  A.cursorTransaction (C.cursorQuery params cursorQuery)
