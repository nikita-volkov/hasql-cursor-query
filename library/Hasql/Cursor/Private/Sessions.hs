module Hasql.Cursor.Private.Sessions
where

import Hasql.Cursor.Private.Prelude
import qualified Hasql.Cursor.Private.Specs as B
import qualified Hasql.Cursor.Private.Transactions as C
import qualified Hasql.Transaction as A
import qualified Hasql.Session as D


-- |
-- Executes CursorQuery in Session provided the parameters.
-- 
-- During the execution it establishes a Read transaction with the ReadCommitted isolation level.
cursorQuery :: params -> B.CursorQuery params result -> D.Session result
cursorQuery params cursorQuery =
  A.run (C.cursorQuery params cursorQuery) A.ReadCommitted A.Read
