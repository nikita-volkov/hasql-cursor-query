-- |
-- A DSL for declaring the specs.
module Hasql.Cursor.Specs
(
  A.CursorQuery,
  A.cursorQuery,
  A.ReducingDecoder,
  A.reducingDecoder,
  B.BatchSize,
  B.batchSize_10,
  B.batchSize_100,
  B.batchSize_1000,
  B.batchSize_10000,
)
where

import qualified Hasql.Cursor.Private.Specs as A
import qualified Hasql.CursorTransaction.Specs as B

