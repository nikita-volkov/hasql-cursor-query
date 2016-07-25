-- |
-- A DSL for declaring queries.
module Hasql.CursorQuery
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

import qualified Hasql.CursorQuery.Private.CursorQuery as A
import qualified Hasql.CursorTransaction as B

