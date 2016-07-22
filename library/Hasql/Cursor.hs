module Hasql.Cursor
(
  StreamingQuery(..),
  F.BatchSize(..),
  run,
)
where

import Hasql.Cursor.Prelude
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B
import qualified Hasql.Transaction as E
import qualified Hasql.Cursor.Queries as C
import qualified Hasql.Cursor.Model as F
import qualified Hasql.Cursor.Transactions as G
import qualified Control.Foldl as D


-- |
-- A specification of a streaming query.
-- 
-- Provides an abstraction over Postgres Cursor,
-- which allows to process result sets of any size in constant memory.
-- 
-- Essentially it is a parametric query specification extended with a reduction strategy and a batch size,
-- where reduction strategy determines how to fold the results into the output,
-- and batch size determines how many rows to fetch during each roundtrip to the database.
data StreamingQuery input output =
  forall row. 
  StreamingQuery {
    template :: !ByteString,
    paramsEncoder :: !(A.Params input),
    rowDecoder :: !(B.Row row),
    rowsFold :: !(D.Fold row output),
    batchSize :: !F.BatchSize
  }

run :: StreamingQuery input output -> input -> E.Transaction output
run StreamingQuery{..} input =
  declareCursor *> fetchFromCursor <* closeCursor
  where
    cursorName =
      "Hasql.Cursor"
    declareCursor =
      E.query input query
      where
        query =
          C.declareCursor cursorName template paramsEncoder
    closeCursor =
      E.query cursorName C.closeCursor
    fetchFromCursor =
      G.fetchAndFoldCursor cursorName batchSize rowDecoder rowsFold
