module Hasql.Cursor.Transactions
(
  cursorQuery,
)
where

import Hasql.Cursor.Prelude
import qualified Hasql.Cursor.Queries as A
import qualified Hasql.Cursor.Model as B
import qualified Hasql.Transaction as C
import qualified Hasql.Decoders as E
import qualified Hasql.Encoders as F
import qualified Control.Foldl as D


-- |
-- Fetch and fold the data from cursor until it dries out.
fetchAndFoldCursor :: ByteString -> B.BatchSize -> E.Row row -> D.Fold row result -> C.Transaction result
fetchAndFoldCursor cursorName batchSize rowDecoder (D.Fold progress enter exit) =
  fmap exit $
  fetchAndFoldMore enter
  where
    fetchAndFoldMore batch =
      do
        (fetched, fetchedBatch) <- fetchBatch
        if fetched
          then return fetchedBatch
          else fetchAndFoldMore fetchedBatch
      where
        fetchBatch =
          fetchAndFoldCursorBatch cursorName batchSize rowDecoder fold
          where
            fold =
              (,) <$> D.null <*> D.Fold progress batch id

fetchAndFoldCursorBatch :: ByteString -> B.BatchSize -> E.Row row -> D.Fold row result -> C.Transaction result
fetchAndFoldCursorBatch cursorName batchSize rowDecoder rowsFold =
  C.query (batchSize, cursorName) (A.fetchFromCursor_fold rowsFold rowDecoder)

cursorQuery :: input -> B.CursorQuery input output -> C.Transaction output
cursorQuery input B.CursorQuery{..} =
  declareCursor *> fetchFromCursor <* closeCursor
  where
    cursorName =
      "Hasql.Cursor"
    declareCursor =
      C.query input query
      where
        query =
          A.declareCursor cursorName template paramsEncoder
    closeCursor =
      C.query cursorName A.closeCursor
    fetchFromCursor =
      fetchAndFoldCursor cursorName batchSize rowDecoder rowsFold
