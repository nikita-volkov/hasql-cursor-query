module Hasql.Cursor.Private.CursorTransactions
where

import Hasql.Cursor.Private.Prelude
import qualified Hasql.Cursor.Private.Specs as B
import qualified Hasql.Cursor.Private.Decoders as I
import qualified Hasql.Transaction as C
import qualified Hasql.Decoders as E
import qualified Hasql.Encoders as F
import qualified Hasql.CursorTransaction as G
import qualified Hasql.CursorTransaction.Specs as H
import qualified Control.Foldl as D


-- |
-- Fetch and fold the data from cursor until it dries out.
fetchAndFoldCursor :: G.Cursor s -> H.BatchSize -> E.Row row -> D.Fold row result -> G.CursorTransaction s result
fetchAndFoldCursor cursor batchSize rowDecoder (D.Fold progress enter exit) =
  fmap exit $
  fetchAndFoldMore enter
  where
    fetchAndFoldMore batch =
      do
        (null, fetchedBatch) <- fetchBatch
        if null
          then return batch
          else fetchAndFoldMore fetchedBatch
      where
        fetchBatch =
          fetchAndFoldCursorBatch cursor batchSize rowDecoder fold
          where
            fold =
              (,) <$> D.null <*> D.Fold progress batch id

fetchAndFoldCursorBatch :: G.Cursor s -> H.BatchSize -> E.Row row -> D.Fold row result -> G.CursorTransaction s result
fetchAndFoldCursorBatch cursor batchSize rowDecoder rowsFold =
  G.fetchBatch cursor batchSize (I.fold rowsFold rowDecoder)

-- |
-- Executes CursorQuery in Transaction provided the parameters.
cursorQuery :: params -> B.CursorQuery params result -> G.CursorTransaction s result
cursorQuery params (B.CursorQuery template encoder (B.ReducingDecoder rowDecoder rowsFold) batchSize) =
  G.withCursor template (H.encodedParams encoder params) $
  \ cursor ->
    fetchAndFoldCursor cursor batchSize rowDecoder rowsFold
