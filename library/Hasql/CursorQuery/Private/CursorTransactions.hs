module Hasql.CursorQuery.Private.CursorTransactions where

import qualified Control.Foldl as D
import qualified Hasql.CursorQuery.Private.CursorQuery as B
import qualified Hasql.CursorQuery.Private.Decoders as I
import Hasql.CursorQuery.Private.Prelude
import qualified Hasql.CursorTransaction as G
import qualified Hasql.Decoders as E

-- |
-- Fetch and fold the data from cursor until it dries out.
fetchAndFoldCursor :: G.Cursor s -> G.BatchSize -> E.Row row -> D.Fold row result -> G.CursorTransaction s result
fetchAndFoldCursor cursor batchSize rowDecoder (D.Fold progress enter exit) =
  fmap exit
    $ fetchAndFoldMore enter
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

fetchAndFoldCursorBatch :: G.Cursor s -> G.BatchSize -> E.Row row -> D.Fold row result -> G.CursorTransaction s result
fetchAndFoldCursorBatch cursor batchSize rowDecoder rowsFold =
  G.fetchBatch cursor batchSize (I.fold rowsFold rowDecoder)

-- |
-- Executes CursorQuery in CursorTransaction provided the parameters.
cursorQuery :: params -> B.CursorQuery params result -> G.CursorTransaction s result
cursorQuery params (B.CursorQuery template encoder (B.ReducingDecoder rowDecoder rowsFold) batchSize) =
  do
    cursor <- G.declareCursor template (G.encodedParams encoder params)
    fetchAndFoldCursor cursor batchSize rowDecoder rowsFold
