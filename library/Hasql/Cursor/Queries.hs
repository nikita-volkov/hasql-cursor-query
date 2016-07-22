module Hasql.Cursor.Queries
where

import Hasql.Cursor.Prelude
import qualified Hasql.Query as A
import qualified Hasql.Encoders as B
import qualified Hasql.Decoders as C
import qualified Hasql.Cursor.Specs as F
import qualified ByteString.TreeBuilder as D
import qualified Control.Foldl as E


declareCursor :: ByteString -> ByteString -> B.Params a -> A.Query a ()
declareCursor name sql encoder =
  A.statement sql' encoder C.unit False
  where
    sql' =
      D.toByteString $
      "DECLARE " <> D.byteString name <> " NO SCROLL CURSOR FOR " <> D.byteString sql

closeCursor :: A.Query ByteString ()
closeCursor =
  A.statement "CLOSE $1" (B.value B.bytea) C.unit True

fetchFromCursor :: (b -> a -> b) -> b -> C.Row a -> A.Query (F.BatchSize, ByteString) b
fetchFromCursor step init rowDec =
  A.statement sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        (B.value batchSize)
        (B.value B.bytea)
      where
        batchSize =
          contramap batchSizeToInt64 B.int8
          where
            batchSizeToInt64 =
              \case
                F.BatchSize_10 -> 10
                F.BatchSize_100 -> 100
                F.BatchSize_1000 -> 1000
                F.BatchSize_10000 -> 10000
    decoder =
      C.foldlRows step init rowDec

fetchFromCursor_fold :: E.Fold row result -> C.Row row -> A.Query (F.BatchSize, ByteString) result
fetchFromCursor_fold (E.Fold progress enter exit) =
  fmap exit . fetchFromCursor progress enter
      
