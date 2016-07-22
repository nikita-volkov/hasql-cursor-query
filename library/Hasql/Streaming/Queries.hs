module Hasql.Streaming.Queries
where

import Hasql.Streaming.Prelude
import qualified Hasql as H
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD
import qualified ByteString.TreeBuilder as TB


declareCursor :: ByteString -> ByteString -> HE.Params a -> H.Query a ()
declareCursor name sql encoder =
  H.Query sql' encoder HD.noResult False
  where
    sql' =
      TB.toByteString $
      "DECLARE " <> TB.byteString name <> " NO SCROLL CURSOR FOR " <> TB.byteString sql

closeCursor :: H.Query ByteString ()
closeCursor =
  H.Query "CLOSE $1" (HE.value HE.bytea) HD.noResult True

fetchFromCursor :: (b -> a -> b) -> b -> HD.Row a -> H.Query (Int64, ByteString) b
fetchFromCursor step init rowDec =
  H.Query sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        (HE.value HE.int8)
        (HE.value HE.bytea)
    decoder =
      HD.foldlRows step init rowDec
