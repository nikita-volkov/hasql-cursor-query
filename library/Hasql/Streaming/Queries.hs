module Hasql.Streaming.Queries
where

import Hasql.Streaming.Prelude
import Hasql.Query
import qualified Hasql.Serialization as S
import qualified Hasql.Deserialization as D


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> S.Params a -> ParametricQuery a ()
declareCursor name template serializer =
  (template', serializer, D.result D.noResult, False)
  where
    template' =
      "DECLARE " <> name <> " NO SCROLL CURSOR FOR " <> template

closeCursor :: ParametricQuery ByteString ()
closeCursor =
  ("CLOSE $1", S.value (S.nonNull S.bytea), D.result D.noResult, True)

fetchFromCursor :: D.Row a -> (b -> a -> b) -> b -> ParametricQuery (Int64, ByteString) b
fetchFromCursor rowDeserializer step init =
  (template, serializer, deserializer, True)
  where
    template =
      "FETCH FORWARD $1 FROM $2"
    serializer =
      contramap fst (S.value (S.nonNull S.int8)) <>
      contramap snd (S.value (S.nonNull S.bytea))
    deserializer =
      D.result (D.foldlRows step init rowDeserializer)
