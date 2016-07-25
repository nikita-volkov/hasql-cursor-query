module Main.CursorQueries where

import Rebase.Prelude
import Hasql.CursorQuery
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B
import qualified Control.Foldl as C


countPGType :: CursorQuery () Int
countPGType =
  cursorQuery sql encoder decoder batchSize_10
  where
    sql =
      "select oid, typname from pg_type"
    encoder =
      A.unit
    decoder =
      reducingDecoder rowDecoder fold
      where
        rowDecoder =
          (,) <$> B.value B.int8 <*> B.value B.text
        fold =
          C.length

slectOIDAndTypeName :: CursorQuery () [(Int64, Text)]
slectOIDAndTypeName =
  cursorQuery sql encoder decoder batchSize_10
  where
    sql =
      "select oid, typname from pg_type"
    encoder =
      A.unit
    decoder =
      reducingDecoder rowDecoder fold
      where
        rowDecoder =
          (,) <$> B.value B.int8 <*> B.value B.text
        fold =
          C.list

