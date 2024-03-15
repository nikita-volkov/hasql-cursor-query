module Main.CursorQueries where

import qualified Control.Foldl as C
import Hasql.CursorQuery
import qualified Hasql.Decoders as B
import qualified Hasql.Encoders as A
import Rebase.Prelude

countPGType :: CursorQuery () Int
countPGType =
  cursorQuery sql encoder decoder batchSize_10
  where
    sql =
      "select oid, typname from pg_type"
    encoder =
      A.noParams
    decoder =
      reducingDecoder rowDecoder fold
      where
        rowDecoder =
          (,) <$> (B.column . B.nonNullable) B.int8 <*> (B.column . B.nonNullable) B.text
        fold =
          C.length

slectOIDAndTypeName :: CursorQuery () [(Int64, Text)]
slectOIDAndTypeName =
  cursorQuery sql encoder decoder batchSize_10
  where
    sql =
      "select oid, typname from pg_type"
    encoder =
      A.noParams
    decoder =
      reducingDecoder rowDecoder fold
      where
        rowDecoder =
          (,) <$> (B.column . B.nonNullable) B.int8 <*> (B.column . B.nonNullable) B.text
        fold =
          C.list
