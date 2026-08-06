module Helpers.CursorQueries where

import Control.Foldl qualified as Foldl
import Hasql.CursorQuery
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Prelude

countPGType :: CursorQuery () Int
countPGType =
  cursorQuery sql encoder decoder batchSize_10
  where
    sql =
      "select oid::int8, typname::text from pg_type"
    encoder =
      E.noParams
    decoder =
      reducingDecoder rowDecoder Foldl.length
      where
        rowDecoder =
          (,) <$> (D.column . D.nonNullable) D.int8 <*> (D.column . D.nonNullable) D.text

selectOidAndTypeName :: CursorQuery () [(Int64, Text)]
selectOidAndTypeName =
  cursorQuery sql encoder decoder batchSize_10
  where
    sql =
      "select oid::int8, typname::text from pg_type"
    encoder =
      E.noParams
    decoder =
      reducingDecoder rowDecoder Foldl.list
      where
        rowDecoder =
          (,) <$> (D.column . D.nonNullable) D.int8 <*> (D.column . D.nonNullable) D.text
