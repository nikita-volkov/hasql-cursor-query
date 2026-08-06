module Helpers.Statements where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement
import Prelude

countPGType :: Statement () Int
countPGType =
  preparable sql encoder decoder
  where
    sql =
      "select count(*) from pg_type"
    encoder =
      E.noParams
    decoder =
      (D.singleRow . D.column . D.nonNullable . fmap fromIntegral) D.int8

selectOidAndTypeName :: Statement () [(Int64, Text)]
selectOidAndTypeName =
  preparable sql encoder decoder
  where
    sql =
      "select oid::int8, typname::text from pg_type"
    encoder =
      E.noParams
    decoder =
      D.rowList rowDecoder
      where
        rowDecoder =
          (,) <$> (D.column . D.nonNullable) D.int8 <*> (D.column . D.nonNullable) D.text
