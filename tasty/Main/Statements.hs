module Main.Statements where

import qualified Hasql.Decoders as B
import qualified Hasql.Encoders as A
import Hasql.Statement
import Rebase.Prelude

countPGType :: Statement () Int
countPGType =
  Statement sql encoder decoder True
  where
    sql =
      "select count(*) from pg_type"
    encoder =
      A.noParams
    decoder =
      (B.singleRow . B.column . B.nonNullable . fmap fromIntegral) B.int8

slectOIDAndTypeName :: Statement () [(Int64, Text)]
slectOIDAndTypeName =
  Statement sql encoder decoder True
  where
    sql =
      "select oid, typname from pg_type"
    encoder =
      A.noParams
    decoder =
      B.rowList rowDecoder
      where
        rowDecoder =
          (,) <$> (B.column . B.nonNullable) B.int8 <*> (B.column . B.nonNullable) B.text
