module Main.Queries where

import Rebase.Prelude
import Hasql.Query
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B


countPGType :: Query () Int
countPGType =
  statement sql encoder decoder True
  where
    sql =
      "select count(*) from pg_type"
    encoder =
      A.unit
    decoder =
      B.singleRow (B.value (fmap fromIntegral B.int8))
      
slectOIDAndTypeName :: Query () [(Int64, Text)]
slectOIDAndTypeName =
  statement sql encoder decoder True
  where
    sql =
      "select oid, typname from pg_type"
    encoder =
      A.unit
    decoder =
      B.rowsList rowDecoder
      where
        rowDecoder =
          (,) <$> B.value B.int8 <*> B.value B.text
