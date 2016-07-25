module Hasql.Cursor.Private.Decoders
where

import Hasql.Cursor.Private.Prelude
import qualified Hasql.Decoders as A
import qualified Control.Foldl as B


fold :: B.Fold a b -> A.Row a -> A.Result b
fold (B.Fold progress enter exit) rowDecoder =
  fmap exit (A.foldlRows progress enter rowDecoder)
