module Hasql.CursorQuery.Private.Decoders where

import qualified Control.Foldl as B
import Hasql.CursorQuery.Private.Prelude
import qualified Hasql.Decoders as A

fold :: B.Fold a b -> A.Row a -> A.Result b
fold (B.Fold progress enter exit) rowDecoder =
  fmap exit (A.foldlRows progress enter rowDecoder)
