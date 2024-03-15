module Hasql.CursorQuery.Private.CursorQuery where

import qualified Control.Foldl as D
import Hasql.CursorQuery.Private.Prelude
import qualified Hasql.CursorTransaction as H
import qualified Hasql.Decoders as B
import qualified Hasql.Encoders as A

-- |
-- A specification of a streaming query.
--
-- Provides an abstraction over Postgres Cursor,
-- which allows to process result sets of any size in constant memory.
--
-- Essentially it is a parametric query specification extended with a reduction strategy and a batch size,
-- where reduction strategy determines how to fold the rows into the final result,
-- and batch size determines how many rows to fetch during each roundtrip to the database.
data CursorQuery params result
  = CursorQuery !ByteString !(A.Params params) !(ReducingDecoder result) !H.BatchSize

instance Profunctor CursorQuery where
  dimap fn1 fn2 (CursorQuery template encoder decoder batchSize) =
    CursorQuery template (contramap fn1 encoder) (fmap fn2 decoder) batchSize

instance Functor (CursorQuery params) where
  fmap =
    rmap

-- |
-- Given an SQL template, a params encoder, a reducing result decoder and a batch-size,
-- constructs CursorQuery.
cursorQuery :: ByteString -> A.Params params -> ReducingDecoder result -> H.BatchSize -> CursorQuery params result
cursorQuery =
  CursorQuery

-- |
-- A specification of how to decode and reduce multiple rows.
--
-- Composable with the Applicative interface.
data ReducingDecoder reduction
  = forall row. ReducingDecoder !(B.Row row) !(D.Fold row reduction)

instance Functor ReducingDecoder where
  fmap fn (ReducingDecoder rowDecoder rowsFold) =
    ReducingDecoder rowDecoder (fmap fn rowsFold)

instance Applicative ReducingDecoder where
  pure reduction =
    ReducingDecoder (pure ()) (pure reduction)
  (<*>) (ReducingDecoder rowDecoder1 rowsFold1) (ReducingDecoder rowDecoder2 rowsFold2) =
    ReducingDecoder rowDecoder3 rowsFold3
    where
      rowDecoder3 =
        strictPair <$> rowDecoder1 <*> rowDecoder2
        where
          strictPair !a !b =
            (a, b)
      rowsFold3 =
        lmap fst rowsFold1 <*> lmap snd rowsFold2

-- |
-- Packs a row decoder and a fold over rows into ReducingDecoder.
reducingDecoder :: B.Row row -> D.Fold row reduction -> ReducingDecoder reduction
reducingDecoder =
  ReducingDecoder
