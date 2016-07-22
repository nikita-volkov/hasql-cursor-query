module Hasql.Cursor.Model
where

import Hasql.Cursor.Prelude
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B
import qualified Control.Foldl as D


-- |
-- Spefifies how many rows to fetch in a single DB rountrip.
data BatchSize =
  BatchSize_10 | BatchSize_100 | BatchSize_1000 | BatchSize_10000
  deriving (Enum, Bounded)


-- |
-- A specification of how to decode and reduce multiple rows.
data ReducingDecoder result =
  forall row. ReducingDecoder !(B.Row row) !(D.Fold row result)

instance Functor ReducingDecoder where
  fmap fn (ReducingDecoder rowDecoder rowsFold) =
    ReducingDecoder rowDecoder (fmap fn rowsFold)

instance Applicative ReducingDecoder where
  pure result =
    ReducingDecoder (pure ()) (pure result)
  (<*>) (ReducingDecoder rowDecoder1 rowsFold1) (ReducingDecoder rowDecoder2 rowsFold2) =
    ReducingDecoder rowDecoder3 rowsFold3
    where
      rowDecoder3 =
        (,) <$> rowDecoder1 <*> rowDecoder2
      rowsFold3 =
        lmap fst rowsFold1 <*> lmap snd rowsFold2


-- |
-- A specification of a streaming query.
-- 
-- Provides an abstraction over Postgres Cursor,
-- which allows to process result sets of any size in constant memory.
-- 
-- Essentially it is a parametric query specification extended with a reduction strategy and a batch size,
-- where reduction strategy determines how to fold the rows into the final result,
-- and batch size determines how many rows to fetch during each roundtrip to the database.
data CursorQuery params result =
  CursorQuery !ByteString !(A.Params params) !(ReducingDecoder result) !BatchSize

instance Profunctor CursorQuery where
  dimap fn1 fn2 (CursorQuery template encoder decoder batchSize) =
    CursorQuery template (contramap fn1 encoder) (fmap fn2 decoder) batchSize

instance Functor (CursorQuery params) where
  fmap =
    rmap
