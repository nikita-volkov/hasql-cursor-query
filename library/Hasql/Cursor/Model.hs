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
-- A specification of a streaming query.
-- 
-- Provides an abstraction over Postgres Cursor,
-- which allows to process result sets of any size in constant memory.
-- 
-- Essentially it is a parametric query specification extended with a reduction strategy and a batch size,
-- where reduction strategy determines how to fold the results into the output,
-- and batch size determines how many rows to fetch during each roundtrip to the database.
data CursorQuery input output =
  forall row. 
  CursorQuery {
    template :: !ByteString,
    paramsEncoder :: !(A.Params input),
    rowDecoder :: !(B.Row row),
    rowsFold :: !(D.Fold row output),
    batchSize :: !BatchSize
  }

instance Profunctor CursorQuery where
  dimap fn1 fn2 (CursorQuery{..}) =
    CursorQuery template (contramap fn1 paramsEncoder) rowDecoder (fmap fn2 rowsFold) batchSize

instance Functor (CursorQuery input) where
  fmap =
    rmap
