module Hasql.Streaming.Model
where

import Hasql.Streaming.Prelude


-- |
-- Spefifies how many rows to fetch in a single DB rountrip.
data BatchSize =
  BatchSize_10 | BatchSize_100 | BatchSize_1000 | BatchSize_10000

