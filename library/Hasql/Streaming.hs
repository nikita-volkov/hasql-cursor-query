module Hasql.Streaming
where

import Hasql.Streaming.Prelude
import qualified Hasql
import qualified Hasql.Encoding as Encoding
import qualified Hasql.Decoding as Decoding
import qualified Hasql.Streaming.Queries as Queries


-- |
-- An abstraction over Postgres Cursor,
-- which allows to process result sets of any size in constant memory.
-- 
-- Essentially it is a parametric query extended with a reduction strategy and a batch size.
-- Where reduction strategy determines how to fold the results and when to terminate,
-- and batch size determines how many rows to fetch during each roundtrip to the database.
-- 
type StreamingQuery a b =
  forall x.
  (
    ByteString
    ,
    (Encoding.Params a)
    ,
    (Decoding.Row x)
    ,
    (Reducer Identity x b)
    ,
    Int64
  )

-- |
-- Given a streaming query specification,
-- parameters for it,
-- and connection,
-- execute the query, aggregating its results, while automatically managing the cursor.
-- 
run :: StreamingQuery a b -> a -> Hasql.Connection -> IO (Either Hasql.ResultsError b)
run ((,,,,) template serializer rowDeserializer (Reducer enter step exit) batch) params connection =
  runEitherT $ do
    EitherT $ Hasql.query connection (Queries.declareCursor name template serializer) params
    acc <-
      ($ runIdentity enter) $ fix $ \loop acc -> do
        let
          foldStep (All continue, acc) row =
            case continue of
              False ->
                (All continue, acc)
              True ->
                runIdentity (step acc row)
          foldInit =
            (All True, acc)
          query =
            Queries.fetchFromCursor rowDeserializer foldStep foldInit
        (All continue, acc) <- EitherT $ Hasql.query connection query (batch, name)
        if continue
          then loop acc
          else pure acc
    pure (runIdentity (exit acc))
  where
    name =
      "Hasql.Streaming"
