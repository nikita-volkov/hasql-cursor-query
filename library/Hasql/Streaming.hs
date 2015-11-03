module Hasql.Streaming
where

import Hasql.Streaming.Prelude
import qualified Hasql.Connection as Connection
import qualified Hasql.Serialization as Serialization
import qualified Hasql.Deserialization as Deserialization
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
    (Serialization.Params a)
    ,
    (Deserialization.Row x)
    ,
    (Reducer Identity x b)
    ,
    Int64
  )

-- |
-- Given a streaming query specification,
-- parameters for it,
-- name for the cursor, which must be unique across the currently running cursors,
-- and connection,
-- execute the query, aggregating its results, while automatically managing the cursor.
-- 
run :: StreamingQuery a b -> a -> ByteString -> Connection.Connection -> IO (Either Connection.ResultsError b)
run ((,,,,) template serializer rowDeserializer (Reducer enter step exit) batch) params name connection =
  runEitherT $ do
    EitherT $ Connection.executeParametricQuery connection (Queries.declareCursor name template serializer) params
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
        (All continue, acc) <- EitherT $ Connection.executeParametricQuery connection query (batch, name)
        if continue
          then loop acc
          else pure acc
    pure (runIdentity (exit acc))
