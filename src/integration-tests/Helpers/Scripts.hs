module Helpers.Scripts
  ( ScopeParams,
    onConnection,
    session,
  )
where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Session qualified as Session
import Pqi qualified
import Prelude

-- |
-- Adapter, host and port of a running isolated postgres server.
type ScopeParams = (Pqi.Adapter, Text, Word16)

-- |
-- Acquire a connection against the running isolated postgres server,
-- releasing it once the action completes.
onConnection :: ScopeParams -> (Connection.Connection -> IO ()) -> IO ()
onConnection (adapter, host, port) action =
  bracket acquire Connection.release action
  where
    acquire =
      Connection.acquire adapter connectionSettings
        >>= either (fail . show) return
    connectionSettings =
      Settings.hostAndPort host port
        <> Settings.user "postgres"
        <> Settings.password "postgres"
        <> Settings.dbname "postgres"

session :: Connection.Connection -> Session.Session a -> IO a
session connection theSession =
  Connection.use connection theSession
    >>= either (fail . show) return
