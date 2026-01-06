module Main.IO where

import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Errors as Errors
import qualified Hasql.Session as Session
import Rebase.Prelude

session :: Session.Session a -> IO a
session session =
  withConnection (flip Connection.use session)
    >>= either (fail . show) (either (fail . show) return)
  where
    withConnection :: (Connection.Connection -> IO a) -> IO (Either Errors.ConnectionError a)
    withConnection handler =
      runExceptT $ acquire >>= \connection -> use connection <* release connection
      where
        acquire =
          ExceptT $ Connection.acquire settings
          where
            settings =
              mconcat
                [ Settings.hostAndPort "localhost" 5432,
                  Settings.user "postgres",
                  Settings.password "postgres",
                  Settings.dbname "postgres"
                ]
        use connection =
          lift $ handler connection
        release connection =
          lift $ Connection.release connection
