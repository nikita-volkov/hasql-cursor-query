module Main.IO where

import qualified Hasql.Connection as B
import qualified Hasql.Session as A
import Rebase.Prelude

session :: A.Session a -> IO a
session session =
  withConnection (A.run session)
    >>= either (fail . show) (either (fail . show) return)
  where
    withConnection :: (B.Connection -> IO a) -> IO (Either B.ConnectionError a)
    withConnection handler =
      runExceptT $ acquire >>= \connection -> use connection <* release connection
      where
        acquire =
          ExceptT $ B.acquire settings
          where
            settings =
              B.settings host port user password database
              where
                host = "localhost"
                port = 5432
                user = "postgres"
                password = ""
                database = "postgres"
        use connection =
          lift $ handler connection
        release connection =
          lift $ B.release connection
