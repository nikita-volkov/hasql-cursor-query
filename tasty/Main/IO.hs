module Main.IO where

import qualified Hasql.Connection as B
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Errors as B
import qualified Hasql.Session as A
import qualified Pqi.Native as Pqi
import Rebase.Prelude

session :: A.Session a -> IO a
session theSession =
  withConnection (\connection -> B.use connection theSession >>= either (fail . show) return)
    >>= either (fail . show) return
  where
    withConnection :: (B.Connection -> IO a) -> IO (Either B.ConnectionError a)
    withConnection handler =
      runExceptT $ acquire >>= \connection -> use connection <* release connection
      where
        acquire =
          ExceptT $ B.acquire Pqi.adapter settings
          where
            settings =
              Settings.hostAndPort "localhost" 5432
                <> Settings.user "postgres"
                <> Settings.password "postgres"
                <> Settings.dbname "postgres"
        use connection =
          lift $ handler connection
        release connection =
          lift $ B.release connection
