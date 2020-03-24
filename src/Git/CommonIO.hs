module Git.CommonIO
  ( sh,
    sh_,
    trySh,
    trySh_,
  )
where

import qualified Control.Exception as Ex
import Data.Functor (($>))
import qualified Data.Text as T
import qualified System.Process as P

-- | Runs a shell command given by `T.Text` on `FilePath`.
sh :: T.Text -> Maybe FilePath -> IO T.Text
sh cmd fp = T.pack <$> P.readCreateProcess proc ""
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

-- | Runs a shell command given by `T.Text` on `FilePath`.
sh_ :: T.Text -> Maybe FilePath -> IO ()
sh_ cmd fp = P.readCreateProcess proc "" $> ()
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

trySh :: T.Text -> Maybe FilePath -> IO (Either Ex.SomeException T.Text)
trySh cmd path = Ex.try (sh cmd path)

trySh_ :: String -> T.Text -> Maybe FilePath -> IO ()
trySh_ err cmd path = do
  res <- Ex.try (sh_ cmd path) :: IO (Either Ex.SomeException ())
  case res of
    Left ex -> do
      putStrLn $ err <> ": " <> show ex
      pure ()
    Right r -> pure r
