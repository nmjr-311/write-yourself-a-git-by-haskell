module Main where

import           GitCommands
import           Init
import           Options.Applicative  (execParser)
import           RIO

-- for debug
main :: IO ()
main = execParser parseInfo >>= \case
  Init dir -> repoCreate dir
  _ -> pure ()
  where
    _ = repoFind
