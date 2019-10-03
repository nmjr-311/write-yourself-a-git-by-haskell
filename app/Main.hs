module Main where

import GitCommands
import Init
import Options.Applicative (execParser)
import Control.Monad.Except

-- for debug
main :: IO ()
main = execParser parseInfo >>= \case
  Init dir -> runExceptT (repoCreate dir) >>= \case
    Left err -> putStrLn err
    Right _ -> return ()
