{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module GitCommands where

import Data.Monoid
import Data.Char
import Text.Read
import Options.Applicative

data Command where
  Add :: Command
  CatFile :: Command
  Checkout :: Command
  Commit :: Command
  HashObject :: Command
  Init :: String -> Command
  Log :: Command
  LsTree :: Command
  Merge :: Command
  Rebase :: Command
  RevParse :: Command
  Rm :: Command
  ShowRef :: Command
  Tag :: Command
  deriving (Show, Eq)

parseInit :: Parser Command
parseInit = Init <$> parsePath
  where
    parsePath :: Parser String
    parsePath = strArgument $ mconcat
      [ help "Where to create the repository."
      , metavar "directory"
      , value "."
      ]

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand = subparser $
  command "init" (parseInit `withInfo` "Initialize a new, empty repository.")

parseInfo :: ParserInfo Command
parseInfo = parseCommand `withInfo` "Write Yourself a Git (wyag) in Haskell"
