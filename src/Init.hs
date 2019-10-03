{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Init (GitRepository(..), repoCreate) where

import System.Directory
import Control.Monad.Except
import Data.Ini
import Data.HashMap.Strict (fromList)
import Prelude hiding (readFile)

data GitRepository = GitRepository
  { worktree :: FilePath
  , gitdir :: FilePath
  , conf :: Maybe Ini
  }

newtype GitDirPath = GDPath FilePath
type Error = String
type Result a = ExceptT Error IO a

gitRepository :: FilePath -> Bool -> Result GitRepository
gitRepository path force = do
  let gitdir' = path ++ "/.git"
  isDir <- liftIO $ doesDirectoryExist gitdir'
  if not $ force || isDir
    then throwError $ "Not a Git repository: " ++ gitdir'
    else checkConfig gitdir' >>
    if force
      then return $ GitRepository path gitdir' Nothing
      else do
      let cf = repoPath (GDPath gitdir') ["config"]
      config <- ExceptT $ readIniFile cf
      vers <- liftEither $ lookupValue "core" "repositoryformatversion" config
      if vers /= "0"
        then throwError $ "Unsupported repositoryformatversion " ++ show vers
        else return $ GitRepository path gitdir' $ Just config
  where
    checkConfig :: FilePath -> Result FilePath
    checkConfig dir = catchError (repoFile (GDPath dir) ["config"] False) $ \_ ->
      liftEither $ if not force
                  then Left $ "Configure file missing: " ++ dir
                  else Right ""

repoPath :: GitDirPath -> [FilePath] -> FilePath
repoPath (GDPath dir) paths = join' "/" (dir : paths)
  where
    join' delim = init . foldr (\a b -> a ++ delim ++ b) ""

repoFile :: GitDirPath -> [FilePath] -> Bool -> Result FilePath
repoFile dir paths mkdir =
  repoDir dir (init paths) mkdir >> return (repoPath dir paths)

repoDir :: GitDirPath -> [FilePath] -> Bool -> Result FilePath
repoDir dir paths mkdir = do
  let path = repoPath dir paths
  dir' <- liftIO $ doesDirectoryExist path
  case (dir', mkdir) of
    (True, _) -> return path
    (False, True) -> liftIO (createDirectoryIfMissing True path) >> return path
    (False, False) -> throwError $ "path missing: " ++ path

repoCreate :: FilePath -> Result ()
repoCreate path = gitRepository path True >>= caseRepo
   where
     caseRepo repo =
       let dir = GDPath $ gitdir repo
       in
         createDirectoryIfMissingOrEmpty dir >>
         repoDir dir ["branches"] True >>
         repoDir dir ["objects"] True >>
         repoDir dir ["refs", "tags"] True >>
         repoDir dir ["refs", "heads"] True >>
         writeDescription dir >>
         writeHead dir >>
         writeConfig dir >>
         liftIO (putStrLn "success")
     createDirectoryIfMissingOrEmpty :: GitDirPath -> Result ()
     createDirectoryIfMissingOrEmpty (GDPath dir) = do
       exist <- liftIO $ doesDirectoryExist dir
       if not exist
         then liftIO $ createDirectory dir
         else do
         ls <- liftIO $ getDirectoryContents dir
         if null ls
           then return ()
           else throwError $ "directory must be empty: " ++ dir
     writeDescription dir = repoFile dir ["description"] False >>=
       liftIO . flip writeFile "Unnamed repository; edit this file 'description' to name the repository.\n"
     writeHead dir = repoFile dir ["HEAD"] False >>=
       liftIO . flip writeFile "ref: refs/heads/master\n"
     writeConfig dir = repoFile dir ["config"] False >>=
       liftIO . flip writeIniFile repoDefaultConfig

repoDefaultConfig :: Ini
repoDefaultConfig = Ini
  { iniSections = fromList
    [( "core"
     , [
         ("repositoryformatversion", "0"),
         ("filemode", "false"),
         ("bare", "false")
       ]
     )]
  , iniGlobals = []
  }

repoFind :: FilePath -> Bool -> Result GitRepository
repoFind path required = loop path
  where
    loop p = do
      currentPath <- liftIO $ canonicalizePath p
      isDir <- liftIO $ doesDirectoryExist currentPath
      if isDir
        then gitRepository (currentPath ++ "/.git") False
        else do
        parentPath <- liftIO . canonicalizePath $ currentPath ++ "/.."
        case (parentPath == currentPath, required) of
          (True, True) -> throwError ""
          (True, False) -> throwError ""
          _ -> loop parentPath
