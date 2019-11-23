{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Init (GitRepository(..), repoCreate, repoFind) where

import           Data.Ini
import           RIO
import qualified RIO.Directory    as D
import           RIO.FilePath
import qualified RIO.HashMap      as M
import           RIO.List.Partial (init)
import qualified RIO.Text         as T
import           System.IO        (hPutStrLn, stdout)

data GitRepository = GitRepository
  { worktree :: FilePath
  , gitdir   :: FilePath
  , conf     :: Maybe Ini
  } deriving (Show, Eq)

newtype GitDirPath = GDPath FilePath deriving (Show, Eq)
type Result a = IO a

newtype GitInitializeException = GitInitializeException String
  deriving (Show, Eq)
instance Exception GitInitializeException

throwGIE :: MonadThrow m => String -> m a
throwGIE = throwM . GitInitializeException

gitRepository :: FilePath -> Bool -> Result GitRepository
gitRepository path force' = do
  let gitdir' = path ++ "/.git"
  isDir <- D.doesDirectoryExist gitdir'
  if not $ force' || isDir
    then throwGIE $ "Not a Git repository: " <> gitdir'
    else checkConfig gitdir' >>
    if force'
      then return $ GitRepository path gitdir' Nothing
      else do
      let cf = repoPath (GDPath gitdir') ["config"]
      config <- readIniFile cf >>= either throwGIE pure
      vers <- either throwGIE pure
              $ lookupValue "core" "repositoryformatversion" config
      if vers /= "0"
        then throwGIE $ "Unsupported repositoryformatversion " <> show (vers :: T.Text)
        else return $ GitRepository path gitdir' $ Just config
  where
    checkConfig :: FilePath -> Result FilePath
    checkConfig dir = catch (repoFile (GDPath dir) ["config"] False) $ \(_e :: SomeException) ->
      if not force'
      then throwGIE $ "Configure file missing: " </> dir
      else pure ""

repoPath :: GitDirPath -> [FilePath] -> FilePath
repoPath (GDPath dir) paths = dir </> foldr (</>) "" paths

repoFile :: GitDirPath -> [FilePath] -> Bool -> Result FilePath
repoFile dir paths mkdir =
  repoDir dir (init paths) mkdir >> return (repoPath dir paths)

repoDir :: GitDirPath -> [FilePath] -> Bool -> Result FilePath
repoDir dir paths mkdir = do
  let path = repoPath dir paths
  dir' <- D.doesDirectoryExist path
  case (dir', mkdir) of
    (True, _)      -> return path
    (False, True)  -> D.createDirectoryIfMissing True path >> return path
    (False, False) -> throwGIE $ "path missing: " <> path

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
         hPutStrLn stdout "success"
     createDirectoryIfMissingOrEmpty :: GitDirPath -> Result ()
     createDirectoryIfMissingOrEmpty (GDPath dir) = do
       exist <- liftIO $ D.doesDirectoryExist dir
       if not exist
         then liftIO $ D.createDirectory dir
         else do
         ls <- liftIO $ D.getDirectoryContents dir
         if null ls
           then return ()
           else throwGIE $ "directory must be empty: " <> dir
     writeDescription dir = repoFile dir ["description"] False >>=
       liftIO . flip writeFileUtf8 "Unnamed repository; edit this file 'description' to name the repository.\n"
     writeHead dir = repoFile dir ["HEAD"] False >>=
       liftIO . flip writeFileUtf8 "ref: refs/heads/master\n"
     writeConfig dir = repoFile dir ["config"] False >>=
       liftIO . flip writeIniFile repoDefaultConfig

repoDefaultConfig :: Ini
repoDefaultConfig = Ini
  { iniSections = M.fromList
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
      currentPath <- liftIO $ D.canonicalizePath p
      isDir <- liftIO $ D.doesDirectoryExist currentPath
      if isDir
        then gitRepository (currentPath ++ "/.git") False
        else do
        parentPath <- liftIO . D.canonicalizePath $ currentPath ++ "/.."
        case (parentPath == currentPath, required) of
          (True, True)  -> throwGIE ""
          (True, False) -> throwGIE ""
          _             -> loop parentPath
