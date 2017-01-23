#!/usr/bin/env stack
-- stack --resolver lts-7.15 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings, TemplateHaskell, NamedFieldPuns #-}

import Data.Text (pack, unpack, isPrefixOf, intercalate)
import Prelude hiding (FilePath)
import Turtle hiding (find)
import Data.List (find, (\\))
import Data.Foldable (forM_)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (showGregorian)
import qualified Control.Foldl as Foldl

-- CONFIG

documents = Folder "Documents" "/home/stefan/Documents"
music = Folder "Music" "/home/stefan/Music"

icarus = Machine "icarus"
  [ documents
  , music
  , Folder "icarus-home" "/home/stefan/" ]

laptop = Machine "stefan-laptop"
  [ documents
  , music
  , Folder "Pictures" "/home/stefan/Pictures"
  , Folder "laptop-home" "/home/stefan" ]

machines :: [ Machine ]
machines = [ icarus, laptop ]

-- CODE
data Folder = Folder { foldername :: Text
                     , folderpath :: FilePath }
  deriving (Show, Eq)

data Machine = Machine { machinename :: Text
                       , machinefolders :: [ Folder ] }
  deriving Show

parser :: Parser FilePath
parser = argPath "backuproot" "ABSOLUTE path to folder where backups should be placed, e.g. /mnt/usbdrive/"

checkFolder (Folder { foldername, folderpath }) = do
  exists <- testdir folderpath
  if exists then pure () else do
    err $ format ("Folder "%w%" does not exist in the filesystem at "%w%".") foldername folderpath
    exit (ExitFailure 1)

excludeList :: Text -> [Folder] -> Either Text [Text]
excludeList _ [] = Right []
excludeList b (f : fs) = do
  t <- toText (folderpath f)
  ts <- excludeList b fs
  if t `isPrefixOf` b
    then pure ts
    else pure ("--exclude=" <> t : ts)

rsync backupRoot previous today fs f@(Folder { foldername, folderpath }) = do
  flags <- pure ["-a", "--info=progress2"]
  -- Trailing slash makes rsync copy the contents of the directory, not the directory itself
  source <- (<> "/") <$> toText folderpath
  excludes <- excludeList source fs
  target <- toText $ backupRoot </> fromText foldername </> fromText today
  linkDest <- toText previous
  pure $ flags <> excludes <> ["--link-dest=" <> linkDest, source, target]

main :: IO ()
main = sh $ do
  backupRoot <- options "A simple backup script" parser
  host <- hostname
  printf ("We're on host "%w%" backing up into folder "%w%".\n") host backupRoot
  let (Just m) = find (\h -> machinename h == host) machines
  forM_ (machinefolders m) checkFolder
  today <- pack . showGregorian . utctDay <$> date
  
  f <- select $ machinefolders m
  mprevious <- fold (ls (backupRoot </> fromText (foldername f))) Foldl.maximum
  case mprevious of
    Nothing -> die $ format ("Can't find a previous backup in "%fp) (backupRoot </> fromText (foldername f))
    Just previous | previous /= backupRoot </> fromText (foldername f) </> fromText today -> do
      let (Right rsyncArgs) = rsync backupRoot previous today (machinefolders m) f
      printf ("About to run rsync "%s%"\n") (intercalate " " rsyncArgs)
      procs "rsync" rsyncArgs empty
    Just p -> die $ format ("There is a backup for today already at "%fp) p

{- DESIGN

Manual, supervised, somewhat regular but not continuous backup.

Subject of backup are folders, e.g.:
 - Documents (paths: icarus:~/Documents, stefan-laptop:~/Documents)
 - Music
 - Pictures (paths: stefan-laptop:~/Pictures, not present on work computer)
 - work HOME (excl. Documents, Music)
 - laptop HOME (excl. Documents, Music, Pictures)

Put backups into folders $BACKUPROOT/$FOLDER/$DATE.

Would be cool features:
1. Mark folders that are expected to not change (Music/, Pictures/, ...) and
   warn if they do (except for new files).
2. Automatically exclude contained folders that are backed up separately already.
   Should work recursively (Mail in Documents, Documents in Home). Rsync excludes are a bit weird.
3. Warn when we are running low on disk space (e.g. 2x current backup wouldn't fit).
   Suggest old backups to delete (exponential backoff/daily+weekly+monthly+yearly/something).
4. Sanity check configuration before run: folders present on host, folders present on backup disk, folders on disk are configured, ...
-}
