#!/usr/bin/env stack
-- stack --resolver lts-7.15 --install-ghc runghc --package turtle --package path --package path-io

{-# LANGUAGE OverloadedStrings, TemplateHaskell, NamedFieldPuns #-}

import Data.Text (pack, unpack)
import Path (Abs, Dir, Path, mkAbsDir, mkRelDir, parseAbsDir)
import Path.IO (doesDirExist)
import Prelude hiding (FilePath)
import Turtle hiding (find)
import Data.List (find)
import Data.Foldable (forM_)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (showGregorian)

-- CONFIG

-- backup target folder names
folders :: [ Text ]
folders = [ "Documents"
          , "Music"
          , "laptop-home"
          , "icarus-home"
          ]

documents = Folder "Documents" $(mkAbsDir "/home/stefan/Documents")

icarus = Machine "icarus"
           [ documents
           , Folder "icarus-home" $(mkAbsDir "/home/stefan/") ]

laptop = Machine "stefan-laptop"
  [ documents
  , Folder "laptop-home" $(mkAbsDir "/home/stefan") ]

machines :: [ Machine ]
machines = [ icarus, laptop ]

-- CODE
data Folder = Folder { foldername :: Text
                     , folderpath :: Path Abs Dir }
  deriving Show

data Machine = Machine { machinename :: Text
                       , machinefolders :: [ Folder ] }
  deriving Show

textToAbsDir :: Text -> Maybe (Path Abs Dir)
textToAbsDir t = parseAbsDir (unpack t)

parser :: Parser (Path Abs Dir)
parser = arg textToAbsDir "backuproot" "ABSOLUTE path to folder where backups should be placed, e.g. /mnt/usbdrive/"

checkFolder (Folder { foldername, folderpath }) = do
  exists <- doesDirExist folderpath
  if exists then pure () else do
    err $ format ("Folder "%w%" does not exist in the filesystem at "%w%".") foldername folderpath
    exit (ExitFailure 1)

main :: IO ()
main = do
  backupRoot <- options "A simple backup script" parser
  host <- hostname
  printf ("We're on host "%w%" backing up into folder "%w%".\n") host backupRoot
  let (Just m) = find (\h -> machinename h == host) machines
  forM_ (machinefolders m) checkFolder
  t <- date
  echo $ pack $ showGregorian (utctDay t)

{- DESIGN

Manual, supervised, somewhat regular but not continuous backup.

Subject of backup are folders, e.g.:
 - Documents (paths: icarus:~/Documents, stefan-laptop:~/Documents)
 - Music
 - Pictures (paths: stefan-laptop:~/Pictures, not present on work computer)
 - work HOME (excl. Documents, Music)
 - laptop HOME (excl. Documents, Music, Pictures)

Put backups into folders $BACKUPROOT/$FOLDER/$DATE.

Sanity-check configuration (host is defined, all paths exists on host, every configured folder has a folder on backup disk, every on disk folder is configured).

Make a hardlink copy of the latest backup (cp -al), then overwrite with rsync.

Would be cool features:
1. Mark folders that are expected to not change (Music/, Pictures/, ...) and
   warn if they do (except for new files).
2. Automatically exclude contained folders that are backed up separately already.
   Should work recursively (Mail in Documents, Documents in Home). Rsync excludes are a bit weird.
3. Warn when we are running low on disk space (e.g. 2x current backup wouldn't fit).
   Suggest old backups to delete (exponential backoff/daily+weekly+monthly+yearly/something).
-}
