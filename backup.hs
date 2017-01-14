#!/usr/bin/env stack
-- stack --resolver lts-7.15 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings, TemplateHaskell, NamedFieldPuns #-}

import Data.Text (pack, unpack)
import Prelude hiding (FilePath)
import Turtle hiding (find)
import Data.List (find, (\\))
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

documents = Folder "Documents" "/home/stefan/Documents"

icarus = Machine "icarus"
           [ documents
           , Folder "code" "/home/stefan/src"
           , Folder "icarus-home" "/home/stefan/" ]

laptop = Machine "stefan-laptop"
  [ documents
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

excludeList [] = []
excludeList (Folder { folderpath } : fs) =
  -- need toText, which returns Either. Because filepaths can have some encoding weirdness. Who'd have thought. God. Writing correct software is hard and fucking annoying. No wonder nobody bothers.
  "--exclude=" <> (show folderpath) : excludeList fs

rsync f@(Folder { foldername, folderpath }) fs =
  ("rsync" <> " " <> "-av" <> " ") <> (concat (excludeList (fs \\ [f]))) <> " --link-dir=TODOpreviousbackup" <> " " <> show folderpath <> " TARGET"

main :: IO ()
main = do
  backupRoot <- options "A simple backup script" parser
  host <- hostname
  printf ("We're on host "%w%" backing up into folder "%w%".\n") host backupRoot
  let (Just m) = find (\h -> machinename h == host) machines
  forM_ (machinefolders m) checkFolder
  today <- pack . showGregorian . utctDay <$> date
  -- todayDir <- parseRelDir today
  echo $ pack $ show $ backupRoot </> fromText today
  -- forM_ (machinefolders m) (\f -> hardlinkFolder backupRoot f todayDir)
  forM_ (machinefolders m) (\f -> echo $ pack $ rsync f (machinefolders m))

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

Rsync manpage suggests it can do hardlinks automatically, so this should do it:

  `rsync -av --link-dest=/$BACKUPROOT/$FOLDERNAME/$LATESTDATE /$FOLDERPATH /$BACKUPROOT/$FOLDERNAME/$TODAY`


Would be cool features:
1. Mark folders that are expected to not change (Music/, Pictures/, ...) and
   warn if they do (except for new files).
2. Automatically exclude contained folders that are backed up separately already.
   Should work recursively (Mail in Documents, Documents in Home). Rsync excludes are a bit weird.
3. Warn when we are running low on disk space (e.g. 2x current backup wouldn't fit).
   Suggest old backups to delete (exponential backoff/daily+weekly+monthly+yearly/something).
-}
