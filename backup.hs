#!/usr/bin/env stack
-- stack --resolver lts-7.15 --install-ghc runghc --package turtle --package hostname --package path

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Data.Text (pack, unpack)
import Network.HostName (getHostName)
import Path (Abs, Dir, Path, mkAbsDir, mkRelDir, parseAbsDir)
import Prelude hiding (FilePath)
import Turtle

-- CONFIG

-- backup target folder names
folders :: [ Text ]
folders = [ "Documents"
          , "Music"
          , "laptop-home"
          , "icarus-home"
          ]

documents = Folder "Documents" $(mkAbsDir "/home/stefan/Documents")

icarus = Host "icarus"
           [ documents
           , Folder "icarus-home" $(mkAbsDir "/home/stefan/") ]

laptop = Host "stefan-laptop"
  [ documents
  , Folder "laptop-home" $(mkAbsDir "/home/stefan") ]

hosts :: [ Host ]
hosts = [ icarus, laptop ]

-- CODE
data Folder = Folder { foldername :: Text
                     , folderpath :: Path Abs Dir }

data Host = Host { hostname :: Text
                 , hostfolders :: [ Folder ] }

textToAbsDir :: Text -> Maybe (Path Abs Dir)
textToAbsDir t = parseAbsDir (unpack t)

parser :: Parser (Path Abs Dir)
parser = arg textToAbsDir "backuproot" "ABSOLUTE path to folder where backups should be placed, e.g. /mnt/usbdrive/"

main :: IO ()
main = do
  backupRoot <- options "A simple backup script" parser
  hostname <- getHostName -- fmap pack getHostName :: IO Text
  printf ("We're on host "%w%" backing up into folder "%w%".\n") hostname backupRoot

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
