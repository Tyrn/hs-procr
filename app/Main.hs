{-# LANGUAGE OverloadedStrings #-}
module Main ( main
            ) where

import Lib
import Turtle hiding (printf, stdout, stderr, find)
import Prelude hiding (FilePath)
import System.IO hiding (FilePath, stdout, stderr)
import qualified Data.Text as T
import Text.Printf
import Data.List
import System.Environment
import qualified Control.Foldl as FL
import Control.Monad.Extra


-- | Serves a list of directories and a list of files
-- | of a given parent
listDir :: Settings -> FilePath -> IO ([FilePath], [FilePath])
listDir args src = do
  list <- fold (ls src) FL.list
  (dirs, files) <- partitionM testdir list
  let sDirs = sortBy (\x y -> cmpstrNaturally (strp x) (strp y)) dirs
  let sFiles = sortBy (\x y -> cmpstrNaturally (strp $ dropExtension x) (strp $ dropExtension y)) files
  return (sDirs, sFiles)


shapeDst :: Settings -> FilePath -> Int -> Int -> Int -> FilePath -> FilePath -> FilePath
shapeDst args dstRoot total totw n dstStep srcFile =
  let prefix = if (sStripDecorations args) && (sUnifiedName args) == Nothing
                 then ""
                 else zeroPad n totw ++ "-"
      name   = case (sUnifiedName args) of
                 Just uName -> T.unpack uName
                 Nothing -> strp $ dropExtension $ filename srcFile
      ext    = case extension srcFile of
                 Just ext -> "." ++ T.unpack ext
                 Nothing -> ""
  in  dstRoot </> dstStep </> fromString (prefix ++ name ++ ext)


copyFile :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> IO ()
copyFile args dstRoot total totw counter dstStep srcFile = do
  n <- counter 1
  let dst = shapeDst args dstRoot total totw n dstStep srcFile
  cp srcFile dst
  putStrLn (printf "%d : %s" n (strp srcFile))


traverseFlatDst :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> IO ()
traverseFlatDst args dstRoot total totw counter dstStep srcDir = do
  (dirs, files) <- listDir args srcDir
  let iterate = copyFile args dstRoot total totw counter dstStep
  mapM_  iterate files
  let traverse = traverseFlatDst args dstRoot total totw counter dstStep
  mapM_ traverse dirs


groom :: Settings -> Int -> IO ()
groom args total = do
  counter <- makeCounter
  let totWidth = length $ show total
  traverseFlatDst args (sDst args) total totWidth counter (wrap "") (sSrc args)
  putStrLn (printf "total: %d, width: %d" total totWidth)


filterAudio :: [FilePath] -> [FilePath]
filterAudio pathList = pathList


listTree :: FilePath -> IO [FilePath]
listTree src = fold (lstree src) FL.list


buildAlbum :: Settings -> IO ()
buildAlbum args = do
  printOptions args
  flatTree <- listTree (sSrc args) -- Counting files for future reference
  groom args (length $ filterAudio flatTree)


copyAlbum :: Settings -> IO ()
copyAlbum args = do
  buildAlbum args


main :: IO ()
main = do
  args <- options description settingsP
  copyAlbum args
