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


-- | Serves the list of all audio files in a given directory.
listTree :: FilePath -> IO [FilePath]
listTree src = do
  lst <- fold (lstree src) FL.list
  return (filter isAudioFile lst)


-- | Serves the list of directories and the list of audio files
-- of a given parent directory (immediate offspring).
listDir :: Settings -> FilePath -> IO ([FilePath], [FilePath])
listDir args src = do
  list <- fold (ls src) FL.list
  (dirs, files) <- partitionM testdir list
  let sDirs  = sortBy (\x y -> cmpstrNaturally (strp x) (strp y)) dirs
  let sFiles = sortBy (\x y -> cmpstrNaturally (strp $ dropExtension x)
                                               (strp $ dropExtension y))
                                               (filter isAudioFile files)
  return (sDirs, sFiles)


-- | Makes destination file path.
shapeDst :: Settings -> FilePath -> Int -> Int -> Int -> FilePath -> FilePath -> FilePath
shapeDst args dstRoot total totw n dstStep srcFile =
  let prefix = if (sStripDecorations args) && (sUnifiedName args) == Nothing
                 then ""
                 else zeroPad n totw ++ "-"
      name   = case (sUnifiedName args) of
                 Just uName -> T.unpack uName
                 Nothing    -> strp $ dropExtension $ filename srcFile
      ext    = case extension srcFile of
                 Just ext   -> "." ++ T.unpack ext
                 Nothing    -> ""
  in  dstRoot </> dstStep </> fromString (prefix ++ name ++ ext)


-- | Makes one copy from source to destination directory.
copyFile :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> IO ()
copyFile args dstRoot total totw counter dstStep srcFile = do
  n <- counter 1
  let dst = shapeDst args dstRoot total totw n dstStep srcFile
  cp srcFile dst
  putCopy args total totw n dst


-- | Walks the source tree.
traverseFlatDst :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> IO ()
traverseFlatDst args dstRoot total totw counter dstStep srcDir = do
  (dirs, files) <- listDir args srcDir
  let iterate = copyFile args dstRoot total totw counter dstStep
  mapM_  iterate files
  let traverse = traverseFlatDst args dstRoot total totw counter dstStep
  mapM_ traverse dirs


-- | Starts walking the source tree according to the settings.
groom :: Settings -> Int -> IO ()
groom args total = do
  counter <- makeCounter
  let totWidth = length $ show total
  putHeader args
  traverseFlatDst args (sDst args) total totWidth counter (wrap "") (sSrc args)
  putFooter args total


-- | Sets boilerplate.
buildAlbum :: Settings -> IO ()
buildAlbum args = do
  -- ~ printOptions args
  flatTree <- listTree (sSrc args)
  groom args (length flatTree)      -- Counting files for future reference


-- | Copies the album.
copyAlbum :: Settings -> IO ()
copyAlbum args = do
  buildAlbum args
  

-- | Prints the header of the output to the console.
putHeader :: Settings -> IO ()
putHeader args = do
  if (sVerbose args)
    then putStr ""
    else putStr "Start "


-- | Prints a single file copy info to the console.
putCopy :: Settings -> Int -> Int -> Int -> FilePath -> IO ()
putCopy args total totw n dstFile = do
  if (sVerbose args)
    then let fmt = "%" ++ (printf "%d" totw) ++ "d/%d %s\n"
         in  putStr (printf fmt n total (strp dstFile))
    else putStr "."


-- | Prints the footer of the output to the console.
putFooter :: Settings -> Int -> IO ()
putFooter args total = do
  if (sVerbose args)
    then putStr (printf "Total of %d file(s) copied\n" total)
    else putStr (printf " Done(%d)\n" total)
    

main :: IO ()
main = do
  args <- options description settingsP
  copyAlbum args
