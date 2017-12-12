{-# LANGUAGE OverloadedStrings #-}
module Main ( main
            ) where

import Lib
import Turtle hiding (printf, stdout, stderr, find)
import Prelude hiding (FilePath)
import System.IO hiding (FilePath, stdout, stderr)
import Text.Regex.TDFA
import Text.Printf
import Data.List hiding (find)
import Data.List.Split
import qualified Data.Text as T
import System.Environment
import qualified Control.Foldl as FL
import Control.Monad.Extra
import Data.IORef

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
  r <- newIORef 0
  return (\i -> do modifyIORef r (+i)
                   readIORef r)


data Settings = Settings
  { sVerbose           :: Bool
  , sDropTracknumber   :: Bool
  , sStripDecorations  :: Bool
  , sFileTitle         :: Bool
  , sFileTitleNum      :: Bool
  , sSortLex           :: Bool
  , sTreeDst           :: Bool
  , sDropDst           :: Bool
  , sReverse           :: Bool
  , sFileType          :: Maybe Text
  , sPrependSubdirName :: Bool
  , sUnifiedName       :: Maybe Text
  , sAlbumNum          :: Maybe Int
  , sArtistTag         :: Maybe Text
  , sAlbumTag          :: Maybe Text
  , sSrc               :: FilePath
  , sDst               :: FilePath
  }

settingsP :: Parser Settings

settingsP =
  Settings <$> switch "verbose" 'v' "Unless verbose, just progress bar is shown"
           <*> switch "droptracknumber" 'd' "Do not set track numbers"
           <*> switch "stripdecorations" 's' "Strip file and directory name decorations"
           <*> switch "filetitle" 'f' "Use file name for title tag"
           <*> switch "filetitlenum" 'F' "Use numbered file name for title tag"
           <*> switch "sortlex" 'x' "Sort files lexicographically"
           <*> switch "treedst" 't' "Retain the tree structure of the source album at destination"
           <*> switch "dropdst" 'p' "Do not create destination directory"
           <*> switch "rev" 'r' "Copy files in reverse order (number one file is the last to be copied)"
           <*> optional (optText "filetype" 'e' "Accept only audio files of the specified type")
           <*> switch "prependsubdirname" 'i' "prepend current subdirectory name to a file name"
           <*> optional (optText "unifiedname" 'u' "Base name for everything, except for the \"Artist\" tag")
           <*> optional (optInt "albumnum" 'b' "Add album number to destination")
           <*> optional (optText "artisttag" 'a' "\"Artist\" tag")
           <*> optional (optText "albumtag" 'g' "\"Album\" tag")
           <*> argPath "src" "Source directory"
           <*> argPath "dst" "Destination directory"


description :: Description
description =
  "pch \"Procrustes\" SmArT is a CLI utility for copying subtrees containing supported\n\
  \audio files in sequence, naturally sorted.\n\
  \The end result is a \"flattened\" copy of the source subtree. \"Flattened\" means\n\
  \that only a namesake of the root source directory is created, where all the files get\n\
  \copied to, names prefixed with a serial number. Tags \"Track\" and \"Tracks Total\" \n\
  \get set, tags \"Artist\" and \"Album\" can be replaced optionally.\n\
  \The writing process is strictly sequential: either starting with the number one file,\n\
  \or in the reversed order. This can be important for some mobile devices."
  

printOptions :: Settings -> IO ()
printOptions opt = do
  print (format ("verbose: "%w) (sVerbose opt))
  print (format ("droptracknumber: "%w) (sDropTracknumber opt))
  print (format ("stripdecorations: "%w) (sStripDecorations opt))
  print (format ("filetitle: "%w) (sFileTitle opt))
  print (format ("filetitlenum: "%w) (sFileTitleNum opt))
  print (format ("sortlex: "%w) (sSortLex opt))
  print (format ("treedst: "%w) (sTreeDst opt))
  print (format ("dropdst: "%w) (sDropDst opt))
  print (format ("reverse: "%w) (sReverse opt))
  print (format ("filetype: "%w) (sFileType opt))
  print (format ("prependsubdirname: "%w) (sPrependSubdirName opt))
  print (format ("unifiedname: "%w) (sUnifiedName opt))
  print (format ("albumnum: "%w) (sAlbumNum opt))
  print (format ("artisttag: "%w) (sArtistTag opt))
  print (format ("albumtag: "%w) (sAlbumTag opt))
  print (format ("src: "%fp) (sSrc opt))
  print (format ("dst: "%fp) (sDst opt))


-- | Returns a zero-padded num literal
--
-- Examples:
--
-- >>> zeroPad 3 5
-- "00003"
-- >>> zeroPad 15331 3
-- "15331"
zeroPad :: Int -> Int -> String
zeroPad n len = printf ("%0" ++ (printf "%d" len) ++ "d") n


-- | Returns a list of integer numbers embedded in a string arguments
--
-- Examples:
--
-- >>> strStripNumbers "ab11cdd2k.144"
-- [11,2,144]
-- >>> strStripNumbers "Ignacio Vazquez-Abrams"
-- []
strStripNumbers :: String -> [Int]
strStripNumbers str =
  let numbers = concat (str =~ ("[0-9]+" :: String) :: [[String]])
  in  [read i :: Int | i <- numbers]


-- | If both strings contain digits, returns numerical comparison based on the numeric
-- | values embedded in the strings, otherwise returns the standard string comparison.
-- | The idea of the natural sort as opposed to the standard lexicographic sort is one of coping
-- | with the possible absence of the leading zeros in 'numbers' of files or directories
--
-- Examples:
--
-- >>> cmpstrNaturally "" ""
-- EQ
-- >>> cmpstrNaturally "2a" "10a"
-- LT
-- >>> cmpstrNaturally "alfa" "bravo"
-- LT
cmpstrNaturally :: String -> String -> Ordering
cmpstrNaturally x y =
  let nx = strStripNumbers x
      ny = strStripNumbers y
  in  if nx /= [] && ny /=  []
      then compare nx ny
      else compare x y


-- | Reduces a string of names to initials
--
-- Examples:
--
-- >>> makeInitials " "
-- "."
-- >>> makeInitials "John ronald reuel\tTolkien"
-- "J.R.R.T."
-- >>> makeInitials "e. B. Sledge"
-- "E.B.S."
-- >>> makeInitials "Apsley  Cherry-Garrard"
-- "A.C-G."
-- >>> makeInitials "Windsor Saxe-\tCoburg - Gotha"
-- "W.S-C-G."
-- >>> makeInitials "Elisabeth Kubler-- - Ross"
-- "E.K---R."
-- >>> makeInitials "Fitz-Simmons Ashton-Burke Leigh"
-- "F-S.A-B.L."
{-- >>> makeInitials "Arleigh\"31-knot\"Burke"-}
{-- "A.B."-}
makeInitials :: String -> Text
makeInitials grandName =
  let parts = splitOn "-" grandName
      splitPart = \part -> concat (part =~ ("[^ \t]+" :: String) :: [[String]])
      inits = (\part -> intercalate "." [[head w] | w <- splitPart part]) <$> parts
  in  T.toUpper $ fromString $ intercalate "-" inits ++ "."


-- | Extracts String From FilePath (unsafe and unofficial)
strp :: FilePath -> String
strp path =
  let parts = splitOn "\"" (show path)
  in  parts !! 1

-- | Serves a list of directories and a list of files
-- | of a given parent
listDir :: FilePath -> IO ([FilePath], [FilePath])
listDir src = do
  list <- fold (ls src) FL.list
  partitionM testdir list


traverseFlatDst :: FilePath -> Int -> Int -> Counter -> FilePath -> IO ()
traverseFlatDst dstRoot total totw cnt srcDir = do
  (dirs, files) <- listDir srcDir
  let iterate = printPath cnt
  mapM_  iterate files -- tracing
  let traverse = traverseFlatDst dstRoot total totw cnt
  mapM_ traverse dirs


printPath :: Counter -> FilePath -> IO ()
printPath counter file = do
  n <- counter 1
  putStrLn (printf "%d : %s" n (strp file))


-- | Trace path list
putFilePaths :: [FilePath] -> IO ()
putFilePaths pathList = do
  mapM_ (\path -> putStrLn $ strp path) pathList


groom :: FilePath -> FilePath -> Int -> IO ()
groom src dst total = do
  counter <- makeCounter
  let totWidth = length $ show total
  traverseFlatDst dst total totWidth counter src
  putStrLn (printf "total: %d, width: %d" total totWidth)


filterAudio :: [FilePath] -> [FilePath]
filterAudio pathList = pathList


listTree :: FilePath -> IO [FilePath]
listTree src = fold (lstree src) FL.list


buildAlbum :: Settings -> IO ()
buildAlbum args = do
  printOptions args
  flatTree <- listTree (sSrc args) -- Counting files for future reference
  groom (sSrc args) (sDst args) (length $ filterAudio flatTree)


copyAlbum :: Settings -> IO ()
copyAlbum args = do
  buildAlbum args


main :: IO ()
main = do
  args <- options description settingsP
  copyAlbum args
