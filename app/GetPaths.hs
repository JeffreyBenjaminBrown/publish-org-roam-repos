module GetPaths where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Text.Regex.TDFA ((=~))


sanitizeRelativePath :: String -> String
sanitizeRelativePath path =
  if (head path == '/')
  then drop 1 path
  else path

-- | do; fs <- relFfilesAnyDepth ".hs" "."; mapM_ putStrLn fs
relFfilesAnyDepth :: String -> FilePath -> IO [FilePath]
relFfilesAnyDepth extension dir =
  map ( sanitizeRelativePath -- Necessary because dropping the length of `dir` might leave the result starting with '/', if `dir` does not include the trailing '/'.
        . ( drop $ length dir ) )
  <$> absFfilesAnyDepth extension dir

absFfilesAnyDepth :: String -> FilePath -> IO [FilePath]
absFfilesAnyDepth
  extension -- ^ PITFALL: Include the leading dot.
  dir = do
    entries <- listDirectory dir
    let paths = map (dir </>) entries
    files <- mapM fileOrFiles paths
    return
      $ filter (=~ ( "\\" -- this escapes the leading dot
                     ++ extension ++ "$"))
      $ concat files
  where

    fileOrFiles :: FilePath -> IO [FilePath]
    fileOrFiles path = do
      -- When given a folder, returns the files in it.
      -- Given a file, returns it (in a list).
      isDir <- doesDirectoryExist path
      if isDir
        then absFfilesAnyDepth extension path
        else return [path]
