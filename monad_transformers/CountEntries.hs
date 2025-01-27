module CountEntries (listDirectory, countEntriesTrad) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM)

listDirectory :: FilePath -> IO [String]
-- listDirectory = liftM (filter notDots) . getDirectoryContents
listDirectory = fmap (filter notDots) . getDirectoryContents
  where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
      let newName = path </> name
      isDir <- doesDirectoryExist newName
      if isDir
        then countEntriesTrad newName
        else return []
  return $ (path, length contents) : concat rest

main :: IO ()
main = do
  filePaths <- countEntriesTrad $ "../" </> "books"
  print $ fmap fst filePaths

