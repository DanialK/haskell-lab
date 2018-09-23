module CountEntriesT (listDirectory, countEntries) where

import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Writer.Lazy (WriterT, tell, runWriterT, execWriterT)

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

main :: IO ()
main = do
  -- Solution 1
  entries1 <- execWriterT (countEntries $ "../" </> "books")
  print entries1
  -- Solution 2
  result <- runWriterT (countEntries $ "../" </> "books")
  let entries2 = snd result
  print entries2


