import Control.Monad.Trans.Reader (ReaderT, Reader, runReader, local, ask)

myName ::  Monad m => String -> ReaderT String m String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)

main :: IO ()
main = print $ runReader localExample "Fred"