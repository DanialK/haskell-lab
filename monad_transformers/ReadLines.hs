import Control.Applicative ((<$>))
import Control.Monad (replicateM)

getLines :: Int -> IO [String]
getLines n = replicateM n getLine

getInts :: Int -> IO [Int]
getInts n = fmap read <$> getLines n

getInt :: IO Int
getInt = fmap read getLine

main :: IO ()
main = do
    t <- getInt
    nums <- getInts t
    print nums