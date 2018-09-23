{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Text.Read (readMaybe)
import Control.Monad (replicateM, when)
import System.Random
import Test.QuickCheck
import Control.Monad.Trans.State
import Data.Char
import Prelude as P
-- a number between 1 and 100
newtype Guess = Guess Int
  deriving (Eq, Num, Ord, Read, Show)

instance Random Guess where
  randomR (Guess lo, Guess hi) g = let (x, g') = randomR (lo, hi) g
                                   in (Guess x, g')
  random = randomR (Guess 1, Guess 100)

instance Arbitrary Guess where
  arbitrary = Guess <$> choose (1, 100)

class Monad m => Program m where
  finish :: a -> m a
  chain  :: m a -> (a -> m b) -> m b
  map    :: m a -> (a -> b) -> m b
--   map a f = fmap f a
--   flatMap  :: m a -> (a -> m b) -> m b
--   flatMap = chain

-- instance Monad Program where
--   return = finish
--   (>>=)= chain

class Monad m => Console m where
  putStrLn :: String -> m ()
  getStrLn :: m String

class Monad m => Rand m where
  nextInt :: Int -> m Int


instance Program IO where 
  finish = return
  chain a f = a >>= f
  map a f = fmap f a

instance Console IO where
  putStrLn = P.putStrLn
  getStrLn = getLine

instance Rand IO where
  nextInt x = return x

main :: IO ()
main = Main.putStrLn "Done"

