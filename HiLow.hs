{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Text.Read (readMaybe)
import Control.Monad (replicateM, when)
import System.Random
import Test.QuickCheck
import Control.Monad.Trans.State
import Data.Char

-- a number between 1 and 100
newtype Guess = Guess Int
  deriving (Eq, Num, Ord, Read, Show)

instance Random Guess where
  randomR (Guess lo, Guess hi) g = let (x, g') = randomR (lo, hi) g
                                   in (Guess x, g')
  random = randomR (Guess 1, Guess 100)

instance Arbitrary Guess where
  arbitrary = Guess <$> choose (1, 100)

class Monad m => MonadHighLow m where
  getGuess :: m (Maybe Guess)
  say      :: String -> m ()

play :: forall m. MonadHighLow m
     => Guess -> m Bool
    
play answer = do
  say "I'm thinking of a number between 1 and 100."
  say "Can you guess which one?"

  let nextTurn :: m Bool
      nextTurn = do
        maybeGuess <- getGuess
        case maybeGuess of
          Nothing -> say "Goodbye!" >> pure False
          Just guess | guess < answer -> say "Higher." >> nextTurn
                     | guess > answer -> say "Lower."  >> nextTurn
                     | otherwise      -> say "Correct!" >> pure True
  nextTurn


instance MonadHighLow IO where
    getGuess = do
        putStrLn "please enter your guess, or anything else to quit:"
        fmap Guess . readMaybe <$> getLine
    say = putStrLn

playAgainstHuman :: IO Bool
playAgainstHuman = do
    answer <- generate arbitrary
    play answer

newtype AI a = AI { runAI :: State [Guess] a }
  deriving (Functor, Applicative, Monad)

instance MonadHighLow AI where
  getGuess = AI $ do
    remainingGuesses <- get
    case remainingGuesses of
      [] -> pure Nothing
      x:xs -> do
        put xs
        pure (Just x)
  say _ = pure ()

playAgainstAI :: Guess -> [Guess] -> Bool
playAgainstAI answer guesses = flip evalState guesses $ runAI $ play answer

getReplay :: IO Bool
getReplay = do
  putStrLn "Would you like to play again?"
  answer <- getLine
  case answer of 
    "y" -> pure True
    _ -> pure False

main :: IO ()
main = do
  win <- playAgainstHuman
  when win $ do
    reply <- getReplay
    when reply main 