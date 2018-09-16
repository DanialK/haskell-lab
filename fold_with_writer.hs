{-# LANGUAGE BangPatterns #-}

import Control.Monad.Writer
import Data.List (foldl')

-- foldl' :: (a -> b -> a) -> a -> [b] -> a
-- foldl' f y [] = y
-- foldl' f !y (x:xs) = foldl' f (f y x) xs

foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' f y [] = y
foldr' f y (x:xs) = f x (foldr' f y xs)


main = do
  let a = [1..10000000]

  putStrLn "foldl"
  let r1 = runWriter $ foldl (\x y -> x >>= (\x' -> writer(x' + y, [show x' ++ " - " ++ show y] ))) (return 0  :: Writer [String] Int) a
  print $ fst r1

  putStrLn "foldr"
  let r2 = runWriter $ foldr (\y x -> x >>= (\x' -> writer(x' + y, [show x' ++ " - " ++ show y] ))) (return 0  :: Writer [String] Int) a
  print $ fst r2
