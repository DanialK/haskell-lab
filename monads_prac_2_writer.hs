-- For a Few Monads More  - http://learnyouahaskell.com/for-a-few-monads-more

-- newtype Writer w a = Writer { runWriter :: (a, w) } 
-- instance (Monoid w) => Monad (Writer w) where  
--     return x = Writer (x, mempty)  
--     (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  

import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  

-- Using flatMap instead of do notation
multWithLog2 :: Writer [String] Int  
multWithLog2 = logNumber 3
    >>= (\a -> logNumber 5
        >>= (\b -> return(a*b)))

-- Using tell to add extra logs without changing the current value
multWithLog3 :: Writer [String] Int  
multWithLog3 = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)


-- Using flatMap instead of do notation
multWithLog4 :: Writer [String] Int  
multWithLog4 = logNumber 3
    >>= (\a -> logNumber 5
        >>= (\b -> tell(["Gonna multiply these two"])
            >>= (\() -> return(a*b)) ))


-- Greatest Common Divisor algorithm and logs
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]  
        return a
    | otherwise = do
        let result = a `mod` b
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (result)]  
        gcd' b (result) 

-- It's inefficient because it ends up associating the use of ++ to the left instead of to the right.
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result  


-- More performant version of gcdReverse
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  

toDiffList :: [a] -> DiffList a 
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int  
gcdReverse' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcdReverse' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result  

-- Performance Lists vs DiffLists
finalCountDownDiffList :: Int -> Writer (DiffList String) ()  
finalCountDownDiffList 0 = do  
    tell (toDiffList ["0"])  
finalCountDownDiffList x = do  
    finalCountDownDiffList (x-1)  
    tell (toDiffList [show x])  

-- This counts very fast
-- mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDownDiffList 500000  

finalCountDownList :: Int -> Writer [String] ()  
finalCountDownList 0 = do  
    tell ["0"]  
finalCountDownList x = do  
    finalCountDownList (x-1)  
    tell [show x]  

-- This counts very slowly 
-- mapM_ putStrLn . snd . runWriter $ finalCountDownList 500000

main :: IO()
main = do
    putStrLn $ show . fst $ runWriter (gcd' 8 3)
    mapM_ putStrLn $ snd $ runWriter (gcd' 8 3) 
    mapM_ putStrLn $ reverse $ snd $ runWriter $ gcdReverse 5 3
    mapM_ putStrLn . reverse . fromDiffList . snd . runWriter $ gcdReverse' 110 34 
    -- mapM_ putStrLn $ reverse $ snd $ runWriter $ gcdReverse' 5 3







