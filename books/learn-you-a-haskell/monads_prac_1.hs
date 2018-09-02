-- A Fistful of Monads  - http://learnyouahaskell.com/a-fistful-of-monads
routine :: Maybe String
routine = do
    start <- return ""
    end <- (\x -> Just ("Danial" ++ x)) start
    _ <- Nothing
    return end


boundtofail :: Maybe Char
boundtofail = do
    (x:xs) <- Just ""
    return x

listOfTuples1 :: [(Int, Char)]
listOfTuples1 = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)



listOfTuples2 :: [(Int, Char)]
listOfTuples2 = do
    n <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)

listOfTuples3 :: [(Int, Char)]
listOfTuples3 = [(n, ch) | n <- [1, 2], ch <- ['a', 'c']]


filteredList = [x | x <- [1..50], '7' `elem` show x]

-- List comprehension above gives us "guard" by default that allows us to filter out some
-- values we itterate over.
-- Lets implement :

class Monad m => MonadPlus m where 
    mzero :: m a
    mplus :: m a -> m a -> m a


instance MonadPlus [] where
    mzero = []
    mplus = (++)

-- '()' is like 'void' in imperative languages or 'Unit' in Scala
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- guard (5 > 2) :: [()]
-- Prints: [()]

-- guard (1 > 2) :: [()]
-- Prints: []

filteredListGuardnBind = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

filteredListGuardnDo = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x


