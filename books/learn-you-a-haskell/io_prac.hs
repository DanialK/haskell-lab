import System.Random

-- TODO: previous sections


-- Because we have to be able to potentially generate an infinite amount of numbers,
-- we can't give the new random generator back.
randoms' :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen  

-- Generates a finite stream of numbers and a new generator
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen)


-- => Random number in a range
-- randomR (1,6) (mkStdGen 359353)

-- => Produce a stream of random values within our defined ranges
-- take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

  
main = do  
    gen <- getStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen)  
    gen' <- newStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen')    