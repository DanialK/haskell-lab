import Control.Monad.State
import System.Random

-- threeCoins :: StdGen -> (Bool, Bool, Bool)  
-- threeCoins gen =   
--     let (firstCoin, newGen) = random gen  
--         (secondCoin, newGen') = random newGen  
--         (thirdCoin, newGen'') = random newGen'  
--     in  (firstCoin, secondCoin, thirdCoin)


-- stateful computation is a function that takes some state and returns 
-- a value along with some new state. s -> (a, s)


-- Example: Stacks and stones
type Stack = [Int]  
    
-- pop :: Stack -> (Int,Stack)  
-- pop (x:xs) = (x,xs)
    
-- push :: Int -> Stack -> ((),Stack)  
-- push a xs = ((),a:xs)

-- stackManip :: Stack -> (Int, Stack)  
-- stackManip stack = let  
--     ((),newStack1) = push 3 stack  
--     (a ,newStack2) = pop newStack1  
--     in pop newStack2

-- The State monad

-- newtype State s a = State { runState :: s -> (a,s) } 
-- instance Monad (State s) where  
--     return x = State $ \s -> (x,s)  
--     (State h) >>= f = State $ \s -> let (a, newState) = h s  
--                                         (State g) = f a  
--                                     in  g newState  

-- NOTE: Type of >>= for State values are
-- (>>=) :: State s a -> (a -> State s b) -> State s b 

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
    
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop  

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8 


-- Random Number Generator Using State Monad

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random


threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)




