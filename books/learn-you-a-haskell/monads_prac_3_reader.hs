
-- Not only is the function type (->) r a functor and an applicative functor,
-- but it's also a monad.

-- instance Monad ((->) r) where  
--     return x = \_ -> x  
--     h >>= f = \w -> f (h w) w  

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

-- addStuff 3 == 19
-- Both (*2) and (+10) get applied to the number 3 in this case.
-- 3 is also applied to return (a+b) as well, but it ignores it 
-- and always presents a+b as the result
