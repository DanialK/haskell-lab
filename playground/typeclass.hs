-- Experiment 1
class Appendable m where
  append :: m -> m -> m

instance Appendable [a] where
  a `append` b = a ++ b

-- Experiment 2
newtype TSum a = Sum { getSum :: a };

instance Num a => Appendable (TSum a) where
  (Sum a) `append` (Sum b) = Sum (a + b)

instance Show a => Show (TSum a) where
  show (Sum a) = show a

main :: IO()
main = do
  putStrLn $ append "Hello " "World"
  putStrLn $ show 3213213
  putStrLn $ (show . getSum) (append (Sum 1) (Sum 2))
  putStrLn $ show (append (Sum 1) (Sum 2))