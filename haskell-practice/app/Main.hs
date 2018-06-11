import Lib

-- 4. Composition Fuctions
f x = x + 10
g x = x * x

-- 5. AddBrackets Functions
addBrackets s = "[" ++ s ++ "]"

-- 6. Control Structures Functions
myIf True thenFunc elseFunc = thenFunc
myIf False thenFunc elseFunc = elseFunc

-- 7. Factorial
factorial n = if n < 2 then 1 else n * factorial (n - 1)


data Color = RGB Int Int Int | CMYK Float Float Float Float deriving Show

colorModel :: Color -> String
colorModel c =
    case c of RGB 1 _ _ -> "RGB 1"
              RGB{} -> "RGB 2"
              CMYK{} -> "CMYK"

main :: IO ()
main = do
  -- 1. Filter
  print $ filter (> 3) [1, 2, 3, 4, 5, 6, 8]

  -- 2. Reduction
  print $ foldr (+) 0 [1, 2, 3]  -- equivalent: sum [1, 2, 3]

  -- 3. Mapping
  print $ map (+ 10) [1, 2, 3]

  -- 4. Composition
  print $ map (g . f) [1, 2, 3]

  -- 5. AddBrackets (String manipulation)
  print $ map addBrackets ["one", "two", "three"]

  -- 6. Control Structures (Function argument pattern matching)
  let x = 5 in print $ myIf (x == 5) "is five" "is not five"

  -- 7. Factorial
  print $ factorial 5

  let c = CMYK 1.0 2.0 3.0 4.0
  putStrLn $ colorModel c
  putStrLn $ colorModel $ RGB 2 2 3
  print $ someFunc "I love my girl"