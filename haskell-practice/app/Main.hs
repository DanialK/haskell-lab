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

-- 9. String of numbers to array of Ints
-- Note: words "hi I am Danial" == ["hi","I","am","Danial"]
-- Note: read "12"::Int == 12
--       read "1.22"::Double == 1.22
--       read "True"::Bool == True
-- readInts has type String mapping to a list of Ints
readInts :: String -> [Int]
readInts s = let ws = words s in map read ws

-- 10. Type constraint in type signature using '=>' (pronounced implies)
--     '->' (pronounced maps to) used in type signatures and anonymous functions
minMax :: Ord a => [a] -> Maybe (a, a)
-- ':' is pronounced cons. cunstructs a list from a new head element and an 
--     existing tail or used for pattern matching like in this case

-- 'Just' and 'Nothing' are data constructors and belong to 'Maybe' type
minMax (h : t) = Just $ foldr -- Non empty list
    (\x (min, max) -> (if x < min then x else min, if x > max then x else max)) -- anonymous function
    (h, h) -- starting value, pair of head elements
    t -- iterate over the tail
minMax _ = Nothing  -- Empty list


-- 12. Where bindings
myWhereFunc :: Int -> Int -> Int
myWhereFunc x y = a + b
    where
        a = x + 1
        b = y + 1

-- 13. data and type
type Port = Int

data Address = Address Int Int Int Int Port

prettyPrint :: Address -> IO ()
prettyPrint (Address ip0 ip1 ip2 ip3 port)
    = putStrLn $
        show ip0 ++ "." ++
        show ip1 ++ "." ++
        show ip2 ++ "." ++
        show ip3 ++ ":" ++ show port


-- 14. Unwords is like [].join(" ")
--  unwords ["I", "am", "Danial"] == "I am Danial"
parenthesizeWords s = unwords $ map parenthesizeWord (words s)
      where parenthesizeWord s = "(" ++ s ++ ")"
-- Equivalent implementation
parenthesizeWords s = unwords $ map (\s -> "(" ++ s ++ ")") (words s)



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

  -- 8. Reading text files
  content <- readFile "./app/numbers.txt"
  putStrLn content

  -- 9. String of numbers to array of Ints
  let values = readInts content
      count = length values
      total = sum values
      mean = fromIntegral total / fromIntegral count
      range = minMax values -- 10. read minMax notes above

  putStrLn $ "count = " ++ show count -- show is equivalent of toString
  putStrLn $ "total = " ++ show total
  putStrLn $ "mean = " ++ show mean
  putStrLn $ "range = " ++ show range

  -- 11. Whitespace indentations
  let
    x = 5
    y = 6
    in print (x + y)

  let x = 5
      y = 6 
      in print (x + y)

  let x = 5; y = 6 in print (x + y)
  
  let { x = 5; y = 6 } in print (x + y)

  -- 12. Where bindings
  print $ myWhereFunc 10 110

  -- 13. data and type
  prettyPrint (Address 127 0 0 1 80)

  -- 14. parenthesizeWords
  print $ parenthesizeWords "I am Danial"

  let c = CMYK 1.0 2.0 3.0 4.0
  putStrLn $ colorModel c
  putStrLn $ colorModel $ RGB 2 2 3
  print $ someFunc "I love my girl"


  -- List comprehention
  -- [[(row, col) | col <- [0..7]] | row <- [0..7]]