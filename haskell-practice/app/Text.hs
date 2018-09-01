
templates = [ "(shoe me|hat is) the sales for pepsi"
            , "hoe (is|are) the sales going"
            ]

walk :: String -> String
walk [] = []
walk (x:xs) = 
    case x of
        '(' -> '(' : xs
        ')' -> ')' : xs
        _ -> x:xs

main :: IO ()
main = do
    putStrLn $ walk $ head templates
    print "ss"