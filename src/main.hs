-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage message = print (message)

-- Write division here
division :: Double -> Double -> Maybe Double
division x 0 = Nothing
division x y = Just (x / y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Write factList here
factList :: Int -> [Int]
factList 1 = [1]
factList n = factList (n-1) ++ [factorial n]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x < y = x:merge xs (y : ys)
    | otherwise = y:merge (x : xs) ys

main = do
    printAMessage "Hello World"
    
    let z = division 1 2
    let w = division 1 1
    let g = division 6 2
    let h = division 5 0
    print (z, w, g, h)

    let a = factorial 1
    let b = factorial 7
    print (a, b)

    let list = factList 5
    print (list)

    let list_1 = [1, 3, 6]
    let list_2 = [2, 4, 5, 6, 7]
    let merged_list = merge list_1 list_2
    print (merged_list)