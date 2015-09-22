module ParametricPolymorphism where

sum :: (Num a) => [a] -> a
sum x = foldl (+) 0 x

listOfInts :: [Int]
listOfInts = [1, 2, 3]

listOfDoubles :: [Double]
listOfDoubles = [1.5, 2.3, 3.1]

-- In this example the length function will always return the type ‘Int.’

lengthInt :: [a] -> Int
lengthInt xs = length' xs 0
    where length' [] i = i
          length' (_:r) i = length' r (i + 1)

-- However, in a static polymorphic type system you can replace the type definition with:

lengthNum :: (Num b) => [a] -> b
lengthNum xs = length' xs 0
    where length' [] i = i
          length' (_:r) i = length' r (i + 1)

-- In this case the calling function determines what type will be used to count the elements in the list. For example:

printLengths :: IO ()
printLengths = do
    let x = lengthNum [1, 2, 3, 4] :: Int
        y = lengthNum [1, 2, 3, 4] :: Integer
        z = lengthNum [1, 2, 3, 4] :: Double
    putStrLn $ "x:" ++ (show x) ++ " y: " ++ (show y) ++ " z:" ++ (show z)
