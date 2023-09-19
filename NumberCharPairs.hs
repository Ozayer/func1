-- Function charsDivisibleBy that returns characters with numbers divisible by n
charsDivisibleBy :: Int -> [Char]
charsDivisibleBy n = [intToChar i | i <- [1..26], i `mod` n == 0]

-- Function to generate a list of all possible pairs from a given list
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

-- Function charsProductOf that returns characters with numbers as a product of numbers in ns
charsProductOf :: [Int] -> [Char]
charsProductOf ns = 
    let products = [x * y | (x, y) <- pairs ns]
    in [intToChar i | i <- [1..26], (fromEnum (intToChar i) - 96) `elem` products]
