-- Function to convert a number to the corresponding character
intToChar :: Int -> Char
intToChar n = toEnum (n + 96)

-- Function to convert a character to the corresponding number
charToInt :: Char -> Int
charToInt c = fromEnum c - 96

-- Function to find characters with numbers divisible by n
charsDivisibleBy :: Int -> [Char]
charsDivisibleBy 0 = []
charsDivisibleBy n = [intToChar i | i <- [1..26], isDivisibleBy n (intToChar i)]
  where
    isDivisibleBy n c = (charToInt c) `mod` n == 0

-- Function to find characters with numbers that are a product of exactly two numbers in ns
charsProductOf :: [Int] -> [Char]
charsProductOf ns = [intToChar i | i <- [1..26], isProductOf ns (intToChar i)]
  where
    isProductOf ns c = length [x | x <- ns, y <- ns, x /= y, x * y == charToInt c] > 0
