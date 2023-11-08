nextIsGreater :: [Int] -> [Int]
nextIsGreater [] = []  
nextIsGreater [_] = []  
nextIsGreater (x:y:xs)
    | y > x     = x : nextIsGreater (y:xs)  
    | otherwise = nextIsGreater (y:xs)      
