points :: Int -> [(Int, Int)]
points x = [(a, b) | a <- [-x..x], b <- [-x..x], abs a + abs b <= x]
