distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y

clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters func d ss = [ [cluster | cluster <- ss, d >= func subStr cluster ] | subStr <- ss]