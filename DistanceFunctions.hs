-- Intersection function
intersect :: String -> String -> String
intersect [] _ = []
intersect (x:xs) ys
  | x `elem` ys = x : intersect xs ys
  | otherwise = intersect xs ys

distance1 :: String -> String -> Float
distance1 [] [] = 0.0
distance1 s1 s2 =
  let diff1 = fromIntegral (length s1 - length (intersect s1 s2))
      diff2 = fromIntegral (length s2 - length (intersect s2 s1))
      totalLength = fromIntegral (length s1 + length s2)
  in (diff1 + diff2) / totalLength

distance2 :: String -> String -> Float
distance2 [] [] = 0.0
distance2 s1 s2 =
  let diff1 = fromIntegral (length [c | c <- s1, not (c `elem` ['0'..'9'])])
      diff2 = fromIntegral (length [c | c <- s2, not (c `elem` ['0'..'9'])])
      totalLength = fromIntegral (length s1 + length s2)
  in (diff1 + diff2) / totalLength

distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter _ _ _ [] = []
distanceFilter distanceFunc maxDistance baseString stringList =
  filter (\s -> distanceFunc baseString s <= maxDistance) stringList

-- Test function
distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y

main :: IO ()
main = do
  let baseString = "example"
  let maxDistance = 2.0
  let stringList = ["exmple", "exampel", "exmplee", "exampl", "ex", "sample"]

  let filteredStrings = distanceFilter distance3 maxDistance baseString stringList
  putStrLn "Filtered strings:"
  mapM_ putStrLn filteredStrings
