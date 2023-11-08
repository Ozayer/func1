gap :: (Char, Char) -> Int -> String -> Int
gap (c1, c2) g s =
  if length s < g + 2
    then 0
    else if s !! 0 == c1 && s !! (g + 1) == c2
      then 1 + gap (c1, c2) g (drop 1 s)
      else gap (c1, c2) g (drop 1 s)
