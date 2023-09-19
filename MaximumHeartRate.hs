maxhr :: Float -> Float
maxhr age
  | age <= 40 = 220 - age
  | otherwise = 207 - 0.7 * age
