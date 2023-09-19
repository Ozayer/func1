credits :: (Char, Int) -> (Char, Int) -> Int
credits ('s', 14) _ = 14
credits _ ('s', 14) = 14
credits (suit1, num1) (suit2, num2)
    | suit1 == suit2 && abs (num1 - num2) == 1 = 8
    | num1 == num2 = 6
    | abs (num1 - num2) == 1 = 4
    | suit1 == suit2 = 2
    | otherwise = 0