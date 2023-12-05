readMaybeInt :: String -> Maybe Integer
readMaybeInt st =
  case reads st of
    [(x, "")] -> Just x
    _ -> Nothing

calculate :: [String] -> [String]
calculate = map calculateExpression

calculateExpression :: String -> String
calculateExpression expr =
  case words expr of
    [num1, "+", num2] -> performCalculation (+) num1 num2
    [num1, "-", num2] -> performCalculation (-) num1 num2
    [num1, "*", num2] -> performCalculation (*) num1 num2
    _                -> "I cannot calculate that"

performCalculation :: (Integer -> Integer -> Integer) -> String -> String -> String
performCalculation op num1 num2 =
  case (readMaybeInt num1, readMaybeInt num2) of
    (Just x, Just y) -> show (x `op` y)
    _                -> "I cannot calculate that"

main :: IO ()
main = do
  line1 <- getLine
  line2 <- getLine
  line3 <- getLine
  let input = [line1, line2, line3]
      result = calculate input
  mapM_ putStrLn result
