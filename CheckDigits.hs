onlyDigits :: String -> Bool
onlyDigits [] = False  
onlyDigits str = all (\c -> c `elem` ['0'..'9']) str
