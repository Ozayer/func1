validate :: String -> Bool
validate x
    | length x /= 18 = False
validate (x:y:z) 
    |   x /= 'F' || y /= 'I' = False
    |   not (null [a | a <- z, a `notElem` ['0'..'9']]) = False
validate (a:b:c:d:e)
    |   (read (e ++ "1518" ++ [c,d])::Integer) `mod` 97 /= 1 = False
validate _ = True