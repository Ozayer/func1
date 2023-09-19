headOrLast :: [String] -> Char -> [String]
headOrLast strings char = [s | s <- strings, not (null s), head s == char || last s == char]
