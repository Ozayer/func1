commonSubstring :: String -> String -> String
commonSubstring s1 s2
  | null s1 || null s2 = []
  | head s1 /= head s2 =
    if null [ch | ch <- s2, ch == head s1]
      then commonSubstring (tail s1) s2
      else commonSubstring s1 (tail s2)
  | head s1 == head s2 = head s1 : commonSubstring (tail s1) (tail s2)