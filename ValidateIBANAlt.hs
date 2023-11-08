validate :: String -> Bool
validate iban =
  length iban == 18 && isFinnishIBAN iban

isFinnishIBAN :: String -> Bool
isFinnishIBAN iban =
  isFinnishCountryCode (take 2 iban) && all isDigit' (drop 2 iban)

isFinnishCountryCode :: String -> Bool
isFinnishCountryCode code =
  code == "FI"

isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'
