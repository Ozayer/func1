data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

newtype CountryCode = CountryCode Integer deriving (Eq)

instance Show CountryCode where
  show (CountryCode num)
    | num >= 0 = "+" ++ show num
    | otherwise = error "Negative country code"

toCountryCode :: Integer -> CountryCode
toCountryCode code
  | code < 0 = error "Negative country code"
  | otherwise = CountryCode code

newtype PhoneNo = PhoneNo Integer deriving (Eq)

instance Show PhoneNo where
  show (PhoneNo num)
    | num >= 0 = show num
    | otherwise = error "Negative phone number"

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
  | num < 0 = error "Negative phone number"
  | otherwise = PhoneNo num

data Phone = Phone { phoneType :: Maybe PhoneType, countryCode :: Maybe CountryCode, phoneNo :: PhoneNo } deriving (Eq)

instance Show Phone where
  show (Phone pType cCode phoneNo) =
    let countryCodeStr = maybe "" (\c -> show c ++ " ") cCode
        phoneTypeStr = maybe "" (\pt -> " (" ++ show pt ++ ")") pType
    in countryCodeStr ++ show phoneNo ++ phoneTypeStr

makePhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
makePhone phoneType countryCode phoneNo
  | maybe (-1) fromCountryCode countryCode < 0 = error "Negative country code"
  | fromPhoneNo phoneNo < 0 = error "Negative phone number"
  | otherwise = Phone { phoneType = phoneType, countryCode = countryCode, phoneNo = phoneNo }

fromCountryCode :: CountryCode -> Integer
fromCountryCode (CountryCode code) = code

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo num) = num

instance Read PhoneType where
    readsPrec _ "WorkLandline" = [(WorkLandline, "")]
    readsPrec _ "PrivateMobile" = [(PrivateMobile, "")]
    readsPrec _ "WorkMobile" = [(WorkMobile, "")]
    readsPrec _ "Other" = [(Other, "")]
    readsPrec _ _ = error "Incorrect phone type"

readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone phonetypestr countrycodestr phonenostr ccodelist =
  let phoneType = readPhoneType phonetypestr
      code = readCountryCode countrycodestr ccodelist
      phoneNo = readPhoneNo phonenostr
  in Phone phoneType code phoneNo

readPhoneType :: String -> Maybe PhoneType
readPhoneType "WorkLandline" = Just WorkLandline
readPhoneType "PrivateMobile" = Just PrivateMobile
readPhoneType "WorkMobile" = Just WorkMobile
readPhoneType "Other" = Just Other
readPhoneType "" = Nothing
readPhoneType _ = Nothing


readCountryCode :: String -> [Integer] -> Maybe CountryCode
readCountryCode countrycodestr ccodelist
  | null countrycodestr = Nothing
  | code `elem` ccodelist = Just (CountryCode code)
  | otherwise = Nothing
  where
    codeStr = removePlusAnd00 countrycodestr
    code = read codeStr :: Integer

removePlusAnd00 :: String -> String
removePlusAnd00 str =
  if head str == '+'
    then tail str
    else if head str == '0' && head (tail str) == '0'
      then tail (tail str)
      else str

readPhoneNo :: String -> PhoneNo
readPhoneNo phonenostr =
  let num = read phonenostr :: Integer
  in if num < 0
    then error "Negative phone number"
    else PhoneNo num