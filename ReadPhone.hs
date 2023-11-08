data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

newtype CountryCode = CountryCode Integer deriving (Eq)

instance Show CountryCode where
  show (CountryCode num)
    | num >= 0 = "CountryCode " ++ show num
    | otherwise = error "Negative country code"

toCountryCode :: Integer -> CountryCode
toCountryCode code
  | code < 0 = error "Negative country code"
  | otherwise = CountryCode code

newtype PhoneNo = PhoneNo Integer deriving (Eq)

instance Show PhoneNo where
  show (PhoneNo num)
    | num >= 0 = "PhoneNo " ++ show num
    | otherwise = error "Negative phone number"

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
  | num < 0 = error "Negative phone number"
  | otherwise = PhoneNo num

data Phone = Phone { phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo } deriving (Eq)

instance Show Phone where
  show (Phone phoneType countryCode phoneNo) = "Phone {phoneType = " ++ show phoneType ++ ", countryCode = " ++ show countryCode ++ ", phoneNo = " ++ show phoneNo ++ "}"

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phoneType countryCode phoneNo
  | fromCountryCode countryCode < 0 = error "Negative country code"
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
  in makePhone phoneType code phoneNo

readPhoneType :: String -> PhoneType
readPhoneType phonetypestr
  | phonetypestr == "WorkLandline" = WorkLandline
  | phonetypestr == "PrivateMobile" = PrivateMobile
  | phonetypestr == "WorkMobile" = WorkMobile
  | phonetypestr == "Other" = Other
  | phonetypestr == "" = error "Missing phone type"
  | otherwise = error "Incorrect phone type"

readCountryCode :: String -> [Integer] -> CountryCode
readCountryCode countrycodestr ccodelist =
  let codeStr = removePlusAnd00 countrycodestr
      code = read codeStr :: Integer
  in if code `elem` ccodelist
    then CountryCode code
    else error "Unknown country code"

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