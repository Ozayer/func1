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
