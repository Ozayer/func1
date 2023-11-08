module Phone_type2(
  fromPhoneNo,
  toPhoneNo,
  readPhone,
  Phone(..)
) where

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Ord, Eq, Read)

data CountryCode = CountryCode Integer deriving (Eq, Ord)

instance Show CountryCode where
  show (CountryCode num) = '+' : show num

fromCountryCode :: CountryCode -> Integer
fromCountryCode (CountryCode code) = code

toCountryCode :: Integer -> CountryCode
toCountryCode code
  | code < 0 = error "Negative country code"
  | otherwise = CountryCode code

instance Num CountryCode where
  fromInteger = toCountryCode
  x + y = let r = fromCountryCode x + fromCountryCode y in toCountryCode r
  x - y =
    let r = fromCountryCode x - fromCountryCode y
     in if r < 0 then error "Negative country code" else toCountryCode r
  x * y = let r = fromCountryCode x * fromCountryCode y in toCountryCode r

data PhoneNo = PhoneNo Integer deriving (Eq, Ord)

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
  | num < 0 = error "Negative phone number"
  | otherwise = PhoneNo num

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo num) = num

instance Num PhoneNo where
  fromInteger = toPhoneNo
  x + y = let r = fromPhoneNo x + fromPhoneNo y in toPhoneNo r
  x - y =
    let r = fromPhoneNo x - fromPhoneNo y
     in if r < 0 then error "Negative phone number" else toPhoneNo r
  x * y = let r = fromPhoneNo x * fromPhoneNo y in toPhoneNo r

instance Show PhoneNo where
  show (PhoneNo num) = show num

data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

instance Show Phone where
  show (Phone phoneType countryCode phoneNo) = show countryCode ++ " " ++ show phoneNo ++ " (" ++ show phoneType ++ ")"

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phone code num
  | code < 0 = error "Negative country code"
  | num < 0 = error "Negative phone number"
  | otherwise = Phone {phoneType = phone, countryCode = code, phoneNo = num}

toPhoneTypeFromString :: String -> PhoneType
toPhoneTypeFromString phoneType
    -- Empty phone type 
    | phoneType == "" = error "Missing phone type"
    -- Match and create
    | phoneType == "WorkLandline" = WorkLandline
    | phoneType == "PrivateMobile" = PrivateMobile
    | phoneType == "WorkMobile" = WorkMobile
    | phoneType == "Other" = Other
    -- Invalid phone type
    | otherwise = error "Incorrect phone type"

predefinedCountryCodes :: [Integer]
predefinedCountryCodes = [44, 358]

stringToIntSafe::String -> String -> Integer
stringToIntSafe numStr errMsg
    | null [a | a <- numStr , a `notElem` ['0'..'9']] = read numStr :: Integer
    | otherwise =  error errMsg

toCountryCodeFromString :: String -> CountryCode
toCountryCodeFromString countryCode
    -- Check if country code is an empty string 
    | countryCode == "" = error "Empty country code"
    -- Check if country code starts with '+'
    | length countryCode > 1 && head countryCode == '+' && toSafeInt (tail countryCode) `elem` predefinedCountryCodes  = toCountryCode (toSafeInt (tail countryCode))
    -- Check if country code starts with "00"
    | length countryCode > 2 && head countryCode == '0' && head (tail countryCode) == '0' && toSafeInt (tail (tail countryCode)) `elem` predefinedCountryCodes = toCountryCode (toSafeInt (tail (tail countryCode)))
    -- Check if country code is predefined
    | toSafeInt countryCode `elem` predefinedCountryCodes = toCountryCode (toSafeInt countryCode)
    -- Invalid country code
    | otherwise = error "Unknown country code"
    where toSafeInt = (`stringToIntSafe` "Incorrect country code")

toPhoneNoFromString::String -> PhoneNo
toPhoneNoFromString phoneNo
    | phoneNo == "" = error "Empty phone number"
    | otherwise = toPhoneNo (stringToIntSafe phoneNo "Incorrect phone number")


readPhone :: String -> String -> String -> Phone
readPhone phoneType countryCode phoneNo = makePhone ptype code no
    where ptype = toPhoneTypeFromString phoneType
          code = toCountryCodeFromString countryCode
          no = toPhoneNoFromString phoneNo

toPhoneBookEntry::String -> Phone -> PhoneBookEntry
toPhoneBookEntry newName newPhone = PhoneBookEntry {name = newName, phone = newPhone}

entryExists::String -> PhoneNo -> PhoneBook -> Bool 
entryExists searchName phoneNum phoneBook = not(null [entry | entry <- phoneBook, name entry == searchName, phoneNo (phone entry) == phoneNum])

findEntries :: String -> PhoneBook -> PhoneBook
findEntries searchName phoneBook = [entry | entry <- phoneBook, name entry == searchName]

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry nameStr phoneType countryCode phoneNo phoneBook
    | entryExists nameStr (toPhoneNoFromString phoneNo) phoneBook = phoneBook
    | otherwise = toPhoneBookEntry nameStr (readPhone phoneType countryCode phoneNo) : phoneBook
