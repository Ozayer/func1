-- PhoneBook.hs

data PhoneNo = PhoneNo Integer deriving (Eq, Ord)

data Phone = Phone { phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo } deriving (Eq)

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo num) = num


readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone phonetypestr countrycodestr phonenostr ccodelist =
  let phoneType = readPhoneType phonetypestr
      code = readCountryCode countrycodestr ccodelist
      phoneNo = readPhoneNo phonenostr
  in makePhone phoneType code phoneNo

readPhoneNo :: String -> PhoneNo
readPhoneNo phonenostr =
  let num = read phonenostr :: Integer
  in if num < 0
    then error "Negative phone number"
    else PhoneNo num

newtype CountryCode = CountryCode Integer deriving (Eq)

instance Show CountryCode where
  show (CountryCode num)
    | num >= 0 = "CountryCode " ++ show num
    | otherwise = error "Negative country code"

toCountryCode :: Integer -> CountryCode
toCountryCode code
  | code < 0 = error "Negative country code"
  | otherwise = CountryCode code

instance Show Phone where
  show (Phone phoneType countryCode phoneNo) = "Phone {phoneType = " ++ show phoneType ++ ", countryCode = " ++ show countryCode ++ ", phoneNo = " ++ show phoneNo ++ "}"

fromCountryCode :: CountryCode -> Integer
fromCountryCode (CountryCode code) = code

readCountryCode :: String -> [Integer] -> CountryCode
readCountryCode countrycodestr ccodelist =
  let codeStr = removePlusAnd00 countrycodestr
      code = read codeStr :: Integer
  in if code `elem` ccodelist
    then CountryCode code
    else error "Unknown country code"

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

-- newtype PhoneNo = PhoneNo Integer deriving (Eq)

instance Show PhoneNo where
  show (PhoneNo num)
    | num >= 0 = "PhoneNo " ++ show num
    | otherwise = error "Negative phone number"

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
  | num < 0 = error "Negative phone number"
  | otherwise = PhoneNo num

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phoneType countryCode phoneNo
  | fromCountryCode countryCode < 0 = error "Negative country code"
  | fromPhoneNo phoneNo < 0 = error "Negative phone number"
  | otherwise = Phone { phoneType = phoneType, countryCode = countryCode, phoneNo = phoneNo }

instance Read PhoneType where
    readsPrec _ "WorkLandline" = [(WorkLandline, "")]
    readsPrec _ "PrivateMobile" = [(PrivateMobile, "")]
    readsPrec _ "WorkMobile" = [(WorkMobile, "")]
    readsPrec _ "Other" = [(Other, "")]
    readsPrec _ _ = error "Incorrect phone type"

readPhoneType :: String -> PhoneType
readPhoneType phonetypestr
  | phonetypestr == "WorkLandline" = WorkLandline
  | phonetypestr == "PrivateMobile" = PrivateMobile
  | phonetypestr == "WorkMobile" = WorkMobile
  | phonetypestr == "Other" = Other
  | phonetypestr == "" = error "Missing phone type"
  | otherwise = error "Incorrect phone type"

removePlusAnd00 :: String -> String
removePlusAnd00 str =
  if head str == '+'
    then tail str
    else if head str == '0' && head (tail str) == '0'
      then tail (tail str)
      else str

data PhoneBookEntry = PhoneBookEntry { name :: String, phone :: Phone } deriving (Eq, Show)
type PhoneBook = [PhoneBookEntry]

-- Find entries by name
findEntries :: String -> PhoneBook -> PhoneBook
findEntries searchName phoneBook = [entry | entry <- phoneBook, name entry == searchName]

entryExists::String -> PhoneNo -> PhoneBook -> Bool 
entryExists searchName phoneNum phoneBook = not(null [entry | entry <- phoneBook, name entry == searchName, phoneNo (phone entry) == phoneNum])

-- Add a new entry
addEntry :: String -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry newName phoneType countryCode phoneNo ccList phoneBook =
  let newPhone = readPhone phoneType countryCode phoneNo ccList
  in if entryExists newName (phoneNo newPhone) phoneBook
       then phoneBook -- Entry already exists, make no change
       else PhoneBookEntry { name = newName, phone = newPhone } : phoneBook

-- Produce an empty phone book
emptyBook :: PhoneBook
emptyBook = []
