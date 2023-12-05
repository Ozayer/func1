module Phone_book(
    findEntries,
    addEntry,
    emptyBook,
    PhoneBookEntry(..),
    PhoneBook,
) where

import Phone_type2 (Phone(..), PhoneType(..), CountryCode(..), PhoneNo(..), PhoneBookEntry(..), fromPhoneNo, toPhoneNo, readPhone)

fromCountryCode :: CountryCode -> Integer
fromCountryCode (CountryCode code) = code

toCountryCode :: Integer -> CountryCode
toCountryCode code
  | code < 0 = error "Negative country code"
  | otherwise = CountryCode code

type PhoneBook = [PhoneBookEntry]

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phone code num
  | code < 0 = error "Negative country code"
  | num < 0 = error "Negative phone number"
  | otherwise = Phone {phoneType = phone, countryCode = code, phoneNo = num}

toPhoneTypeFromString :: String -> PhoneType
toPhoneTypeFromString phoneType
    | phoneType == "" = error "Missing phone type"
    | phoneType == "WorkLandline" = WorkLandline
    | phoneType == "PrivateMobile" = PrivateMobile
    | phoneType == "WorkMobile" = WorkMobile
    | phoneType == "Other" = Other
    | otherwise = error "Incorrect phone type"

predefinedCountryCodes :: [Integer]
predefinedCountryCodes = [1, 44, 358]

stringToIntSafe::String -> String -> Integer
stringToIntSafe numStr errMsg
    | null [a | a <- numStr , a `notElem` ['0'..'9']] = read numStr :: Integer
    | otherwise =  error errMsg

toCountryCodeFromString :: String -> CountryCode
toCountryCodeFromString countryCode
    | countryCode == "" = error "Empty country code"
    | length countryCode > 1 && head countryCode == '+' && toSafeInt (tail countryCode) `elem` predefinedCountryCodes  = toCountryCode (toSafeInt (tail countryCode))
    | length countryCode > 2 && head countryCode == '0' && head (tail countryCode) == '0' && toSafeInt (tail (tail countryCode)) `elem` predefinedCountryCodes = toCountryCode (toSafeInt (tail (tail countryCode)))
    | toSafeInt countryCode `elem` predefinedCountryCodes = toCountryCode (toSafeInt countryCode)
    | otherwise = error "Unknown country code"
    where toSafeInt = (`stringToIntSafe` "Incorrect country code")

toPhoneNoFromString::String -> PhoneNo
toPhoneNoFromString phoneNo
    | phoneNo == "" = error "Empty phone number"
    | otherwise = toPhoneNo (stringToIntSafe phoneNo "Incorrect phone number")

toPhoneBookEntry::String -> Phone -> PhoneBookEntry
toPhoneBookEntry newName newPhone = PhoneBookEntry {name = newName, phone = newPhone}

entryExists::String -> PhoneNo -> PhoneBook -> Bool 
entryExists searchName phoneNum phoneBook = not(null [entry | entry <- phoneBook, name entry == searchName, phoneNo (phone entry) == phoneNum])

findEntries :: String -> PhoneBook -> PhoneBook
findEntries searchName phoneBook = [entry | entry <- phoneBook, name entry == searchName]

addEntry :: String -> String -> [String] -> [String] -> [Integer] -> PhoneBook -> PhoneBook
addEntry nameStr phoneType countryCode phoneNo _ phoneBook =
    let newPhone = Phone { phoneType = toPhoneTypeFromString phoneType, countryCode = toCountryCodeFromString countryCode, phoneNo = toPhoneNoFromString phoneNo }
    in PhoneBookEntry { name = nameStr, phone = newPhone : phones } : phoneBook
  where
    phones = [phone | entry <- phoneBook, name entry == nameStr]

emptyBook :: PhoneBook
emptyBook = []
