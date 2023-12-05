module Phone_book_map
  ( findEntries,
    addEntry,
    emptyBook,
    Name,
    PhoneBook,
  )
where

import qualified Data.Map as Map
import Phone_type2

type Name = String
type PhoneBook = Map.Map Name [Phone]

findEntryByName :: String -> PhoneBook -> Maybe [Phone]
findEntryByName searchName phoneBook = Map.lookup searchName phoneBook

findEntries :: Name -> PhoneBook -> [Phone]
findEntries searchName phoneBook =
  case findEntryByName searchName phoneBook of
    Just entry -> entry
    Nothing -> []

-- addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
-- addEntry nameStr phoneTypeStr countryCodeStr phoneNoStr ccodelist phoneBook =
--   let phoneType = toPhoneTypeFromString phoneTypeStr
--       countryCode = toCountryCodeFromString countryCodeStr
--       phoneNo = toPhoneNoFromString phoneNoStr
--       newPhone = Phone {phoneType = phoneType, countryCode = countryCode, phoneNo = phoneNo}
--   in Map.insertWith (++) nameStr [newPhone] phoneBook

addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry nameStr phoneTypeStr countryCodeStr phoneNoStr ccodelist phoneBook =
  let phoneType = toPhoneTypeFromString phoneTypeStr
      countryCode = toCountryCodeFromString countryCodeStr
      phoneNo = toPhoneNoFromString phoneNoStr
      newPhone = Phone {phoneType = phoneType, countryCode = countryCode, phoneNo = phoneNo}
  in Map.insertWith (\_ existingPhones -> existingPhones ++ [newPhone]) nameStr [newPhone] phoneBook


emptyBook :: PhoneBook
emptyBook = Map.empty