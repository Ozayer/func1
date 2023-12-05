module Phone_book_tree
  ( PhoneBook(..),
    Name,
    addEntry,
    findEntries,
    emptyBook,
  )
where

import Phone_type2

type Name = String

data PhoneBook = Empty | Node Name [Phone] PhoneBook PhoneBook deriving (Show, Eq)

addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry newName phonetype countrycode phoneno ccodelist Empty =
  Node newName [readPhoneType phonetype countrycode phoneno ccodelist] Empty Empty

addEntry newName phonetype countrycode phoneno ccodelist (Node nodeName phones left right)
  | newName == nodeName =
    Node newName (phones ++ [readPhoneType phonetype countrycode phoneno ccodelist]) left right
  | newName < nodeName =
    Node nodeName phones (addEntry newName phonetype countrycode phoneno ccodelist left) right
  | otherwise =
    Node nodeName phones left (addEntry newName phonetype countrycode phoneno ccodelist right)

findEntries :: Name -> PhoneBook -> [Phone]
findEntries _ Empty = []
findEntries searchName (Node nodeName phones left right)
  | searchName == nodeName = phones
  | searchName < nodeName = findEntries searchName left
  | otherwise = findEntries searchName right

emptyBook :: PhoneBook
emptyBook = Empty

-- Helper function to create a Phone from input strings
readPhoneType :: String -> String -> String -> [Integer] -> Phone
readPhoneType phonetype countrycode phoneno ccodelist =
  let phoneType = toPhoneTypeFromString phonetype
      code = toCountryCodeFromString countrycode
      phoneNo = toPhoneNoFromString phoneno
  in makePhone phoneType code phoneNo
