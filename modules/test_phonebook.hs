module TestPhoneBook where

import Phone_book
import Phone_type2

testCase = ("PersonA", "WorkLandline", "00358", "123456789")

predefinedCountryCodes :: [Integer]
predefinedCountryCodes = [1, 44, 358]

-- Initialize an empty phone book
initialPhoneBook = emptyBook

-- Use addEntry to add the test case to the phone book
-- phoneBook = addEntry "PersonA" "WorkLandline" "00358" "123456789" predefinedCountryCodes initialPhoneBook
phoneBook = addEntry "PersonB" "PrivateMobile" "358" "123456789" predefinedCountryCodes initialPhoneBook
phoneBook2 = addEntry "PersonB" "PrivateMobile" "358" "123356789" predefinedCountryCodes phoneBook


-- Print the initial phone book
main :: IO ()
main = do
    putStrLn "Initial Phone Book:"
    print initialPhoneBook

    -- Print the phone book after adding the entry
    putStrLn "\nPhone Book After Adding Entry:"
    print phoneBook2

    let searchName = "PersonB"

    -- Print the found entries
    putStrLn "\nFound Entries:"
    print (findEntries searchName phoneBook2)
