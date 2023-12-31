module Main (main) where

import MyLib

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Test.Tasty.Hedgehog as Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List

testParseSuccess :: String -> Char -> Bool -> ([String], [[String]]) -> Assertion
testParseSuccess input delimiter hasHeader expected = assertEqual
    "Should successfully parse the CSV"
    (Right expected)
    (parseCSV input delimiter hasHeader)

testParseFailure :: String -> Char -> Bool -> String -> Assertion
testParseFailure input delimiter hasHeader expectedErrorMessage = do
    let result = parseCSV input delimiter hasHeader
    case result of
        Right _ -> assertFailure "Expected parsing to fail, but it succeeded."
        Left bundle -> 
            let errorMessage = errorBundlePretty bundle
            in assertBool ("Expected error message to contain: " ++ expectedErrorMessage) (expectedErrorMessage `isInfixOf` errorMessage)
            
testCase1 :: TestTree
testCase1 = testCase "Parse simple CSV with header" $
  testParseSuccess "Name,Age\nSalavat,21\nDima,24" ',' True (["Name", "Age"], [["Salavat", "21"], ["Dima", "24"]])

testCase2 :: TestTree
testCase2 = testCase "Parse simple CSV without header" $
  testParseSuccess "Salavat,21\nDima,24" ',' False ([], [["Salavat", "21"], ["Dima", "24"]])

testCase3 :: TestTree
testCase3 = testCase "Parse CSV without header and one column" $
  testParseSuccess "Salavat\nDima" ',' False ([], [["Salavat"], ["Dima"]])

testCase4 :: TestTree
testCase4 = testCase "Parse CSV without header and one column" $
  testParseSuccess "Name\nSalavat\nDima" ',' True (["Name"], [["Salavat"], ["Dima"]])

testCase5 :: TestTree
testCase5 = testCase "Parse CSV without header and data" $
  testParseSuccess "" ',' False ([], [[""]])

testCase6 :: TestTree
testCase6 = testCase "Parse CSV with header and without data" $
  testParseSuccess "Name\n" ',' True (["Name"], [[""]])

testCase7 :: TestTree
testCase7 = testCase "Parse CSV with header and lines with another count columns should return error" $
  testParseFailure "Name\nSalavat,21" ',' True "Number of cells in row does not match header"

testCase8 :: TestTree
testCase8 = testCase "Parse CSV with wrong delimiter should return error" $
  testParseFailure "Name:Age\nSalavat:21" ':' True "Bad delimiter. Expected `,` or `;`"

tests :: TestTree
tests = testGroup "All tests" [
    testCase1,
    testCase2,
    testCase3,
    testCase4,
    testCase5,
    testCase6,
    testCase7,
    testCase8
  ]

main :: IO ()
main = defaultMain tests