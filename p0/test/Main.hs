module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HG
import Hedgehog
import qualified Test.Tasty.QuickCheck as QC
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import MyLib

h_prop_zipLong :: Hedgehog.Property
h_prop_zipLong = property $ do
  as <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  bs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha

  let result = zipLong as bs
  let expected = if length as > length bs
                      then zip as (bs ++ take (length as - length bs) bs)
                      else zip (as ++ take (length bs - length as) as) bs

  Hedgehog.assert $ result == expected

q_prop_zipLong :: [Int] -> [Char] -> Bool
q_prop_zipLong as bs =
  let zipped = zipLong as bs
  in
    length zipped >= min (length as) (length bs)

tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Unit Tests"
    [ testCase "zipLong [] \"abcd\""     $ zipLong [1,2,3] "abc" @?= [(1,'a'),(2,'b'),(3,'c')]
    , testCase "zipLong [1,2] \"abcd\""  $ zipLong [1,2] "abcd"  @?= [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
    , testCase "zipLong [1,2,3] \"abc\"" $ zipLong [] "abcd"     @?= ([] :: [(Int, Char)])
    ]
  , testGroup "Hedgehog tests"
    [ HG.testProperty "zipLong Hedgehog tests" h_prop_zipLong
    ]
  , testGroup "QuickCheck tests"
    [ QC.testProperty "zipLong QuickCheck tests" q_prop_zipLong
    ]
  ]

main :: IO ()
main = defaultMain tests