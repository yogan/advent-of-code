import Lib
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  let sample1 =
        [ ["abba", "mnop", "qrst"],
          ["abcd", "bddb", "xyyx"],
          ["aaaa", "qwer", "tyui"],
          ["ioxxoj", "asdfgh", "zxcvbn"]
        ]

  let sample2 =
        [ ["aba", "bab", "xyz"],
          ["xyx", "xyx", "xyx"],
          ["aaa", "kek", "eke"],
          ["zazbz", "bzb", "cdb"]
        ]

  describe "Part 1" $ do
    it "works for the example" $ do
      part1 sample1 `shouldBe` 2

  describe "Part 2" $ do
    it "works for the example" $ do
      part2 sample2 `shouldBe` 3

  describe "supportsTls" $ do
    it "is true for the first sample" $ do
      supportsTls (head sample1) `shouldBe` True
    it "is false for the second sample" $ do
      supportsTls (sample1 !! 1) `shouldBe` False
    it "is false for the third sample" $ do
      supportsTls (sample1 !! 2) `shouldBe` False
    it "is true for the fourth sample" $ do
      supportsTls (sample1 !! 3) `shouldBe` True
    it "is false for a combination of sample 1 & 2 (bddb in brackets)" $ do
      supportsTls ["abba", "mnop", "qrstabcd", "bddb", "xyyx"] `shouldBe` False
    it "is true for a combination of sample 1 & 3" $ do
      supportsTls ["abba", "mnop", "qrstaaaa", "qwer", "tyui"] `shouldBe` True

  describe "supportsSsl" $ do
    it "is true for the first sample" $ do
      supportsSsl (head sample2) `shouldBe` True
    it "is false for the second sample" $ do
      supportsSsl (sample2 !! 1) `shouldBe` False
    it "is true for the third sample" $ do
      supportsSsl (sample2 !! 2) `shouldBe` True
    it "is true for the fourth sample" $ do
      supportsSsl (sample2 !! 3) `shouldBe` True

  describe "splitOddEven" $ do
    it "splits an empty list" $ do
      splitOddEven "" `shouldBe` ([], [])
    it "splits a list with one element" $ do
      splitOddEven "a" `shouldBe` (['a'], [])
    it "splits a list with two elements" $ do
      splitOddEven "ab" `shouldBe` (['a'], ['b'])
    it "splits a list with three elements" $ do
      splitOddEven "abc" `shouldBe` (['a', 'c'], ['b'])
    it "splits a list with four elements" $ do
      splitOddEven "abcd" `shouldBe` (['a', 'c'], ['b', 'd'])
    it "splits a list with five elements" $ do
      splitOddEven "abcde" `shouldBe` (['a', 'c', 'e'], ['b', 'd'])

  describe "hasABBA" $ do
    it "is true for abba" $ do
      hasABBA "abba" `shouldBe` True
    it "is true for xabba" $ do
      hasABBA "xabba" `shouldBe` True
    it "is true for abbax" $ do
      hasABBA "abbax" `shouldBe` True
    it "is false for ababa" $ do
      hasABBA "ababa" `shouldBe` False
    it "is false for aaaa" $ do
      hasABBA "aaaa" `shouldBe` False
    it "is false for empty string" $ do
      hasABBA "" `shouldBe` False

  describe "findABAs" $ do
    it "works for aba" $ do
      findABAs "aba" `shouldBe` ["aba"]
    it "works for xaba" $ do
      findABAs "xaba" `shouldBe` ["aba"]
    it "works for abax" $ do
      findABAs "abax" `shouldBe` ["aba"]
    it "works for abba" $ do
      findABAs "abba" `shouldBe` []
    it "works for aaaa" $ do
      findABAs "aaaa" `shouldBe` []
    it "works for empty string" $ do
      findABAs "" `shouldBe` []
    it "works for zazbz" $ do
      findABAs "zazbz" `shouldBe` ["zaz", "zbz"]

  describe "containsBAB" $ do
    it "is true for aba and bab" $ do
      containsBAB "aba" ["bab"] `shouldBe` True
    it "is true for aba and xbab" $ do
      containsBAB "aba" ["xbab"] `shouldBe` True
    it "is true for aba and babx" $ do
      containsBAB "aba" ["babx"] `shouldBe` True

    it "is true for aba and bab as second element" $ do
      containsBAB "aba" ["nope", "bab"] `shouldBe` True
    it "is true for aba and xbab as second element" $ do
      containsBAB "aba" ["nope", "xbab"] `shouldBe` True
    it "is true for aba and babx as second element" $ do
      containsBAB "aba" ["nope", "babx"] `shouldBe` True

    it "gives an error when the input is not an ABA" $ do
      evaluate (containsBAB "abc" ["bab"]) `shouldThrow` anyErrorCall
    it "gives an error when the input is shorter than 3 chars" $ do
      evaluate (containsBAB "ab" ["bab"]) `shouldThrow` anyErrorCall
    it "gives an error when the input is longer than 3 chars" $ do
      evaluate (containsBAB "abaa" ["bab"]) `shouldThrow` anyErrorCall

  describe "parseLine" $ do
    it "splits the first exsample IP" $ do
      parseLine "abba[mnop]qrst" `shouldBe` ["abba", "mnop", "qrst"]
