import Lib (hasABBA, isValid, parseLine, part1, splitOddEven)
import Test.Hspec

main :: IO ()
main = hspec $ do
  let sample =
        [ ["abba", "mnop", "qrst"],
          ["abcd", "bddb", "xyyx"],
          ["aaaa", "qwer", "tyui"],
          ["ioxxoj", "asdfgh", "zxcvbn"]
        ]

  describe "Part 1" $ do
    it "works for the example" $ do
      part1 sample `shouldBe` 2

  describe "isValid" $ do
    it "is true for the first sample" $ do
      isValid (head sample) `shouldBe` True
    it "is false for the second sample" $ do
      isValid (sample !! 1) `shouldBe` False
    it "is false for the third sample" $ do
      isValid (sample !! 2) `shouldBe` False
    it "is true for the fourth sample" $ do
      isValid (sample !! 3) `shouldBe` True
    it "is false for a combination of sample 1 & 2 (bddb in brackets)" $ do
      isValid ["abba", "mnop", "qrstabcd", "bddb", "xyyx"] `shouldBe` False
    it "is true for a combination of sample 1 & 3" $ do
      isValid ["abba", "mnop", "qrstaaaa", "qwer", "tyui"] `shouldBe` True

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

  describe "hasABBC" $ do
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

  describe "parseLine" $ do
    it "splits the first exsample IP" $ do
      parseLine "abba[mnop]qrst" `shouldBe` ["abba", "mnop", "qrst"]
