import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Look-And-Say" $ do
    it "works for the first example" $ do
      lookAndSay "1" `shouldBe` "11"
    it "works for the second example" $ do
      lookAndSay "11" `shouldBe` "21"
    it "works for the third example" $ do
      lookAndSay "21" `shouldBe` "1211"
    it "works for the fourth example" $ do
      lookAndSay "1211" `shouldBe` "111221"
    it "works for the fifth example" $ do
      lookAndSay "111221" `shouldBe` "312211"

  describe "Find consecutive digits" $ do
    it "works for 1" $ do
      findConsecutiveDigits 1 '1' "" `shouldBe` (1, "")
    it "works for 11" $ do
      findConsecutiveDigits 1 '1' "1" `shouldBe` (2, "")
    it "works for 111" $ do
      findConsecutiveDigits 1 '1' "11" `shouldBe` (3, "")
    it "works for 1112" $ do
      findConsecutiveDigits 1 '1' "112" `shouldBe` (3, "2")
    it "works for 1112 starting with length 2" $ do
      findConsecutiveDigits 2 '1' "12" `shouldBe` (3, "2")
    it "works for 12345" $ do
      findConsecutiveDigits 1 '1' "2345" `shouldBe` (1, "2345")
