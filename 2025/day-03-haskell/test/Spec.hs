import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "largest" $ do
    it "finds the largest sequence of 2 digits" $ do
      largest "987654321111111" 2 `shouldBe` "98"
      largest "811111111111119" 2 `shouldBe` "89"
      largest "234234234234278" 2 `shouldBe` "78"
      largest "818181911112111" 2 `shouldBe` "92"

    it "finds the largest sequence of 12 digits" $ do
      largest "987654321111111" 12 `shouldBe` "987654321111"
      largest "811111111111119" 12 `shouldBe` "811111111119"
      largest "234234234234278" 12 `shouldBe` "434234234278"
      largest "818181911112111" 12 `shouldBe` "888911112111"
