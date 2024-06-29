import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  let input = [[1, 2, 3], [1, 1, 1]]

  describe "Part 1" $ do
    it "returns the sum of the volumes" $ do
      part1 input `shouldBe` (6 + 1)

  describe "Part 2" $ do
    it "returns the sum of the surface areas" $ do
      part2 input `shouldBe` ((2 + 2 + 3 + 3 + 6 + 6) + (6 * 1))

  describe "parseLines" $ do
    it "should parse the sample" $ do
      parseLines ["1x2x3", "987x10x1"] `shouldBe` [[1, 2, 3], [987, 10, 1]]
