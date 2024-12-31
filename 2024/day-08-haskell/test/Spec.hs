import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  let sample =
        [ "............",
          "........0...",
          ".....0......",
          ".......0....",
          "....0.......",
          "......A.....",
          "............",
          "............",
          "........A...",
          ".........A..",
          "............",
          "............"
        ]

  describe "findAntennas" $ it "should parse the sample" $ do
    findAntennas sample
      `shouldBe` Map.fromList
        [ ('0', Set.fromList [(1, 8), (2, 5), (3, 7), (4, 4)]),
          ('A', Set.fromList [(5, 6), (8, 8), (9, 9)])
        ]

  describe "dimensions" $ it "should return rows and cols of the sample" $ do
    dimensions sample `shouldBe` (12, 12)

  describe "resonances" $ it "should find resonance positions" $ do
    resonances (3, 4) [] (10, 10) `shouldBe` Set.fromList []
    resonances (3, 4) [(5, 5)] (10, 10) `shouldBe` Set.fromList [(1, 3), (7, 6)]

  describe "allResonances" $ it "should find all resonance positions" $ do
    allResonances [(3, 4), (5, 5)] (10, 10)
      `shouldBe` Set.fromList [(1, 3), (7, 6)]

    allResonances [(3, 4), (4, 8), (5, 5)] (10, 10)
      `shouldBe` Set.fromList [(1, 3), (2, 0), (6, 2), (7, 6)]

  describe "part1" $ it "should find the number of resonances" $ do
    let antennas = findAntennas sample
    let dims = dimensions sample
    part1 antennas dims `shouldBe` 14
