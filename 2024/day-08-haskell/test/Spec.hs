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

  describe "part1" $ it "should find the number of resonances" $ do
    part1 (dimensions sample) (findAntennas sample) `shouldBe` 14

  describe "part2" $ it "should find the number of harmonics" $ do
    part2 (dimensions sample) (findAntennas sample) `shouldBe` 34

  describe "dimensions" $ it "should return rows and cols of the sample" $ do
    dimensions sample `shouldBe` (12, 12)

  describe "findAntennas" $ it "should parse the sample" $ do
    findAntennas sample
      `shouldBe` [ Set.fromList [(1, 8), (2, 5), (3, 7), (4, 4)],
                   Set.fromList [(5, 6), (8, 8), (9, 9)]
                 ]

  let dims = (10, 10)

  describe "antinodes" $ it "should find all resonance positions" $ do
    antinodes dims [(3, 4), (5, 5)] resonances
      `shouldBe` Set.fromList [(1, 3), (7, 6)]
    antinodes dims [(3, 4), (4, 8), (5, 5)] resonances
      `shouldBe` Set.fromList [(1, 3), (2, 0), (6, 2), (7, 6)]

  describe "antinodes" $ it "should find all harmonic positions" $ do
    antinodes dims [(0, 0), (1, 3), (2, 1)] harmonics
      `shouldBe` Set.fromList [(0, 0), (1, 3), (2, 1), (0, 5), (2, 6), (3, 9), (4, 2), (6, 3), (8, 4)]

  describe "resonances" $ it "should find resonance positions" $ do
    resonances dims (3, 4) [] `shouldBe` Set.fromList []
    resonances dims (3, 4) [(5, 5)] `shouldBe` Set.fromList [(1, 3), (7, 6)]

  describe "harmonics" $ it "should find harmonic positions" $ do
    harmonics dims (0, 0) [(1, 3), (2, 1)]
      `shouldBe` Set.fromList [(0, 0), (2, 6), (3, 9), (4, 2), (6, 3), (8, 4)]
    harmonics dims (1, 3) [(2, 1)] `shouldBe` Set.fromList [(0, 5), (1, 3)]
