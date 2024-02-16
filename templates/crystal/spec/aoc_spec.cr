require "spec"
require "../src/aoc"

describe "Process line" do
  it "splits a line by 'x'" do
    process_line("1x2x3").should eq [1, 2, 3]
  end
end

describe "Part 1" do
  it "returns the expected result" do
    part1([[1, 2, 3], [1, 1, 1]]).should eq 6 + 1
  end
end

describe "Part 2" do
  it "returns the expected result" do
    part2([[1, 2, 3], [1, 1, 1]]).should eq (2 + 2 + 3 + 3 + 6 + 6) + (6 * 1)
  end
end
