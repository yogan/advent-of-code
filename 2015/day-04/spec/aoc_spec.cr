require "spec"
require "../src/aoc"

describe "Part 1" do
  it "works for the first example" do
    brute_force("abcdef", "00000").should eq 609043
  end

  it "works for the second example" do
    brute_force("pqrstuv", "00000").should eq 1048970
  end
end
