require "spec"
require "../src/aoc"

describe "Part 1" do
  it "works for the first example" do
    part1("abcdef").should eq 609043
  end

  it "works for the second example" do
    part1("pqrstuv").should eq 1048970
  end
end
