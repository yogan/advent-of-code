require "spec"
require "../src/aoc"

describe "Part 1" do
  it "works for inc commands" do
    part1([["inc", "b"]]).should eq 0
    part1([["inc", "a"]]).should eq 1
    part1([["inc", "a"], ["inc", "b"]]).should eq 1
    part1([["inc", "b"], ["inc", "a"]]).should eq 1
    part1([["inc", "a"], ["inc", "b"], ["inc", "a"]]).should eq 2
  end

  it "works for dec commands" do
    part1([["dec", "b"]]).should eq 0
    part1([["dec", "a"]]).should eq -1
    part1([["dec", "a"], ["dec", "b"]]).should eq -1
    part1([["dec", "b"], ["dec", "a"]]).should eq -1
    part1([["dec", "a"], ["dec", "b"], ["dec", "a"]]).should eq -2
  end

  it "works for cpy commands" do
    part1([["cpy", "23", "a"]]).should eq 23
    part1([["cpy", "-1", "a"]]).should eq -1

    part1([
      ["cpy", "9", "b"],
      ["cpy", "b", "a"],
    ]).should eq 9

    part1([
      ["cpy", "-7", "d"],
      ["cpy", "d", "c"],
      ["cpy", "c", "b"],
      ["cpy", "b", "a"],
    ]).should eq -7
  end

  it "works for jnz commands" do
    part1([
      ["inc", "a"],
      ["jnz", "a", "2"],
      ["inc", "a"], # skipped
      ["inc", "a"],
    ]).should eq 2
  end
end
