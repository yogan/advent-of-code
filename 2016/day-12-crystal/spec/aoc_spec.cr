require "spec"
require "../src/aoc"

describe "run" do
  it "works for inc commands" do
    run([["inc", "b"]]).should eq 0
    run([["inc", "a"]]).should eq 1
    run([["inc", "a"], ["inc", "b"]]).should eq 1
    run([["inc", "b"], ["inc", "a"]]).should eq 1
    run([["inc", "a"], ["inc", "b"], ["inc", "a"]]).should eq 2
  end

  it "works for dec commands" do
    run([["dec", "b"]]).should eq 0
    run([["dec", "a"]]).should eq -1
    run([["dec", "a"], ["dec", "b"]]).should eq -1
    run([["dec", "b"], ["dec", "a"]]).should eq -1
    run([["dec", "a"], ["dec", "b"], ["dec", "a"]]).should eq -2
  end

  it "works for cpy commands" do
    run([["cpy", "23", "a"]]).should eq 23
    run([["cpy", "-1", "a"]]).should eq -1

    run([
      ["cpy", "9", "b"],
      ["cpy", "b", "a"],
    ]).should eq 9

    run([
      ["cpy", "-7", "d"],
      ["cpy", "d", "c"],
      ["cpy", "c", "b"],
      ["cpy", "b", "a"],
    ]).should eq -7
  end

  it "works for jnz commands" do
    run([
      ["inc", "a"],
      ["jnz", "a", "2"],
      ["inc", "a"], # skipped
      ["inc", "a"],
    ]).should eq 2
  end
end
