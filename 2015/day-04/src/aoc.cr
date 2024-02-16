require "digest/md5"

def part1(input : String) : Int
  (0..).each do |i|
    return i if Digest::MD5.hexdigest(input + i.to_s)[..4] == "00000"
  end
end

def main()
  input = File.read("input.txt").lines[0]
  puts part1(input)
end
