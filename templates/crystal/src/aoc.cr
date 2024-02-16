def process_line(line : String) : Array(Int32)
  line.split("x").map { |n| n.to_i }
end

def surface_area(numbers : Array(Int32)) : Int32
  l, w, h = numbers
  2 * (l * w + w * h + h * l)
end

def part1(dimensions : Array(Array(Int32))) : Int32
  dimensions.map { |dim| dim.product() }.sum
end

def part2(dimensions : Array(Array(Int32))) : Int32
  dimensions.map { |dim| surface_area(dim) }.sum
end

def main()
  dimensions = File
    .read("input.txt")
    .lines
    .map { |line| process_line(line) }

  puts part1(dimensions)
  puts part2(dimensions)
end
