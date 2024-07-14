def main
  filename = ARGV.size > 0 ? ARGV[0] : "input.txt"

  ops = File
    .read(filename)
    .lines
    .map { |line| line.split(" ") }

  puts part1(ops)
end

def part1(ops : Array(Array(String))) : Int32
  a, b, c, d, pc = 0, 0, 0, 0, 0

  while pc < ops.size
    op = ops[pc]
    pc += 1

    if op[0] == "inc"
      if op[1] == "a"
        a += 1
      elsif op[1] == "b"
        b += 1
      elsif op[1] == "c"
        c += 1
      elsif op[1] == "d"
        d += 1
      end
    elsif op[0] == "dec"
      if op[1] == "a"
        a -= 1
      elsif op[1] == "b"
        b -= 1
      elsif op[1] == "c"
        c -= 1
      elsif op[1] == "d"
        d -= 1
      end
    elsif op[0] == "cpy"
      val =
        if op[1] == "a"
          a
        elsif op[1] == "b"
          b
        elsif op[1] == "c"
          c
        elsif op[1] == "d"
          d
        else
          op[1].to_i
        end
      if op[2] == "a"
        a = val
      elsif op[2] == "b"
        b = val
      elsif op[2] == "c"
        c = val
      elsif op[2] == "d"
        d = val
      end
    elsif op[0] == "jnz"
      val =
        if op[1] == "a"
          a
        elsif op[1] == "b"
          b
        elsif op[1] == "c"
          c
        elsif op[1] == "d"
          d
        else
          op[1].to_i
        end
      if val != 0
        pc += op[2].to_i - 1 # -1 due to pc += 1 above
      end
    end
  end

  a
end
