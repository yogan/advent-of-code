#!/usr/bin/env ruby
require 'test/unit'

InputFile = Struct.new(:stacks, :instructions)
Instruction = Struct.new(:quantity, :source, :destination)

def parse_file(filename)
  stacks = []
  instructions = []
  read_stacks = true

  File.readlines(filename).map(&:chomp).each do |line|
    if line == ''
      read_stacks = false
      next
    end
    stacks << line if read_stacks
    instructions << line unless read_stacks
  end

  InputFile.new(stacks, instructions)
end

def initialize_stacks(lines)
  number_of_stacks = lines[-1].strip.split(/ +/).size
  stacks = Array.new(number_of_stacks + 1) { [] }

  lines[0..-2].reverse.each do |line|
    entries = parse_stack_line(line, number_of_stacks)
    entries.each_with_index do |entry, index|
      stacks[index + 1] << entry if entry != ' '
    end
  end

  stacks
end

def parse_stack_line(line, number_of_stacks)
  # input:  '    [N] [C]    ', 4 (leading/trailing spaces make everything save)
  # output: [' ','N','C',' ']
  result = []
  number_of_stacks.times.map do |i|
    result << line[4 * i + 1]
  end
  result
end

def initialize_instructions(lines)
  lines.map { |line| parse_instruction_line(line) }
end

def parse_instruction_line(line)
  numbers = line.scan(/\d+/).map(&:to_i)
  Instruction.new(numbers[0], numbers[1], numbers[2])
end

def apply_instructions(stacks, instructions)
  instructions.each do |instruction|
    instruction.quantity.times do
      stacks[instruction.destination].push(stacks[instruction.source].pop)
    end
  end
end

def get_stack_tops(stacks)
  stacks[1..].map { |s| s.last }.join
end

def part1(filename: 'day05.in')
  parsed_input = parse_file(filename)
  stacks       = initialize_stacks(parsed_input.stacks)
  instructions = initialize_instructions(parsed_input.instructions)

  apply_instructions(stacks, instructions)

  get_stack_tops(stacks)
end

print "Part 1: #{part1}\n"
print "\n"

# Tests
class TestDay05 < Test::Unit::TestCase
  @@filename = 'day05.sample'

  def test_parse_file_reads_stack_lines
    stacks = parse_file(@@filename).stacks

    assert_equal 4, stacks.length
    3.times { |i| assert_match(/^[ \[\]A-Z]+$/, stacks[i]) }
    assert_match(/^[ \d]+$/, stacks[3])
  end

  def test_parse_file_reads_instruction_lines
    instructions = parse_file(@@filename).instructions

    assert_equal 4, instructions.length
    4.times { |i| assert_match(/^move \d+ from \d to \d$/, instructions[i]) }
  end

  def test_initialize_stacks
    lines = [
      '    [D]    ',
      '[N] [C]    ',
      '[Z] [M] [P]',
      ' 1   2   3 '
    ]

    assert_equal [[], %w[Z N], %w[M C D], ['P']],
                 initialize_stacks(lines)
  end

  def test_initialize_instructions
    lines = [
      'move 1 from 2 to 3',
      'move 1001 from 9 to 5'
    ]

    assert_equal [
      Instruction.new(1, 2, 3),
      Instruction.new(1001, 9, 5)
    ], initialize_instructions(lines)
  end

  def test_apply_instructions_single
    instructions    = [Instruction.new(1, 2, 1)]
    stacks          = [[], %w[V W], %w[X Y A]]
    expected_stacks = [[], %w[V W A], %w[X Y]]

    apply_instructions(stacks, instructions)

    assert_equal expected_stacks, stacks
  end

  def test_apply_instructions_multiple
    instructions = [
      Instruction.new(1, 3, 2),
      Instruction.new(2, 2, 1)
    ]
    #                         1      2        3
    stacks          = [[], %w[K], %w[V W], %w[X Y A]]
    # after step 1:   [[], %w[K], %w[V W A], %w[X Y]]
    expected_stacks = [[], %w[K A W], %w[V], %w[X Y]]

    apply_instructions(stacks, instructions)

    assert_equal expected_stacks, stacks
  end

  def test_get_stack_tops
    stacks = [[], %w[K A W], %w[V], %w[X Y]]

    assert_equal 'WVY', get_stack_tops(stacks)
  end

  def test_part1
    assert_equal 'CMZ', part1(filename: @@filename)
  end
end
