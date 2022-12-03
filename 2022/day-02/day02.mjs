import { open } from "node:fs/promises"

const ROCK_PLAYER = "X"
const PAPER_PLAYER = "Y"
const SCISSORS_PLAYER = "Z"

const SHALL_LOSE = "X"
const SHALL_DRAW = "Y"
const SHALL_WIN = "Z"

const ROCK_OPPONENT = "A"
const PAPER_OPPONENT = "B"
const SCISSORS_OPPONENT = "C"

const SCORE_ROCK = 1
const SCORE_PAPER = 2
const SCORE_SCISSORS = 3

const LOSS = 0
const DRAW = 3
const WIN = 6

async function parseInput(filename) {
  const matches = []

  const file = await open(filename)
  for await (const line of file.readLines()) {
    matches.push(line.split(" "))
  }

  return matches
}

export async function part1(filename) {
  return (await parseInput(filename))
    .map(([opponent, player]) => score(opponent, player))
    .reduce((sum, x) => sum + x, 0)
}

export async function part2(filename) {
  return (await parseInput(filename))
    .map(([opponent, result]) => [opponent, findPlayerShape(opponent, result)])
    .map(([opponent, player]) => score(opponent, player))
    .reduce((sum, x) => sum + x, 0)
}

const findPlayerShape = (opponent, result) => {
  switch (opponent) {
    case ROCK_OPPONENT:
      switch (result) {
        case SHALL_DRAW:
          return ROCK_PLAYER
        case SHALL_WIN:
          return PAPER_PLAYER
        case SHALL_LOSE:
          return SCISSORS_PLAYER
        default:
          throw Error(`unexpected result "${result}"`)
      }
    case PAPER_OPPONENT:
      switch (result) {
        case SHALL_DRAW:
          return PAPER_PLAYER
        case SHALL_WIN:
          return SCISSORS_PLAYER
        case SHALL_LOSE:
          return ROCK_PLAYER
        default:
          throw Error(`unexpected result "${result}"`)
      }
    case SCISSORS_OPPONENT:
      switch (result) {
        case SHALL_DRAW:
          return SCISSORS_PLAYER
        case SHALL_WIN:
          return ROCK_PLAYER
        case SHALL_LOSE:
          return PAPER_PLAYER
        default:
          throw Error(`unexpected result "${result}"`)
      }
    default:
      throw Error(`unexpected shape "${opponent}"`)
  }
}

const score = (opponent, player) =>
  shape_score(player) + win_score(opponent, player)

const shape_score = (shape) => {
  switch (shape) {
    case ROCK_PLAYER:
      return SCORE_ROCK
    case PAPER_PLAYER:
      return SCORE_PAPER
    case SCISSORS_PLAYER:
      return SCORE_SCISSORS
    default:
      throw Error(`unexpected shape "${shape}"`)
  }
}

const win_score = (opponent, player) => {
  switch (player) {
    case ROCK_PLAYER:
      switch (opponent) {
        case ROCK_OPPONENT:
          return DRAW
        case PAPER_OPPONENT:
          return LOSS
        case SCISSORS_OPPONENT:
          return WIN
        default:
          throw Error(`unexpected shape "${opponent}"`)
      }
    case PAPER_PLAYER:
      switch (opponent) {
        case ROCK_OPPONENT:
          return WIN
        case PAPER_OPPONENT:
          return DRAW
        case SCISSORS_OPPONENT:
          return LOSS
        default:
          throw Error(`unexpected shape "${opponent}"`)
      }
    case SCISSORS_PLAYER:
      switch (opponent) {
        case ROCK_OPPONENT:
          return LOSS
        case PAPER_OPPONENT:
          return WIN
        case SCISSORS_OPPONENT:
          return DRAW
        default:
          throw Error(`unexpected shape "${opponent}"`)
      }
    default:
      throw Error(`unexpected shape "${shape}"`)
  }
}

const scorePart1 = await part1("./day02.in")
const scorePart2 = await part2("./day02.in")

console.log(`Day 02 part 1: ${scorePart1}`)
console.log(`Day 02 part 2: ${scorePart2}`)
