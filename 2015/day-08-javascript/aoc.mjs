import fs from "node:fs"

export const variants = (line) => [
  line,
  eval(line),
  `"${line.replaceAll("\\", "\\\\").replaceAll('"', '\\"')}"`,
]

export const part1And2 = (lines) =>
  lines
    .map(variants)
    .map((strings) => strings.map((s) => s.length))
    .reduce((acc, [l, l1, l2]) => [acc[0] + l - l1, acc[1] + l2 - l], [0, 0])

export async function main() {
  const lines = fs
    .readFileSync(process.argv[2], "utf-8")
    .split("\n")
    .filter(Boolean)

  console.log(part1And2(lines).join("\n"))
}
