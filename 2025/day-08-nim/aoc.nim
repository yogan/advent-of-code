import std/[math, algorithm, sequtils, sets, options, strutils]

type Node* = tuple[x, y, z: int]

func dist(a, b: Node): int =
  (a.x - b.x)^2 + (a.y - b.y)^2 + (a.z - b.z)^2

func findCompIndex(node: int, components: seq[HashSet[int]]): Option[int] =
  for k, comp in components:
    if node in comp: return some(k)
  return none(int)

proc solve*(nodes: seq[Node], isSample: bool): (int, int) =
  let n = nodes.len

  var edges = newSeqOfCap[(int, int, int)](n * n div 2)
  for i in 0 ..< n:
    for j in i + 1 ..< n:
      edges.add((dist(nodes[i], nodes[j]), i, j))
  edges.sort()

  var components: seq[HashSet[int]] = @[]
  var p1, p2 = 0

  let target = if isSample: 9 else: 999

  for connections, (d, i, j) in edges:
    let i_opt = findCompIndex(i, components)
    let j_opt = findCompIndex(j, components)

    if i_opt.isNone and j_opt.isNone:
      var newComp = initHashSet[int]()
      newComp.incl(i); newComp.incl(j)
      components.add(newComp)

    elif i_opt.isSome and j_opt.isSome:
      let idxI = i_opt.get
      let idxJ = j_opt.get

      if idxI == idxJ:
        continue

      let (keep, remove) = if idxI < idxJ: (idxI, idxJ) else: (idxJ, idxI)

      for item in components[remove]:
        components[keep].incl(item)
      components.delete(remove)

    elif i_opt.isSome:
      components[i_opt.get].incl(j)

    elif j_opt.isSome:
      components[j_opt.get].incl(i)

    if connections == target:
      var sizes = components.mapIt(it.len)
      sizes.sort()
      if sizes.len > 0:
        let subset = if sizes.len >= 3: sizes[^3..^1] else: sizes
        p1 = subset.foldl(a * b)

    if components.len > 0 and components[0].len == n:
      p2 = nodes[i].x * nodes[j].x
      break

  return (p1, p2)


proc parseInput*(data: string): seq[Node] =
  result = @[]
  for line in data.splitLines:
    if line.strip.len == 0: continue
    let parts = line.strip.split(',').map(parseInt)
    result.add((parts[0], parts[1], parts[2]))


when isMainModule:
  import std/os
  if paramCount() < 1:
    quit("Usage: ./aoc <filename>")

  let filename = paramStr(1)
  let isSample = filename.extractFilename == "sample.txt"
  let nodes = parseInput(readFile(filename))

  let (p1, p2) = solve(nodes, isSample)
  echo p1
  echo p2
