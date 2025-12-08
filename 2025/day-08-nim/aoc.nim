import std/[math, algorithm, sequtils, sets, options, strutils, sugar]

type Node* = tuple[x, y, z: int]

func dist(a, b: Node): int =
  (a.x - b.x)^2 + (a.y - b.y)^2 + (a.z - b.z)^2

func findCompIndex(node: int, components: seq[HashSet[int]]): Option[int] =
  for k, comp in components:
    if node in comp: return some(k)
  return none(int)

proc solve*(nodes: seq[Node], target: int): (int, int) =
  let n = nodes.len

  var edges = newSeqOfCap[(int, int, int)](n * n div 2)
  for i in 0 ..< n:
    for j in i + 1 ..< n:
      edges.add((dist(nodes[i], nodes[j]), i, j))
  edges.sort()

  var components = newSeq[HashSet[int]]()
  var p1, p2 = 0

  for connections, (_, i, j) in edges:
    let i_comp = findCompIndex(i, components)
    let j_comp = findCompIndex(j, components)

    if i_comp.isNone and j_comp.isNone:
      components.add([i, j].toHashSet)

    elif i_comp.isSome and j_comp.isSome:
      let idxI = i_comp.get
      let idxJ = j_comp.get

      if idxI == idxJ:
        continue

      let (keep, remove) = if idxI < idxJ: (idxI, idxJ) else: (idxJ, idxI)
      for item in components[remove]:
        components[keep].incl(item)
      components.delete(remove)

    elif i_comp.isSome:
      components[i_comp.get].incl(j)

    elif j_comp.isSome:
      components[j_comp.get].incl(i)

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
  collect:
    for line in data.splitLines:
      if line.strip.len > 0:
        let parts = line.strip.split(',').map(parseInt)
        (parts[0], parts[1], parts[2])


when isMainModule:
  import std/os
  if paramCount() < 1:
    quit("Usage: ./aoc <filename>")

  let filename = paramStr(1)
  let isSample = filename.extractFilename == "sample.txt"
  let nodes = parseInput(readFile(filename))

  let (p1, p2) = solve(nodes, target = if isSample: 9 else: 999)
  echo p1
  echo p2
