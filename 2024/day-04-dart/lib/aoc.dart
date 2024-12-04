List<List<String>> parseLines(List<String> lines) {
  return lines.map((line) => line.split('')).toList();
}

int part1(List<List<String>> letters) {
  const offsets = [
    [-1, -1],
    [-1, 0],
    [-1, 1],
    [0, -1],
    [0, 1],
    [1, -1],
    [1, 0],
    [1, 1],
  ];

  var iMax = letters.length;
  var jMax = letters[0].length;

  var count = 0;

  for (var i = 0; i < iMax; i++) {
    for (var j = 0; j < jMax; j++) {
      if (letters[i][j] != 'X') continue;

      for (var offset in offsets) {
        if (i + 3 * offset[0] < 0 ||
            i + 3 * offset[0] >= jMax ||
            j + 3 * offset[1] < 0 ||
            j + 3 * offset[1] >= iMax) continue;

        var word = letters[i][j] +
            letters[i + 1 * offset[0]][j + 1 * offset[1]] +
            letters[i + 2 * offset[0]][j + 2 * offset[1]] +
            letters[i + 3 * offset[0]][j + 3 * offset[1]];

        if (word == 'XMAS') count++;
      }
    }
  }

  return count;
}

int part2(List<List<String>> letters) {
  // Checking corners clockwise like this:
  // 0 . 1      M . M      M . S      S . M      S . S
  // . A .  ->  . A .  or  . A .  or  . A .  or  . A .
  // 3 . 2      S . S      M . S      S . M      M . M
  const validCorners = ["MMSS", "MSSM", "SMMS", "SSMM"];

  var count = 0;

  for (var i = 1; i < letters.length - 1; i++) {
    for (var j = 1; j < letters[i].length - 1; j++) {
      if (letters[i][j] != 'A') continue;

      var corners = letters[i - 1][j - 1] +
          letters[i - 1][j + 1] +
          letters[i + 1][j + 1] +
          letters[i + 1][j - 1];

      if (validCorners.contains(corners)) count++;
    }
  }

  return count;
}
