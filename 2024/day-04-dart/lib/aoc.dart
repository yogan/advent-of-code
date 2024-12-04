List<List<String>> parseLines(List<String> lines) {
  return lines.map((line) => line.split('')).toList();
}

int part1(List<List<String>> letters) {
  var count = 0;

  for (var i = 0; i < letters.length; i++) {
    for (var j = 0; j < letters[i].length; j++) {
      if (letters[i][j] != 'X') {
        continue;
      }

      // north
      if (i >= 3 &&
          letters[i - 1][j] == 'M' &&
          letters[i - 2][j] == 'A' &&
          letters[i - 3][j] == 'S') {
        count++;
      }

      // north east
      if (i >= 3 &&
          j <= letters[i].length - 4 &&
          letters[i - 1][j + 1] == 'M' &&
          letters[i - 2][j + 2] == 'A' &&
          letters[i - 3][j + 3] == 'S') {
        count++;
      }

      // east
      if (j <= letters[i].length - 4 &&
          letters[i][j + 1] == 'M' &&
          letters[i][j + 2] == 'A' &&
          letters[i][j + 3] == 'S') {
        count++;
      }

      // south east
      if (i <= letters.length - 4 &&
          j <= letters[i].length - 4 &&
          letters[i + 1][j + 1] == 'M' &&
          letters[i + 2][j + 2] == 'A' &&
          letters[i + 3][j + 3] == 'S') {
        count++;
      }

      // south
      if (i <= letters.length - 4 &&
          letters[i + 1][j] == 'M' &&
          letters[i + 2][j] == 'A' &&
          letters[i + 3][j] == 'S') {
        count++;
      }

      // south west
      if (i <= letters.length - 4 &&
          j >= 3 &&
          letters[i + 1][j - 1] == 'M' &&
          letters[i + 2][j - 2] == 'A' &&
          letters[i + 3][j - 3] == 'S') {
        count++;
      }

      // west
      if (j >= 3 &&
          letters[i][j - 1] == 'M' &&
          letters[i][j - 2] == 'A' &&
          letters[i][j - 3] == 'S') {
        count++;
      }

      // north west
      if (i >= 3 &&
          j >= 3 &&
          letters[i - 1][j - 1] == 'M' &&
          letters[i - 2][j - 2] == 'A' &&
          letters[i - 3][j - 3] == 'S') {
        count++;
      }
    }
  }

  return count;
}

int part2(List<List<String>> letters) {
  var count = 0;

  for (var i = 1; i < letters.length - 1; i++) {
    for (var j = 1; j < letters[i].length - 1; j++) {
      if (letters[i][j] != 'A') {
        continue;
      }

      // M.M
      // .A.
      // S.S
      if (letters[i - 1][j - 1] == 'M' &&
          letters[i - 1][j + 1] == 'M' &&
          letters[i + 1][j - 1] == 'S' &&
          letters[i + 1][j + 1] == 'S') {
        count++;
        continue;
      }

      // M.S
      // .A.
      // M.S
      if (letters[i - 1][j - 1] == 'M' &&
          letters[i + 1][j - 1] == 'M' &&
          letters[i - 1][j + 1] == 'S' &&
          letters[i + 1][j + 1] == 'S') {
        count++;
        continue;
      }

      // S.M
      // .A.
      // S.M
      if (letters[i - 1][j + 1] == 'M' &&
          letters[i + 1][j + 1] == 'M' &&
          letters[i - 1][j - 1] == 'S' &&
          letters[i + 1][j - 1] == 'S') {
        count++;
        continue;
      }

      // S.S
      // .A.
      // M.M
      if (letters[i + 1][j - 1] == 'M' &&
          letters[i + 1][j + 1] == 'M' &&
          letters[i - 1][j - 1] == 'S' &&
          letters[i - 1][j + 1] == 'S') {
        count++;
        continue;
      }
    }
  }

  return count;
}
