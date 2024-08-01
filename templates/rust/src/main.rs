fn main() {
    let filename = std::env::args().nth(1).expect("Filename required");
    let is_sample = filename.to_owned().ends_with("sample.txt");
    let err_msg = format!("Failed to read {}", filename);
    let input = std::fs::read_to_string(filename).expect(&err_msg);

    let dimensions = parse(&input);

    assert_and_print(1, is_sample, 14545, 9876, part1(&dimensions));
    assert_and_print(2, is_sample, 4978, 21756, part2(&dimensions));
}

fn assert_and_print(
    part: usize,
    is_sample: bool,
    expected: usize,
    expected_sample: usize,
    actual: usize,
) {
    let suffix = if is_sample { " (sample)" } else { "" };
    let expected = if is_sample { expected_sample } else { expected };
    if expected == actual {
        println!("Part {}: {}{}", part, actual, suffix);
    } else {
        panic!(
            "Part {}: expected {}{}, got {}",
            part, expected, suffix, actual
        );
    }
}

fn parse(input: &str) -> Vec<(usize, usize, usize)> {
    input
        .lines()
        .map(|line| {
            let mut parts = line.split('x');
            let l = parts.next().unwrap().parse().unwrap();
            let w = parts.next().unwrap().parse().unwrap();
            let h = parts.next().unwrap().parse().unwrap();
            (l, w, h)
        })
        .collect()
}

fn surface_area(l: usize, w: usize, h: usize) -> usize {
    let lw = l * w;
    let wh = w * h;
    let hl = h * l;
    2 * (lw + wh + hl)
}

fn volume(l: usize, w: usize, h: usize) -> usize {
    l * w * h
}

fn part1(dimensions: &[(usize, usize, usize)]) -> usize {
    dimensions.iter().map(|&(l, w, h)| volume(l, w, h)).sum()
}

fn part2(dimensions: &[(usize, usize, usize)]) -> usize {
    dimensions
        .iter()
        .map(|&(l, w, h)| surface_area(l, w, h))
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn parse_works_for_a_single_line() {
        let input = "1x333x999999999";
        let result = parse(input);

        assert_eq!(result, vec![(1, 333, 999_999_999)]);
    }

    fn multi_line_input() -> &'static str {
        "1x2x3
10x20x30
111x222x333"
    }

    #[test]
    pub fn parse_works_for_multiple_lines() {
        let input = multi_line_input();
        let result = parse(input);

        assert_eq!(result, vec![(1, 2, 3), (10, 20, 30), (111, 222, 333)]);
    }

    #[test]
    pub fn part1_works() {
        let dimensions = vec![(1, 2, 3), (1, 1, 1)];
        let result = part1(&dimensions);

        assert_eq!(result, 6 + 1);
    }

    #[test]
    pub fn part2_works() {
        let dimensions = vec![(1, 2, 3), (1, 1, 1)];
        let result = part2(&dimensions);

        assert_eq!(result, (2 + 2 + 3 + 3 + 6 + 6) + 6);
    }
}
