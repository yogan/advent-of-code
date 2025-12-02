fn part1(dimensions: &[(usize, usize, usize)]) -> usize {
    dimensions.iter().map(|&(l, w, h)| l * w * h).sum()
}

fn parse(input: &str) -> Vec<(usize, usize, usize)> {
    input
        .lines()
        .map(|line| {
            let [l, w, h]: [usize; 3] = line
                .split('x')
                .map(|s| s.parse().unwrap())
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            (l, w, h)
        })
        .collect()
}

fn main() {
    let filename = std::env::args().nth(1).expect("Filename required");
    let is_sample = filename.to_owned().ends_with("sample.txt");
    let err_msg = format!("Failed to read {}", filename);
    let input = std::fs::read_to_string(filename).expect(&err_msg);

    let mut failures = 0;
    let dimensions = parse(&input);

    failures += check(1, is_sample, part1(&dimensions), None, Some(9876));

    if failures > 0 {
        std::process::exit(failures);
    }
}
fn check<T>(
    part: usize,
    is_sample: bool,
    actual: T,
    expected: Option<T>,
    expected_sample: Option<T>,
) -> i32
where
    T: std::fmt::Display + PartialEq,
{
    let target = if is_sample { expected_sample } else { expected };
    let suffix = if is_sample { " (sample)" } else { "" };

    match target {
        None => {
            println!("🤔 Part {}{}: {}", part, suffix, actual);
            0
        }
        Some(e) if actual == e => {
            println!("✅ Part {}{}: {}", part, suffix, actual);
            0
        }
        Some(e) => {
            println!("❌ Part {}{}: {} (expected {})", part, suffix, actual, e);
            1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_INPUT: &str = "1x2x3
987x10x1";

    const SAMPLE_DATA: &[(usize, usize, usize)] = &[(1, 2, 3), (987, 10, 1)];

    #[test]
    fn parse_sample() {
        assert_eq!(parse(SAMPLE_INPUT), SAMPLE_DATA);
    }

    #[test]
    fn part1_sample() {
        assert_eq!(part1(SAMPLE_DATA), 6 + 9870);
    }

    #[test]
    fn parse_works_for_single_line() {
        assert_eq!(parse("1x333x999999999"), vec![(1, 333, 999_999_999)]);
    }
}
