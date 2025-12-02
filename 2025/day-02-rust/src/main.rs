fn parse(input: &str) -> Vec<(usize, usize)> {
    input
        .trim_end()
        .split(',')
        .map(|s| {
            let (start, end) = s.split_once('-').unwrap();
            (start.parse().unwrap(), end.parse().unwrap())
        })
        .collect()
}

fn part1(ranges: &[(usize, usize)]) -> usize {
    sum_invalid_ids(ranges, 1)
}

fn sum_invalid_ids(ranges: &[(usize, usize)], part: u8) -> usize {
    let mut total = 0;

    for &(start, end) in ranges {
        for id in start..=end {
            if part == 1 && repeated_once(id) {
                total += id;
            }
        }
    }

    total
}

fn repeated_once(id: usize) -> bool {
    let s = id.to_string();
    let (left, right) = s.split_at(s.len() / 2);

    left == right
}

fn main() {
    let filename = std::env::args().nth(1).expect("Filename required");
    let is_sample = filename.to_owned().ends_with("sample.txt");
    let err_msg = format!("Failed to read {}", filename);
    let input = std::fs::read_to_string(filename).expect(&err_msg);

    let ranges = parse(&input);
    let mut failures = 0;

    failures += check(
        1,
        is_sample,
        part1(&ranges),
        Some(41294979841),
        Some(1227775554),
    );

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

    const INPUT: &str = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

    const SAMPLE: &[(usize, usize)] = &[
        (11, 22),
        (95, 115),
        (998, 1012),
        (1188511880, 1188511890),
        (222220, 222224),
        (1698522, 1698528),
        (446443, 446449),
        (38593856, 38593862),
        (565653, 565659),
        (824824821, 824824827),
        (2121212118, 2121212124),
    ];

    #[test]
    fn parse_sample() {
        assert_eq!(parse(INPUT), SAMPLE);
    }

    #[test]
    fn part1_sample() {
        assert_eq!(part1(SAMPLE), 1227775554);
    }

    #[test]
    fn test_repeated_once() {
        assert!(repeated_once(11));
        assert!(repeated_once(1010));
        assert!(repeated_once(1188511885));

        assert!(!repeated_once(10));
        assert!(!repeated_once(101));
        assert!(!repeated_once(1188511858));
    }
}
