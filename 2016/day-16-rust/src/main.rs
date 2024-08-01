fn main() {
    let filename = std::env::args().nth(1).expect("Filename required");
    let err_msg = format!("Failed to read {}", filename);
    let input = std::fs::read_to_string(filename).expect(&err_msg);
    let input = input.trim();

    assert_and_print(1, "10010101010011101", &solve(input, 272));
    assert_and_print(2, "01100111101101111", &solve(input, 35651584));
}

fn assert_and_print(part: usize, expected: &str, actual: &str) {
    if expected == actual {
        println!("Part {}: {}", part, actual);
    } else {
        panic!("Part {}: expected »{}«, got »{}«", part, expected, actual);
    }
}

fn solve(input: &str, len: usize) -> String {
    checksum(&fill(input, len))
}

fn dragon(a: &str) -> String {
    let b: String = a
        .chars()
        .rev()
        .map(|c| if c == '0' { '1' } else { '0' })
        .collect();

    format!("{}0{}", a, b)
}

fn fill(data: &str, len: usize) -> String {
    if data.len() >= len {
        data[..len].to_string()
    } else {
        fill(&dragon(data), len)
    }
}

fn checksum(data: &str) -> String {
    if data.len() % 2 == 1 {
        data.to_string()
    } else {
        checksum(
            &data
                .chars()
                .collect::<Vec<_>>()
                .chunks(2)
                .map(|pair| if pair[0] == pair[1] { '1' } else { '0' })
                .collect::<String>(),
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn solve_works_for_sample() {
        assert_eq!(solve("10000", 20), "01100");
    }

    #[test]
    pub fn dragon_works_for_samples() {
        assert_eq!(dragon("1"), "100");
        assert_eq!(dragon("0"), "001");
        assert_eq!(dragon("11111"), "11111000000");
        assert_eq!(dragon("111100001010"), "1111000010100101011110000");
    }

    #[test]
    pub fn fill_works() {
        assert_eq!(fill("101", 3), "101");
        assert_eq!(fill("1010", 3), "101");
        assert_eq!(fill("1", 3), "100");
        assert_eq!(fill("10000", 20), "10000011110010000111");
    }

    #[test]
    pub fn checksum_works() {
        assert_eq!(checksum("00"), "1");
        assert_eq!(checksum("11"), "1");
        assert_eq!(checksum("01"), "0");
        assert_eq!(checksum("10"), "0");
        assert_eq!(checksum("110010110100"), "100");
        assert_eq!(checksum("10000011110010000111"), "01100");
    }
}
