fn main() {
    let filename = std::env::args().nth(1).expect("Filename required");
    let is_sample = filename.to_owned().ends_with("sample.txt");
    let err_msg = format!("Failed to read {}", filename);
    let input = std::fs::read_to_string(filename).expect(&err_msg);

    let door_id = input.trim();
    let pw_len = 8;
    let start_id = 0;

    assert_and_print(
        1,
        is_sample,
        "4543c154",
        "18f47a30",
        part1(door_id, start_id, pw_len),
    );
}

fn assert_and_print(
    part: usize,
    is_sample: bool,
    expected: &str,
    expected_sample: &str,
    actual: String,
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

fn part1(door_id: &str, start_id: usize, pw_len: usize) -> String {
    assert!(pw_len <= 8);
    let mut password = ['_'; 8];
    let mut pw_idx = 0;
    let mut id = start_id;

    loop {
        let input = format!("{}{}", door_id, id);
        let digest = md5::compute(input.as_bytes());
        let digest_hex = format!("{:x}", digest);

        if digest_hex.starts_with("00000") {
            password[pw_idx] = digest_hex.chars().nth(5).unwrap();
            pw_idx += 1;
            if pw_idx == pw_len {
                break;
            }
        }

        id += 1;
    }

    String::from_iter(password)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn part1_first_four_chars_are_found() {
        // to keep things fast:
        let start_id = 3200000;
        let pw_len = 4;
        assert_eq!(part1("abc", start_id, pw_len), "18f4____");
    }
}
