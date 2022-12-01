// extern crate partial_application;
use partial_application::partial;

pub fn part1(inp: String) {
    let lines = read_lines(&inp);

    let max_seat_id: u16 = lines
        .iter()
        .map(|line| convert_to_row_col(line))
        .map(|(row, col)| calc_seat_id(row, col))
        .fold(std::u16::MIN, |max, cur| max.max(cur));

    println!("max seat id: {}", max_seat_id);
}

pub fn part2(inp: String) {
    let lines = read_lines(&inp);

    let mut seat_ids: Vec<u16> = lines
        .iter()
        .map(|line| convert_to_row_col(line))
        .map(|(row, col)| calc_seat_id(row, col))
        .collect();

    seat_ids.sort();

    let mut last = seat_ids[0];
    for cur in &seat_ids[1..] {
        if cur - 1 != last {
            break;
        }
        last = *cur;
    }

    println!("your seat id: {}", last + 1);
}

fn read_lines(inp: &str) -> Vec<&str> {
    inp.split("\n")
        .filter(|line| line.len() > 0)
        .collect::<Vec<&str>>()
}

fn calc_seat_id(row: u8, col: u8) -> u16 {
    row as u16 * 8 + col as u16
}

fn convert_to_row_col(line: &str) -> (u8, u8) {
    let row_str = &line[..7];
    let col_str = &line[7..];

    let convert_row_str_to_number = partial!(convert_code_to_number => 'F', 'B', _);
    let convert_col_str_to_number = partial!(convert_code_to_number => 'L', 'R', _);

    let row = convert_row_str_to_number(row_str);
    let col = convert_col_str_to_number(col_str);

    (row, col)
}

fn convert_code_to_number(zero_char: char, one_char: char, code: &str) -> u8 {
    code.chars()
        .map(|c| {
            if c == zero_char {
                0
            } else if c == one_char {
                1
            } else {
                panic!("unknown char {}, expected {} or {}", c, zero_char, one_char)
            }
        })
        .fold(0, |acc, cur| acc * 2 + cur)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn converts_sample_1() {
        let input = "BFFFBBFRRR";
        let expected_row = 70;
        let expected_col = 7;
        assert_eq!(convert_to_row_col(input), (expected_row, expected_col));
    }

    #[test]
    pub fn converts_sample_2() {
        let input = "FFFBBBFRRR";
        let expected_row = 14;
        let expected_col = 7;
        assert_eq!(convert_to_row_col(input), (expected_row, expected_col));
    }

    #[test]
    pub fn converts_sample_3() {
        let input = "BBFFBBFRLL";
        let expected_row = 102;
        let expected_col = 4;
        assert_eq!(convert_to_row_col(input), (expected_row, expected_col));
    }

    #[test]
    pub fn converts_fffffff_to_0() {
        let input = "FFFFFFF";
        let expected = 0;
        assert_eq!(convert_code_to_number('F', 'B', input), expected);
    }

    #[test]
    pub fn converts_ffffffb_to_1() {
        let input = "FFFFFFB";
        let expected = 1;
        assert_eq!(convert_code_to_number('F', 'B', input), expected);
    }

    #[test]
    pub fn converts_bbbbbbb_to_127() {
        let input = "BBBBBBB";
        let expected = 127;
        assert_eq!(convert_code_to_number('F', 'B', input), expected);
    }
}
