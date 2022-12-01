use regex::Regex;

pub fn part1(inp: String) {
    let lines = inp.split("\n").collect::<Vec<&str>>();
    let regex = Regex::new(r"^(\d+)-(\d+) (\w): (\w+)$").unwrap();

    let valid_passwords = lines
        .iter()
        .filter(|line| line.len() > 0)
        .map(|x| is_valid_1(x, &regex))
        .fold(0, |acc, cur| {
            acc + match cur {
                true => 1,
                false => 0,
            }
        });

    println!("{}", valid_passwords);
}

pub fn part2(inp: String) {
    let lines = inp.split("\n").collect::<Vec<&str>>();
    let regex = Regex::new(r"^(\d+)-(\d+) (\w): (\w+)$").unwrap();

    let valid_passwords = lines
        .iter()
        .filter(|line| line.len() > 0)
        .map(|x| is_valid_2(x, &regex))
        .fold(0, |acc, cur| {
            acc + match cur {
                true => 1,
                false => 0,
            }
        });

    println!("{}", valid_passwords);
}

// sample: 1-3 a: abaaabab
fn is_valid_1(line: &str, regex: &Regex) -> bool {
    for cap in regex.captures_iter(line) {
        let min = cap[1].parse::<usize>().unwrap();
        let max = cap[2].parse::<usize>().unwrap();
        let letter = &cap[3];
        let passwd = &cap[4];

        let letters_in_passwd = passwd.matches(letter).count();

        return min <= letters_in_passwd && letters_in_passwd <= max;
    }

    false
}

fn is_valid_2(line: &str, regex: &Regex) -> bool {
    for cap in regex.captures_iter(line) {
        let pos1 = cap[1].parse::<usize>().unwrap() - 1;
        let pos2 = cap[2].parse::<usize>().unwrap() - 1;
        let letter = cap[3].as_bytes()[0];
        let passwd = cap[4].as_bytes();

        let has_letter_at_pos = |pos| pos < passwd.len() && passwd[pos] == letter;

        return has_letter_at_pos(pos1) ^ has_letter_at_pos(pos2);
    }

    false
}
