use regex::Regex;
use std::collections::HashSet;
use std::iter::FromIterator;

// part 1

pub fn part1(inp: String) {
    let passports = parse_file_part1(&inp);

    let valid_count = passports.iter().filter(|x| is_valid_part1(x)).count();
    println!("Valid (part 1): {}", valid_count);
}

fn parse_file_part1(inp: &str) -> Vec<Vec<String>> {
    inp.split("\n\n")
        .filter(|line| line.len() > 0)
        .map(|line| line.replace("\n", " "))
        .map(|line| parse_passport(&line))
        .collect::<Vec<Vec<String>>>()
}

fn parse_passport(passport: &str) -> Vec<String> {
    passport
        .split(" ")
        .map(|word| word.split(":").next().unwrap())
        .map(|word| word.to_owned())
        .collect::<Vec<String>>()
}

fn is_valid_part1(passport: &Vec<String>) -> bool {
    let valid_fields = vec!["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

    let fields: HashSet<String> = HashSet::from_iter(passport.iter().cloned());

    for valid_field in valid_fields {
        if !fields.contains(valid_field) {
            return false;
        }
    }

    true
}

// part 2

pub fn part2(inp: String) {
    let passports = parse_file(&inp);

    let valid_count = passports.iter().filter(|x| **x).count();
    println!("Valid (part 2): {}", valid_count);
}

fn parse_file(inp: &str) -> Vec<bool> {
    inp.split("\n\n")
        .filter(|line| line.len() > 0)
        .map(|line| line.replace("\n", " "))
        .map(|line| validate_passport(&line))
        .collect::<Vec<bool>>()
}

fn validate_passport(passport: &str) -> bool {
    let hair_color_checker = HairColorChecker::new();
    let pid_checker = PidChecker::new();

    let fields = passport
        .split(" ")
        .map(|x| x.to_owned())
        .collect::<Vec<String>>();

    if !has_all_fields(
        &fields
            .iter()
            .map(|x| x.split(":").next().unwrap().to_owned())
            .collect::<Vec<String>>(),
    ) {
        return false;
    }

    fields
        .iter()
        .map(|word| validate_field(word, &hair_color_checker, &pid_checker))
        .all(|x| x)
}

fn validate_field(
    field: &str,
    hair_color_checker: &HairColorChecker,
    pid_checker: &PidChecker,
) -> bool {
    let name_value = field.split(":").collect::<Vec<&str>>();

    if name_value.len() != 2 {
        return false;
    }

    let name = name_value[0];
    let value = name_value[1];

    match name {
        "byr" => is_valid_birth_year(value),
        "iyr" => is_valid_issue_year(value),
        "eyr" => is_valid_expiration_year(value),
        "hgt" => is_valid_height(value),
        "hcl" => hair_color_checker.is_valid_hair_color(value),
        "ecl" => is_valid_eye_color(value),
        "pid" => pid_checker.is_valid_pid(value),
        "cid" => true,
        _ => panic!("Argh, unknown field type: {}", name),
    }
}

fn has_all_fields(passport: &Vec<String>) -> bool {
    let valid_fields = vec!["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

    let fields: HashSet<String> = HashSet::from_iter(passport.iter().cloned());

    for valid_field in valid_fields {
        if !fields.contains(valid_field) {
            return false;
        }
    }

    true
}

fn is_valid_birth_year(value: &str) -> bool {
    if value.len() != 4 {
        return false;
    }
    let year = value.parse::<u16>().ok();
    match year {
        Some(year) => year <= 2002 && year >= 1920,
        None => false,
    }
}

fn is_valid_issue_year(field: &str) -> bool {
    if field.len() != 4 {
        return false;
    }
    let year = field.parse::<u16>().ok();
    match year {
        Some(year) => year <= 2020 && year >= 2010,
        None => false,
    }
}

fn is_valid_expiration_year(field: &str) -> bool {
    if field.len() != 4 {
        return false;
    }
    let year = field.parse::<u16>().ok();
    match year {
        Some(year) => year <= 2030 && year >= 2020,
        None => false,
    }
}

fn is_valid_height(field: &str) -> bool {
    if field.ends_with("in") {
        let inch = field.split("in").next().unwrap().parse::<u16>().ok();
        return match inch {
            Some(inch) => inch <= 76 && inch >= 59,
            None => false,
        };
    }
    if field.ends_with("cm") {
        let cm = field.split("cm").next().unwrap().parse::<u16>().ok();
        return match cm {
            Some(cm) => cm <= 193 && cm >= 150,
            None => false,
        };
    }

    false
}

struct HairColorChecker {
    regex: Regex,
}

impl HairColorChecker {
    fn new() -> Self {
        let regex = Regex::new("^#[0-9a-f]{6}$").unwrap();
        Self { regex }
    }

    fn is_valid_hair_color(&self, field: &str) -> bool {
        self.regex.is_match(field)
    }
}

struct PidChecker {
    regex: Regex,
}

impl PidChecker {
    fn new() -> Self {
        let regex = Regex::new("^[0-9]{9}$").unwrap();
        Self { regex }
    }

    fn is_valid_pid(&self, field: &str) -> bool {
        self.regex.is_match(field)
    }
}

fn is_valid_eye_color(field: &str) -> bool {
    let eye_colors = vec!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

    for ec in eye_colors {
        if ec == field {
            return true;
        }
    }

    false
}
