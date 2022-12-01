use std::collections::HashMap;

pub fn part1(inp: String) {
    let line = read_input(&inp);
    let sequence = calculate_sequence(line, 2020);
    println!("Number: {}", sequence.last().unwrap());
}

pub fn part2(inp: String) {
    let line = read_input(&inp);
    let sequence = calculate_sequence(line, 30_000_000);
    println!("Number: {}", sequence.last().unwrap());
}

fn read_input(inp: &String) -> &str {
    inp.split("\n")
        .filter(|line| line.len() > 0)
        .next()
        .unwrap()
}

fn calculate_sequence(line: &str, limit: usize) -> Vec<u32> {
    let sequence: Vec<u32> = line.split(",").map(|s| s.parse::<u32>().unwrap()).collect();

    // keys are numbers, values are last seen indices in sequence
    let mut last_seen: HashMap<u32, u32> = HashMap::new();

    sequence.iter().enumerate().for_each(|(index, number)| {
        last_seen.insert(*number, index as u32);
    });

    let mut current_number = sequence[sequence.len() - 1];

    for index in sequence.len()..limit {
        let maybe_last_index = last_seen.get(&current_number);
        match maybe_last_index {
            Some(last_index) => {
                let age = index as u32 - last_index - 1;
                current_number = age;
            }
            None => {
                // never seen before
                current_number = 0
            }
        }
        last_seen.insert(current_number, (index - 1) as u32);
    }

    sequence
}
