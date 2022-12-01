use std::collections::HashMap;

pub fn part1(inp: String) {
    let mut numbers = read_numbers(&inp);
    let (diff_of_1, diff_of_3) = find_diffs(&mut numbers);
    let result = diff_of_1 * diff_of_3;
    println!("1s: {} / 3s: {} / result: {}", diff_of_1, diff_of_3, result);
}

fn find_diffs(adaptors: &mut Vec<usize>) -> (usize, usize) {
    adaptors.push(0); // outlet is 0
    adaptors.sort();
    adaptors.push(adaptors.last().unwrap() + 3); // diff to device is always 3

    let mut diff_of_1 = 0;
    let mut diff_of_3 = 0;

    //    [(0) 2 3 4 6 (last+3)]
    // -> [   2 1 1 2 3        ]

    for (index, adaptor) in adaptors.iter().enumerate().skip(1) {
        let diff = adaptor - adaptors[index - 1];
        if diff == 1 {
            diff_of_1 += 1;
        }
        if diff == 3 {
            diff_of_3 += 1;
        }
    }

    (diff_of_1, diff_of_3)
}

pub fn part2(inp: String) {
    let mut adaptors = read_numbers(&inp);

    adaptors.push(0); // outlet is 0
    adaptors.sort();
    adaptors.push(adaptors.last().unwrap() + 3); // diff to device is always 3

    let next_adaptors = calc_next_adaptors(&adaptors);
    // println!("next adaptors: {:?}", next_adaptors);

    //   key in map is adapter joltage
    // value in map is number of combinations to reach this node
    let mut combinations = HashMap::<usize, usize>::new();
    let device_index = adaptors.len() - 1;
    combinations.insert(device_index, 1);

    for joltage in adaptors.iter().rev() {
        let next = next_adaptors.get(&joltage).unwrap();
        // println!("next for {}: {:?}", joltage, next);
        match next.len() {
            0 => {
                combinations.insert(*joltage, 1);
            }
            1..=3 => {
                let sum = next
                    .iter()
                    .fold(0, |acc, cur| acc + combinations.get(cur).unwrap());
                // println!("combinations for joltage {}: {}", joltage, sum);
                combinations.insert(*joltage, sum);
            }
            _ => {
                panic!("adapter {} has more than 3 next adaptors");
            }
        }
    }

    println!("Adapter arrangements: {}", combinations.get(&0).unwrap());
}

fn calc_next_adaptors(adaptors: &Vec<usize>) -> HashMap<usize, Vec<usize>> {
    //   key in map is adapter joltage
    // value in map is list of joltages of reachable adapters
    let mut next_adaptors = HashMap::<usize, Vec<usize>>::new();

    for (index, adaptor) in adaptors.iter().enumerate() {
        let mut reachable: Vec<usize> = vec![];
        for diff in 1..=3 {
            let ancestor = adaptors.get(index + diff);
            match ancestor {
                Some(next_adaptor) => {
                    if next_adaptor - adaptor <= 3 {
                        reachable.push(*next_adaptor)
                    }
                }
                None => (),
            }
        }
        next_adaptors.insert(*adaptor, reachable);
    }

    next_adaptors
}

fn read_numbers(inp: &str) -> Vec<usize> {
    inp.split("\n")
        .filter(|line| line.len() > 0)
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn find_diffs_part1_small_example() {
        let mut input = vec![16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4];

        let (diff_of_1, diff_of_3) = find_diffs(&mut input);

        assert_eq!(diff_of_1, 7);
        assert_eq!(diff_of_3, 5);
    }

    #[test]
    pub fn find_diffs_part1_large_example() {
        let mut input = vec![
            28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35,
            8, 17, 7, 9, 4, 2, 34, 10, 3,
        ];

        let (diff_of_1, diff_of_3) = find_diffs(&mut input);

        assert_eq!(diff_of_1, 22);
        assert_eq!(diff_of_3, 10);
    }

    #[test]
    pub fn calc_next_adaptors_small_input() {
        let input = vec![0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22];
        let expected_reachable_from_index = vec![
            vec![1],
            vec![4],
            vec![5, 6, 7],
            vec![6, 7],
            vec![7],
            vec![10],
            vec![11, 12],
            vec![12],
            vec![15],
            vec![16],
            vec![19],
            vec![22],
            vec![],
        ];

        let result = calc_next_adaptors(&input);

        for (index, value) in expected_reachable_from_index.iter().enumerate() {
            let from = input.get(index).unwrap();
            assert_eq!(*result.get(&from).unwrap(), *value);
        }
    }
}
