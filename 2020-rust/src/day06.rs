use std::collections::HashSet;

pub fn part1(inp: String) {
    let groups = read_groups(&inp);

    let any_answers = groups.iter().map(|group| get_any_answers_in_group(group));

    let sum: usize = any_answers.map(|a| a.len()).sum();

    println!("part1: {:?}", sum);
}

pub fn part2(inp: String) {
    let groups = read_groups(&inp);

    let common_answers = groups
        .iter()
        .map(|group| get_common_answers_in_group(group));

    let sum: usize = common_answers.map(|a| a.len()).sum();

    println!("part2: {:?}", sum);
}

fn read_groups(inp: &str) -> Vec<&str> {
    inp.split("\n\n")
        .filter(|line| line.len() > 0)
        .collect::<Vec<_>>()
}

fn get_common_answers_in_group(group: &str) -> HashSet<char> {
    let people = group
        .split("\n")
        .filter(|line| line.len() > 0)
        .map(|line| line.chars().collect())
        .collect::<Vec<HashSet<char>>>();

    if people.is_empty() {
        return HashSet::new();
    }

    let mut people_iter = people.iter();
    let mut common_answers = people_iter.next().unwrap().clone();
    people_iter.for_each(|answers| common_answers.retain(|x| answers.contains(x)));

    common_answers
}

fn get_any_answers_in_group(group: &str) -> HashSet<char> {
    group
        .chars()
        .filter(|c| match c {
            'a'..='z' => true,
            _ => false,
        })
        .collect::<HashSet<char>>()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn get_any_answers_in_group_sample1() {
        let group = "abcx\nabcy\nabcz\n";
        let expected_answers: HashSet<_> = ['a', 'b', 'c', 'x', 'y', 'z'].iter().cloned().collect();

        let answers = get_any_answers_in_group(group);

        assert_eq!(answers, expected_answers);
    }

    #[test]
    pub fn get_common_answers_in_group_sample1() {
        let group = "abcx\nabcy\nabcz\n";
        let expected_answers: HashSet<_> = ['a', 'b', 'c'].iter().cloned().collect();

        let answers = get_common_answers_in_group(group);

        assert_eq!(answers, expected_answers);
    }
}
