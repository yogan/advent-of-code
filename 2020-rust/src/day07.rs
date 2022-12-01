use std::collections::HashMap;
use std::collections::HashSet;

pub fn part1(inp: String) {
    let rules = parse_bag_rules(inp);
    let shiny_gold_holding_colors = get_shiny_gold_holding_bag_colors(&rules);

    println!(
        "# of bag colors that can hold shiny gold: {}",
        shiny_gold_holding_colors.len()
    );
}

pub fn part2(inp: String) {
    let rules = parse_bag_rules(inp);
    let number_of_bags = count_bags_in_shiny_gold_bag(&rules);

    println!("# of bags contained in shiny gold bag: {}", number_of_bags);
}

fn parse_bag_rules(inp: String) -> HashMap<String, Option<Vec<Bag>>> {
    read_lines(&inp)
        .iter()
        .filter(|line| line.len() > 0)
        .map(|line| create_rule(line))
        .collect::<HashMap<String, Option<Vec<Bag>>>>()
}

fn read_lines(inp: &str) -> Vec<&str> {
    inp.split("\n")
        .filter(|line| line.len() > 0)
        .collect::<Vec<&str>>()
}

fn create_rule(line: &str) -> (String, Option<Vec<Bag>>) {
    let groups = line.split(" bags contain ");

    let vec: Vec<&str> = groups.collect();

    let color = vec[0].to_owned();
    let contained_bags: Option<Vec<Bag>> = vec[1]
        .split(", ")
        .collect::<Vec<&str>>()
        .iter()
        .map(|value| value.split(" ").collect::<Vec<&str>>())
        .map(|value| match value[0] {
            "no" => None,
            _ => Some(create_bag(value[1], value[2], value[0])),
        })
        .collect();

    (color, contained_bags)
}

#[derive(Debug)]
struct Bag {
    color: String,
    count: usize,
}

fn create_bag(color_1: &str, color_2: &str, count_str: &str) -> Bag {
    let mut color = color_1.to_owned();
    color.push_str(" ");
    color.push_str(color_2);

    let count = count_str.parse::<usize>().unwrap();

    Bag { count, color }
}

fn get_shiny_gold_holding_bag_colors(rules: &HashMap<String, Option<Vec<Bag>>>) -> HashSet<String> {
    let mut shiny_gold_holding_colors = HashSet::<String>::new();

    for rule in rules.iter() {
        if can_contain_shiny_gold_bag(&rule.0, rules, &mut shiny_gold_holding_colors) {
            shiny_gold_holding_colors.insert(rule.0.to_string());
        }
    }

    shiny_gold_holding_colors
}

fn can_contain_shiny_gold_bag(
    color: &String,
    rules: &HashMap<String, Option<Vec<Bag>>>,
    shiny_gold_holding_colors: &mut HashSet<String>,
) -> bool {
    if shiny_gold_holding_colors.contains(color) {
        return true;
    }

    let rule = &rules[color];
    if rule.is_none() {
        return false;
    }

    match rule {
        None => {
            return false;
        }
        Some(bags) => {
            for inner_bag in bags.iter() {
                if is_shiny_gold_bag(inner_bag) {
                    return true;
                } else {
                    if can_contain_shiny_gold_bag(
                        &inner_bag.color,
                        rules,
                        shiny_gold_holding_colors,
                    ) {
                        return true;
                    }
                }
            }
            return false;
        }
    }
}

fn is_shiny_gold_bag(bag: &Bag) -> bool {
    bag.color == "shiny gold"
}

fn count_bags_in_shiny_gold_bag(rules: &HashMap<String, Option<Vec<Bag>>>) -> usize {
    let shiny_gold_bag = &rules["shiny gold"];
    count_inner_bags(shiny_gold_bag, rules)
}

fn count_inner_bags(bags: &Option<Vec<Bag>>, rules: &HashMap<String, Option<Vec<Bag>>>) -> usize {
    match bags {
        None => 0,
        Some(inner_bags) => inner_bags.iter().fold(0, |acc, cur| {
            acc + cur.count + (cur.count * count_inner_bags(&rules[&cur.color], rules))
        }),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn get_shiny_gold_holding_bag_colors_single_empty_bag() {
        let mut rules = HashMap::<String, Option<Vec<Bag>>>::new();
        rules.insert("testing green".to_owned(), None);

        let bag_colors = get_shiny_gold_holding_bag_colors(&rules);

        assert_eq!(bag_colors.len(), 0);
    }

    #[test]
    pub fn get_shiny_gold_holding_bag_colors_single_bag_holding_gold() {
        let mut rules = HashMap::<String, Option<Vec<Bag>>>::new();
        rules.insert(
            "testing green".to_owned(),
            Some(vec![create_bag("shiny", "gold", "4")]),
        );

        let bag_colors = get_shiny_gold_holding_bag_colors(&rules);

        assert_eq!(bag_colors.len(), 1);
    }

    #[test]
    pub fn get_shiny_gold_holding_bag_colors_single_bag_holding_gold_and_other() {
        let mut rules = HashMap::<String, Option<Vec<Bag>>>::new();
        let shiny_bag = create_bag("shiny", "gold", "4");
        let other_bag = create_bag("bording", "grey", "1");
        rules.insert("testing green".to_owned(), Some(vec![shiny_bag, other_bag]));

        let bag_colors = get_shiny_gold_holding_bag_colors(&rules);

        assert_eq!(bag_colors.len(), 1);
    }

    #[test]
    pub fn get_shiny_gold_holding_bag_colors_multiple_rules_one_direct_gold() {
        let mut rules = HashMap::<String, Option<Vec<Bag>>>::new();
        let shiny_bag = create_bag("shiny", "gold", "4");
        let other_bag = create_bag("bording", "grey", "1");
        rules.insert("raging red".to_owned(), None);
        rules.insert("testing green".to_owned(), Some(vec![shiny_bag, other_bag]));

        let bag_colors = get_shiny_gold_holding_bag_colors(&rules);

        assert_eq!(bag_colors.len(), 1);
    }

    #[test]
    pub fn get_shiny_gold_holding_bag_colors_multiple_rules_one_gold_nested_level_1() {
        let mut rules = HashMap::<String, Option<Vec<Bag>>>::new();
        rules.insert(
            "bright white".to_owned(),
            Some(vec![create_bag("shiny", "gold", "1")]),
        );
        rules.insert(
            "dark orange".to_owned(),
            Some(vec![create_bag("bright", "white", "4")]),
        );
        rules.insert("raging red".to_owned(), None);

        let bag_colors = get_shiny_gold_holding_bag_colors(&rules);

        assert_eq!(bag_colors.len(), 2);
    }

    #[test]
    pub fn get_shiny_gold_holding_bag_colors_multiple_rules_one_gold_nested_level_2() {
        let mut rules = HashMap::<String, Option<Vec<Bag>>>::new();
        rules.insert(
            "bright white".to_owned(),
            Some(vec![create_bag("shiny", "gold", "1")]),
        );
        rules.insert(
            "dark orange".to_owned(),
            Some(vec![create_bag("bright", "white", "4")]),
        );
        rules.insert(
            "goofy grey".to_owned(),
            Some(vec![create_bag("dark", "orange", "3")]),
        );
        rules.insert("raging red".to_owned(), None);

        let bag_colors = get_shiny_gold_holding_bag_colors(&rules);

        assert_eq!(bag_colors.len(), 3);
    }

    #[test]
    pub fn official_part_1_sample_input() {
        let input = r#"
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"#;

        let rules = parse_bag_rules(input.to_string());
        let shiny_gold_holding_colors = get_shiny_gold_holding_bag_colors(&rules);

        assert_eq!(shiny_gold_holding_colors.len(), 4);
        assert_eq!(
            shiny_gold_holding_colors.contains("bright white"),
            true,
            "A bright white bag, which can hold your shiny gold bag directly."
        );
        assert_eq!(
            shiny_gold_holding_colors.contains("muted yellow"),
            true,
            "A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags."
        );
        assert_eq!(
            shiny_gold_holding_colors.contains("dark orange"),
             true,
            "A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag."
        );
        assert_eq!(shiny_gold_holding_colors.contains("light red"),
             true,
            "A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag."
        );
    }

    #[test]
    pub fn official_part_2_sample_input() {
        let input = r#"
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
"#;

        let rules = parse_bag_rules(input.to_string());
        let number_of_bags = count_bags_in_shiny_gold_bag(&rules);

        assert_eq!(number_of_bags, 126);
    }
}
