extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

pub fn part1(inp: String) {
    let parse_result = read_input(&inp);

    let mut error_rate = 0;

    for ticket in parse_result.nearby_tickets {
        let invalid_values = find_invalid_values(&ticket, &parse_result.rules);
        error_rate += invalid_values.iter().fold(0, |acc, cur| acc + cur);
    }

    println!("Error rate: {}", error_rate);
}

pub fn part2(inp: String) {
    let parse_result = read_input(&inp);

    let rules = parse_result.rules;
    let your_ticket = parse_result.your_ticket;
    let nearby_tickets = parse_result.nearby_tickets;

    assert_eq!(rules.len(), your_ticket.len());

    let field_indices = resolve_fields(&nearby_tickets, &rules);

    let departure_indices: Vec<usize> = rules
        .iter()
        .filter(|rule| rule.field.starts_with("departure"))
        .map(|rule| *field_indices.get(&rule.field).unwrap())
        .collect();

    let result: usize = departure_indices
        .iter()
        .fold(1, |acc, index| acc * your_ticket[*index]);

    println!("Result: {}", result);
}

fn resolve_fields(nearby_tickets: &Vec<Vec<usize>>, rules: &Vec<Rule>) -> HashMap<String, usize> {
    let valid_tickets = get_valid_tickets(nearby_tickets, rules);

    let all_fields: Vec<String> = rules.iter().map(|rule| rule.field.clone()).collect();

    // Map looks like:
    //     [0 -> ("class", "row", "seat")]
    //     [1 -> ("class", "row", "seat")]
    //     …
    // We'll reduce the sets on the right sides so that only a single field per
    // index remains.
    let mut candidates = HashMap::<usize, HashSet<String>>::new();

    // Initially, all field names are candidates for all indices, so let's copy
    // that floppy.
    for index in 0..rules.len() {
        candidates.insert(index, HashSet::from_iter(all_fields.iter().cloned()));
    }

    loop {
        for ticket in valid_tickets.iter() {
            for field_index in 0..rules.len() {
                let value = ticket[field_index];
                for rule in rules {
                    if rule.is_in_range(&value) {
                        continue;
                    }

                    // if rule does not match, remove from candidates

                    let possible_fields = candidates.get_mut(&field_index).unwrap();
                    possible_fields.remove(&rule.field);
                    // println!("removed \"{}\" from index {}", rule.field, field_index);

                    // this is a bit crazy, but it makes possible_fields available for
                    // yet another .get_mut() below
                    let remaining_field = possible_fields.iter().next().unwrap().clone();

                    if possible_fields.len() == 1 {
                        // println!(
                        //     "only \"{}\" remains, so it has index {}",
                        //     remaining_field, field_index
                        // );
                        // remove the field as candidate from all other indices
                        for remove_index in 0..rules.len() {
                            if remove_index == field_index {
                                continue; // don't remove from precious resolved set :-)
                            }
                            let other_candidates = candidates.get_mut(&remove_index).unwrap();
                            other_candidates.remove(&remaining_field);
                        }
                    }
                }
            }
        }

        let mut all_resolved = true;
        for (_, fields) in &candidates {
            all_resolved &= fields.len() == 1;
        }
        if all_resolved {
            break;
        }
    }

    // println!("Candidates after resolving: {:?}", candidates);

    let mut resolved = HashMap::<String, usize>::new();
    for (index, fields) in candidates {
        assert_eq!(fields.len(), 1);
        // println!("Possible fields for index {}: {:?}", index, fields);
        resolved.insert(fields.iter().next().unwrap().clone(), index);
    }
    println!("Fields with indices: {:?}", resolved);
    resolved
}

fn get_valid_tickets(tickets: &Vec<Vec<usize>>, rules: &Vec<Rule>) -> Vec<Vec<usize>> {
    tickets
        .iter()
        .filter(|ticket| is_ticket_valid(ticket, rules))
        .map(|ticket| ticket.clone())
        .collect()
}

fn is_ticket_valid(ticket: &Vec<usize>, rules: &Vec<Rule>) -> bool {
    let invalid_values = find_invalid_values(ticket, rules);
    invalid_values.len() == 0
}

fn find_invalid_values(values: &Vec<usize>, rules: &Vec<Rule>) -> Vec<usize> {
    values
        .iter()
        .filter(|value| !is_valid(&value, &rules))
        .map(|x| x.clone())
        .collect()
}

fn is_valid(value: &usize, rules: &Vec<Rule>) -> bool {
    for rule in rules {
        let valid = rule.is_in_range(value);
        if valid {
            return true;
        }
    }
    false
}

fn read_input(inp: &String) -> ParseResult {
    let sections: Vec<&str> = inp.split("\n\n").filter(|line| line.len() > 0).collect();

    let rules = parse_rules_section(sections[0]);
    let your_ticket = parse_ticket_section(sections[1]).pop().unwrap();
    let nearby_tickets = parse_ticket_section(sections[2]);

    ParseResult {
        rules,
        your_ticket,
        nearby_tickets,
    }
}

struct ParseResult {
    rules: Vec<Rule>,
    your_ticket: Vec<usize>,
    nearby_tickets: Vec<Vec<usize>>,
}

#[derive(Debug)]
struct Rule {
    field: String,
    first_range: (usize, usize),
    second_range: (usize, usize),
}

impl Rule {
    pub fn is_in_range(&self, value: &usize) -> bool {
        self.first_range.0 <= *value && *value <= self.first_range.1
            || self.second_range.0 <= *value && *value <= self.second_range.1
    }
}

fn parse_rules_section(rule_section: &str) -> Vec<Rule> {
    rule_section
        .split("\n")
        .filter(|line| line.len() > 0)
        .map(|line| parse_rule(line))
        .collect()
}

fn parse_rule(rule_line: &str) -> Rule {
    lazy_static! {
        static ref RE: Regex = Regex::new("(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)").unwrap();
    }

    let captures = RE.captures(rule_line).unwrap();

    let field: String = captures[1].to_string();

    let first_range = (
        captures[2].parse::<usize>().unwrap(),
        captures[3].parse::<usize>().unwrap(),
    );

    let second_range = (
        captures[4].parse::<usize>().unwrap(),
        captures[5].parse::<usize>().unwrap(),
    );

    Rule {
        field,
        first_range,
        second_range,
    }
}

fn parse_ticket_section(ticket_section: &str) -> Vec<Vec<usize>> {
    ticket_section
        .split("\n")
        .filter(|line| line.len() > 0)
        .filter(|line| !line.contains("ticket"))
        .map(|line| parse_ticket(line))
        .collect()
}

fn parse_ticket(ticket_line: &str) -> Vec<usize> {
    ticket_line
        .split(",")
        .map(|num| num.parse::<usize>().unwrap())
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    static RULES_SECTION_PART_1: &str = r#"
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50
"#;

    static RULES_SECTION_PART_2: &str = r#"
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19
"#;

    static YOUR_TICKET_SECTION: &str = r#"
your ticket:
11,12,13"#;

    static NEARBY_TICKETS_SECTION: &str = r#"
nearby tickets:
3,9,18
15,1,5
5,14,9"#;

    #[test]
    pub fn parses_your_ticket_section() {
        let your_ticket = parse_ticket_section(YOUR_TICKET_SECTION);

        assert_eq!(your_ticket, vec![vec![11, 12, 13]]);
    }

    #[test]
    pub fn parses_nearby_tickets_section() {
        let nearby_tickets = parse_ticket_section(NEARBY_TICKETS_SECTION);

        assert_eq!(
            nearby_tickets,
            vec![vec![3, 9, 18], vec![15, 1, 5], vec![5, 14, 9],]
        );
    }

    #[test]
    pub fn find_invalid_values_valid_value_returns_empty_vec() {
        let rules = parse_rules_section(RULES_SECTION_PART_1);
        let ticket_values = vec![7, 3, 47];

        let invalid = find_invalid_values(&ticket_values, &rules);

        assert_eq!(invalid, vec![]);
    }

    #[test]
    pub fn find_invalid_values_invalid_value_returns_vec_with_value() {
        let rules = parse_rules_section(RULES_SECTION_PART_1);
        let ticket_values = vec![40, 4, 50];

        let invalid = find_invalid_values(&ticket_values, &rules);

        assert_eq!(invalid, vec![4]);
    }

    #[test]
    pub fn resolve_fields_finds_indices_for_part2_example() {
        let rules = parse_rules_section(RULES_SECTION_PART_2);
        let nearby_tickets = parse_ticket_section(NEARBY_TICKETS_SECTION);

        let result = resolve_fields(&nearby_tickets, &rules);

        assert_eq!(result.len(), 3);
        assert_eq!(result.get("row").unwrap(), &0);
        assert_eq!(result.get("class").unwrap(), &1);
        assert_eq!(result.get("seat").unwrap(), &2);
    }
}
