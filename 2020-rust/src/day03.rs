pub fn part1(inp: String) {
    let slope = input_to_slope(inp);
    let trees = count_trees(&slope, 3, 1);
    println!("Trees: {}", trees);
}

pub fn part2(inp: String) {
    let slope = input_to_slope(inp);

    let movements = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];

    let trees: usize = movements
        .iter()
        .map(|m| count_trees(&slope, m.0, m.1))
        .product();

    println!("Trees: {}", trees);
}

fn input_to_slope(inp: String) -> Vec<Vec<bool>> {
    let lines = inp.split("\n").collect::<Vec<&str>>();

    lines
        .iter()
        .filter(|line| line.len() > 0)
        .map(|line| to_tree_map(line))
        .collect::<Vec<Vec<bool>>>()
}

fn count_trees(slope: &Vec<Vec<bool>>, right: usize, down: usize) -> usize {
    let row_has_tree = |row: &Vec<bool>, pos| -> usize {
        if *row.iter().cycle().skip(pos).next().unwrap() {
            1
        } else {
            0
        }
    };

    slope
        .iter()
        .step_by(down)
        .enumerate()
        .map(|(index, row)| row_has_tree(row, index * right))
        .sum()
}

fn to_tree_map(row: &str) -> Vec<bool> {
    let mut ret = vec![];
    for ch in row.chars() {
        ret.push(match ch {
            '#' => true,
            _ => false,
        });
    }

    ret
}
