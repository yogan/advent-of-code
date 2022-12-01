pub fn part1(inp: String) {
    let numbers = read_numbers(inp);
    let mut iter1 = numbers.iter().peekable();

    while iter1.peek() != None {
        let cur1 = iter1.next().unwrap();
        let mut iter2 = iter1.clone();
        while iter2.peek() != None {
            let cur2 = iter2.next().unwrap();
            if cur1 + cur2 == 2020 {
                let result = cur1 * cur2;
                println!("{}", result);
                return;
            }
        }
    }
}

pub fn part2(inp: String) {
    let numbers = read_numbers(inp);
    let mut iter1 = numbers.iter().peekable();

    while iter1.peek() != None {
        let cur1 = iter1.next().unwrap();
        let mut iter2 = iter1.clone();
        while iter2.peek() != None {
            let cur2 = iter2.next().unwrap();
            let mut iter3 = iter2.clone();
            while iter3.peek() != None {
                let cur3 = iter3.next().unwrap();
                if cur1 + cur2 + cur3 == 2020 {
                    let result = cur1 * cur2 * cur3;
                    println!("{}", result);
                    return;
                }
            }
        }
    }
}

fn read_numbers(inp: String) -> Vec<i32> {
    let lines = inp.split("\n").collect::<Vec<&str>>();
    lines
        .iter()
        .filter(|line| line.len() > 0)
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
}
