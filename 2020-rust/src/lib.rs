// Source: https://repl.it/@Scoder12/aoc-rust-template

#[macro_use]
extern crate lazy_static;

// Days
pub mod day01;
pub mod day02;
pub mod day03;
pub mod day04;
pub mod day05;
pub mod day06;
pub mod day07;
pub mod day08;
pub mod day09;
pub mod day10;
pub mod day12;
pub mod day13;
pub mod day14;
pub mod day15;
pub mod day16;

pub fn noop(_inp: String) {}

pub type DayFn = fn(String);

pub fn get_day(day: u32) -> (DayFn, DayFn) {
    return match day {
        1 => (day01::part1, day01::part2), // driver: yogan, navigator: t-animal, Russell
        2 => (day02::part1, day02::part2), // driver: yogan, navigator: dnnr, Russell
        3 => (day03::part1, day03::part2), // driver: dnnr,  navigator: yogan, Russell
        4 => (day04::part1, day04::part2), // driver: dnnr,  navigator: yogan, Russell
        5 => (day05::part1, day05::part2), // driver: yogan, navigator: dnnr
        6 => (day06::part1, day06::part2), // driver: dnnr,  navigator: yogan
        7 => (day07::part1, day07::part2), // driver: yogan (lonely driver at night)
        8 => (day08::part1, day08::part2), // driver: yogan, navigator: dnnr
        9 => (day09::part1, day09::part2), // driver: dnnr,  navigator: yogan, Russell
        10 => (day10::part1, day10::part2), // driver: yogan, navigator: Russell (pt. 1, start of pt. 2)
        12 => (day12::part1, day12::part2), // driver: dnnr, navigator: yogan
        13 => (day13::part1, day13::part2), // driver: yogan, navigator: dnnr
        14 => (day14::part1, day14::part2), // driver: dnnr, navigator: yogan
        15 => (day15::part1, day15::part2), // driver: yogan, navigator: Russell, dnnr
        16 => (day16::part1, day16::part2), // driver: yogan, navigator: Russell (pt. 1, start of pt. 2)
        _ => {
            println!("Unknown day: {}", day);
            return (noop, noop);
        }
    };
}
