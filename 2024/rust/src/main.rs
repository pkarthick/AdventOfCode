use day::AoCDay;

mod utils;

mod day;
mod day01;
// mod day02;
// mod day03;
// mod day04;
// mod day05;
// mod day06;
// mod day07;
// mod day08;
// mod day09;
// mod day10;
// mod day11;
//

macro_rules! days {
    ($($day_index: tt),*) => {
            $(day!($day_index),)*
    };
}

macro_rules! day {
    ($day_index: ident, $index: expr) => {
        Box::new($day_index::Day::new($index)).await
    };
}

// const fn get_day() -> impl AoCDay {
//     day!(day01)
// }

#[tokio::main]
async fn main() {
    run_all_days().await;
}

// seq!(N in 1..=1 {
//     #(
//         day~N::Day::new()
//     )*
// });
async fn run_all_days() {
    let day = day01::Day::new(1).await;
    run_day(1, day);
    // day.part1();
    // let mut days: Vec<_> = Vec::with_capacity(25);
    // let days1 = days!(day01, day02);

    // days.push(day!(day01, 1));

    // let mut day01 = day01::Day::new(include_str!("../data/day01"));
    // let mut day02 = day02::Day::new(include_str!("../data/day02"));
    // let mut day03 = day03::Day::new(include_str!("../data/day03"));
    // let mut day04 = day04::Day::new(include_str!("../data/day04"));
    // let mut day05 = day05::Day::new(include_str!("../data/day05"));
    // let mut day06 = day06::Day::new(include_str!("../data/day06"));
    // let mut day07 = day07::Day::new(include_str!("../data/day07"));
    // let mut day08 = day08::Day::new(include_str!("../data/day08"));
    // let mut day09 = day09::Day::new(include_str!("../data/day09"));
    // let mut day10 = day10::Day::new(include_str!("../data/day10"));
    // let mut day11 = day11::Day::new(include_str!("../data/day11"));
    //
    // let days: Vec<Box<&mut dyn AoCDay>> = vec![
    //     Box::new(&mut day01 as &mut dyn AoCDay),
    //     Box::new(&mut day02 as &mut dyn AoCDay),
    //     Box::new(&mut day03 as &mut dyn AoCDay),
    //     Box::new(&mut day04 as &mut dyn AoCDay),
    //     Box::new(&mut day05 as &mut dyn AoCDay),
    //     Box::new(&mut day06 as &mut dyn AoCDay),
    //     Box::new(&mut day07 as &mut dyn AoCDay),
    //     Box::new(&mut day08 as &mut dyn AoCDay),
    //     Box::new(&mut day09 as &mut dyn AoCDay),
    //     Box::new(&mut day10 as &mut dyn AoCDay),
    //     Box::new(&mut day11 as &mut dyn AoCDay),
    // ];

    // for (day_index, day) in days.into_iter().enumerate() {
    //     run_day(day_index + 1, day);
    // }
    println!("Advent of Code 2024... Done!")
}

fn run_day(day_index: usize, day: impl AoCDay) {
    println!("Day {}", day_index);
    println!("Part 1: {}", day.part1());
    println!("Part 2: {}", day.part2());
    println!();
}
