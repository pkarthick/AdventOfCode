use std::env;

use day::DayTrait;

pub mod day;
pub mod days;

fn main() {
    let days = [
        None,                       // 1
        None,                       // 2
        None,                       // 3
        None,                       // 4
        Some(days::day_05::Day {}), // 5
        None,                       // 6
        None,                       // 7
        None,                       // 8
        None,                       // 9
        None,                       // 10
        None,                       // 11
        None,                       // 12
        None,                       // 13
        None,                       // 14
        None,                       // 15
        None,                       // 16
        None,                       // 17
        None,                       // 18
        None,                       // 19
        None,                       // 20
        None,                       // 21
        None,                       // 22
        None,                       // 23
        None,                       // 24
        None,                       // 25
    ];

    let mut days_to_run: Vec<usize> = vec![];

    if let Some(day) = env::args().into_iter().nth(1) {
        let day = day.parse::<usize>().unwrap();
        days_to_run.push(day);
    } else {
        for day in 1_usize..=25 {
            days_to_run.push(day);
        }
    }

    for day in days_to_run {
        if let Some(day) = &days[day - 1] {
            // if let Ok(result) = day.run(1) {
            //     println!("Part 1 Answer: \n{}", result);
            // }
            if let Ok(result) = day.run(2) {
                println!("\nPart 2 Answer: \n{}", result);
            }
        }
    }
}
