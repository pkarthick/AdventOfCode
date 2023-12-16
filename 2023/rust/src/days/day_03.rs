
impl Input {
    fn new(s: &str) -> Self {
        
        Self {  }
    }
}

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Input::new(s))
    }
}

pub struct Day {}

impl crate::day::Day<Input, Input> for Day {

    fn day_number(&self) -> &'static str {
        "03"
    }

    fn part1_input(&self) -> &'static str {
        "day03"
    }

    fn part2_input(&self) -> &'static str {
        "day03"
    }

    fn part1(&self, input: Input) -> String {
        "<<result>>".to_string()
    }

    fn part2(&self, input: Input) -> String {
        "part2".to_string()
    }
}
