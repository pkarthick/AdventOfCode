pub enum FileKind {
    Input,
    Output,
}

pub enum InputKind {
    Sample,
    Puzzle,
}

#[derive(Copy, Clone)]
pub enum PartKind {
    One,
    Two,
}

impl std::fmt::Display for PartKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PartKind::One => write!(f, "1"),
            PartKind::Two => write!(f, "2"),
        }
    }
}

fn get_file_content(
    part_kind: &PartKind,
    day: i32,
    file_kind: FileKind,
    input_kind: &InputKind,
) -> String {
    let input_kind = match input_kind {
        InputKind::Sample => "sample",
        InputKind::Puzzle => "puzzle",
    };

    let file_kind = match file_kind {
        FileKind::Input => "in",
        FileKind::Output => "out",
    };

    let part = match part_kind {
        PartKind::One => 1,
        PartKind::Two => 2,
    };

    let filename = format!("../testdata/{}/{}_{}.{}", day, input_kind, part, file_kind);

    match std::fs::read_to_string(filename) {
        Ok(s) => s,
        Err(_) => String::new(),
    }
}

fn get_input(part_kind: &PartKind, day: i32, input_kind: &InputKind) -> String {
    get_file_content(part_kind, day, FileKind::Input, input_kind)
}

fn get_output(part_kind: &PartKind, day: i32, input_kind: &InputKind) -> String {
    get_file_content(part_kind, day, FileKind::Output, input_kind)
}

pub trait PartSpec {
    fn get_day(&self) -> i32;
    fn get_part_kind(&self) -> PartKind;
}

pub trait TestPart : PartSpec {

    fn test(&self, input_kind: InputKind) -> Result<(), String> {
        
        let day: i32 = self.get_day();

        let part_kind = self.get_part_kind();

        let input = get_input(&part_kind, day, &input_kind);

        let actual = self.process_input(input);

        let expected = get_output(&part_kind, day, &input_kind);

        if actual == expected {
            Ok(())
        } else {
            Err(format!("Day {} {} Failed: Expected {} but got {}", day, part_kind, expected, actual))
        }
    }

    fn test_sample(&self) -> Result<(), String> {
        self.test(InputKind::Sample)
    }

    fn test_puzzle(&self) -> Result<(), String> {
        self.test(InputKind::Puzzle)
    }

    fn process_input(&self, input: String) -> String;

}
