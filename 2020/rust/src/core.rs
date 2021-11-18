pub enum FileKind {
    Input,
    Output,
}

pub enum InputKind {
    Sample,
    Puzzle,
}

pub enum PartKind {
    One,
    Two,
}

pub struct Part<I, O: ToString> {
    day: i32,
    part_kind: PartKind,
    create_input: fn(String) -> I,
    process_input: fn(I) -> Result<O, String>,
}

// pub struct PartTwo {}

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

    let filename = format!("./testdata/{}/{}_{}.{}", day, input_kind, part, file_kind);

    std::fs::read_to_string(filename).expect("File opening issue!")
}


fn get_input(
  part_kind: &PartKind,
  day: i32,
  input_kind: &InputKind,
) -> String {
  get_file_content(part_kind, day, FileKind::Input, input_kind)
}

fn get_output(
  part_kind: &PartKind,
  day: i32,
  input_kind: &InputKind,
) -> String {
  get_file_content(part_kind, day, FileKind::Output, input_kind)
}


impl<I, O: ToString> Part<I, O> {
    pub fn new(
        day: i32,
        part_kind: PartKind,
        create_input: fn(String) -> I,
        process_input: fn(I) -> Result<O, String>,
    ) -> Self {
        Part {
            day,
            part_kind,
            create_input,
            process_input,
        }
    }

    fn test(&self, input_kind: InputKind) -> Result<(), String> {
        let day: i32 = self.day;

        let input = get_input(&self.part_kind, day, &input_kind);

        let create_input = self.create_input;

        let input: I = create_input(input);

        let process_input = self.process_input;

        let actual = process_input(input)?.to_string();

        let expected = get_output(&self.part_kind, day, &input_kind);

        assert_eq!(actual, expected);

        Ok(())
    }

    pub fn test_sample(&self) -> Result<(), String> {
        self.test(InputKind::Sample)
    }

    pub fn test_puzzle(&self) -> Result<(), String> {
        self.test(InputKind::Puzzle)
    }
}

pub fn to_vec(s: &str, sort_asc: Option<bool>) -> Vec<i32> {
    let mut v = s
        .split_ascii_whitespace()
        .filter_map(|x| match x {
            "" => None,
            _ => Some(x.parse().unwrap()),
        })
        .collect();

    match sort_asc {
        None => v,
        Some(true) => {
            v.sort();
            v
        }
        Some(false) => {
            v.sort_by(|b, a| b.cmp(a));
            v
        }
    }
}
