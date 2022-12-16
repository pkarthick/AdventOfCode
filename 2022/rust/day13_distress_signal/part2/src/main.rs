use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq)]
enum List {
    Single(u32),
    Multiple(Vec<List>),
}

#[derive(PartialEq, Eq)]
enum Token {
    Open,
    // Close,
    Comma,
    Num(u32),
    List(Vec<Token>),
}

fn create_multiple(mut tokens: Vec<Token>) -> Vec<List> {
    let mut vec = vec![];
    loop {
        let list = match tokens.pop() {
            Some(Token::List(vec)) => List::Multiple(create_multiple(vec)),
            Some(Token::Num(num)) => List::Single(num),
            None => return vec,
            _ => panic!("Unexpected token!"),
        };
        vec.push(list);
    }
}

fn create_list(mut tokens: Vec<Token>) -> List {
    if tokens.len() == 1 {
        match tokens.pop() {
            Some(Token::List(vec)) => List::Multiple(create_multiple(vec)),
            Some(Token::Num(num)) => List::Single(num),
            _ => panic!("Unexpected token!"),
        }
    } else {
        List::Multiple(create_multiple(tokens))
    }
}

fn parse_tokens(s: &str) -> Vec<Token> {
    let v: Vec<char> = s.chars().collect();
    let mut tokens = Vec::new();

    let mut i: usize = 0;

    while i < v.len() {
        let token = match v[i] {
            '[' => {
                i += 1;
                Token::Open
            }
            ']' => {
                i += 1;

                let mut vec = vec![];

                loop {
                    match tokens.pop() {
                        Some(Token::Open) => break,
                        None => break,
                        Some(Token::Comma) => continue,
                        Some(token) => vec.push(token),
                    }
                }

                Token::List(vec)
            }
            ',' => {
                i += 1;
                Token::Comma
            }
            '0'..='9' => {
                let mut num: u32 = 0;
                for c in v[i..].iter().take_while(|c| c.is_ascii_digit()) {
                    num *= 10;
                    num += (*c as u32) - 48;
                    i += 1;
                }
                Token::Num(num)
            }
            _ => {
                panic!("Invalid char! {}", v[i]);
            }
        };
        tokens.push(token);
    }

    tokens
}

impl List {
    fn new(s: &str) -> Self {
        create_list(parse_tokens(s))
    }

    fn left(&self) -> u32 {
        match self {
            List::Single(num) => *num,
            List::Multiple(v) => {
                if v.is_empty() {
                    0
                } else {
                    v[0].left()
                }
            }
        }
    }
}

impl PartialOrd for List {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for List {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let result = match (self, other) {
            (List::Single(x), List::Single(y)) => x.cmp(y),
            (List::Single(s), List::Multiple(_)) => {
                List::Multiple(vec![List::Single(*s)]).cmp(other)
            }
            (List::Multiple(_), List::Single(s)) => {
                self.cmp(&List::Multiple(vec![List::Single(*s)]))
            }

            (List::Multiple(v1), List::Multiple(v2)) => {
                for (f, s) in v1.iter().zip(v2) {
                    match f.cmp(s) {
                        std::cmp::Ordering::Less => return Ordering::Less,
                        std::cmp::Ordering::Equal => continue,
                        std::cmp::Ordering::Greater => return Ordering::Greater,
                    }
                }

                v1.len().cmp(&v2.len())
            }
        };

        result
    }
}

fn main() {
    let mut packets: Vec<List> = std::io::stdin()
        .lines()
        .map(|res| res.unwrap())
        .filter(|s| !s.is_empty())
        .map(|s| List::new(&s))
        .collect();

    packets.sort();

    let first = packets.iter().take_while(|l| l.left() < 2).count();
    let second = packets.iter().take_while(|l| l.left() < 6).count();

    println!("{}", (first + 1) * (second + 2));
}
