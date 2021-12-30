use core::*;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        18
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

#[derive(Debug)]
enum Token {
    Start,
    End,
    Literal(usize),
    Comma,
}

struct Number {
    tokens: Vec<Token>,
}

impl Number {
    fn find_magnitude(&self) -> usize {
        0
    }

    fn new(line: &str) -> Self {
        let mut iter = line.chars().into_iter();
        let mut tokens = vec![];
        let mut digits = String::new();

        fn get_token(c: char) -> Option<Token> {
            match c {
                '[' => Some(Token::Start),
                ']' => Some(Token::End),
                ',' => Some(Token::Comma),
                _ => None,
            }
        }

        while let Some(ch) = iter.next() {
            if char::is_ascii_digit(&ch) {
                digits.push(ch);
            } else {
                if !digits.is_empty() {
                    if let Ok(num) = digits.parse::<usize>() {
                        tokens.push(Token::Literal(num));
                    }
                    digits.clear();
                }

                match get_token(ch) {
                    Some(token) => tokens.push(token),
                    None => digits.push(ch),
                }
            }
        }
        Number { tokens }
    }

    fn reduce(&mut self) {
        let mut level = 0;
        let mut i = 0;
        let mut left = 0_usize;

        loop {
            
            if i >= self.tokens.len() {
                break;
            }

            match self.tokens[i] {
                Token::Start => {
                    level += 1;
                    if level == 4 {
                        match &self.tokens[i+1 .. i+4] {
                            [Token::Literal(f), Token::Comma, Token::Literal(s), Token::End] => {
                                println!("Found!");
                            },
                            _ => continue,
                        }
                    }
                },
                Token::End => level -= 1,
                Token::Comma => continue,
                Token::Literal(v) => left = v,
                
            }

            i += 1;

        }

    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let magnitude = 4140;

        magnitude.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let part = PartOne {};

        let line1 = "[[[[[9,8],1],2],3],4]";
        let line2 = "[[[[0,9],2],3],4]";

        // let mut num1 = Number::parse(line1);
        // let num2 = Number::parse(line2);

        // num1.reduce(1);

        // assert_eq!(num1, num2);

        let line1 = "[7,[6,[5,[4,[3,2]]]]]";
        let line2 = "[7,[6,[5,[7,0]]]]";

        // let mut num1 = Number::new(line1);
        // let num2 = Number::new(line2);
        // num1.reduce(1);

        // assert_eq!(num1, num2);

        // let line1 = "[[6,[5,[4,[3,2]]]],1]";
        // let line2 = "[[6,[5,[7,0]]],3]";

        // let mut num1 = Number::new(line1);
        // println!("I: {:?}", num1);

        // let num2 = Number::new(line2);

        // num1.reduce(1);

        // println!("A: {:?}", num1);
        // println!("E: {:?}", num2);

        // assert_eq!(num1, num2);

        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
