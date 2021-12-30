use core::*;
use std::collections::HashMap;
    
struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        16
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

#[derive(Debug)]
enum OperatorType {
    Sum,
    Product,
    Mininum,
    Maximum,
    Literal,
    GreaterThan,
    LessThan,
    EqualTo,
}

impl OperatorType {
    fn new(type_id: &str) -> Self {
        match type_id {
            "000" => OperatorType::Sum,
            "001" => OperatorType::Product,
            "010" => OperatorType::Mininum,
            "011" => OperatorType::Maximum,
            "100" => OperatorType::Literal,
            "101" => OperatorType::GreaterThan,
            "110" => OperatorType::LessThan,
            "111" => OperatorType::EqualTo,
            _ => panic!("Unsupported Packet Type!"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum PacketKind {
    Literal,
    Count(usize),
    Length(usize),
}

#[derive(Debug)]
enum Token {
    Type(OperatorType),
    Kind(PacketKind),
    Literal(usize),
    StartLength,
    EndLength,
}

struct Tokenizer {
    binary_digits: String,
    tokens: Vec<Token>,
    cur_operator_type: OperatorType,
    cur: usize,
    end_markers: HashMap<usize, usize>,
}

impl Tokenizer {
    fn decimal_from_binary(b: &str) -> usize {
        usize::from_str_radix(b, 2).unwrap()
    }

    fn binary_from_hexa(h: &str) -> String {
        let mut binary = String::new();

        for c in h.chars().into_iter() {
            let z = u64::from_str_radix(&c.to_string(), 16).unwrap();
            let segment = format!("{:04b}", z);

            binary.push_str(&segment);
        }

        let blen = binary.len();

        if blen % 8 != 0 {
            let q = blen / 8;
            let leading_zeroes_count = (q + 1) * 8 - blen;
            for _ in 0..leading_zeroes_count {
                binary.insert(0, '0');
            }
        }

        binary
    }

    fn new(hexa: &str) -> Tokenizer {
        let binary_digits = Tokenizer::binary_from_hexa(&hexa);

        let s = &binary_digits[3..6];
        let cur_operator_type = OperatorType::new(s);

        match cur_operator_type {
            OperatorType::Literal => PacketKind::Literal,
            _ => {
                let s = &binary_digits[6..7];
                let d = Tokenizer::decimal_from_binary(s);
                if d == 0 {
                    PacketKind::Length(d)
                } else {
                    PacketKind::Count(d)
                }
            }
        };

        Tokenizer {
            binary_digits,
            cur_operator_type,
            tokens: vec![],
            cur: 0,
            end_markers: HashMap::new(),
        }
    }

    fn has_more_tokens(&mut self) -> bool {
        if self.cur < self.binary_digits.len()
            && self.binary_digits.chars().skip(self.cur).any(|d| d == '1')
        {
            true
        } else {
            self.cur = self.binary_digits.len();
            false
        }
    }

    fn tokenize(&mut self) {
        while self.has_more_tokens() {
            if self.end_markers.contains_key(&self.cur) {
                for _ in 0..*self.end_markers.get(&self.cur).unwrap() {
                    self.tokens.push(Token::EndLength);
                }
            }

            self.read_version();
            self.read_type();

            match self.cur_operator_type {
                OperatorType::Sum => self.read_length_or_count(),
                OperatorType::Product => self.read_length_or_count(),
                OperatorType::Mininum => self.read_length_or_count(),
                OperatorType::Maximum => self.read_length_or_count(),
                OperatorType::Literal => self.read_literal(),
                OperatorType::GreaterThan => self.read_length_or_count(),
                OperatorType::LessThan => self.read_length_or_count(),
                OperatorType::EqualTo => self.read_length_or_count(),
            }
        }
    }

    fn read_version(&mut self) {
        let token_size = 3;
        // let s = &self.binary_digits[self.cur..self.cur + token_size];
        // let d = Tokenizer::decimal_from_binary(s);
        // let token = Token::Version(d);
        self.cur += token_size;
        // self.tokens.push(token)
    }

    fn read_type(&mut self) {
        let token_size = 3;
        let s = &self.binary_digits[self.cur..self.cur + token_size];
        self.cur_operator_type = OperatorType::new(s);
        let token = Token::Type(OperatorType::new(s));
        self.cur += token_size;
        self.tokens.push(token)
    }

    fn read_literal(&mut self) {
        let mut lit: String = String::new();

        while self.has_more_tokens() {
            lit.push_str(&self.binary_digits[self.cur + 1..self.cur + 5]);

            if self.binary_digits.chars().into_iter().nth(self.cur) == Some('0') {
                self.cur += 5;
                let token = Token::Literal(Tokenizer::decimal_from_binary(&lit));
                self.tokens.push(token);
                break;
            }

            self.cur += 5;
        }
    }

    fn read_length_or_count(&mut self) {
        let token_size = 1;
        let s = &self.binary_digits[self.cur..self.cur + token_size];
        let type_id = Tokenizer::decimal_from_binary(s);
        self.cur += token_size;

        let pk = if type_id == 0 {
            let token_size = 15;
            let s = &self.binary_digits[self.cur..self.cur + token_size];
            let d = Tokenizer::decimal_from_binary(s);
            self.cur += token_size;

            *self.end_markers.entry(self.cur + d).or_insert(0) += 1;
            self.tokens.push(Token::StartLength);

            PacketKind::Length(d)
        } else {
            let token_size = 11;
            let s = &self.binary_digits[self.cur..self.cur + token_size];
            let d = Tokenizer::decimal_from_binary(s);
            self.cur += token_size;
            PacketKind::Count(d)
        };

        let token = Token::Kind(pk);
        self.tokens.push(token)
    }
}

fn perform_operation(operator_type: &OperatorType, vec: &Vec<usize>) -> usize {
    match operator_type {
        OperatorType::Sum => vec.iter().sum(),
        OperatorType::Product => vec.iter().product(),
        OperatorType::Mininum => *vec.iter().min().unwrap(),
        OperatorType::Maximum => *vec.iter().max().unwrap(),
        OperatorType::Literal => {
            panic!("Unexpected literal hierarchy!");
        }
        OperatorType::GreaterThan => {
            if vec[0] > vec[1] {
                1
            } else {
                0
            }
        }
        OperatorType::LessThan => {
            if vec[0] < vec[1] {
                1
            } else {
                0
            }
        }
        OperatorType::EqualTo => {
            if vec[0] == vec[1] {
                1
            } else {
                0
            }
        }
    }
}

fn twin_literal_pair(tokens: &Vec<Token>, i: usize) -> Option<usize> {
    if i + 2 <= tokens.len() {
        let ts = &tokens[i..i + 2];

        match ts {
            [Token::Type(OperatorType::Literal), Token::Literal(value)] => Some(*value),
            _ => None,
        }
    } else {
        None
    }
}

fn twin_literal_pair_times(tokens: &Vec<Token>, i: usize, count: usize) -> Option<Vec<usize>> {
    let mut j = 0;

    let mut v: Vec<usize> = vec![];

    while i + j * 2 <= tokens.len() && j < count {
        if let Some(value) = twin_literal_pair(tokens, i + j * 2) {
            v.push(value);
        } else {
            return None;
        }

        j += 1;
    }

    if j == count {
        Some(v)
    } else {
        None
    }
}

fn is_count(tokens: &Vec<Token>, i: usize, is_twin: bool) -> Option<usize> {
    if i < tokens.len() {
        match tokens[i] {
            Token::Kind(PacketKind::Count(c)) => Some(c),
            Token::Kind(PacketKind::Length(_)) => {
                if is_twin {
                    Some(2)
                } else {
                    None
                }
            }
            _ => None,
        }
    } else {
        None
    }
}

fn is_twin_args_operator(tokens: &Vec<Token>, i: usize) -> bool {
    if i < tokens.len() {
        match tokens[i] {
            Token::Type(OperatorType::GreaterThan)
            | Token::Type(OperatorType::EqualTo)
            | Token::Type(OperatorType::LessThan) => true,
            _ => false,
        }
    } else {
        false
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let mut tokenizer = Tokenizer::new(&input);
        tokenizer.tokenize();

        let mut tokens = tokenizer.tokens;

        let mut len = tokens.len();

        loop {
            simplify(&mut tokens);

            if tokens.len() == len {
                break;
            } else {
                len = tokens.len()
            }
        }

        if tokens.len() == 2 {
            if let Some(Token::Literal(result)) = tokens.last() {
                return result.to_string();
            }
        }

        println!();
        println!();

        for t in tokens.iter() {
            println!("{:?}", t);
        }

        "Failed!".to_string()
    }
}

fn simplify(tokens: &mut Vec<Token>) {
    let mut i = 0_usize;

    loop {
        if i >= tokens.len() {
            break;
        }

        if i + 3 <= tokens.len() {
            match &tokens[i..i + 3] {
                [Token::Type(OperatorType::Literal), _, _] => {}
                [Token::Type(_), Token::StartLength, _] => {
                    let mut v: Vec<usize> = vec![];
                    let mut j = i + 3;

                    loop {

                        match &tokens[j] {
                            Token::Type(OperatorType::Literal) => {},
                            Token::Type(_) | Token::StartLength => {
                                break;
                            }

                            Token::EndLength => {
                                replace_with_literal(tokens, i, &mut v);

                                for _ in i..=j {
                                    tokens.remove(i + 2);
                                }

                                break;
                            }
                            Token::Literal(l) => {
                                v.push(*l);
                            }
                            _ => {
                                break;
                            }
                        }

                        j += 1;

                        if j == tokens.len() {

                            if v.len() > 1 { //last stream without EndLength
                        
                                replace_with_literal(tokens, i, &v);
        
                                for _ in 2 .. tokens.len() {
                                    tokens.remove(2);
                                }
        
                            }
        
                            break;
                        }

                    }

                }
                _ => {}
            }
        }

        if is_twin_args_operator(&tokens, i) {
            if let Some(_) = is_count(&tokens, i + 1, true) {
                if let Some(v) = twin_literal_pair_times(&tokens, i + 2, 2) {
                    replace_with_literal(tokens, i, &v);

                    for _ in 0..6 {
                        tokens.remove(i + 2);
                    }
                }
            } else {
                if let Some(v) = twin_literal_pair_times(&tokens, i + 1, 2) {
                    replace_with_literal(tokens, i, &v);

                    for _ in 0..5 {
                        tokens.remove(i + 2);
                    }
                }
            }
        } else {
            if let Some(c) = is_count(&*tokens, i + 1, false) {
                if let Some(v) = twin_literal_pair_times(&tokens, i + 2, c) {
                    replace_with_literal(tokens, i, &v);
                    for _ in 0..2 + c * 2 {
                        tokens.remove(i + 2);
                    }
                }
            }
        };

        i += 1;
    }
}

fn replace_with_literal(tokens: &mut Vec<Token>, i: usize, v: &Vec<usize>) {
    if i < tokens.len() {
        match &tokens[i] {
            Token::Type(OperatorType::Literal) => {}
            Token::StartLength => {}
            Token::EndLength => {}
            Token::Kind(_) => {}
            Token::Literal(_) => {}
            Token::Type(op) => {
                let result = perform_operation(&op, &v);
                tokens.insert(i, Token::Type(OperatorType::Literal));
                tokens.insert(i + 1, Token::Literal(result));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let part = PartTwo {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
