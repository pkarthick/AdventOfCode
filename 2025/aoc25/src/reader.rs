use unscanny::Scanner;

pub struct Reader<'a> {
    scanner: Scanner<'a>
}

impl <'a> Reader<'a> {

    pub fn new(data: &'a str) -> Self {
        Self { scanner: Scanner::new(data) }
    }

    pub fn done(&self) -> bool {
        self.scanner.done()
    }

    pub fn read_char(&mut self) -> Option<char> {
        if !self.scanner.done() {
            self.scanner.eat()
        } else {
            None
        }
    }

    pub fn read_int(&mut self) -> Option<i64> {
        if !self.scanner.done() {
            let digits = self.scanner.eat_while(char::is_ascii_digit);
            match digits.parse::<i64>() {
                Ok(i) => Some(i),
                Err(_) => None,
            }
        } else {
            None
        }
    }

    pub fn read_if(&mut self, ch: char) -> bool {
        if !self.scanner.done() {
            if let Some(c) = self.scanner.peek() && c == ch {
                let _ = self.scanner.eat();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

}