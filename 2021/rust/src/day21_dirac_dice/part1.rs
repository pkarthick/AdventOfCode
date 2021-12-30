use core::*;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        21
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

struct Player {
    position: usize,
    score: usize,
}

impl Player {
    fn move_position(&mut self, face: usize) {
        self.position += face % 10;

        if self.position > 10 {
            self.position %= 10;
        }
    }

    fn has_won(&self) -> bool {
        self.score >= 1000
    }

    fn update_score(&mut self) {
        self.score += self.position;
    }

}

struct DiracDice {
    player1: Player,
    player2: Player,
    turns: usize,
    face: usize,
}

impl DiracDice {
    fn new(input: String) -> Self {
        let mut players: Vec<(usize, usize)> = input
            .lines()
            .map(|s| {
                let xs: Vec<&str> = s.split_ascii_whitespace().collect();
                let player_index = xs[1].parse::<usize>().unwrap();
                let player_pos = xs.iter().last().unwrap().parse::<usize>().unwrap();
                (player_index, player_pos)
            })
            .collect();

        players.sort_by(|p1, p2| p1.0.cmp(&p2.0));

        DiracDice {
            player1: Player {
                position: players[0].1,
                score: 0,
            },
            player2: Player {
                position: players[1].1,
                score: 0,
            },
            turns: 0,
            face: 0,
        }
    }

    fn get_result(&self) -> usize {
        if self.player1.score >= 1000 {
            self.player2.score * self.turns
        } else {
            self.player1.score * self.turns
        }
    }

    fn throw_dice(&mut self) {
        self.face += 1;
        self.turns += 1;
    }

    fn play(&mut self) {
        loop {

            for _ in 0 .. 3 {
                self.throw_dice();
                self.player1.move_position(self.face);
            }

            self.player1.update_score();

            if self.player1.has_won() {
                break;
            }

            for _ in 0 .. 3 {
                self.throw_dice();
                self.player2.move_position(self.face);
            }

            self.player2.update_score();

            if self.player2.has_won() {
                break;
            }

        }
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let mut dice = DiracDice::new(input);
        dice.play();
        dice.get_result().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let part = PartOne {};

        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
