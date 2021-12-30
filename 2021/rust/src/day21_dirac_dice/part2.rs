use core::*;
use std::collections::HashMap;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        21
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

#[derive(Clone)]
struct Player {
    position: usize,
    score: usize,
    turns: usize,
    scores: Vec<usize>,
}

impl Player {
    fn new(position: usize) -> Self {
        Player {
            position,
            score: 0,
            turns: 0,
            scores: vec![],
        }
    }

    fn move_position(&mut self, face: usize) {
        if self.turns == 3 {
            self.turns = 0;
        }

        self.position += face % 10;

        if self.position > 10 {
            self.position %= 10;
        }

        self.turns += 1;

        if self.turns == 3 {
            self.update_score();
        }
    }

    fn has_won(&self) -> bool {
        self.score >= 21
    }

    fn update_score(&mut self) {
        self.score += self.position;
        self.scores.push(self.score)
    }
    
    fn explore_position(&mut self) {

        let mut counts : HashMap<usize, usize>  = HashMap::new();

        for i in 1_usize ..= 3 {
            for j in 1_usize ..= 3 {
                for k in 1_usize ..= 3 {
                    *counts.entry(i+j+k).or_insert(0) += 1;
                }
            }
        }

        for (k, v) in counts.iter() {
            self.position += v;
            self.position %= 10;
        }

        loop {
            if self.has_won() {
                break;
            }

            for face in 0..3 {
                self.move_position(face);

            }
        }
    }
}

#[derive(Clone)]
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
            player1: Player::new(players[0].1),
            player2: Player::new(players[1].1),
            turns: 0,
            face: 0,
        }
    }


    fn play(&mut self, player1_wins: &mut usize, player2_wins: &mut usize) {
        if self.player1.has_won() || self.player2.has_won() {
            return;
        } else {
            loop {
                for _ in self.player1.turns..3 {
                    let mut won: bool = false;

                    for face in 1..=3 {
                        let mut universe = self.clone();
                        universe.player1.move_position(face);
                        universe.play(player1_wins, player2_wins);

                        if universe.player1.has_won() {
                            *player1_wins += 1;
                            won = true;
                        }

                        if universe.player2.has_won() {
                            *player2_wins += 1;
                            won = true;
                        }
                    }

                    if won {
                        return;
                    }
                }

                for _ in self.player2.turns..3 {
                    let mut won: bool = false;

                    for face in 1..=3 {
                        let mut universe = self.clone();
                        universe.player2.move_position(face);
                        universe.play(player1_wins, player2_wins);

                        if universe.player1.has_won() {
                            *player1_wins += 1;
                            won = true;
                        }

                        if universe.player2.has_won() {
                            *player2_wins += 1;
                            won = true;
                        }
                    }

                    if won {
                        return;
                    }
                }

                self.player1.turns = 0;
                self.player2.turns = 0;
            }
        }
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let mut player1_wins = 0_usize;
        let mut player2_wins = 0_usize;

        let mut dice = DiracDice::new(input);

        dice.play(&mut player1_wins, &mut player2_wins);

        let wins = if player1_wins > player2_wins {
            player1_wins
        } else {
            player2_wins
        };

        wins.to_string()
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
