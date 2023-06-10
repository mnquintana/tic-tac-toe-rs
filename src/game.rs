use crate::grid::Grid;
use core::time;
use regex::Regex;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::io;
use std::str::FromStr;
use std::thread;

/// A space on a Tic Tac Toe [Grid]. During their turn, a [Player] makes
/// their move on a space.
///
/// A space can only be occupied by one Player, or else occupied by no one.
#[derive(Debug, PartialEq, Default, Copy, Clone)]
pub struct Space(pub Option<Player>);

impl Display for Space {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let space = match self.0 {
            None => "_",
            Some(Player::O) => "O",
            Some(Player::X) => "X",
        };

        write!(f, "{space}")
    }
}

/// One of the 2 possible players: X and O.
#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq)]
pub enum Player {
    X,
    O,
}

impl Display for Player {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

/// The outcome of a [Game].
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Outcome {
    InProgress,
    Draw,
    Win(Player),
}

impl Display for Outcome {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Outcome::Draw => writeln!(f, "Draw"),
            Outcome::InProgress => writeln!(f, "Match in progress"),
            Outcome::Win(p) => writeln!(f, "{p} wins!"),
        }
    }
}

/// Represents a row / column coordinate on the [Grid].
#[derive(Debug, PartialEq)]
pub struct Location(pub u8, pub u8);

impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
/// The [Location] could not be parsed from the string.
#[derive(Debug, Clone)]
pub struct LocationParseStrError {
    loc: String,
}

impl Display for LocationParseStrError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Location cannot be parsed from {}. Locations must be of the format [a-c][1-3], e.g. a1", self.loc)
    }
}

impl FromStr for Location {
    type Err = LocationParseStrError;

    fn from_str(loc: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^([a-c])([1-3])$").map_err(|_| LocationParseStrError {
            loc: loc.to_string(),
        })?;
        let loc = loc.to_lowercase();
        let text = loc.trim();
        let captures = re.captures(text).ok_or(LocationParseStrError {
            loc: loc.to_string(),
        })?;

        let column = captures
            .get(1)
            .ok_or(LocationParseStrError {
                loc: loc.to_string(),
            })?
            .as_str();
        let row = captures
            .get(2)
            .ok_or(LocationParseStrError {
                loc: loc.to_string(),
            })?
            .as_str();

        let column = match column {
            "a" => 0,
            "b" => 1,
            "c" => 2,
            _ => unreachable!(),
        };
        let row = row.parse::<u8>().map_err(|_| LocationParseStrError {
            loc: loc.to_string(),
        })? - 1;

        Ok(Self(column, row))
    }
}

/// The score for both [Player]s after `n` games of Tic Tac Toe.
#[derive(Debug)]
struct Score(HashMap<Player, u32>);

impl Score {
    pub fn new() -> Self {
        let inner = HashMap::from([(Player::X, 0), (Player::O, 0)]);

        Self(inner)
    }
}

impl Display for Score {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = self
            .0
            .iter()
            .map(|(&player, &score)| format!("{player}: {score}\n"))
            .collect::<String>();

        writeln!(f, "SCORE:")?;
        writeln!(f, "{output}")
    }
}

/// Manages games of Tic Tac Toe.
/// Holds state that persists between games.
///
/// Only one concurrent game of Tic Tac Toe is allowed at a time.
pub struct TicTacToe {
    score: Score,
    game: Game,
}

impl TicTacToe {
    pub fn new() -> Self {
        Self {
            score: Score::new(),
            game: Game::new(),
        }
    }

    /// Starts the main Tic Tic Tac Toe game loop, managing the lifecycle of [Game]s
    /// and keeping track of any cross-game state (like the [Score]).
    ///
    /// When the game is over (ie. has an [Outcome] other than [`Outcome::InProgress`]),
    /// the game loop will drop the currently active [Game] and create a new one.
    pub fn start(&mut self) -> io::Result<()> {
        loop {
            self.game.start()?;

            let outcome = &self.game.outcome();

            if let Outcome::Win(p) = outcome {
                let score = self.score.0.entry(*p).or_insert(0);
                *score += 1;
            }

            thread::sleep(time::Duration::from_secs(2));

            clear_screen();
            println!("{}", self.score);

            thread::sleep(time::Duration::from_secs(2));

            println!("\nStarting new game...");

            thread::sleep(time::Duration::from_secs(1));

            self.game = Game::new();
        }
    }
}

/// A single game of Tic Tac Toe.
pub struct Game {
    outcome: Outcome,

    /// A counter tracking what turn number it is. Each player's turn
    /// increments the counter by 1.
    turn: u32,
    current_player: Player,

    /// A grid representing the Tic Tac Toe board.
    grid: Grid,
}

impl Game {
    pub fn new() -> Self {
        Self {
            outcome: Outcome::InProgress,
            current_player: Player::X,
            turn: 1,
            grid: Grid::new(3),
        }
    }

    /// Starts the main loop for a single game of Tic Tac Toe.
    ///
    /// It checks for user input on the command line,
    /// parses it, and passes it on to the rest of the game logic
    /// for each turn, until the game has an [`Outcome`].
    pub fn start(&mut self) -> io::Result<()> {
        while self.outcome == Outcome::InProgress {
            let current_player = self.current_player();

            clear_screen();

            println!("TURN {} - Player {}\n", self.turn, current_player);
            println!("{}", self.grid);

            println!("Where do you want to make a move? (e.g. A1): ");

            let mut input = String::new();
            io::stdin().read_line(&mut input)?;

            if let Ok(loc) = input.parse() {
                if self.make_move(&loc).is_ok() {
                    self.outcome = self.determine_outcome();

                    if self.outcome == Outcome::InProgress {
                        self.advance_turn();
                    }
                }
            }
        }

        clear_screen();

        println!("TURN {} - Player {}\n", self.turn, self.current_player);
        println!("{}", self.grid);

        println!("RESULT: {}", self.outcome);

        Ok(())
    }

    /// Advances to the next turn.
    pub fn advance_turn(&mut self) {
        self.current_player = self.next_player();
        self.turn += 1;
    }

    /// Makes a move at a grid [Location] for the current [Player].
    pub fn make_move(&mut self, loc: &Location) -> Result<(), String> {
        self.grid.update(loc, self.current_player)
    }

    /// Gets the current player for the current turn.
    pub fn current_player(&self) -> Player {
        self.current_player
    }

    /// Gets the player for the next turn.
    pub(self) fn next_player(&self) -> Player {
        match self.current_player {
            Player::X => Player::O,
            Player::O => Player::X,
        }
    }

    /// Checks if any player has won on the two diagonals.
    pub(self) fn check_diagonals(&self) -> bool {
        self.grid.diagonals().any(|diagonal| {
            diagonal.iter().all(|&space| {
                let first_space = diagonal.first().map(|s| **s).unwrap_or_default();

                *space == first_space && space.0.is_some()
            })
        })
    }

    /// Checks if any [Player] has won in any of the rows on the game board.
    pub(self) fn check_rows(&self) -> bool {
        self.grid.rows().any(|row| {
            row.iter().all(|space| {
                let first_space = row.first().copied().unwrap_or_default();

                *space == first_space && space.0.is_some()
            })
        })
    }

    /// Checks if any [Player] has won in any of the columns on the game board.
    pub(self) fn check_columns(&self) -> bool {
        self.grid.columns().any(|column| {
            column.iter().all(|space| {
                let first_space = column.first().copied().unwrap_or_default();
                *space == first_space && space.0.is_some()
            })
        })
    }

    /// Checks if any [Player] has any column, row, or diagonal on the game board.
    pub(self) fn check_for_win(&self) -> bool {
        self.check_columns() || self.check_rows() || self.check_diagonals()
    }

    /// Checks if the game has a Draw.
    ///
    /// There's a draw if there's no win and every space is occupied.
    pub(self) fn check_for_draw(&self) -> bool {
        !self.check_for_win()
            && !self
                .grid
                .rows()
                .any(|row| row.iter().any(|&space| space.0.is_none()))
    }

    /// Gets the outcome of the game.
    pub fn outcome(&self) -> Outcome {
        self.outcome
    }

    /// Determines the outcome of the game based on the current board state
    /// and the current player.
    pub fn determine_outcome(&mut self) -> Outcome {
        let player = self.current_player;

        if self.check_for_win() {
            Outcome::Win(player)
        } else if self.check_for_draw() {
            Outcome::Draw
        } else {
            Outcome::InProgress
        }
    }
}

/// Clears the terminal screen. Useful for faking a frame of animation after
/// making some change.
fn clear_screen() {
    // Clears the screen by sending a control character,
    // courtesy of https://stackoverflow.com/questions/34837011/how-to-clear-the-terminal-screen-in-rust-after-a-new-line-is-printed
    print!("\x1B[2J\x1B[1;1H");
}

#[cfg(test)]
mod tests {
    use crate::game::*;

    #[test]
    fn test_location_from_str() {
        assert_eq!("A1".parse::<Location>().unwrap(), Location(0, 0));
        assert_eq!("B1".parse::<Location>().unwrap(), Location(1, 0));
        assert_eq!("C1".parse::<Location>().unwrap(), Location(2, 0));
        assert_eq!("A2".parse::<Location>().unwrap(), Location(0, 1));
        assert_eq!("B2".parse::<Location>().unwrap(), Location(1, 1));
        assert_eq!("C2".parse::<Location>().unwrap(), Location(2, 1));
        assert_eq!("A3".parse::<Location>().unwrap(), Location(0, 2));
        assert_eq!("B3".parse::<Location>().unwrap(), Location(1, 2));
        assert_eq!("C3".parse::<Location>().unwrap(), Location(2, 2));
    }

    #[test]
    fn test_location_from_almost_valid_input() {
        assert!("B22222".parse::<Location>().is_err());
        assert!("AA2".parse::<Location>().is_err());
    }

    #[test]
    fn test_location_from_invalid_column() {
        assert!("D1".parse::<Location>().is_err())
    }

    #[test]
    fn test_location_from_invalid_row() {
        assert!("C4".parse::<Location>().is_err());
    }

    #[test]
    fn test_grid_get() {
        let mut grid = Grid::default();

        assert_eq!(grid.get(&"A1".parse().unwrap()), Some(&Space(None)));

        assert!(grid.update(&"A1".parse().unwrap(), Player::X).is_ok());

        assert_eq!(
            grid.get(&"A1".parse().unwrap()),
            Some(&Space(Some(Player::X)))
        );
    }

    #[test]
    fn test_grid_get_mut() {
        let mut grid = Grid::default();

        assert_eq!(grid.get_mut(&"A1".parse().unwrap()), Some(&mut Space(None)));

        assert!(grid.update(&"A1".parse().unwrap(), Player::X).is_ok());

        assert_eq!(
            grid.get(&"A1".parse().unwrap()),
            Some(&Space(Some(Player::X)))
        );
    }

    #[test]
    fn test_check_diagonals() {
        for player in [Player::X, Player::O] {
            let mut game = Game::new();

            assert!(!game.check_diagonals());

            // Primary diagonal
            assert!(game.grid.update(&"A1".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"B2".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"C3".parse().unwrap(), player).is_ok());

            assert!(game.check_diagonals());

            let mut game = Game::new();

            // Secondary diagonal
            assert!(game.grid.update(&"C1".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"B2".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"A3".parse().unwrap(), player).is_ok());

            assert!(game.check_diagonals());
        }
    }

    #[test]
    fn test_check_rows() {
        for player in [Player::X, Player::O] {
            let mut game = Game::new();

            assert!(!game.check_rows());

            assert!(game.grid.update(&"A1".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"B1".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"C1".parse().unwrap(), player).is_ok());

            assert!(game.check_rows());
        }
    }

    #[test]
    fn test_check_columns() {
        for player in [Player::X, Player::O] {
            let mut game = Game::new();

            assert!(!game.check_columns());

            assert!(game.grid.update(&"A1".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"A2".parse().unwrap(), player).is_ok());
            assert!(game.grid.update(&"A3".parse().unwrap(), player).is_ok());

            assert!(game.check_columns());
        }
    }

    #[test]
    fn test_outcome() {
        let mut game = Game::new();

        assert_eq!(game.determine_outcome(), Outcome::InProgress);
        assert_eq!(game.current_player(), Player::X);

        assert!(game.make_move(&"A1".parse().unwrap()).is_ok());
        assert!(game.make_move(&"A2".parse().unwrap()).is_ok());
        assert!(game.make_move(&"A3".parse().unwrap()).is_ok());

        assert_eq!(game.current_player(), Player::X);
        assert_eq!(game.determine_outcome(), Outcome::Win(Player::X));

        let mut game = Game::new();

        assert_eq!(game.determine_outcome(), Outcome::InProgress);
        assert_eq!(game.current_player(), Player::X);

        game.advance_turn();

        assert_eq!(game.current_player(), Player::O);

        assert!(game.make_move(&"A1".parse().unwrap()).is_ok());
        assert!(game.make_move(&"B2".parse().unwrap()).is_ok());
        assert!(game.make_move(&"C3".parse().unwrap()).is_ok());

        assert_eq!(game.current_player(), Player::O);
        assert_eq!(game.determine_outcome(), Outcome::Win(Player::O));
    }
}
