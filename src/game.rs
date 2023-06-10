use core::time;
use regex::Regex;
use std::collections::HashMap;
use std::fmt::Display;
use std::io;
use std::str::FromStr;
use std::thread;

/// A space on a Tic Tac Toe [Grid]. During their turn, a [Player] makes
/// their move on a space.
///
/// A space can only be occupied by one Player, or else occupied by no one.
#[derive(Debug, PartialEq, Default, Copy, Clone)]
pub struct Space(Option<Player>);

/// One of the 2 possible players: X and O.
#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq)]
pub enum Player {
    X,
    O,
}

impl Display for Player {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

/// The [Location] could not be parsed from the string.
#[derive(Debug, Clone)]
pub struct LocationParseStrError {
    loc: String,
}

impl Display for LocationParseStrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

/// Represents the Tic Tac Toe game board.
#[derive(Debug, Default, PartialEq)]
pub struct Grid([[Space; 3]; 3]);

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Grid(grid) = self;

        let mut result = grid
            .iter()
            .enumerate()
            .map(|(i, row)| {
                let mut row = row
                    .iter()
                    .map(|space| match space {
                        Space(None) => "_".to_string(),
                        Space(Some(Player::O)) => "O".to_string(),
                        Space(Some(Player::X)) => "X".to_string(),
                    })
                    .collect::<Vec<_>>();

                row.insert(0, format!("{}", i + 1));

                row.join(" | ")
            })
            .collect::<Vec<_>>();

        result.insert(0, "    A | B | C".to_string());

        let result = result.join("\n");

        writeln!(f, "{}", result)
    }
}

impl Grid {
    pub fn new() -> Self {
        Default::default()
    }

    /// Gets a read-only reference to a [Space] on the game board.
    /// Useful for reading, but not modifying, the state of the board.
    pub fn get(&self, loc: &Location) -> Option<&Space> {
        let Self(grid) = self;
        let Location(x, y) = loc;

        grid.get(*y as usize)?.get(*x as usize)
    }

    /// Gets a mutable reference to a [Space] on the game board.
    ///
    /// Useful for making changes to the game board, like a player making a move on their turn.
    pub fn get_mut(&mut self, loc: &Location) -> Option<&mut Space> {
        let Self(grid) = self;
        let Location(x, y) = loc;

        grid.get_mut(*y as usize)?.get_mut(*x as usize)
    }

    /// Updates a [Space] at a [Location] to be occupied by the provided [Player].
    pub fn update(&mut self, loc: Location, player: Player) -> bool {
        let space_to_update = self.get_mut(&loc);
        let default = &mut Space(None);
        let space_to_update = space_to_update.unwrap_or(default);

        if space_to_update.0.is_none() {
            *space_to_update = Space(Some(player));
            true
        } else {
            false
        }
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
        writeln!(f, "{}", output)
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
    /// When the game is over (ie. has an [Outcome] other than [Outcome::InProgress]),
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
    pub turn: u32,
    current_player: Player,

    /// A grid representing the Tic Tac Toe board.
    pub(self) grid: Grid,
}

impl Game {
    pub fn new() -> Self {
        Self {
            outcome: Outcome::InProgress,
            current_player: Player::X,
            turn: 1,
            grid: Grid::new(),
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

            let loc = input.parse();
            let did_update = match loc {
                Err(_) => continue,
                Ok(loc) => self.make_move(loc),
            };

            if did_update {
                self.outcome = self.determine_outcome();

                if self.outcome == Outcome::InProgress {
                    self.advance_turn();
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
    pub fn make_move(&mut self, loc: Location) -> bool {
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
        let Grid(grid) = &self.grid;

        let primary: Vec<_> = (0..grid.len())
            .map(|n| {
                self.grid
                    .get(&Location(n as u8, n as u8))
                    .unwrap_or(&Space(None))
            })
            .collect();

        let secondary: Vec<_> = (0..grid.len())
            .rev()
            .map(|n| {
                self.grid
                    .get(&Location(n as u8, n as u8))
                    .unwrap_or(&Space(None))
            })
            .collect();

        let diagonals = vec![primary, secondary];

        diagonals.iter().any(|diagonal| {
            diagonal.iter().all(|&space| {
                let first_space = diagonal.first().map(|s| **s).unwrap_or_default();

                *space == first_space && space.0.is_some()
            })
        })
    }

    /// Checks if any [Player] has won in any of the rows on the game board.
    pub(self) fn check_rows(&self) -> bool {
        let Grid(grid) = &self.grid;

        grid.iter().any(|row| {
            row.iter().all(|space| {
                let first_space = row.first().copied().unwrap_or_default();

                *space == first_space && space.0.is_some()
            })
        })
    }

    /// Checks if any [Player] has won in any of the columns on the game board.
    pub(self) fn check_columns(&self) -> bool {
        let Grid(grid) = &self.grid;

        (0..grid.len())
            .map(|i| grid.iter().map(|inner| inner[i]).collect::<Vec<_>>())
            .any(|column| {
                column.iter().all(|space| {
                    let first_space = column.first().copied().unwrap_or_default();
                    *space == first_space && space.0.is_some()
                })
            })
    }

    /// Checks if any [Player] has any column, row, or diagonal on the game board.
    pub(self) fn check_for_win(&self) -> bool {
        [
            self.check_columns(),
            self.check_rows(),
            self.check_diagonals(),
        ]
        .into_iter()
        .any(|dimension| dimension)
    }

    /// Checks if the game has a Draw.
    ///
    /// There's a draw if there's no win and every space is occupied.
    pub(self) fn check_for_draw(&self) -> bool {
        let Grid(grid) = &self.grid;

        !self.check_for_win()
            && !grid
                .iter()
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
    fn test_grid_default() {
        let default_grid = Grid([
            [Space(None), Space(None), Space(None)],
            [Space(None), Space(None), Space(None)],
            [Space(None), Space(None), Space(None)],
        ]);

        assert_eq!(Grid::default(), default_grid);
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
        let mut grid = Grid::new();

        assert_eq!(grid.get(&"A1".parse().unwrap()), Some(&Space(None)));

        grid.update("A1".parse().unwrap(), Player::X);

        assert_eq!(
            grid.get(&"A1".parse().unwrap()),
            Some(&Space(Some(Player::X)))
        );
    }

    #[test]
    fn test_grid_get_mut() {
        let mut grid = Grid::new();

        assert_eq!(grid.get_mut(&"A1".parse().unwrap()), Some(&mut Space(None)));

        grid.update("A1".parse().unwrap(), Player::X);

        assert_eq!(
            grid.get(&"A1".parse().unwrap()),
            Some(&Space(Some(Player::X)))
        );
    }

    #[test]
    fn test_check_diagonals() {
        let mut game = Game::new();

        assert_eq!(game.check_diagonals(), false);

        // Primary diagonal
        game.grid.update("A1".parse().unwrap(), Player::X);
        game.grid.update("B2".parse().unwrap(), Player::X);
        game.grid.update("C3".parse().unwrap(), Player::X);

        assert_eq!(game.check_diagonals(), true);

        // Secondary diagonal
        game.grid.update("C1".parse().unwrap(), Player::X);
        game.grid.update("B2".parse().unwrap(), Player::X);
        game.grid.update("A3".parse().unwrap(), Player::X);

        assert_eq!(game.check_diagonals(), true);
    }

    #[test]
    fn test_check_rows() {
        let mut game = Game::new();

        assert_eq!(game.check_rows(), false);

        game.grid.update("A1".parse().unwrap(), Player::X);
        game.grid.update("B1".parse().unwrap(), Player::X);
        game.grid.update("C1".parse().unwrap(), Player::X);

        assert_eq!(game.check_rows(), true);
    }

    #[test]
    fn test_check_columns() {
        let mut game = Game::new();

        assert_eq!(game.check_columns(), false);

        game.grid.update("A1".parse().unwrap(), Player::X);
        game.grid.update("A2".parse().unwrap(), Player::X);
        game.grid.update("A3".parse().unwrap(), Player::X);

        assert_eq!(game.check_columns(), true);
    }

    #[test]
    fn test_outcome() {
        let mut game = Game::new();

        assert_eq!(game.determine_outcome(), Outcome::InProgress);
        assert_eq!(game.current_player(), Player::X);

        game.make_move("A1".parse().unwrap());
        game.make_move("A2".parse().unwrap());
        game.make_move("A3".parse().unwrap());

        assert_eq!(game.current_player(), Player::X);
        assert_eq!(game.determine_outcome(), Outcome::Win(Player::X));

        let mut game = Game::new();

        assert_eq!(game.determine_outcome(), Outcome::InProgress);
        assert_eq!(game.current_player(), Player::X);

        game.advance_turn();

        assert_eq!(game.current_player(), Player::O);

        game.make_move("A1".parse().unwrap());
        game.make_move("B2".parse().unwrap());
        game.make_move("C3".parse().unwrap());

        assert_eq!(game.current_player(), Player::O);
        assert_eq!(game.determine_outcome(), Outcome::Win(Player::O));
    }
}
