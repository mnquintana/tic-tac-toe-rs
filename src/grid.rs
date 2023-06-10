use crate::game::{Location, Player, Space};
use std::fmt::{self, Display};

/// Represents the Tic Tac Toe game board.
#[derive(Debug, Default, PartialEq)]
pub struct Grid(pub [[Space; 3]; 3]);

impl Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

        writeln!(f, "{result}")
    }
}

impl Grid {
    pub fn new() -> Self {
        Grid::default()
    }

    pub fn rows(&self) -> impl Iterator<Item = &[Space; 3]> {
        self.0.iter()
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
    pub fn update(&mut self, loc: &Location, player: Player) -> Result<(), String> {
        let default = &mut Space(None);
        let space_to_update = self.get_mut(loc).unwrap_or(default);

        if space_to_update.0.is_none() {
            *space_to_update = Space(Some(player));
            Ok(())
        } else {
            Err(format!(
                "The space at {loc} is already occupied by {}",
                space_to_update.0.expect("Space is Some()")
            ))
        }
    }
}
