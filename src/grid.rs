use crate::game::{Player, Space};
use crate::location::Location;
use std::fmt::{self, Display};

// To get uppercase A-Z starting at 0
const CHAR_OFFSET: u8 = 65;

/// Represents the Tic Tac Toe game board.
#[derive(Debug, PartialEq)]
pub struct Grid(Vec<Vec<Space>>);

impl Default for Grid {
    fn default() -> Self {
        Grid(vec![
            vec![Space::default(), Space::default(), Space::default()],
            vec![Space::default(), Space::default(), Space::default()],
            vec![Space::default(), Space::default(), Space::default()],
        ])
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Grid(grid) = self;

        let column_header = (0..self.size())
            .map(|i| (i + CHAR_OFFSET) as char)
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(" | ");

        let column_header = std::iter::once(format!("    {column_header}"));

        let result = column_header
            .chain(grid.iter().enumerate().map(|(i, row)| {
                let row_header = std::iter::once(format!("{}", i + 1));

                let row = row_header
                    .chain(row.iter().map(|space| space.to_string()))
                    .collect::<Vec<_>>();

                row.join(" | ")
            }))
            .collect::<Vec<_>>();

        let result = result.join("\n");

        writeln!(f, "{result}")
    }
}

impl Grid {
    pub fn new(size: u8) -> Self {
        assert!(
            (1..=26).contains(&size),
            "Grid size must be between 1 and 26 to allow for alphabetic indexing"
        );

        let grid = (0..size)
            .map(|_| (0..size).map(|_| Space::default()).collect())
            .collect();

        Grid(grid)
    }

    pub fn size(&self) -> u8 {
        self.0.len() as u8
    }

    pub fn rows(&self) -> impl Iterator<Item = Vec<Space>> + '_ {
        self.0.iter().map(|row| row.to_vec())
    }

    pub fn columns(&self) -> impl Iterator<Item = Vec<Space>> + '_ {
        (0..self.0.len()).map(|i| self.0.iter().map(|inner| inner[i]).collect::<Vec<_>>())
    }

    pub fn diagonals(&self) -> impl Iterator<Item = Vec<&Space>> + '_ {
        let primary: Vec<_> = (0..self.0.len())
            .map(|n| {
                self.get(&Location(n as u8, n as u8))
                    .unwrap_or(&Space(None))
            })
            .collect();

        let secondary: Vec<_> = (0..self.0.len())
            .rev()
            .map(|n| {
                let loc = Location(n as u8, (self.0.len() - n - 1) as u8);

                self.get(&loc).unwrap_or(&Space(None))
            })
            .collect();

        vec![primary, secondary].into_iter()
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

    /// Updates a [`Space`] at a [`Location`] to be occupied by the provided [`Player`].
    pub fn update(&mut self, loc: &Location, player: Player) -> Result<(), String> {
        let space_to_update = self
            .get_mut(loc)
            .ok_or(format!("The space at {loc} is out of bounds"))?;

        if space_to_update.is_none() {
            *space_to_update = Space(Some(player));
            Ok(())
        } else {
            Err(format!(
                "The space at {loc} is already occupied by {}",
                space_to_update.expect("Space is Some()")
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grid_default() {
        let default_grid = Grid(vec![
            vec![Space(None), Space(None), Space(None)],
            vec![Space(None), Space(None), Space(None)],
            vec![Space(None), Space(None), Space(None)],
        ]);

        assert_eq!(Grid::default(), default_grid);
    }

    #[test]
    fn new_grid_of_size_3_is_default() {
        assert_eq!(Grid::new(3), Grid::default())
    }

    #[test]
    #[should_panic]
    fn disallows_size_0_grids() {
        let _grid = Grid::new(0);
    }

    #[test]
    #[should_panic]
    fn disallows_size_27_grids() {
        let _grid = Grid::new(27);
    }

    #[test]
    fn grid_new() {
        let grid_1 = Grid(vec![vec![Space(None)]]);
        assert_eq!(Grid::new(1), grid_1);

        let grid_4 = Grid(vec![
            vec![Space(None), Space(None), Space(None), Space(None)],
            vec![Space(None), Space(None), Space(None), Space(None)],
            vec![Space(None), Space(None), Space(None), Space(None)],
            vec![Space(None), Space(None), Space(None), Space(None)],
        ]);
        assert_eq!(Grid::new(4), grid_4);
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
    fn grid_update() {
        let mut grid = Grid::default();

        assert!(
            grid.update(&"A6".parse().unwrap(), Player::X).is_err(),
            "Out of bounds locations should error out"
        );
    }
}
