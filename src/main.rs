use game::TicTacToe;
use std::io;

mod game;
mod grid;
mod location;

fn main() -> io::Result<()> {
    let grid_size = loop {
        game::clear_screen();

        println!("What size grid would you like to play on? (e.g. 3, for a 3x3 grid): ");

        let mut grid_size = String::new();
        io::stdin().read_line(&mut grid_size)?;

        if let Ok(grid_size) = grid_size.trim().parse() {
            if (1..=26).contains(&grid_size) {
                break grid_size;
            }
        }
    };

    let mut game = TicTacToe::new(grid_size);
    game.start()?;

    Ok(())
}
