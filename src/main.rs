use crate::game::TicTacToe;
use std::io;

mod game;
mod grid;
mod location;

fn main() -> io::Result<()> {
    println!("What size grid would you like to play on? (e.g. 3, for a 3x3 grid): ");

    let mut grid_size = String::new();
    io::stdin().read_line(&mut grid_size)?;

    let grid_size: u8 = grid_size.trim().parse().unwrap();

    let mut game = TicTacToe::new(grid_size);
    game.start()?;

    Ok(())
}
