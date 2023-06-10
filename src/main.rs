use crate::game::TicTacToe;
use std::io;

mod game;
mod grid;

fn main() -> io::Result<()> {
    let mut game = TicTacToe::new();
    game.start()?;

    Ok(())
}
