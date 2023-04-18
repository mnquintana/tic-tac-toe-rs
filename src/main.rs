use crate::game::TicTacToe;
use std::io;

mod game;

fn main() -> io::Result<()> {
    let mut game = TicTacToe::new();
    game.start()?;

    Ok(())
}
