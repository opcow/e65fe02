use ascii::{AsciiString, ToAsciiChar};
// use std::io::prelude::*;
use std::io::{stdout, Write};

// printing as 7-bit ascii for antiquated assemblers
pub fn print_ascii(s: &str) {
    stdout()
        .write_all(
            s.chars()
                .map(|c| c.to_ascii_char().unwrap())
                .collect::<AsciiString>()
                .as_bytes(),
        ).expect("### Failed to write to stdout! ###");
}
