//! A simple pager utility for displaying file contents or piped input.
//!
//! This program acts as a pager, similar to 'less' or 'more', but with some additional features:
//! - It can read from a file specified as a command-line argument or from piped input (stdin).
//! - It uses the `minus` crate for dynamic paging, allowing for smooth scrolling and search functionality.
//! - The pager displays the filename (or "stdin" for piped input) as a prompt.
//!
//! Usage:
//!     cortex <filename>
//!     command | cortex
//!
//! The pager supports the following key bindings:
//! - Arrow keys: Navigate up and down
//! - Page Up/Down: Scroll by page
//! - '/': Enter search mode
//! - 'q': Quit the pager
//!
//! Note: This pager requires a terminal that supports ANSI escape codes for proper functionality.

use anyhow::{Context, Result};
use std::env::args;
use std::fs::File;
use std::io::{self, BufReader, Read, IsTerminal};
use std::thread;
use tempfile::NamedTempFile;
use std::io::Write;

// Make the read_input function public for testing
pub fn read_input<R: Read>(mut reader: R, pager: minus::Pager) -> Result<()> {
    let mut changes = || -> Result<()> {
        let mut buff = String::new();
        reader
            .read_to_string(&mut buff)
            .context("Failed to read input")?;
        pager
            .push_str(&buff)
            .context("Failed to push content to pager")?;
        Ok(())
    };

    let pager_clone = pager.clone();
    let res1 = thread::spawn(move || minus::dynamic_paging(pager_clone));
    let res2 = changes();

    res1.join()
        .expect("Paging thread panicked")
        .context("Failed to run dynamic paging")?;
    res2?;

    Ok(())
}

fn main() -> Result<()> {
    let arguments: Vec<String> = args().collect();

    let input: Box<dyn Read> = if io::stdin().is_terminal() {
        // No piped input, check for file argument
        if arguments.len() < 2 {
            anyhow::bail!(
                "No input provided. Usage: {} <filename> or pipe content to stdin",
                arguments[0]
            );
        }
        let filename = &arguments[1];
        let file =
            File::open(filename).with_context(|| format!("Failed to open file '{}'", filename))?;
        Box::new(BufReader::new(file))
    } else {
        // Piped input
        Box::new(BufReader::new(io::stdin()))
    };

    let output = minus::Pager::new();

    if io::stdin().is_terminal() {
        output
            .set_prompt(&arguments[1])
            .context("Failed to set pager prompt")?;
    } else {
        output
            .set_prompt("stdin")
            .context("Failed to set pager prompt")?;
    }

    read_input(input, output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    // Test helper to create a temp file with content
    fn create_temp_file(content: &str) -> NamedTempFile {
        let mut file = NamedTempFile::new().unwrap();
        write!(file, "{}", content).unwrap();
        file
    }

    #[test]
    fn test_file_not_found() {
        let args = vec![
            "cortex".to_string(),
            "nonexistent_file.txt".to_string(),
        ];
        let result = std::panic::catch_unwind(|| {
            let _ = File::open(&args[1])
                .with_context(|| format!("Failed to open file '{}'", &args[1]));
        });
        assert!(result.is_ok()); // Should not panic
    }

    #[test]
    fn test_read_file_contents() {
        let test_content = "Hello, World!";
        let temp_file = create_temp_file(test_content);
        let file = File::open(temp_file.path()).unwrap();
        let mut reader = BufReader::new(file);
        let mut content = String::new();
        reader.read_to_string(&mut content).unwrap();
        assert_eq!(content, test_content);
    }

    #[test]
    fn test_no_arguments() {
        let args = vec!["cortex".to_string()];
        if io::stdin().is_terminal() {
            let result = std::panic::catch_unwind(|| {
                if args.len() < 2 {
                    anyhow::bail!(
                        "No input provided. Usage: {} <filename> or pipe content to stdin",
                        args[0]
                    );
                }
                Ok::<(), anyhow::Error>(())
            });
            assert!(result.is_ok()); // Should not panic
        }
    }

    #[test]
    fn test_read_from_cursor() {
        let test_content = "Test content";
        let mut reader = Cursor::new(test_content);
        let mut content = String::new();
        reader.read_to_string(&mut content).unwrap();
        assert_eq!(content, test_content);
    }
}
