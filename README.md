# less

A modern, feature-rich terminal pager written in Rust, designed to be a more user-friendly alternative to traditional pagers like `less` or `more`.

## Features

- 📜 Smooth scrolling with dynamic paging
- 🔍 Built-in search functionality
- 📁 File content viewing
- 📝 Support for piped input (stdin)
- 🎨 Clean and intuitive interface
- 🚀 Fast and memory-efficient

## Installation

### Prerequisites

- Rust toolchain (1.75.0 or later)
- A terminal that supports ANSI escape codes

### Building from Source

1. Clone the repository:

    ```bash
    git clone https://github.com/rakki194/less.git
    cd less
    ```

2. Build the project:

    ```bash
    cargo build --release
    ```

The compiled binary will be available at `target/release/less`.

## Usage

less can be used in two ways:

1. View a file:

    ```bash
    less <filename>
    ```

2. Read from piped input:

    ```bash
    command | less
    ```

### Key Bindings

- **Arrow Keys**: Navigate up and down through the content
- **Page Up/Down**: Scroll by page
- **/** (forward slash): Enter search mode
- **q**: Quit the pager

## Dependencies

- [minus](https://github.com/AMythicDev/minus) - Provides the core paging functionality with features like dynamic output and search
- [anyhow](https://crates.io/crates/anyhow) - Error handling with backtraces

## Technical Details

less is built with modern Rust practices and provides:

- Error handling with context using `anyhow`
- Thread-safe dynamic paging
- Efficient buffered reading of input
- Automatic detection of input source (file vs. stdin)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is open source and available under the [MIT License](LICENSE).

## Acknowledgments

- Thanks to the creators of the `minus` crate for providing the core paging functionality
- Inspired by traditional Unix pagers while aiming to provide a more modern experience
