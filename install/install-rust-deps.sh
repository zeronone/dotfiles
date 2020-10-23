#!/bin/sh

# -y (no-prompt)
curl --proto '=https' --tlsv1.2 -sSf  https://sh.rustup.rs | sh -s -- -y

rustup update
rustup toolchain install stable
rustup toolchain install nightly
rustup default stable

rustup component add rustfmt
rustup component add clippy
rustup component add rust-std
rustup component add rust-src
rustup component add rust-docs --toolchain nightly
rustup component add rust-analyzer-preview --toolchain nightly

# Add Dev dependencies
cargo +nightly install racer
cargo install ripgrep
cargo install cargo-watch

# poor man repl
cargo install evcxr_repl
