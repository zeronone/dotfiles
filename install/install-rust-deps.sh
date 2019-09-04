#!/bin/sh

# -y (no-prompt)
curl https://sh.rustup.rs -sSf | sh -s -- -y

rustup toolchain add nightly

rustup update

rustup component add rustfmt
rustup component add cargo-check
rustup component add cargo-edit
rustup component add clippy


# rls (with lsp)
rustup component add rust-src
rustup component add rust-docs
rustup component add rls
rustup component add rust-analysis


# Add Dev dependencies
cargo install racer
cargo install ripgrep
cargo install cargo-watch

# install rl_lsp_server
cd ~/bin
git clone https://github.com/rust-analyzer/rust-analyzer && cd rust-analyzer
cargo install-ra

# poor man repl
cargo install evcxr_repl
