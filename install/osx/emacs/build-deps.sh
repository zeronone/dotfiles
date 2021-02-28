#!/bin/bash

# install gcc with JIT
brew install gcc
brew install libgccjit

echo "Install build deps"
brew install giflib jpeg libtiff gnutls openssl@1.1 texinfo gnu-sed libxml2 jansson cairo librsvg harfbuzz


