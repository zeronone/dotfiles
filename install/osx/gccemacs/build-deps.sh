#!/bin/bash
patchfile=${PWD}/gcc9.3.patch
# cp $patchfile /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/
cd /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/
git checkout ./Formula/gcc.rb
git apply -v $patchfile


echo "Installed patched gcc"
#brew install --build-from-source gcc


echo "Install build deps"
brew install giflib jpeg libtiff gnutls openssl@1.1 texinfo gnu-sed libxml2 jansson


