#!/usr/bin/env bash
set -euo pipefail

# pyenv must be already installed
#

# latest stable
pyenv install 3.8.3 --skip-existing
pyenv global 3.8.3


export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if which pyenv > /dev/null; then
    eval "$(pyenv init - --no-rehash)"
fi

pip --version
pip install isort nose pytest
