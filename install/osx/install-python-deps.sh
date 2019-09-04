#!/bin/bash

if which pyenv > /dev/null; then
	echo "pyenv exists, skipping"
else
	brew install pyenv
	eval "$(pyenv init -)"
	# needed for mojave
	sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /
fi

pyenv install -s 3.7.2
pyenv global 3.7.2

git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv

[[ "$(python -V 2>&1)" == "Python 3.7.2" ]] || (echo "python version mismatch"; exit)

echo $(python -V)

# from ~/.zshrc
source ~/.zshrc
echo "Installing dev dependencies"
install_python_dev_deps

# install pipenv
brew install pipenv

# Microsoft python language server
echo "Installing Microsoft Python Language Server"
brew cask install dotnet-sdk

source ~/.zshrc

PYLS_DIR="$HOME/bin/ms-pyls"
if [ -d $PYLS_DIR ]; then
	echo "Directory already exists ($PYLS_DIR), will delete."
	rm -rf $PYLS_DIR
fi
echo "Cloning ms-pyls into ($PYLS_DIR)."
git clone https://github.com/Microsoft/python-language-server.git $PYLS_DIR
cd $PYLS_DIR
# git checkout --track origin/release
cd src/LanguageServer/Impl
dotnet nuget locals all --clear
dotnet publish -c Release -r osx-x64
ln -sf "$PYLS_DIR/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer" ~/.local/bin/


