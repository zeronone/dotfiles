#!/bin/sh

brew uninstall go --force

# install gvm
curl -s -S -L https://raw.githubusercontent.com/moovweb/gvm/master/binscripts/gvm-installer | zsh
source $HOME/.gvm/scripts/gvm
gvm install go1.13.4 -B && gvm use go1.13.4 --default

go version

go get -u github.com/motemen/gore/cmd/gore
go get -u github.com/mdempsky/gocode
go get -u golang.org/x/tools/cmd/godoc
go get -u golang.org/x/tools/cmd/goimports
go get -u golang.org/x/tools/cmd/gorename
go get -u golang.org/x/tools/cmd/guru
go get -u github.com/cweill/gotests/...
go get -u github.com/fatih/gomodifytags

