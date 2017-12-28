

## Installation

```
$ cd $HOME
$ mkdir myfiles
$ cd myfiles
$ git clone https://github.com/zeronone/dotfiles.git
$ cd dotfiles
$ cd home && stow -t ~ .
# other directories based on the environment

# VIM
$ cd $HOME
$ git clone https://github.com/zeronone/vim.git .vim --recursive
$ cd .vim
$ ln -sf $HOME/.vim/.vimrc $HOME/.vimrc
$ cd $HOME/.vim
$ git submodule update --init

# EMACS
$ cd $HOME
$ git clone https://github.com/zeronone/doom-emacs.git .emacs.d
$ cd $HOME/.emacs.d
$ cp init.example.el init.el
```




