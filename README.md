

## Installation

```
# Install dotfiles
$ mkdir -v myfiles
$ cd myfiles
$ git clone https://github.com/zeronone/dotfiles.git $HOME/myfiles/dotfiles
$ export DOTFILES_DIR=$HOME/myfiles/dotfiles
$ cd dotfiles

# Install necassary directories
$ $DOTFILES_DIR/install/install-necassary-dirs.sh

# Install system dependences
# i.e: $DOTFILES_DIR/<operating_system>/install-base.sh

# install oh-my-zsh
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# Apply config
$ cd $DOTFILES_DIR/config/home && stow -t ~ .
# other directories based on the environment
# $ cd ubuntu && stow -t ~ .

$ cd $DOTFILES_DIR/config/root && sudo stow -t / .

# Install VIM config
$ cd $DOTFILES_DIR/install
$ ./install-vim-config.sh

# Install EMACS config
$ cd $HOME
$ git clone --recursive git@github.com:doomemacs/doomemacs.git $HOME/.emacs.d
$ cd $HOME/.emacs.d
$ doom install
# Consider M-x install-all-fonts && M-x unicode-fonts-setup
# Install Fira Mono / Hack fonts

# install private dotfiles
$ git clone https://github.com/zeronone/dotfiles-private.git $HOME/myfiles/dotfiles-private
# install according to profile

# install remaining depdencies
# i.e: ./install/install-rust-deps.sh
```

