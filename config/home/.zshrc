# for profiling (run: env ZSH_PROF= zsh -ic zprof)
# if [[ -v ZSH_PROF ]]; then
#   zmodload zsh/zprof
# fi

# python
# locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# if [ -n "$INSIDE_EMACS" ]; then
#     ZSH_THEME="rawsyntax"
# else
#     ZSH_THEME="robbyrussell"
# fi
# ZSH_THEME="robbyrussell"
# ZSH_THEME="spaceship"
ZSH_THEME="avit"

# spaceship customizations
# SPACESHIP_PROMPT_ADD_NEWLINE="true"
#SPACESHIP_CHAR_SYMBOL="\uf0e7"
#SPACESHIP_CHAR_PREFIX="\uf296"
# SPACESHIP_CHAR_SUFFIX=(" ")
#SPACESHIP_CHAR_COLOR_SUCCESS="yellow"
#SPACESHIP_PROMPT_DEFAULT_PREFIX="$USER"
#SPACESHIP_PROMPT_FIRST_PREFIX_SHOW="true"
#SPACESHIP_USER_SHOW="true"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
#plugins=(git rbenv rails)
if [ -n "$INSIDE_EMACS" ]; then
    plugins=()
else
    plugins=(kubectl)
fi

if [ -n "$INSIDE_EMACS" ]; then
    alias vim="echo not available"
    alias vi="echo not available"
fi

source $ZSH/oh-my-zsh.sh

alias ls="ls -alGh"
alias gl="git log --graph --decorate --pretty=oneline --abbrev-commit"
alias gll="git log --graph --abbrev-commit --decorate --date=relative --all"

# Don't share history betweend different sessions
setopt noincappendhistory
setopt nosharehistory

alias sshu="ssh -Y -l ubuntu"

# Already done in ~/.zshenv
# source local customizations and passwords
source ~/.secretsrc  # already in .zshenv
for file in ~/.localcustomizations.*; do
    source "$file"
done

#####################################################################
# Helper functions
#####################################################################

# Read a single char from /dev/tty, prompting with "$*"
# Note: pressing enter will return a null string. Perhaps a version terminated with X and then remove it in caller?
# See https://unix.stackexchange.com/a/367880/143394 for dealing with multi-byte, etc.
function get_keypress {
  local REPLY IFS=
  >/dev/tty printf '%s' "$*"
  [[ $ZSH_VERSION ]] && read -rk1  # Use -u0 to read from STDIN
  # See https://unix.stackexchange.com/q/383197/143394 regarding '\n' -> ''
  [[ $BASH_VERSION ]] && </dev/tty read -rn1
  printf '%s' "$REPLY"
}

# Get a y/n from the user, return yes=0, no=1 enter=$2
# Prompt using $1.
# If set, return $2 on pressing enter, useful for cancel or defualting
function get_yes_keypress {
  local prompt="${1:-Are you sure} [y/n]? "
  local enter_return=$2
  local REPLY
  # [[ ! $prompt ]] && prompt="[y/n]? "
  while REPLY=$(get_keypress "$prompt"); do
    [[ $REPLY ]] && printf '\n' # $REPLY blank if user presses enter
    case "$REPLY" in
      Y|y)  return 0;;
      N|n)  return 1;;
      '')   [[ $enter_return ]] && return "$enter_return"
    esac
  done
}

# Credit: http://unix.stackexchange.com/a/14444/143394
# Prompt to confirm, defaulting to NO on <enter>
# Usage: confirm "Dangerous. Are you sure?" && rm *
function confirm {
  local prompt="${*:-Are you sure} [y/N]? "
  get_yes_keypress "$prompt" 1
}

# Prompt to confirm, defaulting to YES on <enter>
function confirm_yes {
  local prompt="${*:-Are you sure} [Y/n]? "
  get_yes_keypress "$prompt" 0
}

########################################################################

function install_python_dev_deps() {
    echo "$(python --version)"
    echo "$(pip --version)"
    echo "$(easy_install --version)"

    confirm || return 1

    echo "Will install python dev dependencies"

    # lsp
    echo "Installing python lsp"
    pip install 'python-language-server[all]'

    # flake8
    echo "Installing flake8"
    pip install flake8

    # pylint
    echo "Installing pylint"
    pip install pylint

    # mypy
    echo "Installing mypy/MokeyType"
    pip install mypy
    pip install MonkeyType

    # pep8
    pip install pep8

    # ptvsd
    pip install ptvsd
}

########################################################
### The following didn't work when put in ~/.zshenv
#########################################################
export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
# python
# to avoid loading inside nested shell
function pyenv_init {
    # pyenv / pyenv-virtualenv
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"

    # enable pyenv-virtualenv
    if which pyenv > /dev/null; then
        eval "$(pyenv init -)";
        eval "$(pyenv virtualenv-init -)";
    fi
}
if [[ -z "${PYENV_ROOT}" ]] || (( ${+TMUX} )); then
    pyenv_init
fi

#########################################################

# Creating env for new pipenv python project (don't use pipenv virtualenv)
# pyenv install <version>
# pyenv virrutalenv <version> <project-vvv>  # i.e PRJ-372
# pyenv local <project-vvv>
# pip shell <project-vvv>
# pip install --upgrade pip (OPTIONAL might break some packages)
# pipenv install --dev
# install_python_dev_deps


#####
# starship
eval "$(starship init zsh)"

