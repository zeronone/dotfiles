

# Uncomment for profiling
# set -xv

# python
# locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

alias ls="ls -alGh"
alias gl="git log --graph --decorate --pretty=oneline --abbrev-commit"
alias gll="git log --graph --abbrev-commit --decorate --date=relative --all"
alias edit="~/bin/editor"

# Don't share history betweend different sessions
setopt noincappendhistory
setopt nosharehistory

alias sshu="ssh -Y -l ubuntu"

function cmake_clang_init {
    export LDFLAGS="-L/usr/local/opt/llvm/lib"
    export CPPFLAGS="-I/usr/local/opt/llvm/include"

    cmake -DCMAKE_BUILD_TYPE=Debug \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
        -DCMAKE_C_COMPILER=/usr/local/opt/llvm/bin/clang \
        -DCMAKE_CXX_COMPILER=/usr/local/opt/llvm/bin/clang++ \
        $1
}

function cmake_clang_init_llvm8 {
    export LDFLAGS="-L/usr/local/opt/llvm@8/lib"
    export CPPFLAGS="-I/usr/local/opt/llvm@8/include"

    cmake -DCMAKE_BUILD_TYPE=Debug \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
        -DCMAKE_C_COMPILER="/usr/local/opt/llvm@8/bin/clang" \
        -DCMAKE_CXX_COMPILER="/usr/local/opt/llvm@8/bin/clang++" \
        $1
}


alias emacs_kill_daemon="emacsclient -nc -e '(save-buffers-kill-emacs)'"


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


########################################################
### The following didn't work when put in ~/.zshenv
#########################################################

function load_n {
    export N_PREFIX="$HOME/n";
    [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
}

load_n

# python
# to avoid loading inside nested shell
# Don't use pyenv-virutualenv it makes the shell too slow
function pyenv_init {
    # pyenv
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"

    echo "pyenv_init called"

    if which pyenv > /dev/null; then
        eval "$(pyenv init - --no-rehash)"
    fi
}
if [[ -z "${PYENV_ROOT}" ]] || (( ${+TMUX} )); then
   if [[ -z "${INSIDE_EMACS}" ]]; then
     pyenv_init
   fi
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
# eval "$(starship init zsh)"

#### direnv
eval "$(direnv hook zsh)"

#### tmux
function tmux_cp_loop {
    while true; do
        if test -n "`tmux showb 2> /dev/null`"; then
            tmux saveb -|pbcopy && tmux deleteb
        fi
        sleep 0.5
    done
}

### Disable auto cd
unsetopt AUTO_CD

# Already done in ~/.zshenv
# source local customizations and passwords
source ~/.secretsrc  # already in .zshenv
for file in ~/.localcustomizations.*; do
    source "$file"
done


### python clean
pyclean () {
    find . -type f -name '*.py[co]' -delete -o -type d -name __pycache__ -delete
}


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"


export PATH=$(consolidate-path.sh "$PATH")
