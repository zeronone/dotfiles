
# Uncomment for profiling
# set -xv


# python
# locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

PROMPT='%(?.%F{green}√.%F{red}?%?)%f %B%F{240}%1~%f%b %# '

alias ls="ls -alGh"
alias gl="git log --graph --decorate --pretty=oneline --abbrev-commit"
alias gll="git log --graph --abbrev-commit --decorate --date=relative --all"
alias edit="~/bin/editor"

# Don't share history betweend different sessions
setopt noincappendhistory
setopt nosharehistory

alias emacs_kill_daemon="emacsclient -nc -e '(save-buffers-kill-emacs)'"
function emacshere {

  if test "$#" -ne 1; then
    echo "Usage: emacshere <DIR>"
    return 1
  fi

  local curr_dir_full=$(readlink -f $1)
  local curr_dir=$(basename $curr_dir_full)

  echo "Starting emacs with session=$curr_dir"

  emacs --bg-daemon=$(basename $curr_dir)
  emacsclient -t -s $(basename $curr_dir) $curr_dir

  ps aux | grep emacs | grep bg-daemon | grep $curr_dir | tr -s [:blank:] | cut -d' ' -f 2 | xargs -L 1 kill -9
}


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
. ~/.secretsrc  # already in .zshenv
for file in ~/.localcustomizations.*; do
    . "$file"
done

### python clean
pyclean () {
    find . -type f -name '*.py[co]' -delete -o -type d -name __pycache__ -delete
}

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

export PATH=$(consolidate-path.sh "$PATH")

#### direnv
eval "$(direnv hook zsh)"
# BEGIN env Setup -- Managed by Ansible DO NOT EDIT.
# BEGIN env Setup -- Managed by Ansible DO NOT EDIT.

# Setup INDEED_ENV_DIR earlier.
if [ -z "${INDEED_ENV_DIR}" ]; then
    export INDEED_ENV_DIR="${HOME}/env/"
fi

# Single-brace syntax because this is required in bash and sh alike
if [ -e "${INDEED_ENV_DIR}/etc/indeedrc" ]; then
    . "${INDEED_ENV_DIR}/etc/indeedrc"
fi

# END env Setup -- Managed by Ansible DO NOT EDIT.
