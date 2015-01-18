# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Editor
# Install:
#   pacman -S vim
if [ -f /usr/bin/vim ]; then
  export EDITOR=vim
fi

# rbenv
# Install: 
#   git clone https://github.com/sstephenson/rbenv.git $HOME/pkg/rbenv
#   git clone https://github.com/sstephenson/ruby-build.git $HOME/pkg/rbenv/plugins/ruby-build
if [ -d $HOME/pkg/rbenv ]; then
  export "PATH=$HOME/pkg/rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi

# Go Language
# Install:
#   pacman -S go
#   mkdir "$HOME/pkg/go"
export GOPATH="$HOME/pkg/go"

# Google Cloud SDK
# Install:
#   curl https://sdk.cloud.google.com | CLOUDSDK_PYTHON=python2.7 bash
#   gcloud auth login
if [ -d $HOME/pkg/google-cloud-sdk ]; then
  # Make sure we are using the correct version of Python
  export CLOUDSDK_PYTHON=python2.7

  # The next line updates PATH for the Google Cloud SDK.
  source "$HOME/pkg/google-cloud-sdk/path.bash.inc"

  # The next line enables bash completion for gcloud.
  source "$HOME/pkg/google-cloud-sdk/completion.bash.inc"
fi

# Cabal
# Install:
#   pacman -S cabal
if [ -d $HOME/.cabal ]; then
  export PATH="$PATH:$HOME/.cabal/bin"
fi

# GPG Agent
update_gpg() {
  echo UPDATESTARTUPTTY | gpg-connect-agent
}

start_gpg() {
  if pgrep gpg-agent; then
    pkill gpg-agent
  else
    eval $(gpg-agent --pinentry-program /usr/bin/pinentry-curses --daemon)
    update_gpg
    echo "Prompts for passwords will appear in this window.  To change,"
    echo "enter another window and run 'update_gpg'"
  fi
}

# On a system where GPG Agent should be running
if pgrep gpg-agent; then
  echo "gpg-agent is running, but prompts won't appear in this window.  If you"
  echo "wish propmpts to appear in this window, run 'update_gpg'"
else
  echo "gpg-agent is not running.  If you wish to start it, type 'start_gpg'"
fi

complete -W "agnostic_light linux_terminal sixgun solarized tango" change_theme

# Finally a bin directory to override anything.  Always put this last.
if [ -d "$HOME/.bin" ]; then
  export PATH="$HOME"/.bin:"$PATH"
fi
