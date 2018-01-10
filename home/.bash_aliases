alias vi=vim
alias ls="ls -F"
alias mutt-ygt="mutt -F $HOME/.mutt/ygt.muttrc"
alias mutt-home="mutt -F $HOME/.mutt/home.muttrc"

# Work VM aliases
alias salesup="pushd $HOME/work/vagabond;VAGRANT_USE_RSYNC=1 vagrant up sales;popd"
alias spinup="pushd $HOME/work/vagabond;VAGRANT_USE_RSYNC=1 vagrant up spin;popd"
alias vmlogin="ssh -o \"UserKnownHostsFile=/dev/null\" -o \"StrictHostKeyChecking=no\" -Ap 2222 -L 5901:localhost:5901 mikecharlton@localhost"
alias vmstatus="pushd $HOME/work/vagabond;vagrant status; popd"
alias salesdown="pushd $HOME/work/vagabond;vagrant halt sales;popd"
alias spindown="pushd $HOME/work/vagabond;vagrant halt spin;popd"

# Set up ssh-agent with the correct key
ssh-id() {
  eval $(ssh-agent)
  ssh-add "$HOME/.ssh/$1"
}
