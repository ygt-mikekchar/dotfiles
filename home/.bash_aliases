alias vi=vim
alias ls="ls -F"
alias mutt-ygt="mutt -F $HOME/.mutt/ygt.muttrc"
alias mutt-home="mutt -F $HOME/.mutt/home.muttrc"

ssh-id() {
  killall ssh-agent
  eval $(ssh-agent)
  ssh-add "$HOME/.ssh/$1"
}
