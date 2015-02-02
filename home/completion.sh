#!/bin/bash

complete_change_theme() {
  pushd "$HOME/.Xresources.d/colors" >/dev/null
  COMPREPLY=( $( compgen -G "$2*" $2 ) )
  popd >/dev/null
  return 0
}

complete -o nospace -F complete_change_theme change_theme 
