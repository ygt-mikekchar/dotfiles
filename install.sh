#!/bin/bash

link_files() {
  local file="$1"
  local dest="$2"

  if [ -d "$file" ]; then
    link_directory "$file" "$dest"
  elif [ -e "$file" ]; then
    link_file "$file" "$dest"
  fi
}

link_file() {
  local file="$1"
  local dest="$2"

  if [ -h "$dest/$file" ]; then
    rm "$dest/$file"
  fi
  if [ -e "$dest/$file" ]; then
    echo "Warning: $dest/$file exists and is not a link.  Skipping link file."
  else
    ln -s $(readlink -f "$file") "$dest/$file"
  fi
}

create_directory() {
  local dest=$1

  if [ ! -e "$dest" ]; then
    mkdir "$dest"
  fi
}

link_directory() {
  local file="$1"
  local dest="$2"

  create_directory "$dest/$file"
  if [ ! -d "$dest/$file" ]; then
    echo "Warning: $dest/$file is not a directory.  Skipping link directory."
  else
    link_contents "$file" "$dest/$file"
  fi
}

link_contents() {
  local dir="$1"
  local dest=$(readlink -f "$2")

  pushd $dir >/dev/null
  for i in *; do
    link_files "$i" "$dest"
  done
  for i in .[^.]*; do
    link_files "$i" "$dest"
  done
  popd >/dev/null
}

copy_files() {
  local file="$1"
  local dest="$2"

  if [ -d "$file" ]; then
    copy_directory "$file" "$dest"
  elif [ -e "$file" ]; then
    copy_file "$file" "$dest"
  fi
}

copy_file() {
  local file="$1"
  local dest="$2"

  if [ -h "$dest/$file" ]; then
    rm "$dest/$file"
  fi
  cp $(readlink -f "$file") "$dest/$file"
}

copy_directory() {
  local file="$1"
  local dest="$2"

  create_directory "$dest/$file"
  if [ ! -d "$dest/$file" ]; then
    echo "Warning: $dest/$file is not a directory.  Skipping copy directory."
  else
    copy_contents "$file" "$dest/$file"
  fi
}

copy_contents() {
  local dir="$1"
  local dest=$(readlink -f "$2")

  pushd $dir >/dev/null
  for i in *; do
    copy_files "$i" "$dest"
  done
  for i in .[^.]*; do
    copy_files "$i" "$dest"
  done
  popd >/dev/null
}

DEST=$HOME
link_contents home $DEST
copy_contents shims $DEST
link_file .vim $DEST
