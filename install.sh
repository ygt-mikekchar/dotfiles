#!/bin/sh

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
    echo "Removing old $dest/$file"
    rm "$dest/$file"
  fi 
  if [ -e "$dest/$file" ]; then
    echo "$dest/$file exists and is not a link.  Skipping."
  else
    echo "Linking $dest/$file"
    ln -s $(readlink -f "$file") "$dest/$file"
  fi
}

create_directory() {
  local dest=$1

  if [ ! -e "$dest" ]; then
    echo "Creating directory $dest"
    mkdir "$dest"
  else
    echo "$dest already exists"
  fi
}

link_directory() {
  local file="$1"
  local dest="$2"

  create_directory "$dest/$file"
  if [ ! -d "$dest/$file" ]; then
    echo "$dest/$file is not a directory.  Skipping."
  else
    link_contents "$file" "$dest/$file"
  fi
}

link_contents() {
  local dir="$1"
  local dest=$(readlink -f "$2")

  pushd $dir
  for i in *; do
    link_files "$i" "$dest"
  done
  for i in .[^.]*; do
    link_files "$i" "$dest"
  done
  popd
}

DEST=$HOME
link_contents home/mikekchar $DEST
link_file .vim $DEST

