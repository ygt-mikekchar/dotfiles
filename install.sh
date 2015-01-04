#!/bin/sh

link_file() {
  FILE="$1"
  DEST="$2"
  if [ -h "$DEST/$FILE" ]; then
    echo "Removing old $DEST/$FILE"
    rm "$DEST/$FILE"
  fi 
  if [ -e "$DEST/$FILE" ]; then
    echo "$DEST/$FILE exists and is not a link.  Skipping."
  else
    echo "Linking $DEST/$FILE"
    ln -s `pwd`"/$FILE" "$DEST/$FILE"
  fi
}

link_files() {
  FILE="$1"
  DEST="$2"
  if [ -d "$FILE" ]; then
    link_directory "$FILE" "$DEST"
  else
    link_file "$FILE" "$DEST"
  fi
}

link_directory() {
  FILE="$1"
  DEST="$2"
  if [ ! -e "$DEST/$FILE" ]; then
    echo "Creating directory $DEST/$FILE"
    mkdir "$DEST/$FILE"
  else
    echo "$DEST/$FILE already exists"
  fi
  if [ ! -d "$DEST/$FILE" ]; then
    echo "$DEST/$FILE is not a directory.  Skipping."
  else
    for i in "$FILE"/*; do
      if [ -e "$i" ]; then
        link_files "$i" "$DEST"
      fi
    done
    for i in "$FILE"/.[^.]*; do
      if [ -e "$i" ]; then
        link_files "$i" "$DEST"
      fi
    done
  fi
}

pushd home
link_directory mikekchar /home
popd
link_file .vim /home/mikekchar

