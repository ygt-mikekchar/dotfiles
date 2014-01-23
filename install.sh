#!/bin/sh

# Remove any files that already exist.  I have to do this because I
# am making symlinks

echo "Removing existing files"
for i in ~/.thymrc ~/.tmux.conf ~/.vimrc ~/.config/awesome/rc.lua; do
  echo "  removing $i"
  rm -f $i
done

echo "Creating directories"
# Create dirs if they don't already exist
for i in ~/.config ~/.config/awesome; do
  if [ -d $i ]; then
    echo "  $i already exists"
  else
    echo "  creating $i"
    mkdir $i
  fi
done

echo "Making symbolic links"
for i in .thymrc .tmux.conf .vimrc; do
  echo "  linking $i"
  ln -s `pwd`"/$i" ~
done

echo "Linking bin files into /usr/local/bin"
cd bin
for i in *; do
  echo "  linking bin/$i"
  sudo rm -f "/usr/local/bin/$i"
  sudo ln -s `pwd`"/$i" /usr/local/bin
done
