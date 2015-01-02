#!/bin/sh

# Remove any files that already exist.  I have to do this because I
# am making symlinks

echo "Removing existing files"
for i in ~/.bashrc ~/.bash_aliases ~/.thymerc ~/.tmux.conf ~/.vimrc ~/.vim ~/.config/awesome/* ~/.vnc/* ~/.xinitrc ~/.emacs.d/init.el ~/.gnupg/*.conf; do
  echo "  removing $i"
  rm -f $i
done

echo "Creating directories"
# Create dirs if they don't already exist
for i in ~/.config ~/.config/awesome ~/.vnc ~/.emacs.d ~/.gnupg; do
  if [ -d $i ]; then
    echo "  $i already exists"
  else
    echo "  creating $i"
    mkdir $i
  fi
done

echo "Making symbolic links"
for i in .bashrc .bash_aliases .thymerc .tmux.conf .vimrc .xinitrc .vim; do
  echo "  linking $i"
  ln -s `pwd`"/$i" ~
done
for i in .config/awesome/*; do
  echo "  linking $i"
  ln -s `pwd`"/$i" ~/.config/awesome/
done
for i in .vnc/*; do
  echo "  linking $i"
  ln -s `pwd`"/$i" ~/.vnc
done
for i in .emacs.d/*; do
  echo "  linking $i"
  ln -s `pwd`"/$i" ~/.emacs.d
done
for i in .gnupg/*.conf; do
  echo "  linking $i"
  ln -s `pwd`"/$i" ~/.gnupg
done

# Copying executables because things like startup shells
# don't like links
echo "Copying bin files into /usr/local/bin"
cd bin
for i in *; do
  echo "  copying bin/$i"
  sudo rm -f "/usr/local/bin/$i"
  sudo cp `pwd`"/$i" /usr/local/bin
done
