#Mike's Dotfiles

Before talking about my dotfiles, I would like to give a shout out to
https://github.com/ygt-luca for encouraging me to make this.  Without
his example, I probably wouldn't have caught the "automate everything"
bug.  I never realized how satisfying it is to be able to checkout
a single repository, run a single shell script and be ready to work.

I figure that nobody uses anybody else's dotfiles repository as a whole.
Part of the process is building up your own environment that is
perfect for you -- not for someone else.  Instead, a dotfiles repository
is useful for reference.  I hope some people will think, "how did he
do that", take a look at my setup and then use it to make their
own perfect environment.  That said, in this README I will try to
guide the reader into what I think is interesting.

## Features

To be honest, there is not much here that is unique.  I spend almost all
of my day in a terminal window and rely heavily on tmux.  I generally
use a tiling window manager.  Lately, in order to remove some of
the bloat on my system I have removed all of the modern desktop
environments and use only Urxvt.  The main features are:

### .tmux.conf

  - Xmonad keybindings
  - Cut and paste to and from X cut buffer

### More later

## Organization

### Editing in place

I like editing in place.  I play with my setup a lot and I don't want
to have to fiddle around copying files from one place to another in
order to try something, or to check in.  For that reason, I don't copy
my files from the dotfiles repository into my home directory, but rather
make symbolic links.  This allows me to edit the files in my home directory
and then when everything is working properly, just go to my dotfiles
repository and commit the change.

### Mirrored file structure

I've tried to make the install script simple and to make it obvious where
things will ultimately go.  For that reason I have mirrored my home
directory in the `home` directory.  Everything here will be installed
into `$HOME` when isntall.sh is run.  The file structure mirrors what
is in my home directory.  If a directory doesn't exist in the home
directory during install time, the directory is created.  Everything
else is linked.  For safety sake, if a file already exists in my home
directory and is not a link, it is left alone.

### Vim plugins use submodules

As part of the overall philosophy of mirroring the home directory
and editing in place, vim plugins are all submodules.  The `.vim`
directory is specially linked by the install script and if you
go into that directory everything can be updated easily.  This is
especially important for working on Agnostic because I can make changes
and check them in without having to do anything special in my dotfiles.

FIXME: This causes a problem because I can't recursively descend into
the .vim directory and link everything up like I do for the rest
of my dotfiles.  I will try to fix this soon.

## License

All of the code that I have written in this repository can be used
without restriction for any purpose.  Code that I have not written
retains the license that it originally had.
