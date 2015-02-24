#!/bin/sh

unset SESSION_MANAGER
unset DBUS_SESSION_BUS_ADDRESS
if [ -d "$HOME/.Xresources.d" ]; then
  for i in "$HOME"/.Xresources.d/*; do
    xrdb -merge "$i"
  done
fi
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx
xset -b
start-pulseaudio-x11
synclient TapButton1=-1
synclient TapButton2=-1
synclient TapButton3=-1
pasystray &
nm-applet -s &

# xmonad doesn't have these things
compton --config "$HOME"/.compton.conf -b
feh --bg-scale /usr/share/backgrounds/gnome/Waves.jpg
xsetroot -cursor_name left_ptr
stalonetray &
if pgrep gpg-agent; then
  pkill gpg-agent
fi

gpg-agent --pinentry-program /usr/bin/pinentry-gtk-2 --daemon xmonad
