#!/bin/sh

unset SESSION_MANAGER
unset DBUS_SESSION_BUS_ADDRESS
[ -r $HOME/.Xresources ] && xrdb $HOME/.Xresources
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
feh --bg-scale /usr/share/backgrounds/gnome/Waves.jpg
xsetroot -cursor_name left_ptr
stalonetray &
xmonad
# gpg-agent --daemon awesome
# gpg-agent --daemon gnome-session
# gpg-agent --daemon mutter
