#!/bin/sh

unset SESSION_MANAGER
unset DBUS_SESSION_BUS_ADDRESS
if [ -d "$HOME/.Xresources.d" ]; then
  for i in "$HOME"/.Xresources.d/*; do
    xrdb -merge "$i"
  done
fi

# ibus is completely broken on Suzume
# TODO: Replace with SCIM???
# export GTK_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# export QT_IM_MODULE=ibus
# ibus-daemon -drx

# Turn off the annoying system bell
xset -b

# Display the system tray
stalonetray &

# Start Pulse Audio and
start-pulseaudio-x11
pasystray &

# TODO: Replace with wicd
# nm-applet -s &

# Turn off click by tap
synclient TapButton1=-1
synclient TapButton2=-1
synclient TapButton3=-1

# Set the background picture
feh --bg-scale /usr/share/backgrounds/gnome/Waves.jpg

# Turn on the mouse pointer
xsetroot -cursor_name left_ptr

# Kill gpg-agent if it is running so we can replace it
if pgrep gpg-agent; then
  pkill gpg-agent
fi

# Start xmonad in a gpg-agent shell
gpg-agent --pinentry-program /usr/bin/pinentry-gtk-2 --daemon xmonad
