import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.EwmhDesktops
import System.IO

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "stalonetray" --> doIgnore
  , className =? "org-igoweb-cgoban-CGoban" --> doFloat
  , className =? "t-engine" --> doFullFloat
  ]

main = do
    xmproc <- spawnPipe "xmobar $HOME/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig <+> composeAll myManagementHooks
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , handleEventHook = fullscreenEventHook
        , borderWidth = 2
        , terminal = "urxvt"
        , normalBorderColor = "cccccc"
        , focusedBorderColor = "#cd8b00"
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "dodgerblue3" "" . shorten 50
                        }
        , modMask = mod4Mask
        }
