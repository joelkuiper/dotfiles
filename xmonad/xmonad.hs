import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- LAYOUTS
import XMonad.Layout.NoBorders


myManageHook = composeAll [
    (className =? "Pidgin" <&&> (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
  , (className =? "Pidgin") --> doShift "3"
  , (className =? "Gnome-panel" <&&> title =? "Run Application") --> doCenterFloat
  , (className =? "Gcr-prompter") --> doCenterFloat
  , (className =? "Xfce4-notifyd" -->  doIgnore)
  , isFullscreen --> doFullFloat
  , manageDocks
   ]

main = xmonad $ gnomeConfig {
  modMask            = mod4Mask
  , layoutHook  = smartBorders (layoutHook gnomeConfig)
  , borderWidth      = 2
  , normalBorderColor  = "#cccccc"
  , manageHook       = myManageHook <+> manageHook gnomeConfig
  }
