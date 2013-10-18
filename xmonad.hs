import XMonad
import XMonad.ManageHook
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- LAYOUTS
import XMonad.Layout.NoBorders


myManageHook = composeAll [
    (className =? "Gnome-panel" <&&> title =? "Run Application") --> doCenterFloat
  , (className =? "Xfce4-notifyd" --> doIgnore)
  , (className =? "Do" --> doIgnore)
  , (className =? "Cairo-dock" --> doIgnore)
  , isFullscreen --> doFullFloat
  , manageDocks
   ]

conf = gnomeConfig {
    modMask = mod4Mask
  , layoutHook = smartBorders (layoutHook gnomeConfig)
  , handleEventHook = docksEventHook
  , manageHook = myManageHook <+> manageHook gnomeConfig
  }

main = do
    xmonad conf
        { startupHook = startupHook conf >> setWMName "LG3D"
        }
