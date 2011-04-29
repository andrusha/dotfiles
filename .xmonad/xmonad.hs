import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers   (doCenterFloat, isDialog, isFullscreen, doFullFloat)
import XMonad.Util.Run              (spawnPipe)
import XMonad.Util.EZConfig         (additionalKeysP)
import XMonad.Actions.CycleWS       (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.GridSelect
import System.IO

import qualified XMonad.StackSet as W

main = do
    xmproc <- spawnPipe "xmobar"
    spawn myTrayer
    spawn myXxkb
    xmonad $ defaultConfig
        {
          manageHook    = pbManageHook <+> myManageHook 
        , layoutHook    = smartBorders $ avoidStruts $  layoutHook defaultConfig
        , logHook       = dynamicLogWithPP $ xmobarPP
                            { ppOutput  = hPutStrLn xmproc
                            , ppTitle   = xmobarColor "white" "" . shorten 100
                            , ppCurrent = xmobarColor "white" "black" . pad
                            , ppHidden  = pad
                            , ppSep     = xmobarColor "#555" "" " | "
                            , ppWsSep   = ""
                            , ppLayout  = \x -> ""
                            , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
                            }
        , terminal           = "gnome-terminal"
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces         = myWorkspaces
        , focusFollowsMouse  = True
        } `additionalKeysP` myKeys

-- Progs spawn
myTrayer = " sleep 5 && trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 7 --transparent true --tint 0x000000 --height 13"
myXxkb = "xxkb"

-- Keys
myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: [(String, X())]
myKeys = [ ("M-p"           , yeganesh)
         , ("M-<Right>"     , nextWS)
         , ("M-<Left>"      , prevWS)
         , ("M-S-<Right>"   , shiftToNext)
         , ("M-S-<Left>"    , shiftToPrev)
         , ("<Print>"       , spawn "scrot")
         , ("<XF86AudioMute>", spawn "amixer -c0 -- sset Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -c0 -- sset Master 0.75dB-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -c0 -- sset Master 0.75dB+")

         , ("M-s", spawnSelected defaultGSConfig ["chromium", "thunderbird", "pidgin", "skype"])
         , ("M-q", spawn "killall xxkb trayer; xmonad --recompile; xmonad --restart")
         ]

-- Borders
myBorderWidth :: Dimension
myBorderWidth = 1
--
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#306EFF"
--

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["code", "web", "im", "mail", "vm", "tools", "video", "8", "9"]
--

-- Spawns Yeganesh (dmenu replacement)
yeganesh :: MonadIO m => m ()
yeganesh = spawn "exe=`dmenu_path | yeganesh -- $DMENU_OPTIONS` && eval \"exec $exe\""

-- Default managers
--
-- | Match a string against any one of a window's class, title, name or 
--   role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

-- | Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_NAME"

-- | Match against @WM_ROLE@.
role :: Query String
role = stringProperty "WM_ROLE"

-- Helper functions
--
-- Avoid changing master on new window creation
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
	W.Stack t [] (r:rs) -> W.Stack t [r] rs
	otherwise			-> c

-- | Manage hooks
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks                                       ]
    , [ manageHook defaultConfig                          ]
    , [ isDialog      --> doCenterFloat                   ]
    , [ isFullscreen  --> doF W.focusDown <+> doFullFloat ]
    , [ fmap not isDialog --> doF avoidMaster             ]
    ]

myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions]
    where myActions = [ ("Gimp"           , doFloat)
                      , ("Xmessage"       , doCenterFloat)
                      , ("desktop_window" , doIgnore)
                      , ("Pidgin"         , doShift "im" <+> doFloat)
                      , ("buddy_list"     , doShift "im" <+> doFloat)
                      , ("File Transfers" , doShift "im" <+> doFloat)
                      , ("Skype"          , doShift "im" <+> doFloat)
                      , ("qemu"           , doShift "vm" <+> doFloat)
                      , ("Evince"         , doShift "tools")
                      , ("Lanikai"        , doShift "mail")
                      , ("mathematica"    , doFloat)
                      , ("Mplayer"        , doShift "video" <+> doFloat)
                      , ("Chromium"       , doShift "web")
                      , ("XXkb"           , doIgnore)
                      ]