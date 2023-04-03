import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.Loggers

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

--VARIABLES---------------------------------------------------------------------

myFocusFollowsMouse :: Bool

myTerminal          = "alacritty"
myFocusFollowsMouse = False
myBorderWidth       = 1
myModMask           = mod1Mask

myWorkspaces = ["一","二","三","四","五","六","七","八","九","零"]

myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#0088ee"

--KEY BINDINGS------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf                 ) -- spawn terminal
    , ((modm,               xK_p     ), spawn "dmenu_run"                            ) -- run dmenu
    , ((modm .|. shiftMask, xK_c     ), kill                                         ) -- kill current window
    , ((modm,               xK_Tab   ), sendMessage NextLayout                       ) -- switch layout
    , ((modm .|. shiftMask, xK_Tab   ), setLayout $ XMonad.layoutHook conf           ) -- refresh layout
    , ((modm,               xK_n     ), refresh                                      ) -- reset layout
    , ((modm,               xK_j     ), windows W.focusDown                          ) -- switch focus down
    , ((modm,               xK_k     ), windows W.focusUp                            ) -- switch focus up
    , ((modm,               xK_m     ), windows W.focusMaster                        ) -- switch focus to master
    , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster                         ) -- swap window to master
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown                           ) -- swap window with down
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp                             ) -- swap window with up
    , ((modm,               xK_h     ), sendMessage Shrink                           ) -- shrink master pane
    , ((modm,               xK_l     ), sendMessage Expand                           ) -- expand master pane
    , ((modm,               xK_t     ), withFocused $ windows . W.sink               ) -- bring floating windows back to tiling
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1)                   ) -- increase number of windows in master pane
    , ((modm              , xK_period), sendMessage (IncMasterN (-1))                ) -- decrease number of windows in master pane
    , ((modm              , xK_b     ), sendMessage ToggleStruts                     ) -- toggle xmobar
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)                    ) -- exit xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart" ) -- restart xmonad

    -- MEDIA CONTROLS --
    , ((modm              , xK_Up    ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%") -- volume +5%
    , ((modm              , xK_Down  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%") -- volume -5%
    , ((modm,               xK_space ), spawn "playerctl play-pause"                    ) -- play-pause
    , ((modm              , xK_Right ), spawn "playerctl next"                          ) -- next music
    , ((modm              , xK_Left  ), spawn "playerctl previous"                      ) -- previous music

    -- WORKSPACES --

    , ((modm              , xK_0     ), windows $ W.greedyView "零") -- switch to workspace 0
    , ((modm              , xK_1     ), windows $ W.greedyView "一") -- switch to workspace 1
    , ((modm              , xK_2     ), windows $ W.greedyView "二") -- switch to workspace 2
    , ((modm              , xK_3     ), windows $ W.greedyView "三") -- switch to workspace 3
    , ((modm              , xK_4     ), windows $ W.greedyView "四") -- switch to workspace 4
    , ((modm              , xK_5     ), windows $ W.greedyView "五") -- switch to workspace 5
    , ((modm              , xK_6     ), windows $ W.greedyView "六") -- switch to workspace 6
    , ((modm              , xK_7     ), windows $ W.greedyView "七") -- switch to workspace 7
    , ((modm              , xK_8     ), windows $ W.greedyView "八") -- switch to workspace 8
    , ((modm              , xK_9     ), windows $ W.greedyView "九") -- switch to workspace 9
    , ((modm .|. shiftMask, xK_0     ), windows $ W.shift      "零") -- bring window to workspace 0
    , ((modm .|. shiftMask, xK_1     ), windows $ W.shift      "一") -- bring window to workspace 1
    , ((modm .|. shiftMask, xK_2     ), windows $ W.shift      "二") -- bring window to workspace 2
    , ((modm .|. shiftMask, xK_3     ), windows $ W.shift      "三") -- bring window to workspace 3
    , ((modm .|. shiftMask, xK_4     ), windows $ W.shift      "四") -- bring window to workspace 4
    , ((modm .|. shiftMask, xK_5     ), windows $ W.shift      "五") -- bring window to workspace 5
    , ((modm .|. shiftMask, xK_6     ), windows $ W.shift      "六") -- bring window to workspace 6
    , ((modm .|. shiftMask, xK_7     ), windows $ W.shift      "七") -- bring window to workspace 7
    , ((modm .|. shiftMask, xK_8     ), windows $ W.shift      "八") -- bring window to workspace 8
    , ((modm .|. shiftMask, xK_9     ), windows $ W.shift      "九") -- bring window to workspace 9
    ]
    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--MOUSE BINDINGS----------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

--MATCH HOOKS-------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog            --> doFloat
  ]

--PRETTY PRINTER----------------------------------------------------------------

myXmobarPP :: PP
myXmobarPP = def
  { ppSep             = magenta " • "
  , ppTitleSanitize   = xmobarStrip
  , ppCurrent         = wrap " " "" . xmobarBorder "Bottom" "#0088ee" 2
  , ppVisible         = wrap " " "" . xmobarBorder "Top" "#0088ee" 2
  , ppHidden          = white . wrap " " ""
  , ppHiddenNoWindows = lowWhite . wrap " " ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  , ppOrder           = \[ws, _, _, _] -> [ws]
  , ppExtras          = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#ffffff" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#444444" ""

--LAYOUTS-----------------------------------------------------------------------

myLayout = threeCol ||| tiled ||| Mirror tiled ||| Full
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1
    ratio    = 6/10
    delta    = 2/100

--DEFAULT CONFIG----------------------------------------------------------------

main :: IO()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
  { terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , borderWidth        = myBorderWidth
  , modMask            = myModMask

  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor

  , keys               = myKeys
  , mouseBindings      = myMouseBindings

  , layoutHook         = spacingWithEdge 8 $ myLayout
  , manageHook         = myManageHook
  }
