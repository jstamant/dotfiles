-- This configuration depends on xmonad=0.17 and xmonad-contrib=0.17

-- Core dependencies
import XMonad
import qualified XMonad.StackSet as W -- For stack, window, and view functions
import XMonad.Hooks.EwmhDesktops

-- Status-bar (xmobar) dependencies
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers -- For showing window titles
import XMonad.Util.ClickableWorkspaces -- For clickable workspaces (nothing else is clickable...)

-- Layout dependencies
import XMonad.Hooks.InsertPosition -- For placing new windows at the end of the stack
import XMonad.Layout.NoBorders -- For removing borders on layouts with smartBorders
import XMonad.Layout.Renamed -- For renaming layouts

-- Generic utility dependencies
import qualified Data.Map as M
import System.Exit -- required for quitting xmonad
import XMonad.Util.EZConfig (mkKeymap)

-- Window-rule depencencies
import XMonad.Hooks.ManageHelpers -- Includes basic policies for window rules

-- Unsorted dependencies
import XMonad.Actions.CycleWS (nextScreen, prevScreen)

--
-- Main - putting it all together
--

main :: IO ()
main = xmonad
  . ewmhFullscreen
  . ewmh
  . dynamicEasySBs spawnBar -- TODO need to add a toggleStrutsKey
  $ myConfig

--
-- Configuration
--

myConfig = def
  { borderWidth        = 2
  , workspaces         = myWorkspaces
  , layoutHook         = myLayouts
  , terminal           = "kitty"
  , normalBorderColor  = "#444444"
  , focusedBorderColor = "#6699cc"
  , modMask            = mod4Mask -- Windows key / Super_L
  , keys               = myKeys
  -- , logHook         = using the default log hook
  , startupHook        = myStartupHook
  , mouseBindings      = myMouseBindings
  , manageHook         = myManageHook
  -- , handleEventHook = using the default
  , focusFollowsMouse  = True
  , clickJustFocuses   = True
  }

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

--
-- Keybindings
--

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys = \conf -> mkKeymap conf $
  [ ("M-q",         kill)  -- Close the focused window
  , ("M-<Space>",   sendMessage NextLayout) -- Rotate through the available layouts
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf) -- Reset the layouts on the current workspace to default
  , ("M-n",         refresh) -- Resize viewed windows to the correct size
  -- Move window focus
  , ("M-j",       windows W.focusDown)   -- Move focus to the next window
  , ("M-k",       windows W.focusUp)     -- Move focus to the previous window
  , ("M-<Tab>",   windows W.focusDown)   -- Move focus to the next window
  , ("M-S-<Tab>", windows W.focusUp)     -- Move focus to the previous window
  , ("M-m",       windows W.focusMaster) -- Move focus to the master window
  -- Move screen focus
  , ("M-.", nextScreen) -- Move focus to the next monitor
  , ("M-,", prevScreen) -- Move focus to the previous monitor
  , ("M-o", nextScreen) -- Move focus to the other monitor (with two monitors, nextScreen does the trick)
  -- Modifying the window order
  , ("M-S-j", windows W.swapDown)    -- Swap the focused window with the next window
  , ("M-S-k", windows W.swapUp)      -- Swap the focused window with the previous window
  , ("M-S-m", windows W.shiftMaster) -- Make the focused window the master, and shift everything else
  -- Master/slave commands
  , ("M-h",   sendMessage Shrink)            -- Shrink the master area
  , ("M-l",   sendMessage Expand)            -- Expand the master area
  , ("M-S-h", sendMessage (IncMasterN 1))    -- Increment the number of windows in the master area
  , ("M-S-l", sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area
  -- Floating layer support
  , ("M-t", withFocused $ windows . W.sink)  -- Push window back into tiling
  -- Quit or restart
  , ("M-S-q", io (exitWith ExitSuccess))                      -- Quit xmonad
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart") -- Recompile and restart xmonad
  ]
  ++
  -- Workspace focus and window moving
  -- mod-[1..9]       -- Focus on workspace N
  -- mod-shift-[1..9] -- Move window to workspace N
  [ ("M-" ++ shift ++ [key], action tag)
  | (tag, key) <- zip myWorkspaces "123456789"
  , (shift, action) <- [ ("", windows . W.greedyView)
                       , ("S-", windows . W.shift)]
  ]
  ++
  -- Screen-selection keys
  -- Only works for 2 screens, with the primary being on the right
  [ ("M-" ++ shift ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
  | (key, scr)  <- zip "uiwe" [1,0,1,0]
  , (action, shift) <- [(W.view, ""), (W.shift, "S-")]
  ]

--
-- Mouse bindings
--

-- These were taken from the default config
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  -- mod-button1 %! Set the window to floating mode and move by dragging
  [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                               >> windows W.shiftMaster)
  -- mod-button2 %! Raise the window to the top of the stack
  , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  -- mod-button3 %! Set the window to floating mode and resize by dragging
  , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                               >> windows W.shiftMaster)
  -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

--
-- Layouts
--

myLayouts = myFull ||| myTall
  where
    myTall   = renamed [Replace "TALL"] $ smartBorders $ Tall nmaster delta ratio
    myFull   = renamed [Replace "FULL"] $ smartBorders $ Full
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

--
-- Status bar configuration
--

myXmobarPP :: PP
myXmobarPP = def
  { ppCurrent         = blue . wrap (fgBright "[") (fgBright "]")
  , ppVisible         = fgBright . pad
  , ppHidden          = fg . pad
  , ppHiddenNoWindows = bgBright . pad
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  , ppSep             = blue "• "
  , ppWsSep           = ""
  , ppTitleSanitize   = xmobarStrip
  , ppLayout          = wrap "" " "
  , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
  , ppExtras          = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (brightWhite "[") (brightWhite    "]") . blue  . ppWindow
    formatUnfocused = wrap (white       "[") (white          "]") . white . ppWindow
    -- Windows should have *some* title, which should not not exceed a sane length
    -- TODO make each window the same size in the bar
    -- TODO make workspaces a bit clearer if they're occupied or not..
    ppWindow :: String -> String
    ppWindow    = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30
    blue, white, magenta, red, brightWhite, yellow :: String -> String
    magenta     = xmobarColor "#ff79c6" ""
    blue        = xmobarColor "#6699cc" ""
    white       = xmobarColor "#d3d0c8" ""
    yellow      = xmobarColor "#f1fa8c" ""
    red         = xmobarColor "#ff5555" ""
    brightWhite = xmobarColor "#f2f0ec" ""
    fg          = xmobarColor "#d3d0c8" ""
    fgBright    = xmobarColor "#f2f0ec" ""
    bg          = xmobarColor "#2d2d2d" ""
    bgBright    = xmobarColor "#747369" ""

secondaryPP :: PP
secondaryPP = myXmobarPP
  { ppOrder = \[_, _, _, wins] -> []
  }

primarySB   = statusBarProp "xmobar -x 0 ~/.config/xmobar/xmobarrc" $ clickablePP myXmobarPP
secondarySB :: Int -> StatusBarConfig
secondarySB id = statusBarPropTo "_XMONAD_LOG_2" ("xmobar -x " ++ (show id) ++ " ~/.config/xmobar/xmobarrc-secondary") $ clickablePP secondaryPP

spawnBar :: ScreenId -> IO StatusBarConfig
spawnBar 0 = pure $ primarySB
spawnBar 1 = pure $ secondarySB 1

--
-- Window-management configuration
--

-- New windows are placed at the bottom of the stack
myManageHook :: ManageHook
myManageHook = insertPosition End Newer
  <+> composeAll
  [ className =? "galculator"  --> doCenterFloat
  , className =? "Pavucontrol" --> doCenterFloat
  , className =? "Xarchiver"   --> doCenterFloat
  , isDialog                   --> doCenterFloat
  -- Assigning programs to certain workspaces
  -- Note that I'm doing -1 to show which workspace I'm getting,
  -- which is zero-indexed in the array.
  , className =? "discord"     --> doShift (myWorkspaces !! (9-1))
  ]

--
-- Startup commands
--

-- Start xmonad-specific applications, applications that are common to
-- all my window-managers are started in ~/.xprofile
myStartupHook :: X ()
myStartupHook = do
  spawn "$HOME/.xmonad/systray.sh &" -- System tray
