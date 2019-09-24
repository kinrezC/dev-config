{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Monoid

import XMonad
import qualified Data.Map as M
import Data.List
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import System.IO
import Control.Monad
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce

-- Make our own Picture-in-Picture mode
import XMonad.Config.Prime (LayoutClass)
import Graphics.X11 (Rectangle(..))

data PiP a = PiP deriving (Show, Read)

mkpip rect@(Rectangle sx sy sw sh) (master:snd:ws) = [small, (master, rect)]
  where small = (snd, (Rectangle px py pw ph))
        px = sx + fromIntegral sw - fromIntegral pw - 32
        py = sy + fromIntegral sh - fromIntegral ph - 32
	pw = sw `div` 4
	ph = sh `div` 4
mkpip rect (master:ws) = [(master, rect)]
mkpip rect [] = []

instance LayoutClass PiP a where
    pureLayout PiP rect stack = mkpip rect ws
      where ws = W.integrate stack

    description _ = "PiP"

myTerminal      = "alacritty"
myFocusFollowsMouse = False
myClickJustFocuses = False

myModMask       = mod1Mask -- or mod4Mask for super
myWorkspaces    = ["web","code","cli","music","discord", "slack"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((0, xK_Print), (spawn "scrot"))
    , ((mod4Mask, xK_a), (spawn myTerminal))
    , ((modm, xK_Print), (spawn "scrot -s"))
    , ((0, xF86XK_MonBrightnessDown), (spawn "~/bin/adjust-brightness - &"))
    , ((0, xF86XK_MonBrightnessUp), (spawn "~/bin/adjust-brightness + &"))
    ] 

myLayout = tiled ||| (Mirror tiled) ||| Full ||| PiP
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = toRational (2 / (1 + sqrt 5 :: Double))

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myStartupHook = do
  spawnOnce "feh --bg-scale /home/mattczernik/downloads/background.jpg"
  spawnOnce "alacritty"
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do
    xmonad $ desktopConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = 0,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        keys               = \c -> myKeys c `M.union` keys XMonad.def c,
        layoutHook         = desktopLayoutModifiers $ noBorders $ myLayout,
        handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig,
	startupHook        = myStartupHook <+> startupHook desktopConfig
    }
