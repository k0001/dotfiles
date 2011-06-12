import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
            workspaces         = myWorkspaces,
            manageHook         = manageDocks <+> manageHook defaultConfig,
            layoutHook         = smartBorders $ avoidStruts  $  layoutHook defaultConfig,
            logHook            = dynamicLogWithPP xmobarPP {
               ppOutput        = hPutStrLn xmproc,
               ppTitle         = xmobarColor "green" "" . shorten 50
            },
            terminal           = "urxvt",
            modMask            = mod4Mask,
            focusedBorderColor = "red",
            borderWidth        = 1
        } `additionalKeys` myKeys

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myKeys =
    [
      ((mod4Mask .|. shiftMask, xK_z), spawn "xlock"),
      ((0, xK_Print),                  spawn "scrot"),

      -- Left Alt + Eject triggers Magic SysRq
      -- ((mod1Mask, 0x1008ff2c),          spawn "echo t > /proc/sysrq-trigger"),

      -- Detect X diplays (nvidia) and setup TwinView.
      -- XF86XK_LaunchA
      ((0, 0x1008FF4A),                spawn "disper -d auto -e"),

      -- Audio control. See <X11/XF86keysym.h>
      -- XF86XK_AudioPlay
      ((0, 0x1008FF14),                spawn "ncmpcpp toggle"),
      -- XF86XK_AudioPrev, XF86XK_Prev
      ((0, 0x1008FF16),                spawn "ncmpcpp prev"),
      ((0, 0x1008FF26),                spawn "ncmpcpp prev"),
      -- XF86XK_AudioNext, XF86XK_Next
      ((0, 0x1008FF17),                spawn "ncmpcpp next"),
      ((0, 0x1008FF27),                spawn "ncmpcpp next"),
      -- XF86XK_AudioLowerVolume
      ((0, 0x1008FF11),                spawn "pulsevolume down"),
      -- XF86XK_AudioRaiseVolume
      ((0, 0x1008FF13),                spawn "pulsevolume up"),
      -- XF86XK_AudioMute
      ((0, 0x1008FF12),                spawn "pulsevolume mute"),

      -- Mon/Kbd Backlight control. See <X11/XF86keysym.h>
      -- XF86XK_MonBrightnessUp
      ((0, 0x1008FF02),                spawn "backlight +50"),
      -- XF86XK_MonBrightnessDown
      ((0, 0x1008FF03),                spawn "backlight -50"),
      -- XF86XK_KbdBrightnessUp
      ((0, 0x1008FF05),                spawn "keylight +50"),
      -- XF86XK_KbdBrightnessDown
      ((0, 0x1008FF06),                spawn "keylight -50")
    ]
    ++
    [((m .|. mod4Mask, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
