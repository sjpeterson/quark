---------------------------------------------------------------
--
-- Module:      Quark.Frontend.HSCurses
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Frontend module for HSCurses
--
---------------------------------------------------------------

module Quark.Frontend.HSCurses ( start
                               , end
                               , getKey ) where

import qualified UI.HSCurses.Curses as Curses

import Data.Char ( isPrint )

import Quark.Types ( Key ( CharKey
                         , CtrlKey
                         , FnKey
                         , SpecialKey
                         , ResizeKey
                         , InvalidKey ) )

start :: IO ()
start = do
    Curses.initScr
    hasColors <- Curses.hasColors
    if hasColors then do Curses.startColor
                         Curses.useDefaultColors
                         defineColors
                 else return ()
    Curses.resetParams
    Curses.wclear Curses.stdScr
    Curses.refresh

end :: IO ()
end = Curses.endWin

getKey :: IO (Key)
getKey = translateKey <$> Curses.getCh

translateKey :: Curses.Key -> Key
translateKey k@(Curses.KeyChar c)
    | c == '\r'   = SpecialKey "Return"
    | c == '\DEL' = SpecialKey "Backspace"
    | c == '\t'   = SpecialKey "Tab"
    | c == '\SOH' = CtrlKey 'a'
    | c == '\STX' = CtrlKey 'b'
    | c == '\ETX' = CtrlKey 'c'
    | c == '\EOT' = CtrlKey 'd'
    | c == '\ENQ' = CtrlKey 'e'
    | c == '\ACK' = CtrlKey 'f'
    | c == '\a'   = CtrlKey 'g'
--     | c == '\t'   = CtrlKey 'i' -- identical to Tab
    | c == '\n'   = CtrlKey 'j'
    | c == '\v'   = CtrlKey 'k'
    | c == '\f'   = CtrlKey 'l'
--     | c == '\r'   = CtrlKey 'm' -- identical to Return
    | c == '\SO'  = CtrlKey 'n'
    | c == '\SI'  = CtrlKey 'o'
    | c == '\DLE' = CtrlKey 'p'
    | c == '\DC1' = CtrlKey 'q'
    | c == '\DC2' = CtrlKey 'r'
    | c == '\DC3' = CtrlKey 's'
    | c == '\DC4' = CtrlKey 't'
    | c == '\NAK' = CtrlKey 'u'
    | c == '\SYN' = CtrlKey 'v'
    | c == '\ETB' = CtrlKey 'w'
    | c == '\CAN' = CtrlKey 'x'
    | c == '\EM'  = CtrlKey 'y'
    | c == '\SUB' = CtrlKey 'z'
    | otherwise   = if isPrint c then CharKey c
                                 else InvalidKey $ show k
translateKey k = case k of
    Curses.KeyDC          -> SpecialKey "Delete"
    Curses.KeySDC         -> SpecialKey "Shift-Delete"
    Curses.KeyBTab        -> SpecialKey "Shift-Tab"
    Curses.KeyHome        -> SpecialKey "Home"
    Curses.KeySHome       -> SpecialKey "Shift-Home"
    Curses.KeyUnknown 537 -> SpecialKey "Ctrl-Home"
    Curses.KeyUnknown 538 -> SpecialKey "Ctrl-Shift-Home"
    Curses.KeyEnd         -> SpecialKey "End"
    Curses.KeySEnd        -> SpecialKey "Shift-End"
    Curses.KeyUnknown 532 -> SpecialKey "Ctrl-End"
    Curses.KeyUnknown 533 -> SpecialKey "Ctrl-Shift-End"
    Curses.KeyPPage       -> SpecialKey "PgUp"
    Curses.KeySPrevious   -> SpecialKey "Shift-PgUp"
    Curses.KeyNPage       -> SpecialKey "PgDn"
    Curses.KeySNext       -> SpecialKey "Shift-PgDn"
    Curses.KeyIC          -> SpecialKey "Insert"
    Curses.KeyUp          -> SpecialKey "Arrow-Up"
    Curses.KeyUnknown 569 -> SpecialKey "Ctrl-Up"
    Curses.KeyUnknown 570 -> SpecialKey "Ctrl-Shift-Up"
    Curses.KeyDown        -> SpecialKey "Down"
    Curses.KeyUnknown 528 -> SpecialKey "Ctrl-Down"
    Curses.KeyUnknown 529 -> SpecialKey "Ctrl-Shift-Down"
    Curses.KeyLeft        -> SpecialKey "Left"
    Curses.KeySLeft       -> SpecialKey "Shift-Left"
    Curses.KeyUnknown 548 -> SpecialKey "Ctrl-Left"
    Curses.KeyUnknown 549 -> SpecialKey "Ctrl-Shift-Left"
    Curses.KeyRight       -> SpecialKey "Right"
    Curses.KeySRight      -> SpecialKey "Shift-Right"
    Curses.KeyUnknown 563 -> SpecialKey "Ctrl-Right"
    Curses.KeyUnknown 564 -> SpecialKey "Ctrl-Shift-Right"
    Curses.KeyBackspace   -> CtrlKey 'h'
    Curses.KeyResize      -> ResizeKey
    _                     -> InvalidKey $ show k

-- Color pair indices
defaultColor = 0 :: Int
red          = 1 :: Int
green        = 2 :: Int
orange       = 3 :: Int
blue         = 4 :: Int
purple       = 5 :: Int
teal         = 6 :: Int
lightGray    = 7 :: Int
darkGray     = 8 :: Int
lightRed     = 9 :: Int
lightGreen   = 10 :: Int
yellow       = 11 :: Int
lightBlue    = 12 :: Int
lightPurple  = 13 :: Int
cyan         = 14 :: Int
white        = 15 :: Int
black        = 16 :: Int
-- 17 through 33 are for selections (red + 17 is selected red)
titleBarColor   = 34 :: Int
lineNumberColor = 35 :: Int

-- selection color and an alternative text color to use if they match
selectionColor = darkGray
backupColor    = lightGray

defineColors :: IO ()
defineColors = do
  mapM_ (\n -> defineColor n n (-1)) [1..16]
  defineColor 17 (-1) selectionColor
  mapM_ (\n -> if n == selectionColor
                   then defineColor (n + 17) backupColor selectionColor
                   else defineColor (n + 17) n selectionColor) [1..16]
  defineColor titleBarColor black orange
  defineColor lineNumberColor lightGray (-1)

-- neat shorthand for initializing foreground color f and background color b
-- as color pair n
defineColor :: Int -> Int -> Int -> IO ()
defineColor n f b =
  Curses.initPair (Curses.Pair n) (Curses.Color f) (Curses.Color b)