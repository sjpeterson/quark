module Quark.Colors where

import Quark.Types

import qualified UI.HSCurses.Curses as Curses

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
