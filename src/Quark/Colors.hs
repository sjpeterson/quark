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
-- 17 through 32 are for selections
titleBarColor = 33 :: Int
lineNumberColor = 34 :: Int

defineColors :: IO ()
defineColors = do
  mapM_ (\n -> defineColor n n (-1)) [1..16]
  mapM_ (\n -> defineColor (n + 16) n 8) [1..16]
  -- defineColor 17 16 7
  defineColor 33 16 3
  defineColor 34 7 (-1)
  defineColor 19 (-1) 8

-- neat shorthand for initializing foreground color f and background color b
-- as color pair n
defineColor :: Int -> Int -> Int -> IO ()
defineColor n f b =
  Curses.initPair (Curses.Pair n) (Curses.Color f) (Curses.Color b)
