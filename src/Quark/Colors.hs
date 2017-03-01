module Quark.Colors where

import Quark.Types

-- Color IDs
defaultBg    = (-1) :: Int
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

selectionColor = darkGray
backupColor    = lightGray

-- Color pairs
titleBarPair   = (black, orange)
lineNumberPair = (lightGray, defaultBg)