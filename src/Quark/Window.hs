--------
--
-- Module:      Quark.Window
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for quark windows.
--
--------

module Quark.Window ( Window ( TitleBar
                             , UtilityBar
                             , TextView
                             , DirectoryView ) ) where

import qualified UI.HSCurses.Curses as Curses

import Quark.Types

data Window = TitleBar Curses.Window Size
            | UtilityBar Curses.Window Size
            | TextView Curses.Window Size Offset
            | DirectoryView Curses.Window Size Int deriving Show