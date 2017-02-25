--------
--
-- Module:      Quark.Window.Core
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

module Quark.Window.Core ( Window ( TitleBar
                                  , UtilityBar
                                  , TextView
                                  , DirectoryView )
                         , updateCursor ) where

import qualified UI.HSCurses.Curses as Curses

import Quark.Types

data Window = TitleBar Curses.Window Size
            | UtilityBar Curses.Window Size
            | TextView Curses.Window Size Offset
            | DirectoryView Curses.Window Size Int deriving Show

updateCursor :: Window -> Offset -> Cursor -> IO ()
updateCursor (TextView w _ _) (x0, y0) (x, y) =
    Curses.wMove w (x - x0) (y - y0) >> Curses.wRefresh w
updateCursor (UtilityBar w (rr, cc)) (r, c) (_, y) =
    Curses.wMove w (min r rr) (min (y + c) cc) >> Curses.wRefresh w