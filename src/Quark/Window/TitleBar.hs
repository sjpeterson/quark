{-# LANGUAGE OverloadedStrings #-}

--------
--
-- Module:      Quark.Window.TitleBar
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for functions related to the title bar
--
--------

module Quark.Window.TitleBar where

-- import Data.ByteString (ByteString)

import qualified UI.HSCurses.Curses as Curses

import Quark.Window.Core
import Quark.Colors
import Quark.Helpers

setTitle :: Window -> String -> IO ()
setTitle (TitleBar w (_, c)) title = do
    Curses.attrSet Curses.attr0 (Curses.Pair titleBarColor)
    Curses.mvWAddStr w 0 0 (padMidToLen c leftText rightText)
    Curses.wRefresh w
  where
    leftText = " quark - " ++ title
    rightText = "0.0.1a "

fillBackground :: Window -> Int -> IO ()
fillBackground (TitleBar cTitleBar (h, w)) colorId = do
    Curses.wAttrSet cTitleBar (Curses.attr0, Curses.Pair colorId)
    Curses.mvWAddStr cTitleBar 0 0 (take w $ repeat ' ')
    Curses.wMove cTitleBar 1 0
    Curses.wRefresh cTitleBar