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

module Quark.Window.TitleBar ( setTitle ) where

import Quark.Frontend.HSCurses ( Window ( TitleBar )
                               , setTextColor
                               , mvAddString
                               , move
                               , refresh
                               , hideCursor )

import Quark.Colors
import Quark.Helpers
import Quark.Types

setTitle :: Window -> String -> IO ()
setTitle w path = do
    setTextColor w titleBarPair
    mvAddString w 0 0 (fixToLenPadMid c leftText suffix ' ')
    move w 0 0
    hideCursor
    refresh w
  where
    (TitleBar _ (_, c)) = w
    leftText = prefix ++ (trimPathHead k path)
    k = (c - length prefix - length suffix)
    prefix = "quark - "
    suffix = "0.0.1a "
