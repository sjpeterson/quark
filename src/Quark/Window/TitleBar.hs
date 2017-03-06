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
                               , refresh )

import Quark.Colors
import Quark.Helpers
import Quark.Types

setTitle :: Window -> String -> IO ()
setTitle w title = do
    setTextColor w titleBarPair
    mvAddString w 0 0 (padMidToLen c leftText rightText)
    move w 0 0
    refresh w
  where
    (TitleBar _ (_, c)) = w
    leftText = " quark - " ++ title
    rightText = "0.0.1a "