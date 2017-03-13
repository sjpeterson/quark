{-# LANGUAGE OverloadedStrings #-}

--------
--
-- Module:      Quark.Window.ProjectView
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for functions related to text view windows
--
--------

module Quark.Window.ProjectView ( printTree ) where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as U

import Quark.Frontend.HSCurses ( Window ( ProjectView )
                               , mvAddString
                               , clear
                               , refresh )
import Quark.Types ( ProjectTree )
import Quark.Project ( toLines )

printTree :: Window -> ProjectTree -> IO ()
printTree w@(ProjectView _ (r, c) rr) t = do
    clear w
    mapM_ (\(k, s) -> mvAddString w k 0 s) $ zip [0..] $ drop rr $
                          map (U.toString . (U.take c)) $ toLines t
    refresh w
