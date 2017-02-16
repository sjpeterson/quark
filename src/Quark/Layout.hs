--------
--
-- Module:      Quark.Layout
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for quark layouts.
--
--------

module Quark.Layout ( Layout ( MinimalLayout
                             , BasicLayout
                             , VSplitLayout
                             , HSplitLayout )
                    , mapL
                    , titleBar
                    , utilityBar
                    , directoryPane
                    , primaryPane
                    , secondaryPane
                    , defaultLayout ) where

import qualified UI.HSCurses.Curses as Curses

import Quark.Window.Core

data Layout = MinimalLayout { titleBar :: Window
                            , utilityBar :: Window
                            , primaryPane :: Window }
            | BasicLayout { titleBar :: Window
                          , utilityBar :: Window
                          , directoryPane :: Window
                          , primaryPane :: Window }
            | VSplitLayout { titleBar :: Window
                           , utilityBar :: Window
                           , directoryPane :: Window
                           , primaryPane :: Window
                           , secondaryPane :: Window }
            | HSplitLayout { titleBar :: Window
                           , utilityBar :: Window
                           , directoryPane :: Window
                           , primaryPane :: Window
                           , secondaryPane :: Window } deriving Show

defaultLayout :: IO (Layout)
defaultLayout = do
    (r, c) <- Curses.scrSize
    layout <- if c > 85 + 16 then (basicLayout r c)
                             else (minimalLayout r c)
    return layout

mapL :: (Window -> Window) -> Layout -> Layout
mapL f (MinimalLayout t u p) = MinimalLayout t u $ f p
mapL f (BasicLayout t u d p) = BasicLayout t u d $ f p

basicLayout :: Int -> Int -> IO (Layout)
basicLayout r c = do
    let dpWidth = min 24 (c - 32)
    let mainHeight = r - 3
    let mainWidth = c - dpWidth
    cTitleBar <- Curses.newWin 2 c 0 0
    cUtilityBar <- Curses.newWin 2 c (r - 3) 0
    cDirectoryPane <- Curses.newWin (mainHeight + 1) dpWidth 0 1
    cMainView <- Curses.newWin (mainHeight + 1) mainWidth 1 dpWidth
    let qTitleBar = TitleBar cTitleBar (1, c)
    let qUtilityBar = UtilityBar cUtilityBar (1, c)
    let qDirectoryPane = DirectoryView cDirectoryPane (mainHeight, dpWidth) 0
    let qPrimaryPane = TextView cMainView (mainHeight, (c - dpWidth)) (0, 0)
    return $ BasicLayout qTitleBar qUtilityBar qDirectoryPane qPrimaryPane

minimalLayout :: Int -> Int -> IO (Layout)
minimalLayout r c = do
    let mainHeight = r - 3
    cTitleBar <- Curses.newWin 2 c 0 0
    cUtilityBar <- Curses.newWin 2 c (r - 2) 0
    cMainView <- Curses.newWin (mainHeight + 1) c 2 0
    let qTitleBar = TitleBar cTitleBar (1, c)
    let qUtilityBar = UtilityBar cUtilityBar (1, c)
    let qPrimaryPane = TextView cMainView (mainHeight, c) (0, 0)
    return $ MinimalLayout qTitleBar qUtilityBar qPrimaryPane