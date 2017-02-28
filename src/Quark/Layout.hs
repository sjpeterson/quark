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
                    , firstL
                    , secondL
                    , bimapL
                    , clearLayout
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

firstL :: (Window -> Window) -> Layout -> Layout
firstL f (MinimalLayout t u p) = MinimalLayout t u (f p)
firstL f (BasicLayout t u d p) = BasicLayout t u d (f p)
firstL f (VSplitLayout t u d p s) = VSplitLayout t u d (f p) s
firstL f (HSplitLayout t u d p s) = HSplitLayout t u d (f p) s

secondL :: (Window -> Window) -> Layout -> Layout
secondL f (VSplitLayout t u d p s) = VSplitLayout t u d p (f s)
secondL f (HSplitLayout t u d p s) = HSplitLayout t u d p (f s)

bimapL :: (Window -> Window) -> (Window -> Window) -> Layout -> Layout
bimapL f0 f1 (VSplitLayout t u d p s) = VSplitLayout t u d (f0 p) (f1 s)
bimapL f0 f1 (HSplitLayout t u d p s) = HSplitLayout t u d (f0 p) (f1 s)

clearLayout :: Layout -> IO ()
clearLayout (MinimalLayout t u p) = do
    Curses.wclear $ cursesWin t
    Curses.wclear $ cursesWin u
    Curses.wclear $ cursesWin p
clearLayout (BasicLayout t u d p) = do
    Curses.wclear $ cursesWin t
    Curses.wclear $ cursesWin u
    Curses.wclear $ cursesWin d
    Curses.wclear $ cursesWin p
clearLayout (VSplitLayout t u d p s) = do
    Curses.wclear $ cursesWin t
    Curses.wclear $ cursesWin u
    Curses.wclear $ cursesWin d
    Curses.wclear $ cursesWin p
    Curses.wclear $ cursesWin s
clearLayout (HSplitLayout t u d p s) = do
    Curses.wclear $ cursesWin t
    Curses.wclear $ cursesWin u
    Curses.wclear $ cursesWin d
    Curses.wclear $ cursesWin p
    Curses.wclear $ cursesWin s

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