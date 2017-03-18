----------------------------------------------------------------
--
-- Module:      Quark.Layout
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Module for frontend-agnostic layout functions.
--
----------------------------------------------------------------

module Quark.Layout ( firstL
                    , secondL
                    , thirdL
                    , bimapL ) where

import Quark.Frontend.HSCurses ( Window
                               , Layout ( MinimalLayout
                                        , BasicLayout
                                        , VSplitLayout
                                        , HSplitLayout ))

firstL :: (Window -> Window) -> Layout -> Layout
firstL f (MinimalLayout t u p) = MinimalLayout t u (f p)
firstL f (BasicLayout t u d p) = BasicLayout t u d (f p)
firstL f (VSplitLayout t u d p s) = VSplitLayout t u d (f p) s
firstL f (HSplitLayout t u d p s) = HSplitLayout t u d (f p) s

secondL :: (Window -> Window) -> Layout -> Layout
secondL f (VSplitLayout t u d p s) = VSplitLayout t u d p (f s)
secondL f (HSplitLayout t u d p s) = HSplitLayout t u d p (f s)

thirdL :: (Window -> Window) -> Layout -> Layout
thirdL f (BasicLayout t u d p) = BasicLayout t u (f d) p
thirdL f (VSplitLayout t u d p s) = VSplitLayout t u (f d) p s
thirdL f (HSplitLayout t u d p s) = HSplitLayout t u (f d) p s

bimapL :: (Window -> Window) -> (Window -> Window) -> Layout -> Layout
bimapL f0 f1 (VSplitLayout t u d p s) = VSplitLayout t u d (f0 p) (f1 s)
bimapL f0 f1 (HSplitLayout t u d p s) = HSplitLayout t u d (f0 p) (f1 s)
