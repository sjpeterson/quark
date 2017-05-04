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

module Quark.Window.ProjectView ( printTree
                                , updateOffsetP ) where

import qualified Data.Text as T

import Quark.Frontend.HSCurses ( Window ( ProjectView )
                               , setTextColor
                               , mvAddString
                               , clear
                               , refresh )
import Quark.Types ( ProjectTree
                   , Row )
import Quark.Project ( toLines
                     , idxOfActive' )
import Quark.Colors ( treeActivePair
                    , treeDefaultPair )
import Quark.Helpers ( fixTo )

printTree :: Bool -> Window -> ProjectTree -> IO ()
printTree indicateActive w@(ProjectView _ (r, c) rr) t = do
    clear w
    mapM_ (\(k, a, s) -> printTreeLine w k a s) treeList
    refresh w
  where
    treeList = zip3 [0..] isActive (drop rr $
                   map (fixTo c. T.unpack . (T.take c)) $ toLines t)
    isActive = take (r - 1) $ drop rr [(\x -> if x == n
                                                  then True && indicateActive
                                                  else False) x | x <- [0..]]
    n = idxOfActive' t

printTreeLine :: Window -> Row -> Bool -> String -> IO ()
printTreeLine w k isActive s = do
    setTextColor w $ if isActive then treeActivePair else treeDefaultPair
    mvAddString w k 0 s

updateOffsetP :: Int -> Window -> Window
updateOffsetP n p@(ProjectView w (r, c) rr) = case inRange of
    True  -> p
    False -> updateOffsetP n $ ProjectView w (r, c) $
                                   if n < rr then max 0 (rr -3) else rr + 3
  where
    inRange = if n >= rr && n < rr + r - 1 then True else False