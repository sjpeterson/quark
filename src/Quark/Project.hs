{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------
--
-- Module:      Quark.Project
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Module for quark projects.
--
-- A project is a higher level container for a Flipper ExtendedBuffer with some
-- additional data.
--
---------------------------------------------------------------

module Quark.Project ( Project
                     , ProjectTree
                     , emptyProjectMeta
                     , projectRoot
                     , findDefault
                     , replaceDefault
                     , setBuffers
                     , setRoot
                     , setFindDefault
                     , setReplaceDefault
                     , setProjectTree
                     , assumeRoot
                     , activeP
                     , active' ) where

import System.FilePath ( takeDirectory )
import Data.Bifunctor ( second )
import Data.List ( sort )

import Data.ByteString.UTF8 ( ByteString )

import Quark.Buffer
import Quark.Flipper ( Flipper
                     , active
                     , firstF
                     , flipNext
                     , flipPrevious
                     , flipTo
                     , flipToLast
                     , nextEmpty
                     , previousEmpty )
import Quark.Types ( ProjectTree
                   , ProjectTreeElement ( RootElement
                                        , FileElement
                                        , DirectoryElement ) )

data ProjectMeta = ProjectMeta { root' :: FilePath
                               , projectTree :: ProjectTree
                               , findDefault' :: ByteString
                               , replaceDefault' :: ByteString } deriving Eq

type Project = (Flipper ExtendedBuffer, ProjectMeta)

----------------------------
-- Core project functions --
----------------------------

emptyProjectMeta :: ProjectMeta
emptyProjectMeta = ProjectMeta "" (RootElement "", [], []) "" ""

projectRoot :: Project -> FilePath
projectRoot (_, projectMeta) = root' projectMeta

findDefault :: Project -> ByteString
findDefault (_, projectMeta) = findDefault' projectMeta

replaceDefault :: Project -> ByteString
replaceDefault (_, projectMeta) = replaceDefault' projectMeta

setBuffers :: Flipper ExtendedBuffer -> Project -> Project
setBuffers buffers (_, projectMeta) = (buffers, projectMeta)

setRoot :: FilePath -> Project -> Project
setRoot s project = second (setRoot' s) project

setRoot' :: FilePath -> ProjectMeta -> ProjectMeta
setRoot' s (ProjectMeta _ a b c) = ProjectMeta s a b c

setFindDefault :: ByteString -> Project -> Project
setFindDefault s project = second (setFindDefault' s) project

setFindDefault' :: ByteString -> ProjectMeta -> ProjectMeta
setFindDefault' s (ProjectMeta a b _ c) = ProjectMeta a b s c

setReplaceDefault :: ByteString -> Project -> Project
setReplaceDefault s project = second (setReplaceDefault' s) project

setReplaceDefault' :: ByteString -> ProjectMeta -> ProjectMeta
setReplaceDefault' s (ProjectMeta a b c _) = ProjectMeta a b c s

setProjectTree :: ProjectTree -> Project -> Project
setProjectTree t project = second (setProjectTree' t) project

setProjectTree' :: ProjectTree -> ProjectMeta -> ProjectMeta
setProjectTree' t (ProjectMeta a _ b c) = ProjectMeta a t b c

assumeRoot :: FilePath -> FilePath
assumeRoot = takeDirectory

activeP :: Project -> ExtendedBuffer
activeP (buffers, _) = active buffers

---------------------------
-- ProjectTree Functions --
---------------------------

active' :: ProjectTree -> ProjectTreeElement
active' t = case active t of
    DirectoryElement t' -> active' t'
    x                   -> x

flipNext' :: ProjectTree -> ProjectTree
flipNext' t@(a, p, n) = case a of
    DirectoryElement t' -> if nextEmpty t'
                               then flipNext t
                               else (DirectoryElement $ flipNext' t', p, n)
    _                   -> flipNext t

flipPrevious' :: ProjectTree -> ProjectTree
flipPrevious' t@(a, p, n) = case a of
    DirectoryElement t' -> if previousEmpty t'
                               then (flipToLast' a', p', n')
                               else (DirectoryElement $ flipPrevious' t', p, n)
    _                   -> (flipToLast' a', p', n')
  where
    (a', p', n') = flipPrevious t

flipToLast' :: ProjectTreeElement -> ProjectTreeElement
flipToLast' x = case x of
    DirectoryElement (a, p, []) -> DirectoryElement (flipToLast' a, p, [])
    DirectoryElement t          -> flipToLast' (DirectoryElement $ flipToLast t)
    _                           -> x

expand :: ProjectTreeElement -> [ProjectTreeElement] -> ProjectTreeElement
expand (DirectoryElement t) p = DirectoryElement $ flipNext (r, [], sort p)
  where
    (r, _, _) = flipTo 0 t
expand x _                    = x

contract :: ProjectTreeElement -> ProjectTreeElement
contract (DirectoryElement t) = DirectoryElement (r, [], [])
  where
    (r, _, _) = flipTo 0 t
contract x                    = x