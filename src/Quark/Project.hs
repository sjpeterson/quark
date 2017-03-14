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
                     , projectTree
                     , findDefault
                     , replaceDefault
                     , setBuffers
                     , setRoot
                     , setFindDefault
                     , setReplaceDefault
                     , setProjectTree
                     , flipNext'
                     , flipPrevious'
                     , toLines
                     , assumeRoot
                     , activeP
                     , active'
                     , activePath
                     , firstF'
                     , firstTree
                     , idxOfActive'
                     , expand
                     , contract ) where

import System.FilePath ( takeDirectory
                       , takeFileName
                       , joinPath )
import Data.Bifunctor ( second )
import Data.List ( sort
                 , isPrefixOf )

import Data.ByteString.UTF8 ( ByteString )
import qualified Data.ByteString.UTF8 as U

import Quark.Buffer ( ExtendedBuffer )
import Quark.Flipper ( Flipper
                     , active
                     , firstF
                     , flipNext
                     , flipPrevious
                     , flipTo
                     , flipToLast
                     , toList
                     , nextEmpty
                     , previousEmpty )
import Quark.Helpers ( (~~) )
import Quark.Types ( ProjectTree
                   , ProjectTreeElement ( RootElement
                                        , FileElement
                                        , DirectoryElement ) )

data ProjectMeta = ProjectMeta { root' :: FilePath
                               , projectTree' :: ProjectTree
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

projectTree :: Project -> ProjectTree
projectTree (_, projectMeta) = projectTree' projectMeta

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

firstTree :: (ProjectTree -> ProjectTree) -> Project -> Project
firstTree f project = setProjectTree (f $ projectTree project) project

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

activePath :: ProjectTree -> FilePath
activePath t = case active t of
    RootElement _       -> rootPath
    DirectoryElement t' -> activePath t'
    FileElement path    -> joinPath [rootPath, path]
  where
    (RootElement rootPath, _, _) = flipTo 0 t

idxOfActive' :: ProjectTree -> Int
idxOfActive' t = case active t of
    DirectoryElement t' -> lengthPrevious t + idxOfActive' t'
    _                   -> lengthPrevious t

lengthPrevious :: ProjectTree -> Int
lengthPrevious (_, p, _) = sum $ map length' p

length' :: ProjectTreeElement -> Int
length' (DirectoryElement t') = sum $ map length' $ toList t'
length' _                     = 1

firstF' :: (ProjectTreeElement -> ProjectTreeElement)
        -> ProjectTree -> ProjectTree
firstF' f (a, p, n) = case a of
    DirectoryElement t' -> (DirectoryElement $ firstF' f t', p, n)
    _                   -> (f a, p, n)

-- this version stops at the parent DirectoryElement of an active RootElement
firstF'' :: (ProjectTreeElement -> ProjectTreeElement)
        -> ProjectTree -> ProjectTree
firstF'' f (a, p, n) = case a of
    DirectoryElement (RootElement _, _, _) ->(f a, p, n)
    DirectoryElement t' -> (DirectoryElement $ firstF'' f t', p, n)
    _                   -> (f a, p, n)

flipNext' :: Project -> Project
flipNext' = firstTree flipNext''

flipPrevious' :: Project -> Project
flipPrevious' = firstTree flipPrevious''

flipNext'' :: ProjectTree -> ProjectTree
flipNext'' t@(a, p, n) = case a of
    DirectoryElement t' -> if nextEmpty t'
                               then flipNext t
                               else (DirectoryElement $ flipNext'' t', p, n)
    _                   -> flipNext t

flipPrevious'' :: ProjectTree -> ProjectTree
flipPrevious'' t@(a, p, n) = case a of
    DirectoryElement t' -> if previousEmpty t'
                               then (flipToLast' a', p', n')
                               else (DirectoryElement $ flipPrevious'' t', p, n)
    _                   -> (flipToLast' a', p', n')
  where
    (a', p', n') = flipPrevious t

flipToLast' :: ProjectTreeElement -> ProjectTreeElement
flipToLast' x = case x of
    DirectoryElement (a, p, []) -> DirectoryElement (flipToLast' a, p, [])
    DirectoryElement t          -> flipToLast' (DirectoryElement $ flipToLast t)
    _                           -> x

expand :: [ProjectTreeElement] -> Project -> Project
expand p project = case active' $ projectTree project of
    RootElement _ -> firstTree (firstF'' (expand' p)) project
    _             -> project

expand' :: [ProjectTreeElement] -> ProjectTreeElement -> ProjectTreeElement
expand' p (DirectoryElement t) = DirectoryElement $ flipNext (r, [], sort p)
  where
    (r, _, _) = flipTo 0 t
expand' _ x                    = x

contract :: Project -> Project
contract project = case active' $ projectTree project of
    RootElement _ -> firstTree (firstF'' contract') project
    _             -> project

contract' :: ProjectTreeElement -> ProjectTreeElement
contract' (DirectoryElement t) = DirectoryElement (r, [], [])
  where
    (r, _, _) = flipTo 0 t
contract' x                    = x

toLines :: ProjectTree -> [ByteString]
toLines t = concat $ map (toLines' 0) (take k treeList) ++
                map (toLines' 1) (drop k treeList)
  where
    (treeList, k) = treeListAndSplit t

toLines' :: Int -> ProjectTreeElement -> [ByteString]
toLines' headStyle x =  case x of
    (RootElement path)   -> [U.fromString (takeFileName path) ~~ "/"]
    (FileElement path)   -> [ownHead ~~ U.fromString path]
    (DirectoryElement t) -> zipWith (~~) dirHead $ concat $
                                map (toLines' 2) (take k treeList) ++
                                    map (toLines' 3) (drop k treeList)
                              where
                                (treeList, k) = treeListAndSplit t
  where
    dirHead = ownHead:(repeat childHead)
    ownHead
        | headStyle == 0 = "\226\149\159\226\148\128"
        | headStyle == 1 = "\226\149\153\226\148\128"
        | headStyle == 2 = "\226\148\156\226\148\128"
        | otherwise      = "\226\148\148\226\148\128"
    childHead
        | headStyle == 0 = "\226\149\145 "
        | headStyle == 2 = "\226\148\130 "
        | otherwise      = "  "

treeListAndSplit :: ProjectTree -> ([ProjectTreeElement], Int)
treeListAndSplit t = (treeList, length treeList - 1)
  where
    treeList = toList t

branch :: Int -> ByteString -> ByteString -> [ByteString]
branch k own last'
    | k < 1     = []
    | k == 1    = [""]
    | otherwise = "":(replicate (k - 2) own) ++ [last']