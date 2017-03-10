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
                     , emptyProjectMeta
                     , projectRoot
                     , findDefault
                     , replaceDefault
                     , setBuffers
                     , setRoot
                     , setFindDefault
                     , setReplaceDefault
                     , assumeRoot
                     , activeP ) where

import System.FilePath ( takeDirectory )
import Data.Bifunctor ( second )

import Data.ByteString.UTF8 ( ByteString )

import Quark.Buffer
import Quark.Flipper

data ProjectMeta = ProjectMeta { root' :: FilePath
                               , findDefault' :: ByteString
                               , replaceDefault' :: ByteString } deriving Eq

type Project = (Flipper ExtendedBuffer, ProjectMeta)

emptyProjectMeta :: ProjectMeta
emptyProjectMeta = ProjectMeta "" "" ""

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
setRoot' s (ProjectMeta _ a b) = ProjectMeta s a b

setFindDefault :: ByteString -> Project -> Project
setFindDefault s project = second (setFindDefault' s) project

setFindDefault' :: ByteString -> ProjectMeta -> ProjectMeta
setFindDefault' s (ProjectMeta a _ b) = ProjectMeta a s b

setReplaceDefault :: ByteString -> Project -> Project
setReplaceDefault s project = second (setReplaceDefault' s) project

setReplaceDefault' :: ByteString -> ProjectMeta -> ProjectMeta
setReplaceDefault' s (ProjectMeta a b _) = ProjectMeta a b s

assumeRoot :: FilePath -> FilePath
assumeRoot = takeDirectory

activeP :: Project -> ExtendedBuffer
activeP (buffers, _) = active buffers