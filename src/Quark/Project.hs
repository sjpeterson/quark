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
                     , projectRoot
                     , setRoot
                     , assumeRoot
                     , activeP ) where

import System.FilePath ( takeDirectory )

import Quark.Buffer
import Quark.Flipper

type Project = (Flipper ExtendedBuffer, FilePath)

projectRoot :: Project -> FilePath
projectRoot (_, path) = path

setRoot :: FilePath -> Project -> Project
setRoot newPath (buffers, _) = (buffers, newPath)

assumeRoot :: FilePath -> FilePath
assumeRoot = takeDirectory

activeP :: Project -> ExtendedBuffer
activeP (buffers, _) = active buffers