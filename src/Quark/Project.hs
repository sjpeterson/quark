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

module Quark.Project where

import qualified Quark.Buffer as QB

type Project = (FilePath, Flipper QB.ExtendedBuffer)

projectRoot :: Project -> FilePath
projectRoot (path, _) = path

setRoot :: FilePath -> Project -> Project
setRoot newPath (_, buffers) = (newPath, buffers)

