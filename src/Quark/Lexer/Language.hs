{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Window.Core
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Umbrella module to handle varying languages
--
---------------------------------------------------------------

module Quark.Lexer.Language ( assumeLanguage
                            , tokenize
                            , colorize ) where

import System.FilePath ( takeExtension )

import qualified Data.Text as T

import Quark.Lexer.Core ( tokenizeNothing
                        , nothingColors )
import Quark.Lexer.Haskell ( tokenizeHaskell
                           , haskellColors )
import Quark.Lexer.Python ( tokenizePython
                          , pythonColors )
import Quark.Lexer.Rust ( tokenizeRust
                        , rustColors )
import Quark.Lexer.Clojure ( tokenizeClojure
                           , clojureColors )
-- import Quark.Lexer.Shell ( tokenizeShellScript
--                          , shellScriptColors )

import Quark.Types

assumeLanguage :: FilePath -> Language
assumeLanguage path
    | extension == ".py"  = "Python"
    | extension == ".hs"  = "Haskell"
    | extension == ".sh"  = "Shell script"
    | extension == ".rs"  = "Rust"
    | extension == ".clj" = "Clojure"
    | otherwise           = "Undefined_"
  where
    extension = takeExtension path

tokenize :: Language -> T.Text -> [Token]
tokenize language
    | language == "Haskell" = tokenizeHaskell
    | language == "Python"  = tokenizePython
    | language == "Rust"    = tokenizeRust
    | language == "Clojure" = tokenizeClojure
    | otherwise             = tokenizeNothing

colorize :: Language -> Token -> Int
colorize language
    | language == "Haskell" = haskellColors
    | language == "Python"  = pythonColors
    | language == "Rust"    = rustColors
    | language == "Clojure" = clojureColors
    | otherwise             = nothingColors