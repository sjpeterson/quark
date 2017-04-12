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
import Data.ByteString.UTF8 (ByteString)
-- import qualified Data.ByteString.Char8 as B

import Quark.Lexer.Core ( tokenizeNothing
                        , nothingColors )
import Quark.Lexer.Haskell ( tokenizeHaskell
                           , haskellColors )
import Quark.Lexer.Python ( tokenizePython
                          , pythonColors )
import Quark.Lexer.Rust ( tokenizeRust
                        , rustColors )
-- import Quark.Lexer.Shell ( tokenizeShellScript
--                          , shellScriptColors )

import Quark.Types

assumeLanguage :: FilePath -> Language
assumeLanguage path
    | extension == ".py" = "Python"
    | extension == ".hs" = "Haskell"
    | extension == ".sh" = "Shell script"
    | extension == ".rs" = "Rust"
    | otherwise          = "UndefinedFish"
  where
    extension = takeExtension path

tokenize :: Language -> [Token] -> ByteString -> [Token]
tokenize language
    | language == "Haskell" = tokenizeHaskell
    | language == "Python"  = tokenizePython
    | language == "Rust"    = tokenizeRust
    | otherwise             = tokenizeNothing

colorize :: Language -> Token -> Int
colorize language
    | language == "Haskell" = haskellColors
    | language == "Python"  = pythonColors
    | language == "Rust"    = rustColors
    | otherwise             = nothingColors