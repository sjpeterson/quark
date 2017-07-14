----------------------------------------------------------------------
--
-- Module:      Quark.Types
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------------
--
-- A collection of types and type synonyms that improve readability
--
----------------------------------------------------------------------

module Quark.Types where

import qualified Data.Text as T

import Quark.Flipper ( Flipper
                     , active
                     , flipTo )

-- Type aliases for primitive types
type Row = Int
type Col = Int
type Index = Int
type Selection = Int
type ColorId = Int

type Language = T.Text
type Clipboard = T.Text
type Name = T.Text

-- Type aliases for tuples
type Deletion = (Int, Int)              -- (backspaces, deletes)
type Cursor = (Row, Col)
type Size = (Row, Col)
type Offset = (Row, Col)
type PrintRange = (Size, Offset)
type Option a = (Char, T.Text, a)
type ColorPair = (ColorId, ColorId)     -- (foreground, background)

-- Algebraic data types
data Direction = Backward | Forward | Up | Down deriving (Show, Eq)
data Match = Exact | Prefix | Suffix | Infix | Negative deriving (Show, Eq)
data TokenState = DefaultState | Selected | Highlighted

data Key = CharKey Char
         | WideCharKey String
         | CtrlKey Char
         | FnKey Int
         | SpecialKey String
         | ResizeKey
         | InvalidKey String deriving (Show, Eq)

data Token = Comment T.Text
           | DocComment T.Text
           | Pragma T.Text
           | TypeIdent T.Text
           | VarIdent T.Text
           | ReservedIdent T.Text
           | Operator T.Text
           | Bracket T.Text
           | Separator T.Text
           | IntLiteral T.Text
           | FloatLiteral T.Text
           | NumLiteral T.Text
           | StringLiteral T.Text
           | CharLiteral T.Text
           | BoolLiteral T.Text
           | Whitespace T.Text
           | Tabs T.Text
           | Newline T.Text
           | Decorator T.Text
           | Symbol T.Text
           | Unclassified T.Text deriving (Show, Eq)

-- Types and synonyms for projects

type ProjectTree = Flipper ProjectTreeElement

data ProjectTreeElement = RootElement FilePath
                        | FileElement FilePath
                        | DirectoryElement ProjectTree deriving (Show, Eq)

instance Ord ProjectTreeElement where
    (RootElement a)      `compare` (RootElement b)      = a `compare` b
    (RootElement _)      `compare` _                    = LT
    _                    `compare` (RootElement b)      = GT
    (DirectoryElement a) `compare` (DirectoryElement b) =
        (active $ flipTo 0 a) `compare` (active $ flipTo 0 b)
    (DirectoryElement _) `compare` _                    = LT
    _                    `compare` (DirectoryElement b) = GT
    (FileElement a)      `compare` (FileElement b)      = a `compare` b

