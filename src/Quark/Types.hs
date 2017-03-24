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

import qualified Data.ByteString.Char8 as B

import Quark.Flipper ( Flipper
                     , active
                     , flipTo )

-- Type aliases for primitive types
type Row = Int
type Col = Int
type Index = Int
type Selection = Int
type ColorId = Int

type Language = B.ByteString
type Clipboard = B.ByteString
type Name = B.ByteString

-- Type aliases for tuples
type Deletion = (Int, Int)              -- (backspaces, deletes)
type Cursor = (Row, Col)
type Size = (Row, Col)
type Offset = (Row, Col)
type PrintRange = (Size, Offset)
type Option a = (Char, B.ByteString, a)
type ColorPair = (ColorId, ColorId)     -- (foreground, background)

-- Algebraic data types
data Direction = Backward | Forward | Up | Down deriving (Show, Eq)

data Key = CharKey Char
         | WideCharKey String
         | CtrlKey Char
         | FnKey Int
         | SpecialKey String
         | ResizeKey
         | InvalidKey String deriving (Show, Eq)

data Token = Comment B.ByteString
           | DocComment B.ByteString
           | Pragma B.ByteString
           | TypeIdent B.ByteString
           | VarIdent B.ByteString
           | ReservedIdent B.ByteString
           | Operator B.ByteString
           | Bracket B.ByteString
           | Separator B.ByteString
           | IntLiteral B.ByteString
           | FloatLiteral B.ByteString
           | NumLiteral B.ByteString
           | StringLiteral B.ByteString
           | CharLiteral B.ByteString
           | BoolLiteral B.ByteString
           | Whitespace B.ByteString
           | Newline B.ByteString
           | Decorator B.ByteString
           | Symbol B.ByteString
           | Unclassified B.ByteString deriving (Show, Eq)

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

