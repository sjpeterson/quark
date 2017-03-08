{-# LANGUAGE OverloadedStrings #-}

--------
--
-- Module:      Quark.History
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Types, aliases and functions to deal with an edit history, providing undo
-- and redo functionality.
--
-- Edit is a generic type that covers three distinct types of edits - inserts,
-- deletions and replacements.
--
--------

module Quark.History where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as B

import Quark.Types
import Quark.Helpers ( lineSplitIx
                     , (~~) )

-- (Edit n s ix) means to delete n characters at index ix, replacing them with
-- string s.
data Edit = Edit Deletion ByteString Index Selection Bool
          | IndentLine Row Int Index Selection deriving (Show, Eq)

editIndex:: Edit -> Index
editIndex (Edit _ _ ix _ _) = ix
editIndex (IndentLine _ _ ix _) = ix

editSelection :: Edit -> Selection
editSelection (Edit _ _ _ sel _) = sel
editSelection (IndentLine _ _ _ sel) = sel

-- EditHistory consists of lists of present and future edits and an integer
-- indicating the number of edits since last save.
type EditHistory = (Int, [Edit], [Edit])

-- Initialize an edit history from a string
fromString :: ByteString -> EditHistory
fromString s = (0, [Edit (0, 0) s 0 0 False], [])

-- An empty edit history
emptyEditHistory :: EditHistory
emptyEditHistory = fromString "" 

-- Construct a string from the edit history
toString :: EditHistory -> ByteString
toString (_, [], _)     = ""
toString (_, x:xs, _) = doEdit x (toString (0, xs, []))

-- Apply an edit to a string
doEdit :: Edit -> ByteString -> ByteString
doEdit (Edit (n, m) sx ix sel _) s = (U.take p s0) ~~ sx ~~ (U.drop q s1)
  where
    p = (U.length s0) - n + (min 0 sel)
    q = m + (max 0 sel)
    (s0, s1)  = U.splitAt ix s
doEdit (IndentLine r n _ _) s =
    s0 ~~ (B.replicate (max 0 n) ' ') ~~ (U.drop n' s1)
  where
    n' = min (U.length $ B.takeWhile (\c -> c == ' ') s1) $ max 0 (-n)
    (s0, s1) = U.splitAt (lineSplitIx r s) s

-- Undo by moving the most recent edit to the head of future edits
-- Second pattern is needed when loading files
undoEdit :: EditHistory -> EditHistory
undoEdit (k, [], future)   = (k, [], future)
undoEdit (k, x:[], future) = (k, [x], future)
undoEdit (k, x:xs, future) = (k - 1, xs, x:future)

-- Redo by moving the head of future edits to the head of present edits
redoEdit :: EditHistory -> EditHistory
redoEdit (k, present, [])   = (k, present, [])
redoEdit (k, present, x:xs) = (k + 1, x:present, xs)

-- Add a new edit to the head of present edits, clearing future edits
newEdit :: Edit -> EditHistory -> EditHistory
newEdit x (k, present, _) = (k + 1, x:present, [])

-- Add a new edit to the head of present edits, merging with the previous edit
-- if appropriate
addEditToHistory :: Edit -> EditHistory -> EditHistory
addEditToHistory
  x0@(Edit (n0, m0) s0 ix0 sel0 f0) (k, x1@(Edit (n1, m1) s1 ix1 sel1 f1):xs, _)
    | p0 && p1  = (k, (Edit (n1 + n0, m1 + m0) (s1 ~~ s0) (ix1) sel1 f0):xs, [])
  where
    p0 = k /= 0 && sel0 == 0 && ix0 == ix1 - n1 + (min 0 sel1) + (U.length s1)
    p1 = f0 && f1 && (n0 == 0 || U.length s1 == 0)
addEditToHistory x0@(IndentLine r0 n0 _ _) ( k
                                           , x1@(IndentLine r1 n1 ix sel):xs
                                           , _)
    | r0 == r1 && signum n0 == signum n1 =
        (k, (IndentLine r0 (n0 + n1) ix sel):xs, [])
addEditToHistory x0 (k, (x1:xs), _) = (k + 1, x0:x1:xs, [])
addEditToHistory x _ = (1, [x], [])
