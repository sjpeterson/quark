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

import qualified Data.Text as T

import Quark.Types
import Quark.Helpers ( lineSplitIx
                     , (~~) )

-- (Edit n s ix) means to delete n characters at index ix, replacing them with
-- string s.
data Edit = Edit Deletion T.Text Index Selection Bool T.Text
          | IndentLine Row Int Char Index Selection
          | EditGroup [Edit] Index Selection deriving (Show, Eq)

editIndex:: Edit -> Index
editIndex (Edit _ _ ix _ _ _)     = ix
editIndex (IndentLine _ _ _ ix _) = ix
editIndex (EditGroup _ ix _)      = ix

editSelection :: Edit -> Selection
editSelection (Edit _ _ _ sel _ _)     = sel
editSelection (IndentLine _ _ _ _ sel) = sel
editSelection (EditGroup _ _ sel)      = sel

-- EditHistory consists of lists of present and future edits and an integer
-- indicating the number of edits since last save.
type EditHistory = (Int, [Edit], [Edit])

-- An empty edit history
emptyEditHistory :: EditHistory
emptyEditHistory = (0, [], [])

-- Apply an edit to a string
doEdit :: Edit -> T.Text -> T.Text
doEdit (Edit (n, m) sx ix sel _ _) s = (T.take p s0) ~~ sx ~~ (T.drop q s1)
  where
    p = (T.length s0) - n + (min 0 sel)
    q = m + (max 0 sel)
    (s0, s1)  = T.splitAt ix s
doEdit (IndentLine r n c _ _) s =
    s0 ~~ (T.replicate (max 0 n) $ T.singleton c) ~~ (T.drop n' s1)
  where
    n' = min (T.length $ T.takeWhile (\c' -> c' == c) s1) $ max 0 (-n)
    (s0, s1) = T.splitAt (lineSplitIx r s) s
doEdit (EditGroup edits _ _) s = case edits of
    []     -> s
    (x:xs) -> doEdit (EditGroup xs 0 0) $ doEdit x s

-- Undo an edit to a string
undoEdit' :: Edit -> T.Text -> T.Text
undoEdit' (Edit (n, m) sx ix sel _ deletedS) s =
    s0 ~~ deletedS ~~ (T.drop (T.length sx) s1)
  where
    (s0, s1) = T.splitAt (ix - n + (min 0 sel)) s
undoEdit' (IndentLine r n c ix sel) s = doEdit (IndentLine r (-n) c ix sel) s
undoEdit' (EditGroup edits _ _) s = case edits of
    [] -> s
    xs -> undoEdit' (EditGroup (init xs) 0 0) $ undoEdit' (last xs) s

-- Undo by moving the most recent edit to the head of future edits
undoEdit :: EditHistory -> EditHistory
undoEdit (k, [], future)   = (k, [], future)
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
  x0@(Edit _ _ _ _ _ _) (k, x1@(Edit _ _ _ _ _ _):xs, _)
    | p0 && p1  = (k, (Edit (n, m) (s1 ~~ s0) (ix1) sel1 f0 deletedS):xs, [])
  where
    (n, m) = (n1 + n0, m1 + m0)
    deletedS = s0' ~~ deletedS1 ~~ s1'
    (s0', s1') = T.splitAt n0 deletedS0
    (Edit (n0, m0) s0 ix0 sel0 f0 deletedS0) = x0
    (Edit (n1, m1) s1 ix1 sel1 f1 deletedS1) = x1
    p0 = k /= 0 && sel0 == 0 && ix0 == ix1 - n1 + (min 0 sel1) + (T.length s1)
    p1 = f0 && f1 && (n0 == 0 || T.length s1 == 0)
addEditToHistory x0 (k, x1@(IndentLine r1 n1 c1 ix sel):xs , _)
    | r0 == r1 && signum n0 == signum n1 && c0 == c1 =
        (k, (IndentLine r0 (n0 + n1) c0 ix sel):xs, [])
  where
    (IndentLine r0 n0 c0 _ _) = x0
addEditToHistory x0 (k, (x1:xs), _) = (k + 1, x0:x1:xs, [])
addEditToHistory x _ = (1, [x], [])
