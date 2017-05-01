{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Quark.Cursor ( ixToCursor
                    , cursorToIx
                    , distance
                    , move ) where

import qualified Data.Text as T

import Quark.Types ( Cursor
                   , Index
                   , Direction ( Backward
                               , Forward
                               , Up
                               , Down ) )
import Quark.Helpers ( nlTail
                     , strHeight
                     , tabbedLength
                     , (~~) )
import Quark.Settings ( tabWidth )

-- Convert a linear index of a string to a cursor
ixToCursor :: Index -> T.Text -> Cursor
ixToCursor ix s = (row, col)
  where
    row = (length s0Lines) - 1
    col = (tabbedLength tabWidth $ last s0Lines) - 1
    s0Lines = T.lines $ s0 ~~ " "
    (s0, _) = T.splitAt ix s

-- Convert a cursor on a string to a linear index
cursorToIx :: Cursor -> T.Text -> Index
cursorToIx _ "" = 0
cursorToIx (0, col) xs = loop (0, 0, col) xs
  where
    loop (n, k, col') xs
        | col' <= 0 = n
        | otherwise =
              case T.uncons xs of
                  Just ('\n', xs') -> n
                  Just ('\t', xs') -> loop (n + nAdd, k + kk, col' - kk) xs'
                    where
                      kk = tabWidth - mod k tabWidth
                      nAdd = if col' >= kk then 1 else 0
                  Just (_, xs')    -> loop (n + 1, k + 1, col' - 1) xs'
                  Nothing          -> n
cursorToIx (row, col) (T.uncons -> Just (x, xs))
    | row < 0   = 0
    | x == '\n' = 1 + cursorToIx (row - 1, col) xs
    | otherwise = 1 + cursorToIx (row, col) xs

-- Compute distance between two cursors on a string (may be negative)
distance :: Cursor -> Cursor -> T.Text -> Int
distance crs0 crs1 s = (cursorToIx crs1 s) - (cursorToIx crs0 s)

-- Move a cursor on a string
move :: Direction -> T.Text -> Cursor -> Cursor
move Backward s crs = ixToCursor (max ((cursorToIx crs s) - 1) 0) s
move Forward s crs = ixToCursor (min ((cursorToIx crs s) + 1) (T.length s)) s
move Up _ (row, col) = (max (row - 1) 0, col)
move Down s (row, col) = (min (row + 1) $ strHeight s - 1, col)