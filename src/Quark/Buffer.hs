--------
--
-- Module:      Quark.Buffer
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for quark buffers.
--
--------

module Quark.Buffer where

import Quark.Types ( Clipboard
                   , Cursor
                   , Direction ( Backward
                               , Forward
                               , Up
                               , Down )
                   , Name
                   , PrintRange
                   , Offset
                   , Size
                   , Index )
import Quark.History ( Edit ( Edit )
                     , EditHistory
                     , emptyEditHistory
                     , addEditToHistory
                     , undoEdit
                     , redoEdit
                     , toString )
import Quark.Cursors ( move
                     , distance
                     , orderTwo
                     , ixToCursor
                     , cursorToIx )

-- The Buffer data type
data Buffer = Buffer EditHistory Cursor Cursor Clipboard Name PrintRange
    deriving Show

emptyBuffer :: Size -> Buffer
emptyBuffer bSize =
    Buffer emptyEditHistory (0, 0) (0, 0) "" "Untitled" ((0, 0), bSize)

bufferToString :: Buffer -> String
bufferToString (Buffer h _ _ _ _ _) = toString h

-- Insert string at selection
insert :: String -> Buffer -> Buffer
insert s (Buffer h crs sel clip n r) = Buffer newH newCrs newCrs clip n r
  where
    newH@(_, present, _) = addEditToHistory edit h
    newCrs = ixToCursor ((cursorToIx crs s0) + (length s)) $ toString newH
    edit = Edit (0, 0) s (cursorToIx crs s0) (distance crs sel s0)
    s0 = toString h

delete :: Buffer -> Buffer
delete (Buffer h crs sel clip n r) = Buffer newH newCrs newCrs clip n r
  where
    newH = addEditToHistory edit h
    (newCrs, _) = orderTwo crs sel
    edit = Edit (0, m) "" (cursorToIx crs s0) (distance crs sel s0)
    m = if (distance crs sel s0) == 0 then 1 else 0
    s0 = toString h

backspace :: Buffer -> Buffer
backspace (Buffer h crs sel clip n r) = Buffer newH newCrs newCrs clip n r
  where
    newH = addEditToHistory edit h
    (newCrs, _) = orderTwo crs sel
    edit = Edit (m, 0) "" (cursorToIx crs s0) (distance crs sel s0)
    m = if (distance crs sel s0) == 0 then 1 else 0
    s0 = toString h

-- Perform undo on buffer, moving cursor to the beginning of the undone edit
-- TODO: restore selections
undo :: Buffer -> Buffer
undo (Buffer h crs sel clip n r) = Buffer newH newSel newCrs clip n r
  where
    newH@(_, _, future) = undoEdit h
    (newCrs, newSel) = case future of
        x@(Edit _ _ ix sel):_ -> (newCrs', newSel')
                                      where
                                        newCrs' = ixToCursor ix s
                                        newSel' = ixToCursor (ix + sel) s
                                        s = toString newH
        []                     -> (crs, sel)    -- Nothing was undone

{-
-- Perform undo on buffer, moving cursor relative to undone edit only
undo' :: Buffer -> Buffer
undo' (Buffer h crs sel range clipboard) =
    Buffer newH newCrs sel range clipboard
  where
    newH@(_, _, future) = undoEdit h -- what if nothing is undone?
    newCrs = case future of
      x@(Edit n s ix):_ -> crs + n - length s
      _                 -> crs
-}
-- Perform redo on buffer, moving cursor to the end of the redone edit
redo :: Buffer -> Buffer
redo (Buffer h@(_, _, f) crs sel clip n r) = Buffer newH newCrs newSel clip n r
  where
    newH = redoEdit h
    (newCrs, newSel) = case f of
        x@(Edit _ s ix sel):_ -> (newCrs', newCrs')
                                      where
                                        newCrs' = ixToCursor (ix + length s) s0
                                        s0 = toString newH
        _                 -> (crs, sel)     -- Nothing was redone, cursors stay
{-
-- Perform redo on buffer, moving cursor relative to redone edit only
redo' :: Buffer -> Buffer
redo' (Buffer h crs sel range clipboard) =
    Buffer newH newCrs sel range clipboard
  where
    newH@(_, present, _) = redoEdit h -- what if nothing is redone?
    newCrs = case present of
        x@(Edit n s ix):_ -> crs + length s - n
        _                 -> crs
-}
-- Move the cursor (and set selection cursor to same)
moveCursor :: Direction -> Buffer -> Buffer
moveCursor d (Buffer h crs sel clip n r) = Buffer h newCrs newCrs clip n r
  where
    newCrs
        | distance crs sel s == 0 = move' crs
        | d == Backward = minCrs
        | d == Forward  = move' maxCrs
        | otherwise     = move' minCrs
    move' = move d s
    s = toString h
    (minCrs, maxCrs) = orderTwo crs sel

-- Move the selection cursor
moveSelection :: Direction -> Buffer -> Buffer
moveSelection d (Buffer h crs sel clip n r) = Buffer h crs newSel clip n r
  where
    newSel = move d (toString h) sel

-- Rename a buffer
rename :: Name -> Buffer -> Buffer
rename name (Buffer h crs sel clip _ r) = Buffer h crs sel clip name r

-- Set offset for a buffer
offset :: Offset -> Buffer -> Buffer
offset o (Buffer h crs sel clip n (_, bSize)) =
    Buffer h crs sel clip n (o, bSize)

-- Modify offset for a buffer
displace :: Offset -> Buffer -> Buffer
displace (x, y) (Buffer h crs sel clip n ((x0, y0), bSize)) =
    Buffer h crs sel clip n ((x + x0, y + y0), bSize)

-- Set print size for a buffer
resize :: Size -> Buffer -> Buffer
resize bSize (Buffer h crs sel clip name (o, _)) =
    Buffer h crs sel clip name (o, bSize)