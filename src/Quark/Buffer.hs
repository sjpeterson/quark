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

module Quark.Buffer ( Buffer
                    , editHistory
                    , cursor
                    , input
                    , paste
                    , delete
                    , backspace
                    , selection
                    , undo
                    , redo
                    , moveCursor
                    , selectMoveCursor) where

import Quark.Types ( Clipboard
                   , Cursor
                   , Direction ( Backward
                               , Forward
                               , Up
                               , Down )
                   , Name
                   , Index )
import Quark.History ( Edit ( Edit )
                     , EditHistory
                     , emptyEditHistory
                     , addEditToHistory
                     , undoEdit
                     , redoEdit
                     , toString
                     , fromString )
import Quark.Cursor ( move
                     , distance
                     , minCursor
                     , orderTwo
                     , ixToCursor
                     , cursorToIx )

-- The Buffer data type
data Buffer = LockedBuffer String
            | Buffer { editHistory :: EditHistory
                     , cursor :: Cursor
                     , selectionCursor :: Cursor } deriving Show
-- TODO:
-- It might be neat to have a type of buffer that mirrors and updates with
-- another buffer, but is non-editable. The low-tech version of this would be
-- to clone a Buffer to a LockedBuffer

-- Input single character
input :: Char -> Buffer -> Buffer
input c = insert [c] True

paste :: String -> Buffer -> Buffer
paste s = insert s False

-- Generic insert string at current selection
insert :: String -> Bool -> Buffer -> Buffer
insert s fusible (Buffer h crs sel) = Buffer newH newCrs newCrs
  where
    newH = addEditToHistory edit h
    edit = Edit (0, 0) s (cursorToIx crs s0) (distance crs sel s0) fusible
    s0 = toString h
    newCrs = ixToCursor newIx newS
    newIx = (cursorToIx (minCursor crs sel) newS) + (length s)
    newS = toString newH

-- Delete
delete :: Buffer -> Buffer
delete = genericDelete 0

-- Backspace
backspace :: Buffer -> Buffer
backspace = genericDelete 1

-- Generic forward or backward deletion (not exported)
genericDelete :: Int -> Buffer -> Buffer
genericDelete d (Buffer h crs sel) = Buffer newH newCrs newCrs
  where
    newH = addEditToHistory edit h
    (newCrs, _) = orderTwo crs sel
    edit = Edit deletion "" (cursorToIx crs s0) (distance crs sel s0) True
    deletion = if (distance crs sel s0) == 0 then (d, 1 - d) else (0, 0)
    s0 = toString h

-- Cut selection
-- cut :: Buffer -> Buffer
-- cut buffer@(Buffer h crs sel _ path) = backspace (Buffer h crs sel s path)
--   where
--     s = copy buffer

-- Copy selection
selection :: Buffer -> String
selection (Buffer h crs sel) = take l $ drop k s
  where
    l = abs $ distance crs sel s
    k = cursorToIx (minCursor crs sel) s
    s = toString h

-- Paste contents of clipboard at current selection
-- paste :: Buffer -> Buffer
-- paste buffer@(Buffer _ _ _ clipboard _) = insert clipboard buffer

-- Perform undo on buffer, moving cursor to the beginning of the undone edit
undo :: Buffer -> Buffer
undo buffer@(Buffer h@(_, past, _) crs sel') = case past of
    x0@(Edit (n, m) s ix sel _):x1:xs -> Buffer newH newCrs newSel
                                         where
        newH = undoEdit h
        (newCrs, newSel) = case (cursorToIx crs s0) == postIx && crs == sel' of
            False -> (postCrs, postCrs)
            _     -> (preCrs, preSel)
        postIx = ix + length s
        postCrs = ixToCursor postIx s0
        s0 = toString h
        preCrs = ixToCursor ix newS
        preSel = ixToCursor (ix + sel) newS
        newS = toString newH
    _                               -> buffer   -- nothing to undo

-- Perform redo on buffer, moving cursor to the end of the redone edit
redo :: Buffer -> Buffer
redo buffer@(Buffer h@(_, _, future) crs sel') = case future of
    x@(Edit _ s ix sel _):xs -> Buffer newH newSel newCrs
                                where
        newH = redoEdit h
        (newCrs, newSel) = case (cursorToIx crs s0) == ix && d == sel of
            False -> (preCrs, preSel)
            _     -> (postCrs, postCrs)
        d = distance crs sel' s0
        preCrs = ixToCursor ix s0
        preSel = ixToCursor (ix + sel) s0
        s0 = toString h
        postCrs = ixToCursor (ix + length s) $ toString newH
    _                      -> buffer            -- nothing to redo

-- Move the cursor, set selection cursor to same
moveCursor :: Direction -> Buffer -> Buffer
moveCursor = genericMoveCursor True

-- Move the cursor, keeping selection cursor in place
selectMoveCursor :: Direction -> Buffer -> Buffer
selectMoveCursor = genericMoveCursor False

-- Generic move cursor (not exported)
genericMoveCursor :: Bool -> Direction -> Buffer -> Buffer
genericMoveCursor moveSel d (Buffer h crs sel) =
    Buffer h newCrs newSel
  where
    newCrs
        | moveSel == False = move d s crs
        | d == Backward    = minCrs
        | d == Forward     = maxCrs
        | otherwise        = move d s crs
    newSel = case moveSel of
        True -> newCrs
        _    -> sel
    s = toString h
    (minCrs, maxCrs) = orderTwo crs sel

-- Load a buffer from a file
-- loadString :: FilePath -> String -> Buffer
-- loadString path s = Buffer (fromString s) (0, 0) (0, 0) "" path

-- load :: FilePath -> IO (Buffer)
-- load path = do
--     s <- readFile path
--     return $ Buffer (fromString s) (0, 0) (0, 0) "" path

-- Save a buffer to it's path
-- save :: Buffer -> IO ()
-- save buffer@(Buffer h _ _ _ path) = writeFile path $ toString h

-- Buffer offset shift by +/- 5
-- Always show line above/below cursor and character before/after if any