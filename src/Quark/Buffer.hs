{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------
--
-- Module:      Quark.Buffer
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Module for quark buffers.
--
---------------------------------------------------------------

module Quark.Buffer ( Buffer ( Buffer
                             , LockedBuffer )
                    , ExtendedBuffer
                    , emptyXB
                    , ebToString
                    , ebSelection
                    , ebCursors
                    , condense
                    , editHistory
                    , cursor
                    , selectionCursor
                    , input
                    , nlAutoIndent
                    , paste
                    , tab
                    , unTab
                    , delete
                    , backspace
                    , selection
                    , undo
                    , redo
                    , moveCursor
                    , moveCursorN
                    , selectMoveCursor
                    , endOfLine
                    , startOfLine
                    , endOfFile
                    , startOfFile
                    , selectAll
                    , unsavedXB ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Quark.Types ( Clipboard
                   , Cursor
                   , Direction ( Backward
                               , Forward
                               , Up
                               , Down )
                   , Name
                   , Index
                   , Selection
                   , Language )
import Quark.History ( Edit ( Edit
                            , IndentLine )
                     , EditHistory
                     , editIndex
                     , editSelection
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
import Quark.Helpers ( lnIndent
                     , lineSplitIx )

-- The Buffer data type
data Buffer = LockedBuffer String
            | Buffer { editHistory :: EditHistory
                     , cursor :: Cursor
                     , selectionCursor :: Cursor } deriving (Eq, Show)
-- TODO:
-- It might be neat to have a type of buffer that mirrors and updates with
-- another buffer, but is non-editable. The low-tech version of this would be
-- to clone a Buffer to a LockedBuffer

type BufferMetaData = (FilePath, Language, Bool)
type ExtendedBuffer = (Buffer, BufferMetaData)

emptyXB :: ExtendedBuffer
emptyXB = ( (Buffer (fromString "") (0, 0) (0, 0))
          , ("Untitled", "Undefined", False) )

ebToString :: ExtendedBuffer -> ByteString
ebToString ((Buffer h _ _), _) = toString h

ebSelection :: ExtendedBuffer -> (Index, Selection)
ebSelection ((Buffer h crs sel), _) =
    (cursorToIx selectionStart s, distance selectionStart selectionEnd s)
  where
    (selectionStart, selectionEnd) = orderTwo crs sel
    s = toString h

ebCursors :: ExtendedBuffer -> (Cursor, Cursor)
ebCursors ((Buffer h crs sel), _) = (crs, sel)

condense :: ExtendedBuffer -> Buffer
condense (b, _) = b

unsaved :: Buffer -> Bool
unsaved (Buffer (k, _, _) _ _) = case k of
    0 -> False
    _ -> True

unsavedXB :: ExtendedBuffer -> Bool
unsavedXB (b, _) = unsaved b

-- Input single character
input :: Char -> Buffer -> Buffer
input c = insert (B.cons c "") True

paste :: ByteString -> Buffer -> Buffer
paste s = insert s False

tab :: Int -> Buffer -> Buffer
tab tabWidth b@(Buffer h crs@(r, c) sel@(rr, cc))
    | crs == sel = insert (B.replicate n ' ') True b
    | otherwise  = Buffer newH (r, c + n') (rr, cc + n')
  where
    n = tabWidth - (mod c tabWidth)
    newH = addEditToHistory (IndentLine r n' ix sel') h
    (ix, sel') = ixAndSel b
    n' = tabWidth - mod (lnIndent r $ toString h) tabWidth
    s0 = toString h

unTab :: Int -> Buffer -> Buffer
unTab tabWidth b@(Buffer h (r, c) (rr, cc))
    | n == 0    = b
    | otherwise = Buffer newH (r, c + n') (rr, cc + n')
  where
    n = lnIndent r $ toString h
    newH = addEditToHistory (IndentLine r n' ix sel) h
    (ix, sel) = ixAndSel b
    n' = (-tabWidth) + mod (-n) tabWidth

nlAutoIndent :: Buffer -> Buffer
nlAutoIndent b@(Buffer h crs sel) =
    insert (B.cons '\n' (B.replicate n ' ')) True b
  where
    n = lnIndent r (toString h)
    (r, _) = minCursor crs sel

ixAndSel :: Buffer -> (Index, Selection)
ixAndSel (Buffer h crs sel) = ((cursorToIx crs s), (distance crs sel s))
  where
    s = toString h

-- Generic insert string at current selection
insert :: ByteString -> Bool -> Buffer -> Buffer
insert s fusible b@(Buffer h crs sel) = Buffer newH newCrs newCrs
  where
    newH = addEditToHistory edit h
    edit = Edit (0, 0) s ix sel' fusible
    (ix, sel') = ixAndSel b
    s0 = toString h
    newCrs = ixToCursor newIx newS
    newIx = (cursorToIx (minCursor crs sel) s0) + (B.length s)
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
    newCrs = if c0 && d == 1 then move Backward s0 crs
                             else (\(x, y) -> x) $ orderTwo crs sel
    edit = Edit deletion "" (cursorToIx crs s0) (distance crs sel s0) True
    deletion = if c0 then (d, 1 - d) else (0, 0)
    s0 = toString h
    c0 = (distance crs sel s0) == 0

-- Copy selection
selection :: Buffer -> ByteString
selection (Buffer h crs sel) = B.take l $ B.drop k s
  where
    l = abs $ distance crs sel s
    k = cursorToIx (minCursor crs sel) s
    s = toString h

-- Perform undo on buffer, moving cursor to the beginning of the undone edit
undo :: Buffer -> Buffer
undo buffer@(Buffer h@(_, past, _) crs sel) = case past of
    (x0:x1:xs) -> alignCursor True $ Buffer (undoEdit h) crs sel
    _          -> buffer

-- Perform redo on buffer, moving cursor to the end of the redone edit
redo :: Buffer -> Buffer
redo buffer@(Buffer h@(_, _, future) crs sel) = case future of
     (x:xs) -> alignCursor False $ Buffer (redoEdit h) crs sel
     _      -> buffer

alignCursor :: Bool -> Buffer -> Buffer
alignCursor True (Buffer h@(_, _, y:ys) _ _) = Buffer h newCrs newSel
  where
    newCrs = ixToCursor (editIndex y) s
    newSel = ixToCursor (editIndex y + editSelection y) s
    s = toString h
alignCursor False b@(Buffer h@(_, x:xs, _) crs sel) = Buffer h newCrs newSel
  where
    newCrs = case x of
        (Edit (n, _) s ix _ _) -> ixToCursor (ix - n + B.length s) $ toString h
        (IndentLine _ n ix _)  -> ixToCursor (ix + n) $ toString h
    newSel = case x of
        (Edit _ _ _ _ _)      -> newCrs
        (IndentLine _ n ix sel) -> ixToCursor (ix + sel + n) $ toString h
alignCursor _ b = b

-- End and Home functions
endOfLine :: Bool -> Buffer -> Buffer
endOfLine moveSel (Buffer h (r, _) sel) = Buffer h newCrs newSel
  where
    newCrs = case drop r $ B.lines s of (x:xs) -> (r, B.length x)
                                        _      -> ixToCursor (B.length s - 1) s
    newSel = case moveSel of True  -> newCrs
                             False -> sel
    s = toString h

startOfLine :: Bool -> Buffer -> Buffer
startOfLine moveSel (Buffer h (r, _) sel) = Buffer h (r, 0) newSel
  where
    newSel = case moveSel of True  -> (r, 0)
                             False -> sel

endOfFile :: Bool -> Buffer -> Buffer
endOfFile moveSel (Buffer h _ sel) = Buffer h newCrs newSel
  where
    newCrs = ixToCursor (B.length s) s
    newSel = case moveSel of True  -> newCrs
                             False -> sel
    s = toString h

startOfFile :: Bool -> Buffer -> Buffer
startOfFile moveSel (Buffer h _ sel) = Buffer h (0, 0) newSel
  where
    newSel = case moveSel of True  -> (0, 0)
                             False -> sel

selectAll :: Buffer -> Buffer
selectAll (Buffer h _ _) = Buffer h (0, 0) $ ixToCursor (B.length s) s
  where
    s = toString h

-- Move the cursor one step, set selection cursor to same
moveCursor :: Direction -> Buffer -> Buffer
moveCursor = moveCursorN 1

moveCursorN :: Int -> Direction -> Buffer -> Buffer
moveCursorN = genericMoveCursor True

-- Move the cursor, keeping selection cursor in place
selectMoveCursor :: Direction -> Buffer -> Buffer
selectMoveCursor = genericMoveCursor False 1

-- Generic move cursor (not exported)
genericMoveCursor :: Bool -> Int -> Direction -> Buffer -> Buffer
genericMoveCursor moveSel n d (Buffer h crs sel)
    | n == 1    = Buffer h newCrs newSel
    | otherwise = genericMoveCursor moveSel (n - 1) d (Buffer h newCrs newSel)
  where
    newCrs
        | moveSel == False = move d s crs
        | crs == sel       = move d s crs
        | d == Backward    = minCrs
        | d == Forward     = maxCrs
        | otherwise        = move d s crs
    newSel = case moveSel of
        True  -> newCrs
        False -> sel
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
