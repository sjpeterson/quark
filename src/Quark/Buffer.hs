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
                    , ebEmpty
                    , ebToString
                    , ebSelection
                    , ebCursors
                    , ebNew
                    , ebFirst
                    , setPath
                    , path
                    , language
                    , tokens
                    , writeProtected
                    , condense
                    , editHistory
                    , cursor
                    , selectionCursor
                    , input
                    , insert
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
                    , deselect
                    , ebUnsaved
                    , bufferFind ) where

import Data.Bifunctor ( second )

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as U
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
                   , Language
                   , Token )
import Quark.Settings ( tabWidth
                      , tabToSpacesDefault )
import Quark.History ( Edit ( Edit
                            , IndentLine
                            , EditGroup )
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
                    , ixToCursor
                    , cursorToIx )
import Quark.Lexer.Core ( tokenLines )
import Quark.Lexer.Language ( tokenize )
import Quark.Helpers ( lnIndent
                     , lnIndent'
                     , lineSplitIx
                     , tabbedLength
                     , findIx
                     , xnor
                     , orderTwo )

-- The Buffer data type
data Buffer = LockedBuffer String
            | Buffer { editHistory :: EditHistory
                     , cursor :: Cursor
                     , selectionCursor :: Cursor } deriving (Eq, Show)
-- TODO:
-- It might be neat to have a type of buffer that mirrors and updates with
-- another buffer, but is non-editable. The low-tech version of this would be
-- to clone a Buffer to a LockedBuffer

data BufferMetaData = BufferMetaData { path' :: FilePath
                                     , language' :: Language
                                     , tokens' :: [Token]
                                     , writeProtected' :: Bool
                                     , tabToSpaces' :: Bool } deriving Eq
type ExtendedBuffer = (Buffer, BufferMetaData)

language :: ExtendedBuffer -> Language
language (_, bufferMetaData) = language' bufferMetaData

writeProtected :: ExtendedBuffer -> Bool
writeProtected (_, bufferMetaData) = writeProtected' bufferMetaData

path :: ExtendedBuffer -> FilePath
path (_, bufferMetaData) = path' bufferMetaData

tabToSpaces :: ExtendedBuffer -> Bool
tabToSpaces (_, bufferMetaData) = tabToSpaces' bufferMetaData

setPath :: FilePath -> ExtendedBuffer -> ExtendedBuffer
setPath path'' b = second (setPath' path'') b

setPath' :: FilePath -> BufferMetaData -> BufferMetaData
setPath' path'' (BufferMetaData _ a b c d) = BufferMetaData path'' a b c d

setTokens' :: [Token] -> BufferMetaData -> BufferMetaData
setTokens' tokens'' (BufferMetaData a b _ c d) =
    BufferMetaData a b tokens'' c d

tokens :: ExtendedBuffer -> [Token]
tokens (_, bufferMetaData) = tokens' bufferMetaData

ebEmpty :: FilePath -> Language -> ExtendedBuffer
ebEmpty path language =
    ( (Buffer (fromString "") (0, 0) (0, 0))
    , BufferMetaData path'' language'' [] False tabToSpacesDefault)
  where
    path'' = if path == "" then "Untitled" else path
    language'' = if language == "" then "Unknown" else language

ebToString :: ExtendedBuffer -> ByteString
ebToString ((Buffer h _ _), _) = toString h

ebSelection :: ExtendedBuffer -> (Index, Selection)
ebSelection ((Buffer h crs sel), _) =
    (cursorToIx selectionStart s, distance selectionStart selectionEnd s)
  where
    (selectionStart, selectionEnd) = orderTwo crs sel
    s = toString h


ebNew :: FilePath -> ByteString -> Language -> ExtendedBuffer
ebNew path'' contents language'' =
    ( Buffer (fromString contents) (0, 0) (0, 0)
    , BufferMetaData path'' language'' ts False tabToSpaces'')
  where
    ts = tokenize language'' [] contents
    tabToSpaces'' = if (B.elem '\t' contents) then False
                                              else tabToSpacesDefault

ebCursors :: ExtendedBuffer -> (Cursor, Cursor)
ebCursors ((Buffer h crs sel), _) = (crs, sel)

ebFirst :: (Buffer -> Buffer) -> ExtendedBuffer -> ExtendedBuffer
ebFirst f (b, bufferMetaData) = (b', bufferMetaData')
  where
    b' = f b
    bufferMetaData' = if editHistory b' == editHistory b
                          then bufferMetaData
                          else setTokens' ts bufferMetaData
    ts = tokenize (language' bufferMetaData') (tokens' bufferMetaData) s'
    s' = toString $ editHistory b'

condense :: ExtendedBuffer -> Buffer
condense (b, _) = b

unsaved :: Buffer -> Bool
unsaved (Buffer (k, _, _) _ _) = case k of
    0 -> False
    _ -> True

ebUnsaved :: ExtendedBuffer -> Bool
ebUnsaved (b, _) = unsaved b

-- Input single character
input :: Char -> Bool -> Buffer -> Buffer
input c m = insert (B.cons c "") m True

paste :: ByteString -> Buffer -> Buffer
paste s = insert s False False

tab :: ExtendedBuffer -> ExtendedBuffer
tab xb@(b@(Buffer h crs@(r, c) sel@(rr, cc)), bufferMetaData)
    | crs == sel = case tabToSpaces' bufferMetaData of
                       True  -> ebFirst (insert (B.replicate n ' ') False True) xb
                       False -> ebFirst (insert "\t" False True) xb
    | otherwise  = (b', setTokens' ts bufferMetaData)
  where
    b' = Buffer newH (r, c + n0) (rr, cc + n1)
    n = tabWidth - (mod c tabWidth)
    newH = addEditToHistory tabEdit h
    tabEdit = EditGroup [ IndentLine r' (tabWidth' r') c' 0 0
                        | r' <- [r0..r1] ] ix sel'
    (ix, sel') = ixAndSel b
    tabWidth' r'' = if tabToSpaces' bufferMetaData
                        then tabWidth - mod (lnIndent ' ' r'' s0) tabWidth
                        else 1
    c' = if tabToSpaces' bufferMetaData then ' ' else '\t'
    n0 = tabWidth - mod (lnIndent ' ' r s0) tabWidth
    n1 = tabWidth - mod (lnIndent ' ' rr s0) tabWidth
    r0 = min r rr
    r1 = max r rr
    s0 = toString h
    ts = tokenize (language' bufferMetaData) (tokens' bufferMetaData) s'
    s' = toString $ editHistory b'

unTab :: ExtendedBuffer -> ExtendedBuffer
unTab xb@(b@(Buffer h (r, c) (rr, cc)), bufferMetaData)
    | all (== 0) n = xb
    | otherwise    = (b', setTokens' ts bufferMetaData)
  where
    b' = Buffer newH (r, c + cAdd r) (rr, cc + cAdd rr)
    newH = addEditToHistory unTabEdit h
    unTabEdit = EditGroup [ IndentLine r' n' c' 0 0
                          | (r', n') <- zip [r0..r1] n ] ix sel
    (ix, sel) = ixAndSel b
    c' = if tabToSpaces' bufferMetaData then ' ' else '\t'
    r0 = min r rr
    r1 = max r rr
    n = [ unTabWidth r' | r' <- [r0..r1] ]
    cAdd r'' = if tabToSpaces' bufferMetaData then tabWidth * unTabWidth r''
                                              else unTabWidth r''
    unTabWidth r'' = if thisIndent == 0
                         then 0
                         else if tabToSpaces' bufferMetaData
                                  then (-tabWidth) + mod (-thisIndent) tabWidth
                                  else (-1)
      where
        thisIndent = lnIndent c' r'' s0
    s0 = toString h
    ts = tokenize (language' bufferMetaData) (tokens' bufferMetaData) s'
    s' = toString $ editHistory b'

nlAutoIndent :: Buffer -> Buffer
nlAutoIndent b@(Buffer h crs sel) =
    insert (B.cons '\n' $ lnIndent' r (toString h)) False True b
  where
    (r, _) = min crs sel

ixAndSel :: Buffer -> (Index, Selection)
ixAndSel (Buffer h crs sel) = ((cursorToIx crs s), (distance crs sel s))
  where
    s = toString h

-- Generic insert string at current selection
insert :: ByteString -> Bool -> Bool -> Buffer -> Buffer
insert s m fusible b@(Buffer h crs sel) = Buffer newH newCrs newCrs
  where
    newH = addEditToHistory edit h
    edit = Edit (0, 0) s ix sel' fusible
    (ix, sel'') = ixAndSel b
    sel' = if m && sel'' == 0 && doInsert then 1 else sel''
    s0 = toString h
    doInsert = if U.take 1 s0' == "\n" then False else True
    (_, s0') = U.splitAt ix s0
    newCrs = ixToCursor newIx newS
    newIx = (cursorToIx (min crs sel) s0) + (U.length s)
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
selection (Buffer h crs sel) = U.take l $ U.drop k s
  where
    l = abs $ distance crs sel s
    k = cursorToIx (min crs sel) s
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
        (Edit (n, _) s ix _ _) -> ixToCursor (ix - n + U.length s) $ toString h
        (IndentLine _ n ' ' ix _)  -> ixToCursor (ix + n) $ toString h
    newSel = case x of
        (Edit _ _ _ _ _)      -> newCrs
        (IndentLine _ n ' ' ix sel) -> ixToCursor (ix + sel + n) $ toString h
alignCursor _ b = b

-- End and Home functions
endOfLine :: Bool -> Buffer -> Buffer
endOfLine moveSel (Buffer h (r, _) sel) = Buffer h newCrs newSel
  where
    newCrs = case drop r $ U.lines s of (x:xs) -> (r, tabbedLength tabWidth x)
                                        _      -> ixToCursor (U.length s - 1) s
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
    newCrs = ixToCursor (U.length s) s
    newSel = case moveSel of True  -> newCrs
                             False -> sel
    s = toString h

startOfFile :: Bool -> Buffer -> Buffer
startOfFile moveSel (Buffer h _ sel) = Buffer h (0, 0) newSel
  where
    newSel = case moveSel of True  -> (0, 0)
                             False -> sel

selectAll :: Buffer -> Buffer
selectAll (Buffer h _ _) = Buffer h (0, 0) $ ixToCursor (U.length s) s
  where
    s = toString h

deselect :: Buffer -> Buffer
deselect (Buffer h crs _) = Buffer h crs crs

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

bufferFind :: Bool -> Bool -> ByteString -> Buffer -> Buffer
bufferFind next doStep findString b@(Buffer h crs sel) =
    Buffer h foundCrs foundSel
  where
    foundIx = findIx k next findString s
    foundCrs = ixToCursor foundIx s
    foundSel = ixToCursor (foundIx + U.length findString) s
    s = toString h
    k = (cursorToIx crs s) + ixStep
    ixStep = if (selection b == findString) && (xnor next doStep) then 1 else 0