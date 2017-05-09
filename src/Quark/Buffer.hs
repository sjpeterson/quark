{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------
--
-- Module:      Quark.Buffer
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Portable
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
                    , ebContents
                    , ebSelection
                    , ebCursors
                    , ebNew
                    , ebFirst
                    , setPath
                    , path
                    , language
                    , tabToSpaces
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

import qualified Data.Text as T

import Quark.Types ( Cursor
                   , Direction ( Backward
                               , Forward )
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
                     , doEdit
                     , undoEdit'
                     , editIndex
                     , editSelection
                     , emptyEditHistory
                     , addEditToHistory
                     , undoEdit
                     , redoEdit )
import Quark.Cursor ( move
                    , distance
                    , ixToCursor
                    , cursorToIx )
import Quark.Lexer.Core ( tokenLines )
import Quark.Lexer.Language ( tokenize )
import Quark.Helpers ( lnIndent
                     , lnIndent'
                     , tabbedLength
                     , findIx
                     , xnor
                     , orderTwo )

-- The Buffer data type
data Buffer = LockedBuffer String
            | Buffer { contents :: T.Text
                     , editHistory :: EditHistory
                     , cursor :: Cursor
                     , selectionCursor :: Cursor } deriving (Eq, Show)

data BufferMetaData = BufferMetaData { path' :: FilePath
                                     , language' :: Language
                                     , tokenLines' :: [[Token]]
                                     , writeProtected' :: Bool
                                     , tabToSpaces' :: Bool } deriving Eq
type ExtendedBuffer = (Buffer, BufferMetaData)

-- Some functions that should eventually be replaced with lenses

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

setTokenLines' :: [[Token]] -> BufferMetaData -> BufferMetaData
setTokenLines' tokenLines'' (BufferMetaData a b _ c d) =
    BufferMetaData a b tokenLines'' c d

tokens :: ExtendedBuffer -> [[Token]]
tokens (_, bufferMetaData) = tokenLines' bufferMetaData

ebContents :: ExtendedBuffer -> T.Text
ebContents (b, _) = contents b

-- Basic buffer functions

emptyBuffer :: Buffer
emptyBuffer = Buffer "" emptyEditHistory (0, 0) (0, 0)

newBuffer :: T.Text -> Buffer
newBuffer bufferContents = Buffer bufferContents emptyEditHistory (0,0) (0,0)

unsaved :: Buffer -> Bool
unsaved (Buffer _ (k, _, _) _ _) = case k of
    0 -> False
    _ -> True
unsaved _ = False

input :: Char -> Bool -> Buffer -> Buffer
input c m = insert (T.cons c "") m True

paste :: T.Text -> Buffer -> Buffer
paste s = insert s False False

tab' :: Bool -> Buffer -> Buffer
tab' tabToSpaces'' b@(Buffer s h crs@(r, c) sel@(rr, cc))
    | crs == sel =
          if tabToSpaces''
              then (insert (T.replicate n $ T.singleton ' ') False True) b
              else (insert "\t" False True) b
    | otherwise  = Buffer newS newH (r, c + n0) (rr, cc + n1)
  where
    n = tabWidth - (mod c tabWidth)
    newH = addEditToHistory tabEdit h
    tabEdit = EditGroup [ IndentLine r' (tabWidth' r') c' 0 0
                        | r' <- [r0..r1] ] ix sel'
    (ix, sel') = ixAndSel b
    tabWidth' r'' = if tabToSpaces''
                        then tabWidth - mod (lnIndent ' ' r'' newS) tabWidth
                        else 1
    c' = if tabToSpaces'' then ' ' else '\t'
    n0 = tabWidth - mod (lnIndent ' ' r newS) tabWidth
    n1 = tabWidth - mod (lnIndent ' ' rr newS) tabWidth
    r0 = min r rr
    r1 = max r rr
    newS = doEdit tabEdit s
tab' _ b = b

unTab' :: Bool -> Buffer -> Buffer
unTab' tabToSpaces'' b@(Buffer s h (r, c) (rr, cc))
    | all (== 0) n = b
    | otherwise    = Buffer newS newH (r, c + cAdd r) (rr, cc + cAdd rr)
  where
    newH = addEditToHistory unTabEdit h
    unTabEdit = EditGroup [ IndentLine r' n' c' 0 0
                          | (r', n') <- zip [r0..r1] n ] ix sel
    (ix, sel) = ixAndSel b
    c' = if tabToSpaces'' then ' ' else '\t'
    r0 = min r rr
    r1 = max r rr
    n = [ unTabWidth r' | r' <- [r0..r1] ]
    cAdd r'' = if tabToSpaces'' then unTabWidth r''
                                else tabWidth * unTabWidth r''
    unTabWidth r'' = if thisIndent == 0
                         then 0
                         else if tabToSpaces''
                                  then (-tabWidth) + mod (-thisIndent) tabWidth
                                  else (-1)
      where
        thisIndent = lnIndent c' r'' s
    newS = doEdit unTabEdit s
unTab' _ b = b

nlAutoIndent :: Buffer -> Buffer
nlAutoIndent b@(Buffer s _ crs sel) =
    insert (T.cons '\n' $ lnIndent' r s) False True b
  where
    (r, _) = min crs sel
nlAutoIndent b = b

ixAndSel :: Buffer -> (Index, Selection)
ixAndSel (Buffer s _ crs sel) = ((cursorToIx crs s), (distance crs sel s))
ixAndSel _ = (0, 0)

-- Generic insert string at current selection
insert :: T.Text -> Bool -> Bool -> Buffer -> Buffer
insert s m fusible b@(Buffer s0 h crs sel) = Buffer newS newH newCrs newCrs
  where
    newH = addEditToHistory edit h
    edit = Edit (0, 0) s ix sel' fusible $ selection b
    (ix, sel'') = ixAndSel b
    sel' = if m && sel'' == 0 && doInsert then 1 else sel''
    doInsert = if T.take 1 s0' == "\n" then False else True
    (_, s0') = T.splitAt ix s0
    newCrs = ixToCursor newIx newS
    newIx = (cursorToIx (min crs sel) s0) + (T.length s)
    newS = doEdit edit s0
insert _ _ _ b = b

-- Delete
delete :: Buffer -> Buffer
delete = genericDelete 0

-- Backspace
backspace :: Buffer -> Buffer
backspace = genericDelete 1

-- Generic forward or backward deletion (not exported)
genericDelete :: Int -> Buffer -> Buffer
genericDelete d (Buffer s h crs sel) = Buffer newS newH newCrs newCrs
  where
    newH = addEditToHistory edit h
    newCrs = if c0 && d == 1 then move Backward s crs
                             else (\(x, _) -> x) $ orderTwo crs sel
    edit = Edit (n, m) "" (cursorToIx crs s) (distance crs sel s) True delS
    (n, m) = if c0 then (d, 1 - d) else (0, 0)
    c0 = (distance crs sel s) == 0
    delS = T.take (l + n + m) $ T.drop (k - n) s
    l = abs $ distance crs sel s
    k = cursorToIx (min crs sel) s
    newS = doEdit edit s
genericDelete _ b = b

-- Copy selection
selection :: Buffer -> T.Text
selection (Buffer s _ crs sel) = T.take l $ T.drop k s
  where
    l = abs $ distance crs sel s
    k = cursorToIx (min crs sel) s
selection _ = ""

-- Perform undo on buffer, moving cursor to the beginning of the undone edit
undo :: Buffer -> Buffer
undo buffer@(Buffer s h@(_, past, _) crs sel) = case past of
    (x:_) -> alignCursor True $ Buffer newS (undoEdit h) crs sel
               where
                 newS = undoEdit' x s
    _     -> buffer
undo b = b

-- Perform redo on buffer, moving cursor to the end of the redone edit
redo :: Buffer -> Buffer
redo buffer@(Buffer s h@(_, _, future) crs sel) = case future of
     (x:_) -> alignCursor False $ Buffer newS (redoEdit h) crs sel
                where
                  newS = doEdit x s
     _     -> buffer
redo b = b

alignCursor :: Bool -> Buffer -> Buffer
alignCursor True (Buffer s h@(_, _, y:_) _ _) = Buffer s h newCrs newSel
  where
    newCrs = ixToCursor (editIndex y) s
    newSel = ixToCursor (editIndex y + editSelection y) s
alignCursor False (Buffer s h@(_, x:_, _) crs sel) =
    Buffer s h newCrs newSel
  where
    newCrs = case x of
        (Edit (n, _) s' ix sel' _ _) ->
            ixToCursor (ix - n + T.length s' + sel') s
        (IndentLine _ n ' ' ix _)    -> ixToCursor (ix + n) s
        _                            -> crs
    newSel = case x of
        (Edit _ _ _ _ _ _)           -> newCrs
        (IndentLine _ n ' ' ix sel') -> ixToCursor (ix + sel' + n) s
        _                            -> sel
alignCursor _ b = b

-- End and Home functions
endOfLine :: Bool -> Buffer -> Buffer
endOfLine moveSel (Buffer s h (r, _) sel) = Buffer s h newCrs newSel
  where
    newCrs = case drop r $ T.lines s of (x:_) -> (r, tabbedLength tabWidth x)
                                        _     -> ixToCursor (T.length s - 1) s
    newSel = case moveSel of True  -> newCrs
                             False -> sel
endOfLine _ b = b

startOfLine :: Bool -> Buffer -> Buffer
startOfLine moveSel (Buffer s h (r, c) sel) = Buffer s h (r, newC) newSel
  where
    newC = if c == c' then 0 else c'
    c' = T.length $ lnIndent' r s
    newSel = case moveSel of True  -> (r, newC)
                             False -> sel
startOfLine _ b = b

endOfFile :: Bool -> Buffer -> Buffer
endOfFile moveSel (Buffer s h _ sel) = Buffer s h newCrs newSel
  where
    newCrs = ixToCursor (T.length s) s
    newSel = case moveSel of True  -> newCrs
                             False -> sel
endOfFile _ b = b

startOfFile :: Bool -> Buffer -> Buffer
startOfFile moveSel (Buffer s h _ sel) = Buffer s h (0, 0) newSel
  where
    newSel = case moveSel of True  -> (0, 0)
                             False -> sel
startOfFile _ b = b

selectAll :: Buffer -> Buffer
selectAll (Buffer s h _ _) = Buffer s h (0, 0) $ ixToCursor (T.length s) s
selectAll b = b

deselect :: Buffer -> Buffer
deselect (Buffer s h crs _) = Buffer s h crs crs
deselect b = b

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
genericMoveCursor moveSel n d (Buffer s h crs sel)
    | n == 1    = Buffer s h newCrs newSel
    | otherwise = genericMoveCursor moveSel (n - 1) d (Buffer s h newCrs newSel)
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
    (minCrs, maxCrs) = orderTwo crs sel
genericMoveCursor _ _ _ b = b

bufferFind :: Bool -> Bool -> T.Text -> Buffer -> Buffer
bufferFind next doStep findString b@(Buffer s h crs _) =
    Buffer s h foundCrs foundSel
  where
    foundIx = findIx k next findString s
    foundCrs = ixToCursor foundIx s
    foundSel = ixToCursor (foundIx + T.length findString) s
    k = (cursorToIx crs s) + ixStep
    ixStep = if (selection b == findString) && (xnor next doStep) then 1 else 0
bufferFind _ _ _ b = b

-- Basic functions for extended buffers

ebEmpty :: FilePath -> Language -> ExtendedBuffer
ebEmpty p l =
    ( emptyBuffer
    , BufferMetaData p' l' [] False tabToSpacesDefault)
  where
    p' = if p == "" then "Untitled" else p
    l' = if l == "" then "Unknown" else l

-- TODO: convert to Buffer -> (Index, Selection)
ebSelection :: ExtendedBuffer -> (Index, Selection)
ebSelection ((Buffer s _ crs sel), _) =
    (cursorToIx selectionStart s, distance selectionStart selectionEnd s)
  where
    (selectionStart, selectionEnd) = orderTwo crs sel
ebSelection _                         = (0, 0)

ebNew :: FilePath -> T.Text -> Language -> ExtendedBuffer
ebNew path'' fileContents language'' =
    ( newBuffer fileContents
    , BufferMetaData path'' language'' tokenLines'' False tabToSpaces'')
  where
    tokenLines'' = tokenLines $ tokenize language'' fileContents
    tabToSpaces'' = if (T.isInfixOf (T.singleton '\t') fileContents)
                        then False
                        else tabToSpacesDefault

-- TODO: convert to Buffer -> (Cursor, Cursor)
ebCursors :: ExtendedBuffer -> (Cursor, Cursor)
ebCursors ((Buffer _ _ crs sel), _) = (crs, sel)
ebCursors _                         = ((0, 0), (0, 0))

ebFirst :: (Buffer -> Buffer) -> ExtendedBuffer -> ExtendedBuffer
ebFirst f (b, bufferMetaData) = (b', bufferMetaData')
  where
    b' = f b
    bufferMetaData' = if editHistory b' == editHistory b
                          then bufferMetaData
                          else setTokenLines' tokenLines'' bufferMetaData
    tokenLines'' = tokenLines $ tokenize (language' bufferMetaData') s'
    s' = contents b'

condense :: ExtendedBuffer -> Buffer
condense (b, _) = b

ebUnsaved :: ExtendedBuffer -> Bool
ebUnsaved = unsaved . condense

tab :: ExtendedBuffer -> ExtendedBuffer
tab (b, bufferMetaData) = (newB, newBufferMetaData)
  where
    newB = tab' (tabToSpaces' bufferMetaData) b
    newBufferMetaData = setTokenLines' tokenLines'' bufferMetaData
    tokenLines'' = tokenLines $ tokenize (language' bufferMetaData) s
    s = contents newB

unTab :: ExtendedBuffer -> ExtendedBuffer
unTab (b, bufferMetaData) = (newB, newBufferMetaData)
  where
    newB = unTab' (tabToSpaces' bufferMetaData) b
    newBufferMetaData = setTokenLines' tokenLines'' bufferMetaData
    tokenLines'' = tokenLines $ tokenize (language' bufferMetaData) s
    s = contents newB
