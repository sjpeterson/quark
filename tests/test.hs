{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import qualified Data.Text as T

import Quark.Types
import Quark.Settings
import Quark.Helpers
import Quark.Cursor
import Quark.History
import Quark.Buffer

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ properties
                          , helpersUnitTests
                          , cursorUnitTests
                          , historyUnitTests
                          , bufferUnitTests ]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Order two cursors" $
      \x y -> (orderTwo (x :: Cursor) (y :: Cursor)) == (orderTwo y x)
  ,  QC.testProperty "fixToLenPadMid" $
      \x y z -> length (fixToLenPadMid x y z ' ') == max x 0
  ]

{- Unit tests for Helpers.hs -}
helpersUnitTests = testGroup "Unit tests for Helpers.hs"
  [ testCase "Height of string ending in newline" $
      assertEqual "" 3 $ strHeight "test\nstring\n"
  , testCase "Height of string not ending in newline" $
      assertEqual "" 2 $ strHeight "test\nstring"
  , testCase "Height of string with blank lines" $
      assertEqual "" 4 $ strHeight "test\n\n\nstring"
  , testCase "Width of single line string" $
      assertEqual "" 11 $ strWidth "test string"
  , testCase "Width of multiline string" $
      assertEqual "" 6 $ strWidth "test\nstring"
  , testCase "Size of string" $
      assertEqual "" (3, 6) $ strSize "test\nstring\n"
  , testCase "Line numbers width, test 1" $
      assertEqual "" 2 $ lnWidth "test\nstring"
  , testCase "Line numbers width, test 2" $
      assertEqual "" 4 $ lnWidth $ T.unlines $ replicate 145 "test"
  , testCase "Mixed line indent" $
      assertEqual "" " \t   \t\t" $ lnIndent' 1"test\n \t   \t\tstring"
  , testCase "lineSplitIx: newline-terminated string" $
      assertEqual "" 5 $ lineSplitIx 1 "test\nstring\n"
  , testCase "lineSplitIx: open-ended string" $
      assertEqual "" 5 $ lineSplitIx 1 "test\nstring"
  , testCase "lineSplitIx: line 0, newline-terminated string" $
      assertEqual "" 0 $ lineSplitIx 0 "test\nstring\n"
  , testCase "lineSplitIx: line 0, open-ended string" $
        assertEqual "" 0 $ lineSplitIx 0 "test\nstring"
  , testCase "lineSplitIx: beyond last line, newline-terminated string" $
      assertEqual "" 12 $ lineSplitIx 4 "test\nstring\n"
  , testCase "lineSplitIx: beyond last line, open-ended string" $
      assertEqual "" 11 $ lineSplitIx 4 "test\nstring"
  , testCase "tabbedLength: simple, tab width 4" $
      assertEqual "" 4 $ tabbedLength 4 "\t"
  , testCase "tabbedLength: simple, tab width 7" $
      assertEqual "" 7 $ tabbedLength 7 "\t"
  , testCase "tabbedLength: mixed with spaces, tab width 4" $
      assertEqual "" 4 $ tabbedLength 4 "  \t"
  , testCase "tabbedLength: mixed with spaces and newlines, tab width 4" $
      assertEqual "" 15 $ tabbedLength 4 "test\n   \tstring"
  ]

{- Unit tests for Cursors.hs
   the distance function is indirectly verified through cursorToIx -}
cursorUnitTests = testGroup "Unit tests for Cursors.hs"
  [ testCase "Ordinary move forward" $
      assertEqual "" (0, 4) $ move Forward "test string" (0, 3)
  , testCase "Move forward over newline" $
      assertEqual "" (1, 0) $ move Forward "test\nstring" (0, 4)
  , testCase "Move forward from end of string" $
      assertEqual "" (1, 6) $ move Forward "test\nstring" (1, 6)
  , testCase "Move forward from col exceeding line legnth" $
      assertEqual "" (1, 0) $ move Forward "test\nstring" (0, 14)
  , testCase "Ordinary move backward" $
      assertEqual "" (0, 3) $ move Backward "test string" (0, 4)
  , testCase "Move backward over newline" $
      assertEqual "" (0, 4) $ move Backward "test\nstring" (1, 0)
  , testCase "Move backward from beginning of string" $
      assertEqual "" (0, 0) $ move Backward "test string" (0, 0)
  , testCase "Move backward from col exceeding line length" $
      assertEqual "" (0, 3) $ move Backward "test\nstring" (0, 14)
  , testCase "Move backward with single tab" $
      assertEqual "" (0, 9) $ move Backward "\ttest string" (0, 10)
  , testCase "Move backward with multiple tabs" $
      assertEqual "" (0, 12) $ move Backward "\t\ttest string" (0, 13)
  , testCase "Ordinary move down" $
      assertEqual "" (1, 3) $ move Down "test\nstring" (0, 3)
  , testCase "Move down from last line" $   -- or move to end?
      assertEqual "" (1, 3) $ move Down "test\nstring" (1, 3)
  , testCase "Move down at column exceeding line length" $
      assertEqual "" (1, 14) $ move Down "test\nstring" (0, 14)
  , testCase "Ordinary move up" $
      assertEqual "" (0, 3) $ move Up "test\nstring" (1, 3)
  , testCase "Move up from first line" $    -- or move to beginning?
      assertEqual "" (0, 3) $ move Up "test\nstring" (0, 3)
  , testCase "Move up at column exceeding line length" $
      assertEqual "" (0, 14) $ move Up "test\nstring" (1, 14)
  , testCase "Cursor to index on first line" $
      assertEqual "" 3 $ cursorToIx (0, 3) "test\nstring"
  , testCase "Cursor to index beyond first line" $
      assertEqual "" 7 $ cursorToIx (1, 2) "test\nstring"
  , testCase "Cursor to index at col exceeding line length" $
      assertEqual "" 11 $ cursorToIx (1, 14) "longer\ntest\nstring"
  , testCase "Cursor to index exceeding string length" $
      assertEqual "" 11 $ cursorToIx (1, 14) "test\nstring"
  , testCase "Cursor to index, negative col" $  -- not strictly required
      assertEqual "" 0 $ cursorToIx (0, -3) "test\nstring"
  , testCase "Cursor to index, negative row" $  -- not strictly required
      assertEqual "" 0 $ cursorToIx (-2, 7) "test\nstring"
  , testCase "Index to cursor on first line" $
      assertEqual "" (0, 3) $ ixToCursor 3 "test\nstring"
  , testCase "Index to cursor beyond first line" $
      assertEqual "" (1, 2) $ ixToCursor 7 "test\nstring"
  , testCase "Index to cursor exceeding string length" $
      assertEqual "" (1, 6) $ ixToCursor 14 "test\nstring"
  , testCase "Negative index to cursor" $       -- not strictly required
      assertEqual "" (0, 0) $ ixToCursor (-3) "test\nstring"
  , testCase "ixToCursor with special characters" $
      assertEqual "" (0, 4) $ ixToCursor 4 "Mårdhund"
  , testCase "cursorToIx: incomplete tab does not count" $
      assertEqual "" 0 $ cursorToIx (0, tabWidth - 1) "\ttest string"
  , testCase "cursorToIx: full tab counts" $
      assertEqual "" 1 $ cursorToIx (0, tabWidth) "\ttest string"
  , testCase "cursorToIx: multiple tabs" $
      assertEqual "" 2 $ cursorToIx (0, 2*tabWidth) "\t\ttest string"
  ]

{- Unit tests for History.hs -}
historyUnitTests = testGroup "Unit tests for History.hs"
  [ testCase "Fuse insert with deletes" $
      assertEqual "" (1, [ (Edit (0, 2) "a" 3 0 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "a" 3 0 True "")
              (1, [ (Edit (0, 2) "" 3 0 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse insert with backspaces" $
      assertEqual "" (1, [ (Edit (2, 0) "a" 5 0 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "a" 3 0 True "")
              (1, [ (Edit (2, 0) "" 5 0 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse insert with inserts" $
      assertEqual "" (1, [ (Edit (0, 0) "abcd" 3 0 True "")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "d" 6 0 True "")
              (1, [ (Edit (0, 0) "abc" 3 0 True "")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Do not fuse paste" $
      assertEqual "" (2, [ (Edit (0, 0) "de" 6 0 False "")
                         , (Edit (0, 0) "abc" 3 0 True "")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "de" 6 0 False "")
              (1, [ (Edit (0, 0) "abc" 3 0 True "")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Do not fuse with paste" $
      assertEqual "" (2, [ (Edit (0, 0) "d" 6 0 True "")
                         , (Edit (0, 0) "abc" 3 0 False "")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "d" 6 0 True "")
              (1, [ (Edit (0, 0) "abc" 3 0 False "")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse insert with forward selection delete" $
      assertEqual "" (1, [ (Edit (0, 0) "a" 3 2 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "a" 3 0 True "")
              (1, [ (Edit (0, 0) "" 3 2 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse insert with backward selection delete" $
      assertEqual "" (1, [ (Edit (0, 0) "a" 5 (-2) True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "a" 3 0 True "")
              (1, [ (Edit (0, 0) "" 5 (-2) True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse delete with backspace" $
      assertEqual "" (1, [ (Edit (2, 2) "" 5 0 True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 2) "" 3 0 True "st")
              (1, [ (Edit (2, 0) "" 5 0 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse delete with insert" $
      assertEqual "" (1, [ (Edit (0, 2) "abc" 3 0 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 2) "" 6 0 True "t ")
              (1, [ (Edit (0, 0) "abc" 3 0 True "")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse delete with delete" $
      assertEqual "" (1, [ (Edit (0, 4) "" 3 0 True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 2) "" 3 0 True "st")
              (1, [ (Edit (0, 2) "" 3 0 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse delete with forward selection delete" $
      assertEqual "" (1, [ (Edit (0, 2) "" 3 2 True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 2) "" 3 0 True "st")
              (1, [ (Edit (0, 0) "" 3 2 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse delete with backward selection delete" $
      assertEqual "" (1, [ (Edit (0, 2) "" 5 (-2) True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 2) "" 3 0 True "st")
              (1, [ (Edit (0, 0) "" 5 (-2) True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse backspace with delete" $
      assertEqual "" (1, [ (Edit (2, 2) "" 3 0 True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (2, 0) "" 3 0 True "t ")
              (1, [ (Edit (0, 2) "" 3 0 True "st")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse backspace with backspace" $
      assertEqual "" (1, [ (Edit (4, 0) "" 5 0 True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (2, 0) "" 3 0 True "t ")
              (1, [ (Edit (2, 0) "" 5 0 True "st")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Do not fuse backspace with insert" $
      assertEqual "" (2, [ (Edit (2, 0) "" 6 0 True "bc")
                         , (Edit (0, 0) "abc" 3 0 True "")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (2, 0) "" 6 0 True "bc")
              (1, [ (Edit (0, 0) "abc" 3 0 True "")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse backspace with forward selection delete" $
      assertEqual "" (1, [ (Edit (2, 0) "" 3 2 True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (2, 0) "" 3 0 True "t ")
              (1, [ (Edit (0, 0) "" 3 2 True "st")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse backspace with backward selection delete" $
      assertEqual "" (1, [ (Edit (2, 0) "" 5 (-2) True "t st")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (2, 0) "" 3 0 True "t ")
              (1, [ (Edit (0, 0) "" 5 (-2) True "st")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Fuse insert with mixed edit" $
      assertEqual "" (1, [ (Edit (2, 1) "abcd" 3 2 True "est s")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "d" 4 0 True "")
              (1, [ (Edit (2, 1) "abc" 3 2 True "est s")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Do not fuse when cursor has moved" $
      assertEqual "" (2, [ (Edit (0, 0) "abc" 4 0 True "")
                         , (Edit (0, 2) "" 3 0 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "abc" 4 0 True "")
              (1, [ (Edit (0, 2) "" 3 0 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Do not fuse edit with forward selection delete" $
      assertEqual "" (2, [ (Edit (0, 0) "abc" 3 2 True "st")
                         , (Edit (0, 2) "" 3 0 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "abc" 3 2 True "st")
              (1, [ (Edit (0, 2) "" 3 0 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "Do not fuse edit with backward selection delete" $
      assertEqual "" (2, [ (Edit (0, 0) "abc" 3 (-2) True "es")
                         , (Edit (0, 2) "" 3 0 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          addEditToHistory (Edit (0, 0) "abc" 3 (-2) True "es")
              (1, [ (Edit (0, 2) "" 3 0 True "t ")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "addEditToHistory: Do not fuse IndentLine with Edit" $
      assertEqual "" (2, [ (IndentLine 1 4 ' ' 7 1)
                         , (Edit (0, 0) "string" 5 0 True "")
                         , (Edit (0, 0) "test\n" 0 0 False "") ], []) $
          addEditToHistory (IndentLine 1 4 ' ' 7 1)
              (1, [ (Edit (0, 0) "string" 5 0 True "")
                  , (Edit (0, 0) "test\n" 0 0 False "") ], [])
  , testCase "newEdit" $
      assertEqual "" (2, [ (Edit (0, 0) "de" 6 0 True "")
                         , (Edit (0, 0) "abc" 3 0 True "")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          newEdit (Edit (0, 0) "de" 6 0 True "")
              (1, [ (Edit (0, 0) "abc" 3 0 True "")
                  , (Edit (0, 0) "test string" 0 0 False "") ], [])
  , testCase "emptyEditHistory" $
      assertEqual "" (0, [], []) $
          emptyEditHistory
  , testCase "doEdit insert" $ assertEqual "" "tesabct string" $
      doEdit (Edit (0, 0) "abc" 3 0 True "") "test string"
  , testCase "doEdit backspace" $ assertEqual "" "tt string" $
      doEdit (Edit (2, 0) "" 3 0 True "es") "test string"
  , testCase "doEdit delete" $ assertEqual "" "tesstring" $
      doEdit (Edit (0, 2) "" 3 0 True "t ") "test string"
  , testCase "doEdit backward selection delete" $ assertEqual "" "tt string" $
      doEdit (Edit (0, 0) "" 3 (-2) True "es") "test string"
  , testCase "doEdit forward selection delete" $ assertEqual "" "tesstring" $
      doEdit (Edit (0, 0) "" 3 2 True "t ") "test string"
  , testCase "doEdit indent line" $ assertEqual "" "test\n    string" $
      doEdit (IndentLine 1 4 ' ' 7 1) "test\nstring"
  , testCase "doEdit indent line, file ending with linebreak" $
      assertEqual "" "test\n    string\n" $
          doEdit (IndentLine 1 4 ' ' 7 1) "test\nstring\n"
  , testCase "doEdit dedent line" $ assertEqual "" "test\nstring" $
      doEdit (IndentLine 1 (-4) ' ' 11 0) "test\n    string"
  , testCase "doEdit dedent line, file ending with line break" $
      assertEqual "" "test\nstring\n" $
          doEdit (IndentLine 1 (-4) ' ' 11 0) "test\n    string\n"
  , testCase "undoEdit' forward selection delete" $
      assertEqual "" "test string" $
          undoEdit' (Edit (0, 0) "" 3 2 True "t ") "tesstring"
  , testCase "undoEdit' backward selection delete" $
      assertEqual "" "test string" $
          undoEdit' (Edit (0, 0) "" 5 (-2) True "t ") "tesstring"
  , testCase "Ordinary undo edit" $
      assertEqual "" ( 1
                     , [(Edit (0, 2) "" 3 0 True "t ")]
                     , [(Edit (0, 0) "test string" 0 0 False "")] ) $
          undoEdit (2, [ (Edit (0, 0) "test string" 0 0 False "")
                       , (Edit (0, 2) "" 3 0 True "t ") ], [])
  , testCase "Undo saved edit" $
      assertEqual "" ( (-1)
                     , [(Edit (0, 2) "" 3 0 True "t ")]
                     , [(Edit (0, 0) "test string" 0 0 False "")] ) $
          undoEdit (0, [ (Edit (0, 0) "test string" 0 0 False "")
                       , (Edit (0, 2) "" 3 0 True "t ") ], [])
  , testCase "Undo solitary edit" $
      assertEqual "" (0, [], [(Edit (0, 0) "test string" 0 0 False "")]) $
          undoEdit (1, [(Edit (0, 0) "test string" 0 0 False "")], [])
  , testCase "Ordinary redo edit" $
      assertEqual "" (2, [ (Edit (0, 2) "" 3 0 True "t ")
                         , (Edit (0, 0) "test string" 0 0 False "") ], []) $
          redoEdit ( 1
                   , [(Edit (0, 0) "test string" 0 0 False "")]
                   , [(Edit (0, 2) "" 3 0 True "t ")] )
  , testCase "Nothing to redo" $
      assertEqual "" (4, [(Edit (0, 0) "test string" 0 0 False "")], []) $
          redoEdit (4, [(Edit (0, 0) "test string" 0 0 False "")], [])
  ]

{- Unit tests for Buffer.hs -}
bufferUnitTests = testGroup "Unit tests for Buffer.hs"
  []