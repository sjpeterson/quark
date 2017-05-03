{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import Quark.Lexer.Core
import Quark.Lexer.Language
import Quark.Lexer.Haskell
import Quark.Lexer.Rust
import Quark.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ lexerCoreUnitTests
                          , lexerLanguageUnitTests
                          , hsLexerUnitTests
                          , nothingLexerUnitTests]

lexerCoreUnitTests = testGroup "Unit tests for Lexer/Core.hs"
  [ testCase "splitT t []" $
      assertEqual "" [Unclassified "test string"] $
        splitT (Unclassified "test string") []
  , testCase "splitT t [k]" $
      assertEqual "" [ Unclassified "tes"
                     , Unclassified "t string"] $
        splitT (Unclassified "test string") [3]
  , testCase "splitT t [k0, k1]" $
      assertEqual "" [ Unclassified "tes"
                     , Unclassified "t st"
                     , Unclassified "ring" ] $
        splitT (Unclassified "test string") [3, 7]
  , testCase "hintTabs, simple case" $
      assertEqual "" [ Tabs "\8677   " ] $
        hintTabs [ Tabs "\t" ]
  , testCase "hintTabs, complex case" $
      assertEqual "" [ Unclassified "te"
                     , Tabs "\8677 \8677   "
                     , Unclassified "st string" ] $
        hintTabs [ Unclassified "te"
                 , Tabs "\t\t"
                 , Unclassified "st string" ]
  , testCase "Dummy" $
        assertEqual "" 1 1]

lexerLanguageUnitTests = testGroup "Unit tests for Lexer/Language.hs"
  [ testCase "assumeLanguage haskell" $
      assertEqual "" "Haskell" $ assumeLanguage "/home/test/File.hs"
  , testCase "assumeLanguage python" $
      assertEqual "" "Python" $ assumeLanguage "/home/test/File.py"
  , testCase "assumeLanguage rust" $
      assertEqual "" "Rust" $ assumeLanguage "/home/test/File.rs"]

rsLexerUnitTests = testGroup "Unit tests for Lexer/Rust.hs"
  [ testCase "=>" $
      assertEqual "" [Symbol "=>"] $
        tokenizeRust "=>",
    testCase "Ordering::Less   => println!(\"test string\")," $
      assertEqual "" [ VarIdent "Ordering"
                     , Symbol "::"
                     , VarIdent "Less"
                     , Whitespace "   "
                     , Symbol "=>"
                     , Whitespace " "
                     , VarIdent "println!"
                     , Bracket "("
                     , StringLiteral "\"test string\""
                     , Bracket ")"
                     , Separator ","] $
        tokenizeRust "Ordering::Less   => println!(\"test string\"),"]

hsLexerUnitTests = testGroup "Unit tests for Lexer/Haskell.hs"
  [ testCase "Simple StringLiteral" $
      assertEqual "" [StringLiteral "\"Try Me\"" ] $
        tokenizeHaskell "\"Try Me\""
  , testCase "StringLiteral with escaped quotes" $
      assertEqual "" [StringLiteral "\"Try \\\"Me\\\"\""] $
        tokenizeHaskell "\"Try \\\"Me\\\"\""
  , testCase "StringLiteral with block comment" $
      assertEqual "" [StringLiteral "\"Try {-Me-}\""] $
        tokenizeHaskell "\"Try {-Me-}\""
  , testCase "StringLiteral with line comment" $
      assertEqual "" [StringLiteral "\"Try -- Me\""] $
        tokenizeHaskell "\"Try -- Me\""
  , testCase "StringLiteral with newline" $
      assertEqual "" [ StringLiteral "\"Try\\"
                     , Newline "\n"
                     , StringLiteral "\\Me\"" ] $
        tokenizeHaskell "\"Try\\\n\\Me\""
  , testCase "Simple Block comment" $
      assertEqual "" [Comment "{- Block comment -}"] $
        tokenizeHaskell "{- Block comment -}"
  , testCase "Block comment with string quotes" $
      assertEqual "" [Comment "{- \"Still a block comment\" -}"] $
        tokenizeHaskell "{- \"Still a block comment\" -}"
  , testCase "Block comment with newline" $
      assertEqual "" [ Comment "{- Block"
                     , Newline "\n"
                     , Comment "comment -}" ] $
        tokenizeHaskell "{- Block\ncomment -}"
  , testCase "Simple Line comment" $
      assertEqual "" [Comment "-- Line comment"] $
        tokenizeHaskell "-- Line comment"
  , testCase "Line comment terminated by newline" $
      assertEqual "" [ Comment "-- Line comment"
                     , Newline "\n"
                     , VarIdent "myFunction"] $
        tokenizeHaskell "-- Line comment\nmyFunction"
  , testCase "Line comment with in-line block comment" $
      assertEqual "" [ Comment "-- Line {- comment -}"] $
        tokenizeHaskell "-- Line {- comment -}"
  , testCase "Line comment with many dashes" $
      assertEqual "" [ Comment "---------------"] $
        tokenizeHaskell "---------------"
  , testCase "ReservedIdent: case" $
      assertEqual "" [ReservedIdent "case"] $
        tokenizeHaskell "case"
  , testCase "ReservedIdent: class" $
      assertEqual "" [ReservedIdent "class"] $
        tokenizeHaskell "class"
  , testCase "ReservedIdent: data" $
      assertEqual "" [ReservedIdent "data"] $
        tokenizeHaskell "data"
  , testCase "ReservedIdent: default" $
      assertEqual "" [ReservedIdent "default"] $
        tokenizeHaskell "default"
  , testCase "ReservedIdent: deriving" $
      assertEqual "" [ReservedIdent "deriving"] $
        tokenizeHaskell "deriving"
  , testCase "ReservedIdent: do" $
      assertEqual "" [ReservedIdent "do"] $
        tokenizeHaskell "do"
  , testCase "ReservedIdent: else" $
      assertEqual "" [ReservedIdent "else"] $
        tokenizeHaskell "else"
  , testCase "ReservedIdent: foreign" $
      assertEqual "" [ReservedIdent "foreign"] $
        tokenizeHaskell "foreign"
  , testCase "ReservedIdent: if" $
      assertEqual "" [ReservedIdent "if"] $
        tokenizeHaskell "if"
  , testCase "ReservedIdent: import" $
      assertEqual "" [ReservedIdent "import"] $
        tokenizeHaskell "import"
  , testCase "ReservedIdent: in" $
      assertEqual "" [ReservedIdent "in"] $
        tokenizeHaskell "in"
  , testCase "ReservedIdent: infix" $
      assertEqual "" [ReservedIdent "infix"] $
        tokenizeHaskell "infix"
  , testCase "ReservedIdent: infixl" $
      assertEqual "" [ReservedIdent "infixl"] $
        tokenizeHaskell "infixl"
  , testCase "ReservedIdent: infixr" $
      assertEqual "" [ReservedIdent "infixr"] $
        tokenizeHaskell "infixr"
  , testCase "ReservedIdent: instance" $
      assertEqual "" [ReservedIdent "instance"] $
        tokenizeHaskell "instance"
  , testCase "ReservedIdent: let" $
      assertEqual "" [ReservedIdent "let"] $
        tokenizeHaskell "let"
  , testCase "ReservedIdent: module" $
      assertEqual "" [ReservedIdent "module"] $
        tokenizeHaskell "module"
  , testCase "ReservedIdent: newtype" $
      assertEqual "" [ReservedIdent "newtype"] $
        tokenizeHaskell "newtype"
  , testCase "ReservedIdent: of" $
      assertEqual "" [ReservedIdent "of"] $
        tokenizeHaskell "of"
  , testCase "ReservedIdent: then" $
      assertEqual "" [ReservedIdent "then"] $
        tokenizeHaskell "then"
  , testCase "ReservedIdent: type" $
      assertEqual "" [ReservedIdent "type"] $
        tokenizeHaskell "type"
  , testCase "ReservedIdent: where" $
      assertEqual "" [ReservedIdent "where"] $
        tokenizeHaskell "where"
  , testCase "ReservedIdent: _" $
      assertEqual "" [ReservedIdent "_"] $
        tokenizeHaskell "_"
  , testCase "Simple type" $
      assertEqual "" [TypeIdent "MyType"] $
        tokenizeHaskell "MyType"
  , testCase "Qualified type" $
      assertEqual "" [TypeIdent "MyModule.MyType"] $
        tokenizeHaskell "MyModule.MyType"
  , testCase "Qualified type with numbers" $
      assertEqual "" [TypeIdent "MyModule1.MyType9"] $
        tokenizeHaskell "MyModule1.MyType9"
  , testCase "simple variable" $
        assertEqual "" [VarIdent "myVariable0"] $
          tokenizeHaskell "myVariable0"
  , testCase "qualified variable (function)" $
        assertEqual "" [VarIdent "MyModule.myVariable0"] $
          tokenizeHaskell "MyModule.myVariable0"
  , testCase "Operator: ::" $
      assertEqual "" [Operator "::"] $
        tokenizeHaskell "::"
  , testCase "Separator: ," $
      assertEqual "" [Separator ","] $
        tokenizeHaskell ","
  , testCase "Dummy" $
      assertEqual "" 1 1]

nothingLexerUnitTests = testGroup "Unit tests for tokenizeNothing"
  [ testCase "Handle trailing newline" $
      assertEqual "" [ Unclassified "test"
                     , Newline "\n"
                     , Unclassified "string"
                     , Newline "\n"
                     , Unclassified "" ] $ tokenizeNothing "test\nstring\n"]