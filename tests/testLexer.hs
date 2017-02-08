import Test.Tasty
import Test.Tasty.HUnit

import Quark.Lexer.Haskell
import Quark.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ hsLexerUnitTests ]

hsLexerUnitTests = testGroup "Unit tests for Helpers.hs"
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
      assertEqual "" [ StringLiteral "\"Try"
                     , Newline "\n"
                     , StringLiteral "Me\"" ] $
        tokenizeHaskell "\"Try\nMe\""
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
                     , Newline "\n" ] $
        tokenizeHaskell "-- Line comment\n"
  , testCase "Line comment with in-line block comment" $
      assertEqual "" [ Comment "-- Line {- comment -}"] $
        tokenizeHaskell "-- Line {- comment -}"
  , testCase "Line comment with many dashes" $
      assertEqual "" [ Comment "---------------"] $
        tokenizeHaskell "---------------"
  , testCase "Keyword: case" $
      assertEqual "" [Keyword "case"] $
        tokenizeHaskell "case"
  , testCase "Keyword: class" $
      assertEqual "" [Keyword "class"] $
        tokenizeHaskell "class"
  , testCase "Keyword: data" $
      assertEqual "" [Keyword "data"] $
        tokenizeHaskell "data"
  , testCase "Keyword: default" $
      assertEqual "" [Keyword "default"] $
        tokenizeHaskell "default"
  , testCase "Keyword: deriving" $
      assertEqual "" [Keyword "deriving"] $
        tokenizeHaskell "deriving"
  , testCase "Keyword: do" $
      assertEqual "" [Keyword "do"] $
        tokenizeHaskell "do"
  , testCase "Keyword: else" $
      assertEqual "" [Keyword "else"] $
        tokenizeHaskell "else"
  , testCase "Keyword: foreign" $
      assertEqual "" [Keyword "foreign"] $
        tokenizeHaskell "foreign"
  , testCase "Keyword: if" $
      assertEqual "" [Keyword "if"] $
        tokenizeHaskell "if"
  , testCase "Keyword: import" $
      assertEqual "" [Keyword "import"] $
        tokenizeHaskell "import"
  , testCase "Keyword: in" $
      assertEqual "" [Keyword "in"] $
        tokenizeHaskell "in"
  , testCase "Keyword: infix" $
      assertEqual "" [Keyword "infix"] $
        tokenizeHaskell "infix"
  , testCase "Keyword: infixl" $
      assertEqual "" [Keyword "infixl"] $
        tokenizeHaskell "infixl"
  , testCase "Keyword: infixr" $
      assertEqual "" [Keyword "infixr"] $
        tokenizeHaskell "infixr"
  , testCase "Keyword: instance" $
      assertEqual "" [Keyword "instance"] $
        tokenizeHaskell "instance"
  , testCase "Keyword: let" $
      assertEqual "" [Keyword "let"] $
        tokenizeHaskell "let"
  , testCase "Keyword: module" $
      assertEqual "" [Keyword "module"] $
        tokenizeHaskell "module"
  , testCase "Keyword: newtype" $
      assertEqual "" [Keyword "newtype"] $
        tokenizeHaskell "newtype"
  , testCase "Keyword: of" $
      assertEqual "" [Keyword "of"] $
        tokenizeHaskell "of"
  , testCase "Keyword: then" $
      assertEqual "" [Keyword "then"] $
        tokenizeHaskell "then"
  , testCase "Keyword: type" $
      assertEqual "" [Keyword "type"] $
        tokenizeHaskell "type"
  , testCase "Keyword: where" $
      assertEqual "" [Keyword "where"] $
        tokenizeHaskell "where"
  , testCase "Keyword: _" $
      assertEqual "" [Keyword "_"] $
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
  , testCase "Dummy" $
      assertEqual "" 1 1]