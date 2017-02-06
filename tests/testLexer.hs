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
        tokenizeHaskell "\"Try Me\"" ]