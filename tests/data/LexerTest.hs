-------- Test file for haskell lexer

module LexerTest where

import System.Directory
import qualified Data.Text as T
import qualified My.Hypothetical.Module as Hyp

s = "A string literal containing all {- manner of \" \
     \odd -} 'd' things + that 6.3 would \n indicate other tokens"

myFunction :: Hyp.Stuff -> Char
myFunction (Thing k _)
    | k > 6     = 'a'
    | otherwise = 'b'

myOtherFunction :: (Num a) => a -> a -> a -> a
myOtherFunction x y z = z + (5.2 * y / (-8.2)) * (y - x)

{- Block
   comment -}

capitalize :: String -> String
capitalize []   = []
capitalize (x:xs) = (T.toUpper):(capitalize xs) {-

I can start a comment wherever I want and even put functions in it.
Just look at this:

myId :: a -> a
myId x = x
-}

mySum :: (Num a) => [a] -> a
mySum [] = 0                    -- Line comment
mySum (x:xs) = x + mySum xs

-- More things to come