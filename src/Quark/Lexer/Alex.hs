{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Alex
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Common functions for all Alex lexers.
--
---------------------------------------------------------------

module Quark.Lexer.Alex ( AlexInput
                        , alexGetByte
                        , alexInputPrevChar ) where

import qualified Data.Text as T
import qualified Data.Bits
import Data.Word ( Word8 )
import Data.Char ( ord )

type AlexInput = ( Char     -- previous char
                 , [Word8]  -- remaining bytes for the current character
                 , T.Text ) -- current input string

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (_, [], s)     = case T.uncons s of
    Just (c, s) -> case utf8Encode c of
                       (b:bs) -> Just (b, (c, bs, s))
                       []     -> Nothing
    Nothing     -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

{-
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (c,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (c, b:bs, s) = Just (b,(c,bs,s))
alexGetByte (_, [], [])    = Nothing
alexGetByte (_, [], c:s) = case utf8Encode (snd c) of
                             (b:bs) -> Just (b, ((snd c), bs, s))
                             [] -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act (take len str) : go inp'
-}