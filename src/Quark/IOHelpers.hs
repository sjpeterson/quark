module Quark.IOHelpers where

import Data.Char( isPrint )

import qualified UI.HSCurses.Curses as Curses

cGetLine :: IO (String)
cGetLine = reverse <$> cGetLine' ""

-- TODO: ignore non-printable rather than considering them as EOL
-- TODO: handle backspace, arrows and delete
cGetLine' :: String -> IO (String)
cGetLine' s = do
    c <- keyToChar <$> Curses.getCh
    s' <- case c of
              Nothing -> return s
              Just c' -> cGetLine' (c':s)
    return s'

keyToChar :: Curses.Key -> Maybe Char
keyToChar (Curses.KeyChar c)
    | c == '\r' = Nothing
    | otherwise = case isPrint c of
                      True -> Just c
                      _    -> Nothing
keyToChar _ = Nothing