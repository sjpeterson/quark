module Quark.IOHelpers ( autoComplete ) where

import System.FilePath ( addTrailingPathSeparator
                       , splitFileName
                       , joinPath)
import System.Directory ( listDirectory
                        , doesDirectoryExist )

autoComplete :: Int -> FilePath -> IO (FilePath)
autoComplete k path = do
    c <- (filter $ startsWith partialName) <$> (listDirectory directory)
    let s0  = if length c == 0
                  then ""
                  else head (drop (rem k $ length c) c)
    sIsDir <- doesDirectoryExist $ joinPath [directory, s0]
    let s = if s0 /= "" && sIsDir
                then addTrailingPathSeparator s0
                else s0
    if length c == 1 && k > 0
        then (drop n) <$> (s ++) <$> (autoComplete (k - 1) $ directory ++ s)
        else return $ drop n s
  where
    (directory, partialName) = splitFileName path
    n = length partialName

startsWith :: String -> String -> Bool
startsWith "" _ = True
startsWith s0 s = take (length s0) s == s0