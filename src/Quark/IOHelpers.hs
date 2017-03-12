module Quark.IOHelpers ( autoComplete
                       , listDirectory' ) where

import System.FilePath ( addTrailingPathSeparator
                       , splitFileName
                       , takeFileName
                       , joinPath )
import System.Directory ( listDirectory
                        , doesDirectoryExist
                        , makeAbsolute )

import Quark.Types ( ProjectTreeElement ( RootElement
                                        , FileElement
                                        , DirectoryElement ) )

autoComplete :: Int -> FilePath -> IO FilePath
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

listDirectory' :: FilePath -> IO [ProjectTreeElement]
listDirectory' path = do
    path' <- makeAbsolute path
    directoryContents <- listDirectory path'
    sequence $
        map (pathToElement . (\x -> joinPath [path', x])) directoryContents

pathToElement :: FilePath -> IO ProjectTreeElement
pathToElement path = do
    isDirectory <- doesDirectoryExist path
    case isDirectory of
        True  -> return $ DirectoryElement (RootElement path, [], [])
        False -> return $ FileElement $ takeFileName path