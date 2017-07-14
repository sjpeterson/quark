module Quark.Colors where

import Quark.Types

-- Color IDs
defaultBg    = (-1) :: Int
defaultColor = (-1) :: Int
black        = 0 :: Int
red          = 1 :: Int
green        = 2 :: Int
orange       = 3 :: Int
blue         = 4 :: Int
purple       = 5 :: Int
teal         = 6 :: Int
lightGray    = 7 :: Int
darkGray     = 8 :: Int
lightRed     = 9 :: Int
lightGreen   = 10 :: Int
yellow       = 11 :: Int
lightBlue    = 12 :: Int
lightPurple  = 13 :: Int
cyan         = 14 :: Int
white        = 15 :: Int

lineNumberInt  = 51 :: Int
titleBarInt    = 52 :: Int
treeDefaultInt = 53 :: Int
treeActiveInt  = 54 :: Int
errorColorInt  = 55 :: Int

selectionColor = darkGray
hintColor      = darkGray
highlightColor = blue
backupColor    = lightGray -- different from selectionColor and highlightColor

-- Color pairs
titleBarPair    = (black, lightGray)
lineNumberPair  = (lightGray, defaultBg)
rulerPair       = (darkGray, defaultBg)
treeDefaultPair = (defaultColor, defaultBg)
treeActivePair  = (defaultColor, selectionColor)
errorPair       = (lightRed, red)

-- Variations for 8-color terminals
selectionColor'  = lightGray
highlightColor'  = teal
backupColor'     = defaultColor
treeDefaultPair' = (defaultColor, defaultBg)
treeActivePair'  = (black, selectionColor')
lineNumberPair'  = (lightGray, defaultBg)
titleBarPair'    = (black, lightGray)
errorPair'       = (black, red)

colorTriples :: Int -> [(Int, Int, Int)]
colorTriples k = baseColors ++ selectedColors ++ highlightColors ++ extraColors
  where
    baseColors                                              -- 0..16
        | k >= 17   = map (\n -> (n, n, (-1))) [1..16]
        | k >= 16   = (16, 15, (-1)):(map (\n -> (n, n, (-1))) [1..15])
        | k >= 8    = (16, 0, (-1)):(map (\n -> (n, mod n 8, (-1))) [1..15])
        | otherwise = map (\n -> (n, (-1), (-1))) [1..16]
    selectedColors  = bgColors 1 $ if k >= 16               -- 17..33
                                       then (selectionColor, backupColor)
                                       else (selectionColor', backupColor')
    highlightColors = bgColors 2 $ if k >= 16               -- 34..50
                                       then (highlightColor, backupColor)
                                       else (highlightColor', backupColor')
    extraColors
            | k >= 16   = [ cons lineNumberInt lineNumberPair
                          , cons titleBarInt titleBarPair
                          , cons treeDefaultInt treeDefaultPair
                          , cons treeActiveInt treeActivePair
                          , cons errorColorInt errorPair ]
            | k >= 8    = [ cons lineNumberInt lineNumberPair'
                          , cons titleBarInt titleBarPair'
                          , cons treeDefaultInt treeDefaultPair'
                          , cons treeActiveInt treeActivePair'
                          , cons errorColorInt errorPair' ]
            | otherwise = [ (lineNumberInt, 0, (-1))
                          , (titleBarInt, (-1), 0)
                          , (treeDefaultInt, 0, (-1))
                          , (treeActiveInt, (-1), 0)
                          , (errorColorInt, 0, (-1)) ]
      where
              cons x (y, z) = (x, y, z)
    bgColors q (col, backupCol)
        | k >= 17   = let p0 = (q*17, (-1), col)
                          f = \n -> if n == col
                                        then (n + q*17, backupCol, col)
                                        else (n + q*17, n, col)
                      in p0:(map f [1..16])
        | k >= 16   = let p0 = (q*17, (-1), col)
                          p1 = (q*17 + 16, 15, col)
                          f = \n -> if n == col
                                        then (n + q*17, backupCol, col)
                                        else (n + q*17, n, col)
                      in p0:p1:(map f [1..16])
        | k >= 8    = let p0 = (q*17, (-1), col)
                          p1 = (q*17 + 16, (-1), col)
                          f = \n -> if n == col
                                        then (n + q*17, backupCol, col )
                                        else (n + q*17, n, col )
                      in p0:p1:(map (f . (\n -> mod n 8)) [1..16])
        | otherwise = map (\n -> (n, (-1), 0)) $ map (+(q*17)) [0..16]
