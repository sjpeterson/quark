module Quark.Colors where

import Quark.Types

-- Color IDs
defaultBg    = (-1) :: Int
defaultColor = 0 :: Int
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
black        = 16 :: Int

selectionColor = darkGray
hintColor      = darkGray
backupColor    = lightGray -- must be different from selectionColor

-- Color pairs
titleBarPair    = (black, lightGray)
lineNumberPair  = (lightGray, defaultBg)
rulerPair       = (darkGray, defaultBg)
treeDefaultPair = (defaultColor, defaultBg)
treeActivePair  = (defaultColor, selectionColor)

-- Variations for 8-color terminals
selectionColor' = lightGray
backupColor'    = defaultColor
treeActivePair' = (defaultColor, selectionColor')
lineNumberPair' = (lightGray, defaultBg)
titleBarPair'   = ((-1), lightGray)

colorTriples :: Int -> [(Int, Int, Int)]
colorTriples k = baseColors ++ selectedColors
  where
    baseColors
        | k >= 17   = map (\n -> (n, n, (-1))) [1..16]
        | k >= 16   = (16, 15, (-1)):(map (\n -> (n, n, (-1))) [1..15])
        | k >= 8    = (16, 0, (-1)):(map (\n -> (n, mod n 8, (-1))) [1..15])
        | otherwise = map (\n -> (n, 0, (-1))) [1..16]
    selectedColors
        | k >= 17   = let p0 = (17, (-1), selectionColor)
                          f = \n -> if n == selectionColor
                                        then ( n + 17
                                             , backupColor
                                             , selectionColor)
                                        else ( n + 17
                                             , n
                                             , selectionColor)
                      in p0:(map f [1..16])
        | k >= 16   = let p0 = (17, (-1), selectionColor)
                          p1 = (33, 15, selectionColor)
                          f = \n -> if n == selectionColor
                                        then ( n + 17
                                             , backupColor
                                             , selectionColor)
                                        else ( n + 17
                                             , n
                                             , selectionColor)
                      in p0:p1:(map f [1..16])
        | k >= 8    = let p0' = (17, (-1), selectionColor')
                          p1 = (33, 0, selectionColor')
                          f = \n -> if n == selectionColor'
                                        then ( n + 17
                                             , backupColor'
                                             , selectionColor' )
                                        else ( n + 17
                                             , n
                                             , selectionColor )
                      in p0':p1:(map (f . (\n -> mod n 8)) [1..16])
        | otherwise = map (\n -> (n, (-1), 0)) [17..33]
    extraColors
        | k >= 16   = [ cons 34 lineNumberPair
                      , cons 35 titleBarPair ]
        | k >= 8    = [ cons 34 lineNumberPair'
                      , cons 35 titleBarPair' ]
        | otherwise = [ (34, 0, (-1))
                      , (35, (-1), 0) ]
      where
        cons x (y, z) = (x, y, z)