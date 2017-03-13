--------
--
-- Module:      Quark.Flipper
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for flippers. A flipper is a circular buffer-like construct that
-- is used to keep track of and handle open buffers.
--
--------

module Quark.Flipper ( Flipper
                     , active
                     , idxOfActive
                     , add
                     , remove
                     , replace
                     , flipNext
                     , flipPrevious
                     , flipTo
                     , flipToLast
                     , nudge
                     , nudgeBack
                     , nudgeTo
                     , firstF
                     , toList
                     , fromList
                     , nextEmpty
                     , previousEmpty ) where

-- (current, previous, next)
type Flipper a = (a, [a], [a])

active :: Flipper a -> a
active (x, _, _) = x

idxOfActive :: Flipper a -> Int
idxOfActive (_, p, _) = length p

add :: a -> Flipper a -> Flipper a
add newX (x, y, z) = (newX, (reverse z) ++ x:y, [])

remove :: Flipper a -> Maybe (Flipper a)
remove (_, x:xs, ys) = Just (x, xs, ys)
remove (_, [], y:ys) = Just (y, [], ys)
remove _             = Nothing

replace :: a -> Flipper a -> Flipper a
replace newX (x, y, z) = (newX, y, z)

flipNext :: Flipper a -> Flipper a
flipNext (x, [], []) = (x, [], [])
flipNext (x, y, []) = (last y, [], drop 1 $ reverse (x:y))
flipNext (x, y, z:zs) = (z, x:y, zs)

flipPrevious :: Flipper a -> Flipper a
flipPrevious (x, y, z) = (\(xx, yy, zz) -> (xx, zz, yy)) $ flipNext (x, z, y)

flipTo :: Int -> Flipper a -> Flipper a
flipTo k f@(x, y, z) = case compare (mod k $ length' f) (length y) of
    EQ -> f
    LT -> flipTo k $ flipPrevious f
    _  -> flipTo k $ flipNext f

flipToLast :: Flipper a -> Flipper a
flipToLast f = flipTo (length' f) f

nudge :: Flipper a -> Flipper a
nudge (x, [], []) = (x, [], [])
nudge (x, y, []) = (x, [], reverse y)
nudge (x, y, z:zs) = (x, z:y, zs)

nudgeBack :: Flipper a -> Flipper a
nudgeBack (x, y, z) = (\(xx, yy, zz) -> (xx, zz, yy)) $ nudge (x, z, y)

nudgeTo :: Int -> Flipper a -> Flipper a
nudgeTo k f@(x, y, z) = case compare (mod k $ length' f) (length y) of
    EQ -> f
    LT -> nudgeTo k $ nudge f
    _  -> nudgeTo k $ nudgeBack f

firstF :: (a -> a) -> Flipper a -> Flipper a
firstF f (x, y, z) = (f x, y, z)

toList :: Flipper a -> [a]
toList (x, y, z) = (reverse y) ++ x:z

fromList :: [a] -> Maybe (Flipper a)
fromList []     = Nothing
fromList (x:xs) = Just ((x, [], xs))

length' :: Flipper a -> Int
length' (x, y, z) = 1 + (length y) + (length z)

nextEmpty :: Flipper a -> Bool
nextEmpty (_, _, []) = True
nextEmpty _          = False

previousEmpty :: Flipper a -> Bool
previousEmpty (_, [], _) = True
previousEmpty _          = False