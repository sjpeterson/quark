{-|
Module:      Quark.Flipper
Description: Module for flippers
Copyright:   (c) Stefan Peterson
License:     MIT License
Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
Stability:   Stable
Portability: Portable

A flipper is a construct that behaves similarly to a circular buffer by
representing a list as a triple of active element, previous elements and
next elements.

Flippers are used to manage open buffers and navigate the project tree.
-}

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

-- | @Flipper a@ is a triple (a, [a], [a]) of the active element,
-- previous elements (in reverse order) and next elements)
type Flipper a = (a, [a], [a])

-- | Get the currently active index
active :: Flipper a -> a
active (x, _, _) = x

-- | Get the index of the currently active element
idxOfActive :: Flipper a -> Int
idxOfActive (_, p, _) = length p

-- | Add a new element to a flipper
add :: a -> Flipper a -> Flipper a
add newX (x, y, z) = (newX, (reverse z) ++ x:y, [])

-- | Remove the currently active element
remove :: Flipper a -> Maybe (Flipper a)
remove (_, x:xs, ys) = Just (x, xs, ys)
remove (_, [], y:ys) = Just (y, [], ys)
remove _             = Nothing

-- | Replace the currently active element
replace :: a -> Flipper a -> Flipper a
replace newX (x, y, z) = (newX, y, z)

-- | Flip to the next element, wrapping around at the end
flipNext :: Flipper a -> Flipper a
flipNext (x, [], []) = (x, [], [])
flipNext (x, y, []) = (last y, [], drop 1 $ reverse (x:y))
flipNext (x, y, z:zs) = (z, x:y, zs)

-- | Flip to the previous element, wrapping around at the beginning
flipPrevious :: Flipper a -> Flipper a
flipPrevious (x, y, z) = (\(xx, yy, zz) -> (xx, zz, yy)) $ flipNext (x, z, y)

-- | Flip to an element with a given index
flipTo :: Int -> Flipper a -> Flipper a
flipTo k f@(x, y, z) = case compare (mod k $ length' f) (length y) of
    EQ -> f
    LT -> flipTo k $ flipPrevious f
    _  -> flipTo k $ flipNext f

-- | Flip to the last element
flipToLast :: Flipper a -> Flipper a
flipToLast f = flipTo (length' f) f

-- | Move the currently active element forward
nudge :: Flipper a -> Flipper a
nudge (x, [], []) = (x, [], [])
nudge (x, y, []) = (x, [], reverse y)
nudge (x, y, z:zs) = (x, z:y, zs)

-- | Move the currently active element backward
nudgeBack :: Flipper a -> Flipper a
nudgeBack (x, y, z) = (\(xx, yy, zz) -> (xx, zz, yy)) $ nudge (x, z, y)

-- | Move the currently active element to a given index
nudgeTo :: Int -> Flipper a -> Flipper a
nudgeTo k f@(x, y, z) = case compare (mod k $ length' f) (length y) of
    EQ -> f
    LT -> nudgeTo k $ nudge f
    _  -> nudgeTo k $ nudgeBack f

-- | Apply a function to the currently active element
firstF :: (a -> a) -> Flipper a -> Flipper a
firstF f (x, y, z) = (f x, y, z)

-- | Get the elements of a flipper as a list
toList :: Flipper a -> [a]
toList (x, y, z) = (reverse y) ++ x:z

-- | Turn a list into a flipper with the first element active
fromList :: [a] -> Maybe (Flipper a)
fromList []     = Nothing
fromList (x:xs) = Just ((x, [], xs))

-- | Return True if the currently active element is also the last element
nextEmpty :: Flipper a -> Bool
nextEmpty (_, _, []) = True
nextEmpty _          = False

-- | Return True if the currently active element is also the first element
previousEmpty :: Flipper a -> Bool
previousEmpty (_, [], _) = True
previousEmpty _          = False

-- Return the number of elements in a flipper
length' :: Flipper a -> Int
length' (x, y, z) = 1 + (length y) + (length z)
