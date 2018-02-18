module Ext.Data.Either where

import Data.Either

rightMerge :: (b -> a) -> Either a b -> a
rightMerge = either id

leftMerge :: (a -> b) -> Either a b -> b
leftMerge f = either f id

merge :: Either a a -> a
merge = either id id

leftMap :: (a -> c) -> Either a b -> Either c b
leftMap f (Left x) = Left(f x)
leftMap _ (Right x) = Right x

-- | Turns a list of 'Either' into a single Either
-- Returns either a 'Left' containing a list, in order, of all the 'Left'
-- elements from the input or a 'Right' containing a list, in order, of all the
-- 'Right' elements s from the input. If the input contains both Lefts and
-- Rights, it returns the Lefts as a Left.
--
-- Intended for amalgamating a list of Eithers representing possible
-- success / failure cases into a single success / failure case - hence any
-- failure overrides all successes.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let list = [ Left "foo", Left "bar", Left "baz" ]
-- >>> leftPartition list
-- Left ["foo","bar","baz"]
--
-- >>> let list = [ Right 3, Right 7 ]
-- >>> leftPartition list
-- Right [3,7]
--
-- >>> let list = [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- >>> leftPartition list
-- Left ["foo","bar","baz"]
--
leftPartition :: [Either a b] -> Either [a] [b]
leftPartition xs = case partitionEithers xs of
    ([], bs) -> Right bs
    (as, _) -> Left as
