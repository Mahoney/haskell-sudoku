module Ext.Data.Either where

rightMerge :: (b -> a) -> Either a b -> a
rightMerge = either id

leftMerge :: (a -> b) -> Either a b -> b
leftMerge f = either f id

merge :: Either a a -> a
merge = either id id

leftMap :: (a -> c) -> Either a b -> Either c b
leftMap f (Left x) = Left(f x)
leftMap _ (Right x) = Right x
