module Ext.Data.Either where

rightMerge :: (b -> a) -> Either a b -> a
rightMerge = either id

leftMerge :: (a -> b) -> Either a b -> b
leftMerge f = either f id

merge :: Either a a -> a
merge = either id id
