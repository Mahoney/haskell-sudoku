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


work :: Either a b -> Either [a] [b] -> Either [a] [b]
work (Right b) (Right bs) = Right (b:bs)
work (Left a) (Right _)   = Left [a]
work (Left a) (Left as)   = Left (a:as)
work (Right _) (Left as)  = Left as

split :: [Either a b] -> Either [a] [b]
split = foldr work (Right [])
