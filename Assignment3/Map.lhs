> module Map where

Author: Lindsay Groves, VUW, 2017.

Implement a map as an unordered list of key-value pairs with no duplicate
keys.  These are all done directly - could instead implement in terms of set
operations.  

Operations provided:
emptyMap :: Map a b
hasKey :: a -> Map a b -> Bool
setVal :: a -> Map a b -> Map a b
getVal :: a -> Map a b -> b
delKey :: a -> Map a b -> Map a b

> type Map a b = [(a, b)]

> emptyMap :: Map a b
> emptyMap = []

> hasKey :: Eq a => a -> Map a b -> Bool
> hasKey k m = any (\(x,_) -> x == k) m

> setVal :: Eq a => a -> b -> Map a b -> Map a b
> setVal k v m = (k,v) : delKey k m

> getVal :: Eq a => a -> Map a b -> b
> getVal k m | length r == 0 = error "Missing key"
> 	     | otherwise = snd $ head r
>	     where r = dropWhile (\(x,_) -> x /= k) m

> delKey :: Eq a => a -> Map a b -> Map a b
> delKey k m = filter (\(x,_) -> x /= k) m