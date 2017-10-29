Discussion:

I have used fmap to wrap the contents inside the ziplist and apply functions to modify the values. ZipList is not suitable as a monad because it violates the monad laws. For example, if we were to implement join for a ZipList, given [[a1,a2,a3],[b1,b2],[c1]] and to join them into a single ziplist we would take the Nth element of the Nth list, the problem is that there are no c2 element when we try to join them together. This is why ZipList is not suitable as a monad. On the other hand, fixed length or infinite-length ziplists are monads.
\begin{code}

newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show)

instance Functor ZipList where
    fmap f (ZipList a) = ZipList (fmap f a)

instance Applicative ZipList where 
         pure x = ZipList (repeat x)
         ZipList as <*> ZipList bs = ZipList (zipWith (\f x -> f x) as bs)
         
-- instance Monad ZipList where
--      return = ZipList . repeat
--      ZipList xs >>= f = ZipList $ zipWith ((!!) . getZipList . f) xs [0..]

\end{code}