\begin{code}
import Control.Applicative
import Control.Monad.State
import Control.Monad
\end{code}

Discussion:
--left identity
-- return a >>= f = f a
Box a >>= f = f a

--right identity
-- m >>= return = m
Box a >>= return = Box a

--associativity
-- (m >>= f1) >>= f2 = m >>= (\a -> f1 >>= f2)
-- m >>= f1
(Box a >>= f1) >>= f2 = Box a >>= (\a -> f1 a >>= f2)
-- f1 a >>= f2
Box a >>= f2          = Box a >>= f2


\begin{code}
-- part 1
data Box a = Box a deriving (Show, Eq)

instance Functor Box where
    fmap f (Box a) = Box (f a)

instance Applicative Box where
    pure = Box
    (<*>) (Box f) (Box a) = Box (f a)

instance Monad Box where 
    return = pure
    (Box a) >>= f = f a

-- Function to prove left identity
func :: Box Int -> Box Int
func (Box a) = Box (a+1)

left_id1 :: Box Int
left_id1 = func (Box 1)

left_id2 :: Box Int
left_id2 = do
    x <- return (Box 1)
    func x
\end{code}

Disccussion:
The LockableBox Functor is just two scenario of fmapping an UnlockedBox and a LockedBox. Fmapping a LockedBox will stay the same because we cannot touch what is inside the LockedBox. For the LockableBox Applicative, pure is just an UnlockedBox and anything applied<*> to the LockedBox will remain the same. Applying<*> something to the UnlockedBox is fmapped with something. I have changed the LockableBox type of the lock and unlock functions, because I am assuming the type of data inside each box is the same, the lock and unlock function only changes the state of the box rather than the type of the data.

\begin{code}
-- part 3

data LockableBox b a = UnlockedBox a | LockedBox b deriving (Show, Eq)

instance Functor (LockableBox b) where
    fmap f (UnlockedBox a) = UnlockedBox (f a)
    fmap f (LockedBox a) = LockedBox a

instance Applicative (LockableBox b) where
    pure = UnlockedBox
    _ <*> (LockedBox a)           = LockedBox a
    (LockedBox a) <*> _           = LockedBox a
    (UnlockedBox a) <*> something = fmap a something

instance Monad (LockableBox b) where
    return = pure
    UnlockedBox a >>= f = f a
    LockedBox b >>= _ = LockedBox b

lock :: LockableBox a a -> LockableBox a a
lock (LockedBox a) = LockedBox a
lock (UnlockedBox a) = LockedBox a

unlock :: LockableBox a a -> LockableBox a a
unlock (UnlockedBox a) = UnlockedBox a
unlock (LockedBox a) = UnlockedBox a 

-- Test
unlock1 :: (Num a) => LockableBox a a
unlock1 = unlock (LockedBox 1) 

unlock2 :: (Num a) => LockableBox a a
unlock2 = unlock (UnlockedBox 1)

lock1 :: (Num a) => LockableBox a a
lock1 = lock (UnlockedBox 1)

lock2 :: (Num a) => LockableBox a a
lock2 = lock (LockedBox 1)

-- add1 :: (Num a) => LockableBox a a -> LockableBox a a
-- add1 (LockableBox a) = LockableBox (a+1)

-- test1 :: (Num a) => LockableBox a a
-- test1 = add1 (LockedBox 1)
\end{code}

Discussion:
I have implemented the dup and swap function, which does what its suppose to do. I think my program is concatenative because each state can be used as the input of the next.

\begin{code}
-- part 4

pop :: State [Int] Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State [Int] ()
push a = state $ \xs -> ((), a:xs)

dup :: State [Int] ()
dup = state $ \(x:xs) -> ((), (x:x:xs))

swap :: State [Int] ()
swap = state $ \(x:y:xs) -> ((), (y:x:xs))

demo = flip runState [] $ do
    push 3
    push 5
    push 7
    swap
    pop
    dup

\end{code}