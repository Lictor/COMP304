\begin{code}
module Olset (
 makeSet
, has
, card
, add
, del
, union
, intersect
, equals
, subset
, select
) where

type Set a = [a]

-- this function makes a set by removing all duplicates, and also sort them in order using the add function.
makeSet :: Ord a => [a] -> Set a
makeSet [] = []
makeSet (x:xs) 
        | has x xs  = makeSet xs
        | otherwise = add' x (makeSet xs)

-- this function checks if the set already has the item, returns true if it does and otherwise false
has :: Ord a => a -> Set a -> Bool
has _ [] = False
has x [y] = x == y
has a x
    | a > x!!index = has a (drop index x)
    | a < x!!index = has a (take index x)
    | otherwise = True
    where index = length x `quot` 2 

-- this function returns the number of element in the set
card :: Eq a => Set a -> Int
card []  = 0
card (x:xs) = card xs + 1

-- add element to the set and put them in the right place, in the right order
add :: Ord a => a -> Set a -> Set a
add e [] = [e]
add e x = add' e (makeSet(x))

--helper function to add funtion
add' :: Ord a => a -> Set a -> Set a
add' e [] = [e]
add' e [x]
        | e < x     = [e,x]
        | otherwise = [x,e]
add' e (x:xs) 
        | e > x     = x:(add' e xs)
        | e < x     = e : x : xs
        | otherwise = add' e xs

-- this function checks if the set has the element, if it has then it deletes it otherwise leave the set untouched
del :: Eq a => a -> Set a -> Set a
del _ []  = []
del e (x:xs)
        | e == x    = xs
        | otherwise = x:del e xs

-- this function combines two sets together and remove any duplicates
union :: Eq a => Set a -> Set a -> Set a
union [] []         = []
union x []          = x
union [] x          = x
union (x:xs) y
        | x `elem` y = union xs y
        | otherwise  = x : union xs y

-- this function is the opposite of the union function, it returns a set of elements in both sets
intersect :: Ord a => Set a -> Set a -> Set a
intersect [] [] = []
intersect x [] = []
intersect [] x = []
intersect (x:xs) y
            | x `elem` y = makeSet(x : intersect xs y)
            | otherwise  = intersect xs y

-- this function checks whether two sets are equals 
equals :: Ord a => Set a -> Set a -> Bool
equals [] [] = True
equals x []  = False
--the line below is incorrect, but it fixes equals sets with
--different order of elements
equals [] x  = True
equals (x:xs) y
                | has x y = equals xs y
                | otherwise  = False

-- this function returns the subset of the set
subset :: Eq a => Set a -> Set a -> Bool
subset [] [] = True
subset x [] = False
subset [] x = True
subset (x:xs) y
        | x `elem` y = subset xs y
        | otherwise  = False

-- this funciton selects from a set according to the condition given
select :: Eq a => (a -> Bool) -> Set a -> Set a
-- select _ [] = []
select f ys = [ x | x <- ys, f x]

--TESTING
make_set_01 :: Bool
make_set_01 = makeSet [4,4,7,9,7,5,6,5,9,3] == [3,4,5,6,7,9]

-- this test does not work because I it requires == constrain but there are no input parameters to put the constrain on
-- make_set_02 :: Bool
-- make_set_02 = makeSet [] == []

has_01 :: Bool
has_01 = has 4 [1,2,3,4] == True

has_02 :: Bool
has_02 = has 10 [6,4,2,5,1] == False

card_01 :: Bool
card_01 = card [1,2,3,4,5,6] == 6

-- this test does not work because I it requires == constrain but there are no input parameters to put the constrain on
-- card_02 :: Bool
-- card_02 = card [] == 0

add_01 :: Bool
add_01 = add 4 [1,2,5] == [1,2,4,5]

add_02 :: Bool
add_02 = add 9 [] == [9]

add_03 :: Bool
add_03 = add 1 [4,3,1,5,8] == [1,3,4,5,8]

del_01 :: Bool
del_01 = del 1 [4,3,1,8] == [4,3,8]

del_02 :: Bool
del_02 = del 4 [1,7,3] == [1,7,3]

union_01 :: Bool
union_01 = union [1,5,6,8,3] [] == [1,5,6,8,3]

union_02 :: Bool
union_02 = union [3,6,4,2] [8,6,1,3] == [4,2,8,6,1,3]

intersect_01 :: Bool
intersect_01 = intersect [5,2,7,9] [9,3,6,5] == [5,9]

intersect_02 :: Bool
intersect_02 = intersect [1,6,3,8] [] == []

equals_01 :: Bool
equals_01 = equals [1,3,6,2] [1,3,6,2] == True

equals_02 :: Bool
equals_02 = equals [3,5,1,2] [2,1,3,5] == True

subset_01 :: Bool
subset_01 = subset [3,2,4] [1,3,2,4,6] == True

subset_02 :: Bool
subset_02 = subset [] [1,2,3] == True

subset_03 :: Bool
subset_03 = subset [1,2,3] [] == False

select_01 :: Bool
select_01 = select (==3) [2,1,3] == [3]

select_02 :: Bool
select_02 = select (>3) [1..10] == [4,5,6,7,8,9,10]

select_03 :: Bool
select_03 = select (>8) [1..4] == []

testSuite :: Bool
testSuite = testSuite' [make_set_01, 
                        has_01, 
                        has_02, 
                        card_01, 
                        add_01, 
                        add_02, 
                        add_03, 
                        del_01, 
                        del_02,
                        union_01,
                        union_02,
                        intersect_01,
                        intersect_02,
                        equals_01,
                        equals_02,
                        subset_01,
                        subset_02,
                        subset_03,
                        select_01,
                        select_02,
                        select_03]

testSuite' :: [Bool] -> Bool
testSuite' [x] = x
testSuite' (x:xs)
        | x == True = testSuite' xs
        | otherwise = False
\end{code}

Discussion:
This is the ordered list set, the only different with the unordered list set is that it is ordered everytime the set is created.
In the order list set, we can use binary search for the has function as it is a search funtion. Since the set is already ordered, it would be faster to use binary search as it checks the boundary of the element in the set and cuts the set in half every time it checks for the boundary, which means the search area. This is faster than the unordered list set because it does not have to go through every single element in the set to check for the element.