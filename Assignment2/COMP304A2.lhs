\begin{code}
import Olset
sort l = head [ s | s <- perms l, asc s]

-- this function checks if the list given is ordered, if yes then return true otherwise false
asc :: Ord a => [a] -> Bool
asc [] = True
asc [x] = True
asc (x:(y:ys))
    | x <= y     = asc (y:ys)
    | otherwise = False

-- this function returns a list of every combination of the set passed in
perms :: [a] -> [[a]]
perms []  = [[]]
perms (x:xs) = [y | p <- perms xs, y <- interleave p]
  where
    interleave yss = (x:yss) : interleave' yss
    interleave' [] = []
    interleave' (y:ys) = map (y:) (interleave ys)

--TESING
asc_01 :: Bool
asc_01 = asc [1,2,3,4] == True

asc_02 :: Bool
asc_02 = asc "abcd" == True

asc_03 :: Bool
asc_03 = asc [2,3,1,7] == False

asc_04 :: Bool
asc_04 = asc "qwgve" == False

asc_05 :: Bool
asc_05 = asc [1] == True

perms_01 :: Bool
perms_01 = perms [1,2,3] == [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

perms_02 :: Bool
perms_02 = perms [1]  == [[1]]

sort_01 :: Bool
sort_01 = sort [5,4,3,1] == [1,3,4,5]

sort_02 :: Bool
sort_02 = sort [1] == [1]

type Set a = [a]
type Graph a b = (Set a, Set (a,b,a))
type Path a b = [(a,b,a)]

-- this function creates a graph given list of vertices and edges, raise an error 
--if start or end vertex is not in the list of vertices, and raise an error if there 
--are repeated vertices or repeated edges
makeGraph :: (Eq a, Ord a, Ord b) => ([a], [(a,b,a)]) -> Graph a b
makeGraph ([],[]) = ([],[])
makeGraph (v,e) 
        | checkall (v,e) = (Olset.makeSet(v), Olset.makeSet(e))
        | otherwise      = error "error"

-- this function find the predecessors given a vertex, finds all vertex u such that there is an edge from u
-- to the given vertex
predecessors :: Eq a => Graph a b -> a -> Set a
predecessors ([],[]) _  = []
predecessors (x,[]) _   = []
predecessors (v,(e:es)) y
        | end(e) == y = start(e) : predecessors (v,es) y
        | otherwise   = predecessors (v,es) y

-- this function find all the successors given a vertex, it finds all vertex v, such that there is an edge from
-- the given vertex to v
successors :: Eq a => Graph a b -> a -> Set a
successors ([],[]) _  = []
successors (x,[]) _   = []
successors (v,(e:es)) y
        | start(e) == y = end(e) : successors (v,es) y
        | otherwise   = successors (v,es) y

isConnected :: Eq a => Graph a b -> a -> Bool
isConnected ([],[]) st = True
isConnected x st 
        | isConnected' x st = True
        | otherwise = False

--Helper functions for isConnected
checkStartInV :: Eq a => [a] -> a -> Bool
checkStartInV x y = y `elem` x

checkConnect :: Eq a => [(a,b,a)] -> a -> Bool
checkConnect [] st = True
checkConnect [x] st 
        | start(x) == st = True
        | end(x) == st = True
        | otherwise = False
checkConnect (x:xs) st
        | start(x) == st = checkConnect xs (end(x))
        | otherwise      = checkConnect xs st

isConnected' :: Eq a => Graph a b -> a -> Bool
isConnected' (v,e) st = isConnected''[checkStartInV v st,
                                      checkConnect e st]

isConnected'' :: [Bool] -> Bool
isConnected'' [x] = x
isConnected'' (x:xs)
        | x == True = isConnected'' xs
        | otherwise = False
--Helper functions for makeGraph
checkall :: (Eq a, Ord a, Ord b) => ([a],[(a,b,a)]) -> Bool
checkall (v,e) = checkall' [checkstart v e,
                              checkend v e,
                              checkdupV v,
                              checkdupE e,
                              checkStartEndV e]

checkall' :: [Bool] -> Bool
checkall' [x] = x
checkall' (x:xs)
        | x == True = checkall' xs
        | otherwise = False

checkstart :: Eq a => [a] -> [(a,b,a)] -> Bool
checkstart v [e]
        | start(e) `elem` v = True
        | otherwise         = error "start vertex error"
checkstart v (e:es)
        | start(e) `elem` v = checkstart v es
        | otherwise         = error "start vertex error"

checkend :: Eq a => [a] -> [(a,b,a)] -> Bool
checkend v [e] 
        | end(e) `elem` v = True
        | otherwise       = error "end vertex must be in the vertex list"
checkend v (e:es)
        | end(e) `elem` v = checkend v es
        | otherwise       = error "end vertex must be in the vertex list"

checkdupV :: Eq a => [a] -> Bool
checkdupV [x] = True
checkdupV [x,y] 
        | x == y    = error "there are duplicate vertices"
        | otherwise = True
checkdupV (x:xs) 
        | x `elem` xs = error "there are duplicate vertices"
        | otherwise   = checkdupV xs

checkdupE :: (Eq a, Eq b) => [(a,b,a)] -> Bool
checkdupE [x] = True
checkdupE [x,y] 
        | x == y    = error "there are duplicate edges"
        | otherwise = True
checkdupE (x:xs)
        | x `elem` xs = error "there are duplicate edges"
        | otherwise   = checkdupE xs

checkStartEndV :: Eq a => [(a,b,a)] -> Bool
checkStartEndV [x] 
        | start(x) /= end(x) = True
        | otherwise          = error "start vertex must not equals to end vertex" 
checkStartEndV (x:xs) 
        | start(x) /= end(x) = checkStartEndV xs
        | otherwise          = error "start vertex must not equals to end vertex" 

--Helper functions to get both vertices of the edge
start :: (a,b,a) -> a
start (x,_,_) = x

end :: (a,b,a) -> a
end (_, _, x) = x

--TESTING
makeGraph_01 :: Bool
makeGraph_01 = makeGraph ([2,4,1],[(4,9,2),(2,7,1)]) == ([1,2,4],[(2,7,1),(4,9,2)])

predecessors_01 :: Bool
predecessors_01 = predecessors (makeGraph ([1,3,6,9],[(1,2,3),(9,2,3),(6,4,3)])) 2 == []

predecessors_02 :: Bool
predecessors_02 = predecessors (makeGraph ([1,3,6,9],[(1,2,3),(9,2,3),(6,4,3)])) 3 == [1,6,9]

successors_01 :: Bool
successors_01 = successors (makeGraph ([1,3,6,9],[(9,2,6),(9,2,3),(9,4,1)])) 9 == [3,6,1]

successors_02 :: Bool
successors_02 = successors (makeGraph ([1,3,6,9],[(9,2,6),(9,2,3),(9,4,1)])) 2 == []

testSuite :: Bool
testSuite = testSuite' [asc_01,
                        asc_02,
                        asc_03,
                        asc_04,
                        perms_01,
                        perms_02,
                        sort_01,
                        sort_02,
                        makeGraph_01,
                        predecessors_01,
                        predecessors_02,
                        successors_01,
                        successors_02]

testSuite' :: [Bool] -> Bool
testSuite' [x] = x
testSuite' (x:xs)
        | x == True = testSuite' xs
        | otherwise = False
\end{code}

Discussion:
The silly sort has been implemented, the cost is increase greatly according to the size of the list given. This is because the list have to go through premutation, which means that the larger the list, the more different combination will need to be generated. From the permutation, the asc will decide which combination is sorted and return that result.

For the graph part of this assignment, I have only managed to finish makeGraph, predecessors and successors. I have attempted the isConnected function, but it is not fully functional. The makeGraph function consist of many helper checking functions. The predecessors and successors function is simple function that only requirement to check for the start and end vertex and return the list. I have created tests for all the working functions that I implemented.