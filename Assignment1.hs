import Data.Char
--q1a
lineToWords :: String -> [String]
lineToWords [] = []
lineToWords (x : xs) 
              | isAlphaNum x = addString (x : xs) : lineToWords (drop (length (addString (x:xs))) xs)
              | otherwise    = lineToWords xs

addString :: String -> String
addString [] = []
addString (x : xs)
             | x == ' '     = []
             | isAlphaNum x = x : addString xs
             | otherwise    = addString xs

--a1b

linesToWords :: [String] -> [String]
linesToWords [] = []
linesToWords x = lineToWords(getALine x) ++ (linesToWords (dropALine x))
getALine x = x !! 0
dropALine x = drop 1 x

--q1c

posOfWords :: [String] -> [(String, Int, Int)]
posOfWords [] = []
posOfWords x = pos x 1

pos :: [String] -> Int -> [(String, Int, Int)]
pos [] n = []
pos (x:xs) n = (getIndex x n 1) ++ (pos xs (n+1))

getIndex :: String -> Int -> Int -> [(String, Int, Int)]
getIndex [] n m = []
getIndex x n m = (getWord x, n, m) : (getIndex (removeNonWord x) n (m + length(getWord x) + 1))

getWord :: String -> String
getWord xs = takeWhile (isAlphaNum) xs

removeNonWord :: String -> String
removeNonWord xs = dropWhile (not . isAlphaNum)  (dropWhile (isAlphaNum) xs)

--q2a
--wrapLines :: Int -> [String] -> [String] 
--q2b
--justifyLines :: Int -> [String] -> [String] 
--q3a

encode :: [String] -> ([String], [Int])
encode [] = ([],[])
encode [x] = ([x],[1])
encode x = (getString x, getNums (getString x) 1)

getString :: [String] -> [String]
getString [] = []
getString x = reverseList(removeDuplicate(reverseList(linesToWordsM x)))

getNums :: [String] -> Int -> [Int]
getNums [] x = []
getNums x y  = encodeNum x (assignNum x 1)

reverseList :: [String] -> [String]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

removeDuplicate :: [String] -> [String]
removeDuplicate [] = []
removeDuplicate (x : xs)
                   | x `elem` xs = removeDuplicate xs
                   | otherwise   = x : removeDuplicate xs

assignNum :: [String] -> Int -> [(String, Int)]
assignNum [] y       = [("",y)]
assignNum [x] y        = [(x, y)]
assignNum (x : xs) y = (x,y) : assignNum xs (y+1)

encodeNum :: [String] -> [(String,Int)] -> [Int]
encodeNum [] [] = []
encodeNum (x:xs) [(word, num)]
                    | x == word = num : encodeNum xs [(word, num)]
                    | otherwise = encodeNum xs [(word, num)]

lineToWordsM :: String -> [String]
lineToWordsM [] = []
lineToWordsM (x : xs) 
              | isAlphaNum x = addStringM (x : xs) : lineToWordsM (drop (length (addStringM (x:xs))) xs)
              | otherwise    = lineToWordsM xs

addStringM :: String -> String
addStringM [] = []
addStringM (x : xs)
             | x == ' '     = []
--             | isAlphaNum x = x : addStringM xs
             | otherwise    = x : addStringM xs

linesToWordsM :: [String] -> [String]
linesToWordsM [] = []
linesToWordsM x = lineToWordsM(getALine x) ++ (linesToWordsM (dropALine x))

--q3b
decode :: ([String], [Int]) -> [String]
decode ([],[]) = []
decode (x,[]) = []
decode ([],x) = []
decode (x,[n]) = [x!!(n-1)]
decode (x,(n:ns)) = x!!(n-1) : decode (x,ns)