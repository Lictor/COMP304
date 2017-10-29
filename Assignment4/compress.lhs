\begin{code}
import Data.Map
import Control.Monad.State

getID :: String -> State (Int, Map String Int) Int
getID s = do 

compress :: String -> ([Int], Map String Int)
compress' :: [String] -> State (Int, Map String Int) [Int]

\end{code}

Expected Output:
compress "See Spot See Spot run Run Spot run"
([0,1,0,1,2,3,1,2],fromList [("Run",3),("See",0),("Spot",1),("run",2)])