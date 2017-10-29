\begin{code}
import Control.Monad

person :: String -> ()
person str = do
	name <- str
	return name

-- married :: String -> String -> m ()
-- isChildOf :: String -> String -> m ()
-- male :: String -> m ()
-- female :: String -> m ()

-- parents :: String -> [String]
-- parents = undefined

-- grandparents :: String -> [String]
-- grandparents = undefined

-- siblings :: String -> [String]
-- siblings = undefined

-- grandchildren :: String -> [String]
-- grandchildren = undefined

-- spouse :: String -> [String]
-- spouse = undefined

\end{code}