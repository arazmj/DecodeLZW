

--main = interact (unlines . readTestCase . lines)

readTestCase :: [t] -> (t, [t])
readTestCase (x:lx) = (x, lx)

showResult :: (t, [t]) -> String
showResult (encoding, [message]) = (unwords message)

--"Encoding: " ++ encoding ++ " Messages " ++ 