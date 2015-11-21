data Tree a = Leaf Char | Branch (Tree a) (Tree a)

main = interact (unlines . showResult . readTestCase . lines)

readTestCase :: [t] -> (t, [t])
readTestCase (x:lx) = (x, lx)

showResult :: (String, [String]) -> [String]
showResult (encoding, messages) = map (decodeMessage encodingTree) messages
    where (_, encodingTree) =  (encodeTree encoding)

encodeTree :: String -> (String, Tree a)
encodeTree ('*':encoding)  =
    let (encodingLeft, left)  = encodeTree encoding
        (encodingRight, right) = encodeTree (tail encodingLeft)
    in (encodingRight, Branch left right)
encodeTree (encoding)  = encoding, Leaf (head encoding)

decodeChar :: String -> Tree t -> (Char, String)
decodeChar rest     (Leaf c)         = (c, rest)
decodeChar ('0':xb) (Branch left _)  = decodeChar xb left
decodeChar ('1':xb) (Branch _ right) = decodeChar xb right

decodeMessage :: Tree t -> String -> String
decodeMessage tree []      = []
decodeMessage tree message =
    let (c, rest) = decodeChar message tree
    in c:decodeMessage tree rest
