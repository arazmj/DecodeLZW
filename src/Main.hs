{-
 - Author: Amir Razmjou, arazmjou2014@my.fit.edu
 - Author: Benjamin Yue, byue2013@my.fit.edu
 - Course: CSE 4250, Fall 2015
 - Project: Proj3, Decoding Text
 -}

data Tree = Leaf Char | Branch Tree Tree

main :: IO ()
main = interact (unlines . showResult . readTestCase . lines)

readTestCase :: [String] -> (String, [String])
readTestCase (x:lx) = (x, lx)
readTestCase _ =  error "The input must be more than one line."

showResult :: (String, [String]) -> [String]
showResult (encoding, messages) = map (decodeMessage encodingTree) messages
    where (_, encodingTree) =  (encodeTree encoding)

encodeTree :: String -> (String, Tree)
encodeTree ('*':encoding) =
    let (encodingLeft, left)   = encodeTree encoding
        (encodingRight, right) = encodeTree (tail encodingLeft)
    in (encodingRight, Branch left right)
encodeTree (encoding)          = (encoding, Leaf (head encoding))

decodeChar :: String -> Tree -> (Char, String)
decodeChar rest     (Leaf c)         = (c, rest)
decodeChar ('0':xb) (Branch left _)  = decodeChar xb left
decodeChar ('1':xb) (Branch _ right) = decodeChar xb right
decodeChar _  _ = error "Either the message or the encoding is corrupted."

decodeMessage :: Tree -> String -> String
decodeMessage _ []      = []
decodeMessage tree message =
    let (c, rest) = decodeChar message tree
    in c:decodeMessage tree rest
