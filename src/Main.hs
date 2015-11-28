{-
 - Author: Amir Razmjou, arazmjou2014@my.fit.edu
 - Author: Benjamin Yue, byue2013@my.fit.edu
 - Course: CSE 4250, Fall 2015
 - Project: Proj3, Decoding Text
 -}

-- | The tree structure on holds chars on leaves.
data Tree = Leaf Char | Branch Tree Tree

main :: IO ()
main = interact (unlines . showResult . readTestCase . lines)

-- | Seperates the first line (encoding string ) from the rest
-- of lines (encoded messages). Rises an error if there is no
-- more than a line.
readTestCase :: [String]           -- ^ List of input strings
             -> (String, [String]) -- ^ Encoding string and
                                   -- the encoded messages
readTestCase (x:lx) = (x, lx)
readTestCase _ =  error "The input must be more than one line."

-- | Maps the provided encoding to all messages
showResult :: (String, [String]) -- ^ Encoding string and
                                 -- the encoded messages
           -> [String]           -- ^ List of decoded messages
showResult (encoding, messages) =
    map (decodeMessage encodingTree) messages
    where (_, encodingTree) =  (encodeTree encoding)

-- | Constructs the encoding tree from encoding string
-- parses encoding tree recursively for on left and right
-- string but only passes to the left branch the unparsed
-- part of by right branch, to pass the state to left
-- branch.
encodeTree :: String         -- ^ Encoding string
           -> (String, Tree) -- ^ Root of encoding tree node
encodeTree ('*':encoding) =
    let (encodingLeft, left)   = encodeTree encoding
        (encodingRight, right) = encodeTree (tail encodingLeft)
    in (encodingRight, Branch left right)
encodeTree (encoding)  = (encoding, Leaf (head encoding))


-- | Traverses the encoded string with the the encoding tree
-- until it reaches a leaf. Returns the leaf (decoded char)
-- and the remaining encoded string that was not part of the
-- path. Also rises an error when there is encoding symbols
-- other than 0 or 1 or there is part of the encoded message
-- left that cannot be decoded any further.
decodeChar :: String         -- ^ Encoded message
           -> Tree           -- ^ Encoding tree
           -> (Char, String) -- ^ The decoded char and the
                             -- ^ encoded string remaining
decodeChar rest     (Leaf c)         = (c, rest)
decodeChar ('0':xb) (Branch left _)  = decodeChar xb left
decodeChar ('1':xb) (Branch _ right) = decodeChar xb right
decodeChar _  _ = error "Either the message or \
                            \the encoding is corrupted."

-- | Recursively calls 'decodeChar' until nothing of encoded
-- message is left.
decodeMessage :: Tree     -- ^ Encoding tree
                -> String -- ^ Encoded message
                -> String -- ^ Decoded message
decodeMessage _ [] = []
decodeMessage tree message =
    let (c, rest) = decodeChar message tree
    in c:decodeMessage tree rest
