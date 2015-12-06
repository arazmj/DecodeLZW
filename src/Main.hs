{-
 - Author: Amir Razmjou, arazmjou2014@my.fit.edu
 - Author: Benjamin Yue, byue2013@my.fit.edu
 - Course: CSE 4250, Fall 2015
 - Project: Proj3, Decoding Text
 -}

-- | The 'Tree' structure.
data Tree = Leaf Char | Branch Tree Tree

main :: IO ()
main = interact (unlines . showResults . lines)

-- | Maps the provided 'encoding' to a list of
-- binary 'messages'
showResults :: [String] -- ^ Input lines
            -> [String] -- ^ List of decoded messages
showResults  messages =
    map (fst . decode) (tail messages)
    where (_, encodingTree) = encodeTree (head messages)
          decode message    = until (null . snd)
                 (decodeChar encodingTree) ([], message)

-- | Constructs the encoding tree from the encoding string
-- parses the 'encoding' recursively on left and right branches
-- and passes the 'encodingLeft' to the left branch as unparsed
-- part of by right branch in order to keep states between
-- recursive calls.
-- The returning value has an empty string as its first
-- element 'encodingRight' and the root of the tree as its
-- second parameter.
encodeTree :: String         -- ^ Encoding string
           -> (String, Tree) -- ^ An empty String and the
                             -- root of the encoding tree
encodeTree ('*':encoding) =
    let (encodingLeft, left)   = encodeTree encoding
        (encodingRight, right) = encodeTree (tail encodingLeft)
    in (encodingRight, Branch left right)
encodeTree (encoding)  = (encoding, Leaf (head encoding))

-- | Traverses the encoded string with the the encoding tree
-- until it reaches a leaf. Returns the leaf (decoded char)
-- and the remaining encoded string that was not part of the
-- path.
-- Rises an error when there is encoding symbols
-- other than 0 or 1 or there is part of the encoded message
-- left that cannot be decoded any further.
decodeChar :: Tree              -- ^ Encoding tree
           -> (String, String)  -- ^ Encoded message
           -> (String, String)  -- ^ The tuple of so far decoded
                                -- the remaining of the message
decodeChar (Leaf c)         (l, rest)     = (l ++ [c], rest)
decodeChar (Branch left _)  (l, ('0':xb)) = decodeChar left  (l, xb)
decodeChar (Branch _ right) (l, ('1':xb)) = decodeChar right (l, xb)
decodeChar _  _ = error "Either the message or the encoding is corrupted."
