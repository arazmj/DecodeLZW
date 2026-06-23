{-
 - Author: Amir Razmjou, arazmjou2014@my.fit.edu
 - Author: Benjamin Yue, byue2013@my.fit.edu
 - Course: CSE 4250, Fall 2015
 - Project: Proj3, Decoding Text
 -
 - Huffman codec: builds a prefix-code tree from the input text, then
 - compresses and decompresses using the very same on-disk format the
 - original decoder understands:
 -
 -   * line 1   -> the encoding tree in prefix notation
 -                 ('*' marks a branch, any other character is a leaf)
 -   * lines 2+ -> one '0'/'1' bit string per encoded message
 -}

module Huffman
    ( Tree (..)
    , compress
    , decompress
    , buildTree
    , serializeTree
    , parseTree
    , buildCodes
    ) where

import Data.List (group, sort, sortBy)
import Data.Ord  (comparing)

-- | The 'Tree' structure: leaves carry a character, branches carry the
-- left ('0') and right ('1') sub-trees.
data Tree = Leaf Char | Branch Tree Tree

-- | Compresses text. Each input line is encoded independently against a
-- single tree built from the characters of the whole input, so the result
-- can be fed straight back into 'decompress'.
compress :: String  -- ^ Plain text, one message per line
         -> String  -- ^ Encoding tree followed by one bit string per line
compress input =
    case concat messages of
        []   -> ""  -- nothing encodable: no characters in the input
        text -> let tree  = buildTree text
                    codes = buildCodes tree
                in unlines (serializeTree tree : map (encodeMessage codes) messages)
  where
    messages = lines input

-- | Decompresses text produced by 'compress' (or accepted by the original
-- decoder): the first line is the encoding tree, every following line is a
-- bit string to decode.
decompress :: String  -- ^ Encoding tree followed by one bit string per line
           -> String  -- ^ Decoded text, one message per line
decompress input =
    case lines input of
        []              -> ""
        (treeStr:coded) -> let tree = parseTree treeStr
                           in unlines (map (decodeMessage tree) coded)

-- | Builds a Huffman tree from the characters of the input. A single
-- distinct character is given a one-bit code so the tree always has a
-- branch for the decoder to consume bits from.
buildTree :: String -> Tree
buildTree text =
    case frequencies text of
        []        -> error "Huffman.buildTree: empty input"
        [(c, _)]  -> Branch (Leaf c) (Leaf c)
        freqs     -> combine (map (\(c, w) -> (w, Leaf c)) freqs)
  where
    combine [(_, tree)] = tree
    combine forest      =
        let (w1, t1) : (w2, t2) : rest = sortBy (comparing fst) forest
        in combine ((w1 + w2, Branch t1 t2) : rest)

-- | Counts how often each character occurs in the input.
frequencies :: String -> [(Char, Int)]
frequencies = map (\cs -> (head cs, length cs)) . group . sort

-- | Serializes a tree into prefix notation: '*' for a branch followed by
-- its left and right sub-trees, or the bare character for a leaf.
serializeTree :: Tree -> String
serializeTree (Leaf c)     = [c]
serializeTree (Branch l r) = '*' : serializeTree l ++ serializeTree r

-- | Parses prefix notation back into a 'Tree'. Inverse of 'serializeTree'.
parseTree :: String -> Tree
parseTree = snd . parse
  where
    parse ('*':rest) = let (rest1, left)  = parse rest
                           (rest2, right) = parse rest1
                       in (rest2, Branch left right)
    parse (c:rest)   = (rest, Leaf c)
    parse []         = error "Huffman.parseTree: malformed encoding tree"

-- | Builds the table mapping every character to its bit string by walking
-- the tree, appending '0' for left turns and '1' for right turns.
buildCodes :: Tree -> [(Char, String)]
buildCodes tree = walk tree ""
  where
    walk (Leaf c)     prefix = [(c, prefix)]
    walk (Branch l r) prefix = walk l (prefix ++ "0") ++ walk r (prefix ++ "1")

-- | Encodes one message by concatenating the bit string of every character.
encodeMessage :: [(Char, String)] -> String -> String
encodeMessage codes = concatMap code
  where
    code c = case lookup c codes of
                 Just bits -> bits
                 Nothing   -> error ("Huffman.encodeMessage: no code for " ++ show c)

-- | Decodes one bit string by walking the tree until each leaf is reached.
decodeMessage :: Tree -> String -> String
decodeMessage tree = fst . until (null . snd) step . (,) []
  where
    step (decoded, bits) = decodeChar tree (decoded, bits)

-- | Traverses the tree consuming bits until a leaf (one character) is found,
-- returning the decoded-so-far text and the remaining bits.
decodeChar :: Tree -> (String, String) -> (String, String)
decodeChar (Leaf c)        (decoded, rest)    = (decoded ++ [c], rest)
decodeChar (Branch left _) (decoded, '0':bs)  = decodeChar left  (decoded, bs)
decodeChar (Branch _ rght) (decoded, '1':bs)  = decodeChar rght  (decoded, bs)
decodeChar _ _ = error "Either the message or the encoding is corrupted."
