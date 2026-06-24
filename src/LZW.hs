{-
 - Author: Amir Razmjou, arazmjou2014@my.fit.edu
 - Author: Benjamin Yue, byue2013@my.fit.edu
 - Course: CSE 4250, Fall 2015
 - Project: Proj3, Decoding Text
 -
 - Lempel-Ziv-Welch (LZW) codec. The whole input (newlines included) is
 - treated as one stream and turned into a list of integer codes. The format
 - is self-describing so it round-trips any text exactly:
 -
 -   * line 1 -> the alphabet: the ordinals of the distinct characters in the
 -               input, sorted ascending and space separated. Code i (for
 -               0 <= i < alphabet size) denotes the i-th of these characters;
 -               new dictionary codes are assigned from the alphabet size up.
 -   * line 2 -> the LZW code stream as space separated integers.
 -}

module LZW
    ( compress
    , decompress
    ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Compresses text with LZW, emitting the alphabet header followed by the
-- integer code stream.
compress :: String -> String
compress []    = ""
compress input =
    let alphabet = Set.toAscList (Set.fromList input)        -- distinct chars, sorted
        initDict = Map.fromList (zip (map (: []) alphabet) [0 ..])
        header   = unwords (map (show . fromEnum) alphabet)
        codes    = encode initDict (length alphabet) "" input
    in unlines [header, unwords (map show codes)]

-- | The LZW encoding loop. Emits the code of the longest dictionary match,
-- then extends the dictionary with that match plus the next character.
encode :: Map String Int  -- ^ String -> code dictionary
       -> Int             -- ^ Next code to assign
       -> String          -- ^ Current match (always a dictionary key)
       -> String          -- ^ Remaining input
       -> [Int]
encode dict _    current []     = [dict Map.! current | not (null current)]
encode dict next current (c:cs) =
    let extended = current ++ [c]
    in if Map.member extended dict
           then encode dict next extended cs
           else dict Map.! current
                    : encode (Map.insert extended next dict) (next + 1) [c] cs

-- | Decompresses an LZW stream produced by 'compress'.
decompress :: String -> String
decompress input =
    case lines input of
        (alphaLine : codeLines) ->
            let alphabet = map (toEnum . read) (words alphaLine) :: String
                codes    = map read (concatMap words codeLines) :: [Int]
            in decode alphabet codes
        _ -> ""

-- | The LZW decoding loop. Rebuilds the initial dictionary from the alphabet,
-- then walks the codes, handling the special case where a code refers to the
-- entry currently being built.
decode :: String -> [Int] -> String
decode _        []             = ""
decode alphabet (first : rest) =
    let initDict  = Map.fromList (zip [0 ..] (map (: []) alphabet))
        firstWord = initDict Map.! first
    in concat (firstWord : loop initDict (length alphabet) firstWord rest)
  where
    loop _    _    _        []       = []
    loop dict next previous (k : ks) =
        let entry = case Map.lookup k dict of
                        Just word -> word
                        Nothing   -> previous ++ take 1 previous
            dict' = Map.insert next (previous ++ take 1 entry) dict
        in entry : loop dict' (next + 1) entry ks
