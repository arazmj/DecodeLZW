{-
 - Author: Amir Razmjou, arazmjou2014@my.fit.edu
 - Author: Benjamin Yue, byue2013@my.fit.edu
 - Course: CSE 4250, Fall 2015
 - Project: Proj3, Decoding Text
 -
 - Command line front end for the Huffman and LZW codecs. Each command reads
 - from stdin and writes to stdout:
 -
 -   DecodingText huffman compress     Huffman: text -> tree + bit strings
 -   DecodingText huffman decompress   Huffman: tree + bit strings -> text
 -   DecodingText lzw compress         LZW: text -> alphabet + codes
 -   DecodingText lzw decompress       LZW: alphabet + codes -> text
 -
 -   DecodingText compress             Huffman compress (shorthand)
 -   DecodingText decompress           Huffman decompress (shorthand)
 -   DecodingText                      Huffman decompress (default, legacy)
 -}

module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import qualified Huffman
import qualified LZW

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["huffman", "compress"]   -> interact Huffman.compress
        ["huffman", "decompress"] -> interact Huffman.decompress
        ["lzw", "compress"]       -> interact LZW.compress
        ["lzw", "decompress"]     -> interact LZW.decompress
        ["compress"]              -> interact Huffman.compress
        ["decompress"]            -> interact Huffman.decompress
        []                        -> interact Huffman.decompress
        _                         -> usage

-- | Prints a short usage message and exits with a failure status.
usage :: IO ()
usage = do
    name <- getProgName
    mapM_ (hPutStrLn stderr)
        [ "usage: " ++ name ++ " [huffman|lzw] [compress|decompress]"
        , "       " ++ name ++ " [compress|decompress]   (Huffman shorthand)"
        ]
    exitFailure
