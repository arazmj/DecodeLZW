{-
 - Author: Amir Razmjou, arazmjou2014@my.fit.edu
 - Author: Benjamin Yue, byue2013@my.fit.edu
 - Course: CSE 4250, Fall 2015
 - Project: Proj3, Decoding Text
 -
 - Command line front end for the Huffman codec. It reads from stdin and
 - writes to stdout:
 -
 -   DecodingText compress     compress plain text into tree + bit strings
 -   DecodingText decompress   decode tree + bit strings back into text
 -   DecodingText              decode (default, backwards compatible)
 -}

module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import Huffman (compress, decompress)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["compress"]   -> interact compress
        ["decompress"] -> interact decompress
        []             -> interact decompress
        _              -> usage

-- | Prints a short usage message and exits with a failure status.
usage :: IO ()
usage = do
    name <- getProgName
    hPutStrLn stderr ("usage: " ++ name ++ " [compress|decompress]")
    exitFailure
