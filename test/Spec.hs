{-
 - Round-trip tests for the Huffman and LZW codecs. No external test
 - framework is used so the suite builds with just `base` and `containers`.
 -
 -   * LZW round-trips any input exactly:        decompress . compress == id
 -   * Huffman round-trips up to line joining:    decompress . compress == unlines . lines
 -}

module Main (main) where

import System.Exit (exitFailure)

import qualified Huffman
import qualified LZW

-- | One checked round trip.
data Case = Case
    { caseLabel :: String
    , passed    :: Bool
    , expected  :: String
    , actual    :: String
    }

main :: IO ()
main = do
    let cases    = concatMap roundTrips samples
        failures = filter (not . passed) cases
    mapM_ (putStrLn . describe) cases
    if null failures
        then putStrLn ("All " ++ show (length cases) ++ " round-trip tests passed.")
        else do
            putStrLn (show (length failures) ++ " test(s) failed.")
            exitFailure

-- | Builds the LZW and Huffman round-trip cases for one input.
roundTrips :: String -> [Case]
roundTrips s =
    [ Case ("lzw     " ++ show s) (lzwOut == s)         s          lzwOut
    , Case ("huffman " ++ show s) (huffmanOut == hExp)  hExp       huffmanOut
    ]
  where
    lzwOut     = LZW.decompress (LZW.compress s)
    huffmanOut = Huffman.decompress (Huffman.compress s)
    hExp       = unlines (lines s)   -- Huffman normalises line joins

-- | Renders a case as a single result line.
describe :: Case -> String
describe c
    | passed c  = "ok   " ++ caseLabel c
    | otherwise = "FAIL " ++ caseLabel c
                      ++ "  expected " ++ show (expected c)
                      ++ " but got "  ++ show (actual c)

-- | Inputs exercised by both codecs (each has at least one character so the
-- Huffman contract is well defined).
samples :: [String]
samples =
    [ "a"
    , "aaaaaa"
    , "ababababab"
    , "abracadabra"
    , "Mississippi"
    , "TOBEORNOTTOBEORTOBEORNOT"
    , "the quick, brown fox! 123."
    , "line one\nline two"
    , "hello world\nfoo bar\nhello world\n"
    ]
