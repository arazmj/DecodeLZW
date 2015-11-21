{-# LANGUAGE ViewPatterns #-}
import Data.List

binaryTree = "**B**DECA"
input = "10110101"

--  B   00
--  D   0100
--  E   0101
--  C   011
--  A   1

getLetterIndices preOrder@('*':_) encoding = self 1 "0" ++ self 2 "1"
    where self d b = getLetterIndices (drop d preOrder) (encoding ++ b)

getLetterIndices preOrder encoding = [(encoding)]

getEncodings preOrder = zip encodings starStripped
    where
        starStripped = filter (/= '*') preOrder
        encodings = getLetterIndices preOrder []


myFunc (stripPrefix "toaster" -> Just _) = "toaster"
myFunc _ = "not a toaster"

decode encoded encodings decoded = map (decode encoded encodings decoded)
-- decode "01010101" [("00",'B'),("0100",'D')]
--test pat a = subtest pat a
--    where
--        subtest _ (stripPrefix pat -> Just _) = "Pattern matchies"
--        subtest _ _       = "Do not match"