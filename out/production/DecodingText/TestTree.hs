module TestTree where


data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))

tree2 = Branch '*' Empty
                    (Branch '*' Empty
                    (Branch 'B' Empty
                    (Branch '*' Empty
                    (Branch '*' Empty
                    (Branch 'D' Empty
                    (Branch 'E' Empty
                    (Branch 'C' Empty
                    (Branch 'A' Empty Empty))))))))



countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r

treeToPreorder :: Tree Char -> String
treeToPreorder = preorder
    where preorder Empty = ""
          preorder (Branch x l r) = x : preorder l ++ preorder r


treeToInorder :: Tree Char -> String
treeToInorder = inorder
    where inorder Empty = ""
          inorder (Branch x l r) = inorder l ++ x : inorder r

-- Given a preorder string produce a binary tree such that its preorder string
-- is identical to the given one.
preToTree :: String -> Tree Char
preToTree "" = Empty
preToTree (c:cs) = Branch c Empty (preToTree cs)

preToTree2 :: String -> Tree Char
preToTree2 "" = Empty
preToTree2 ('*':cs) = Branch '*'  (preToTree2 cs) (preToTree2 cs)

