-- Zadanie 1
data Book = Albatros | Ameet | ELAY | WarBook | Prodoks

type Title = String

-- A)
wyd_ks::Book -> Title
wyd_ks Albatros = "ks1"
wyd_ks Ameet = "ks2"
wyd_ks ELAY = "ks3"
wyd_ks WarBook = "ks4"
wyd_ks Prodoks = "ks5"

-- B)
liczb_tyt:: Book -> Int
liczb_tyt Albatros = 2
liczb_tyt Ameet = 3
liczb_tyt ELAY = 4
liczb_tyt WarBook = 5
liczb_tyt Prodoks = 6

-- Zadanie 2

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- A) DRZEWO INT

treeInt :: Tree Int  
treeInt =   
    Node 1  
        (Node 2  
            (Node 4  
                (Node 8 Empty Empty) 
                Empty   
            )  
            (Node 5  
                (Node 9 Empty Empty)  
                (Node 10 Empty Empty)  
            )  
        )  
        (Node 3  
            (Node 6  
                Empty  
                (Node 11 Empty Empty)  
            )  
            (Node 7 Empty Empty)  
        )  

-- B) DRZEWO CHAR
treeChar :: Tree Char  
treeChar =   
    Node 'a'  
        (Node 'b'  
            (Node 'd'  
                Empty
                Empty  
            )  
            (Node 'e'  
                (Node 'g' Empty Empty)  
                Empty  
            )  
        )  
        (Node 'c'  
            (Node 'f'  
                (Node 'h' Empty Empty)  
                Empty  
            )  
            Empty 
        )  
-- FUNKCJE DO SPRAWDZANIA Z WYKLADU

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

-- 

-- ZADANIE 3

-- PREORDER
tree_member_pre :: Eq a => Tree a -> a -> Bool
tree_member_pre Empty x = False
tree_member_pre (Node a l r) x | a == x = True
                            | tree_member_pre l x == True = True
                            | otherwise = tree_member_pre r x
-- INORDER
tree_member_in :: Eq a => Tree a -> a -> Bool
tree_member_in Empty x = False
tree_member_in (Node a l r) x | tree_member_in l x == True = True
                            | a == x = True
                            | otherwise = tree_member_in r x
-- POSTORDER
tree_member_post :: Eq a => Tree a -> a -> Bool
tree_member_post Empty x = False
tree_member_post (Node a l r) x | tree_member_post l x == True = True
                            | tree_member_post r x == True = True
                            | a == x = True
                            | otherwise = False


-- ZADANIE 4

subtree :: Eq a => Tree a -> Tree a -> Bool
subtree a b=subList (preorder a) (preorder b)
    where subList [] [] = True
          subList _ []    = False
          subList [] _    = True
          subList (x:xs) (y:ys) 
               | x == y    = subList xs ys   
               | otherwise = subList (x:xs) ys

-- PRZYKLADOWE DRZEWO DO SPRAWDZENIA PRZYKLADU (PRAWA GALAZ treeChar)

treeChar2 :: Tree Char  
treeChar2 =   
    Node 'c'  
            (Node 'f'  
                (Node 'h' Empty Empty)  
                Empty  
            )  
            Empty 
        

-- ZADANIE 5

searchBF :: Tree a -> [a]
searchBF tree = tbf [tree]
    where
        tbf [] = []
        tbf ys = map nodeValue ys ++ tbf (concat (map leftAndRightNodes ys))
        nodeValue (Node a _ _) = a
        leftAndRightNodes (Node _ Empty Empty) = []
        leftAndRightNodes (Node _ Empty b)     = [b]
        leftAndRightNodes (Node _ a Empty)     = [a]
        leftAndRightNodes (Node _ a b)         = [a,b]
