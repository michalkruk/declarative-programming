-- ZADANIE 1
import Kruk

-- ZADANIE 2
import Data.Map (fromListWith, toList)
pairList :: (Ord a) => [a] -> [(a, Int)]
pairList xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- ZADANIE 3 

-- Tworzenie drzewka do sprawdzenia 
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
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

-- FUNKCJA WLASCIWA
minDepth :: Tree a -> Int
minDepth Empty = 0
minDepth (Node _ l r) = 1 + min (minDepth l) (minDepth r)

-- ZADANIE 4

-- FUNKCJA POMOCNICZA - ZWRACA LISTE Z PODZIALEM [LEWA_GALAZ KORZEN PRAWA_GALAZ]
makeInOrder :: Tree a -> [a]
makeInOrder Empty = []
makeInOrder (Node a l r) = makeInOrder l ++ [a] ++ makeInOrder r

-- FUNKCJA POMOCNICZA - SPRAWDZA CZY ELEMENTY LISTY SA CALKOWITE
checkIfListIsAsc :: (Ord a) => [a] -> Bool
checkIfListIsAsc [] = True
checkIfListIsAsc [x] = True
checkIfListIsAsc (x:y:xs) = x <= y && checkIfListIsAsc (y:xs)

-- FUNKCJA WLASCIWA
correctTree :: Ord a => Tree a -> Bool
correctTree t = if(checkIfListIsAsc(makeInOrder t)==True) then True else False

-- ZADANIE 5

-- FUNKCJA POMOCNICZA
insertOnTree :: (Ord a) => a -> Tree a -> Tree a
insertOnTree x Empty = Node x Empty Empty
insertOnTree x (Node n left right)
  | x == n = Node n left right
  | x > n = Node n left (insertOnTree x right)
  | x < n = Node n (insertOnTree x left) right

-- FUNKCJA POMOCNICZA
listTree :: (Ord a) => [a] -> Tree a
listTree [] = Empty
listTree xs = foldl (\tree x -> insertOnTree x tree) Empty xs

-- FUNKCJA POMOCNICZA
sort [] = []
sort(x:xs)=sort(filter(<x)xs)++
            [x]++
            sort(filter(>=x)xs)

-- FUNKCJA WLASCIWA
organizeTree :: Ord a => Tree a -> Tree a
organizeTree t = listTree(sort(makeInOrder t))