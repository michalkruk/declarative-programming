--zad 1
module OperacjeZbiory where

suma :: Eq a => [a] -> [a] -> [a]
suma [] z = z
suma (x:xs) z | element x z == True = suma xs z
              | otherwise = [x] ++ suma xs z

iloczzn :: Eq a => [a] -> [a] -> [a]
iloczyn [] z = []
iloczyn (x:xs) z | element x z == True = [x] ++ iloczyn xs z
                 | otherwise = iloczzn xs z

roznica :: Eq a => [a] -> [a] -> [a]
roznica [] z = []
roznica (x:xs) z | element x z == True = roznica xs z
                 | otherwise = [x] ++ roznica xs z

podzbior :: Eq a => [a] -> [a] -> Bool
podzbior [] z = True
podzbior (x:xs) z | element x z == True = podzbior xs z
                  | otherwise = False


