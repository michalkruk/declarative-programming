-- Zadania cw 3 poniedzialek
-- Zadanie 1
--A
dodajPoczatek n b = b : n
--B

--C
dodajKoniec n b = n ++ [b]

-- Zadanie 2
--A
drugiEl n = head $ tail n
--B

--C

-- Zadanie 3
odwrot :: [a] -> [a] 
odwrot    []  = [] 
odwrot (x:xs) = odwrot xs ++ [x]

-- Zadanie 4
zmiana list = last list : (init . tail $ list) ++ [head list]