-- zadania do zrobienia G L 
-- zadanie A
pow a n = powHELP a n 1
powHELP a n s = if n==0 then s
else powHELP a(n-1)(s*a)

-- zadanie B
function r
 | r>2 = (r*r)
 | ( r<=2 && r > 0) = r-1
 | otherwise = if r<0 then r * (-1) else r
 
-- zadanie H
suma (n) = if n == 0 then 0 else suma (n-1)+n

-- zadanie I (sumowanie akumulatorowe)
sumowanie n = sumHELP n 0
sumHELP n s = if n==0 then s
else sumHELP (n-1) (s+n)

-- zadanie J iloczyn a i b (rekurencyjne)
mnozenie a b = if (a==0 || b==0) then 0 else a + (mnozenie a (b-1)) 

-- zadanie K
ciag n 
 | n == 1 = 0
 | n == 2 = 1
 | otherwise = (2*ciag(n-1))-ciag(n-2)
 
-- zadanie N
maksymalna a b c 
 | (a>b && a>c) = a
 | (b>a && b>c) = b
 | (c>b && c>a) = c
 | otherwise = a
 
-- zadanie M 
pierwiastki a b c 
 | (b^2-(4*a*c)) > 0 =   [(((-b)-sqrt((b^2)-(4*a*c)))/(2*a)),(((-b)+sqrt((b^2)-(4*a*c)))/(2*a))]
 | (b^2-(4*a*c)) == 0 = [(-b/(2*a)),0]
 | otherwise = [0,0]

 --zadanie G (polozenie punktow od poczatku ukladu)
odleglosc a b c d = if (sqrt( (0 - a)^2 + (0 - b)^2 ) >= sqrt( (0 - c)^2 + (0 - d)^2 ) ) then [a,b] else [c,d]

 --zadanie L
ciagAkl n = ciagAklHELP n 0
ciagAklHELP n s 
 | n == 1 = s
 | n == 2 = s
 | otherwise = ciagAklHELP (n-1) (s-1)