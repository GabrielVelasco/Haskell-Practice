-- module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc


maiorDeTres a b c = if ((a >= b) && (a >= c)) then a 
                    else if (b >= c) then b 
                    else c 

--
pii = 3.14 -- func const
areaCirc r = r**2 * pii

sumLtoR l r = if l < r
              then l + sumLtoR (l+1) r
              else l

sucessor x = x + 1 
-- >>> sucessor (areaCirc (sumLtoR 1 2))
-- >>> 29.26

multp x y z = x * y * z
soma x y = x + y
-- >>> soma (soma 1 (multp 3 2 3)) (multp 5 3 2) // composicao de funcs
-- >>> 49

med3n x y z = (x + (soma y z)) / 3 
-- >>> med3n 3 6 9
-- >>> 6

areaQuad a = a * a
areaRet a b = a * b
areaCirc2 r = pi * r**2
areaTri a h = (a * h )/ 2
areaTra a b h = (a + b)/2 * h

-- if then else -- if then else -- if then else
menor a b = if a < b then a else b -- menor valor
campeao t = if t == "galo" then "sim" else "nao"

checa x y = if (soma x y) > 1 then True else False


-- ano bisexual
bix a = 
    if (mod a 4 /= 0) -- a `mod` 4
    then False 
    else if (mod a 100 /= 0) 
    then True 
    else if (mod a 400 /= 0) 
    then False 
    else True 

-- guardas, alternativa para if then else
numMes m 
    | (m == 1) = "JAN" 
    | (m == 2) = "FEV" 

bixGuard a 
    | (mod a 4 /= 0) = False 
    | (mod a 100 /= 0) = True 
    | (mod a 400 /= 0) = False 
    | True = True 

diasMes m 
    | m == 2 = 28 
    | otherwise = 30 

maiorDeTres2 a b c
  | a >= b && a >= c   = a
  | b >= c             = b -- a ja nao eh o maior
  | otherwise          = c

imc p a 
    | (p / (a**2)) < 18.5 = "baixo" 
    | (p / (a**2)) > 30 = "alto" 
    | True = "normal" -- otherwise = "normal"

precoComDesconto p q l d = 
    if q > l then ((q*p)*(1-d)) else (q*p)

pedraTesouraPapel m1 m2
    | m1 == 0 && m2 == 1 = True 
    | m1 == 0 && m2 == 2 = False 
    | m1 == 1 && m2 == 0 = False 
    | m1 == 1 && m2 == 2 = True 
    | m1 == 2 && m2 == 0 = True 
    | m1 == 2 && m2 == 1 = False 

sobeDesceBagunca :: Int -> Int -> Int -> Int
sobeDesceBagunca x y z 
    | x >= y && y >= z = -1
    | x <= y && y <= z = 1
    | otherwise = 0
