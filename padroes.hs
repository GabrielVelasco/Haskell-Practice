import Data.Char (isUpper,isLower,toUpper,toLower,isLetter)

diasMes m 
    | ((m == 1) || (m == 12)) = 31
    | otherwise = 30 -- preguica kk

-- PADROES == FATOS --
-- USAR GUARDAS/IF THEN ELSE para execoes --

timeGrande "GALO" = True
timeGrande t 
    | t /= "GALO" = False 

diasMesPadrões 1 = 31 
diasMesPadrões 12 = 31 
diasMesPadrões 11 = 30 
diasMesPadrões n 
    | n == 0 = error "erro" 
    | n >= 13 = error "erro" 
    | otherwise = 30 

eLógico True True = True 
eLógico b1 b2 = False 

ouLógico False False = False
ouLógico x y = True 

éVogal "a" = True 
éVogal "e" = True 
éVogal "i" = True 
éVogal "o" = True 
éVogal "u" = True 

éVogal c = False 

-- FATORIAL

fat 0 = 1 
fat n 
    | (n > 0) = n * fat (n-1) 
    | otherwise = error "Num negativo caralho!!!" 

-- ================================================================================ --

    -- WHERE (DEFINICOES LOCAIS, ESCOPOS) --

-- mostra como var/funcs vao ser usadas e depois a definicao de cada uma
verfTime :: Int -> String 
verfTime numTitulos 
    | numTitulos <= camp = "campeao" 
    | numTitulos == bicap = "bicarbonato" 
    | numTitulos >= tricamp = "tricapetao+" 
    where   camp = 1 
            bicap = 2
            tricamp = 3

fibW 0 = 0
fibW 1 = 1
fibW n = fibnMenos1 + fibnMenos2 
    where   fibnMenos1 = fibW (n-1) 
            fibnMenos2 = fibW (n-2) 

imcW p a
    | imcW' <= baixo = "Baixo"
    | imcW' <= normal = "Normal"
    | imcW' <= alto = "Alto"
    where imcW' = p/a^2
          baixo = 18.5
          normal = 25.0
          alto = 30.0

areaQuadradoW l1 l2 = area 
    where area = l1 * l2 


inverteCase :: Char -> Char
inverteCase c 
    | cUpVerf' = cLower -- igual ao orignial
    | otherwise = cUpper 

    where   cUpper = toUpper c 
            cLower = toLower c 
            cUpVerf' = isUpper c -- verifica (uf c == up = true ...)

dataPorExtenso d m a = (diaPorExtenso' d) ++ " de " ++ (mêsPorExtenso' m) ++ " de " ++ (show a)
    where diaPorExtenso' dia = if dia == 1 then "primeiro" else "dois"
          mêsPorExtenso' mmes = if mmes == 1 then "janeiro" else "fevereiro" 

    -- LET IN --

-- define/especifica cada variavel/func e depois mostra como vao ser usadas


fib 0 = 0
fib 1 = 1
fib n = let prev     = fib (n - 1) 
            prevPrev = fib (n - 2)
        in prev + prevPrev -- variaveis sao validas APENAS dentro do IN

areaCilindro r a = 
    let areaLateral = 2 * pi * r * a
        areaTopo = pi * r^2  
    in  areaLateral + 2 * areaTopo -- variaveis sao validas APENAS dentro do IN

-- execucao "em tempo": 
    -- >>> let [areaQuadradoLet l1 l2 = l1*l2] in [areaQuadradoLet 2 3]
    -- >>> let [prev x = x - 1] in [prev 3]
