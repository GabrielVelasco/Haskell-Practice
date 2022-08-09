module Exercise where
import Data.Char (isUpper,isLower,toUpper,toLower,isLetter)

-- Defina as seguintes funções usando let in e where para fazer definições locais de testes de condições.
-- Inclua os tipos.


imc :: Float -> Float -> String
imc p a
    | imc' <= baixo  = "Baixo"
    | imc' <= normal = "Normal"
    | imc' <= alto   = "Alto"
    | otherwise     = "Altíssimo"
    where imc' = p/a**2
          baixo = 18.5
          normal = 25.0
          alto = 30.0

{-
Esta função recebe uma letra e inverte o case dela. Isto é, se for minúscula, retorna maiúscula e se forma maiúscula retorna minúscula. Se na nao for letra, retorna a mesma coisa.

inverteCase
- Entrada: c - caractere
- Saída: se c é minúscula/maiúscula, então o maiúsculo/minúsculo correspondente.

>>>inverteCase 'a'
'A'

>>>inverteCase 'A'
'a'

>>>inverteCase '3'
'3'

>>>inverteCase ' '
' '

-}
inverteCase :: Char -> Char
inverteCase c 
    | cUpVerf' = cLower -- igual ao orignial
    | otherwise = cUpper 
    
    where   cUpper = toUpper c 
            cLower = toLower c 
            cUpVerf' = isUpper c -- verifica (uf c == up = true ...)


{-
Esta função retorna a representação textual de uma data. Isto é, dados dia, mês e ano em sua forma numérica, retorna a data por extenso.

Use ++ para concatenar duas Strings.
Use show para gerar uma string a partir de um número.
>>>dataPorExtenso 10 10 2021
"dez de Outubro de 2021"

>>>dataPorExtenso 1 1 1900
"primeiro de Janeiro de 1900"

-}

dataPorExtenso d m a = (diaPorExtenso' d) ++ " de " ++ (mêsPorExtenso' m) ++ " de " ++ (show a)
    where diaPorExtenso' dia = if dia == 1 then "primeiro" else "dois"
          mêsPorExtenso' mmes = if mmes == 1 then "Janeiro" else "Fevereiro" 