import Data.Char

-- Gabriel Velasco Exercicio 1 a
numof::Char->[Char]->Int
numof _ [] = 0

numof c (x:xs)
    | c == x = 1 + numof c xs
    | otherwise = numof c xs

-- Gabriel Velasco Exercicio 1 b
ellen::[[Char]]->[Int]
ellen (x:xs) = map length (x:xs)

-- Gabriel Velasco Exercicio 1 c
-- cria nova lista com o quadrado dos positivos e soma a lista
funcTest::Int->Int
funcTest x 
    | x >= 0 = x*x
    | otherwise = 0

somaLista::[Int]->Int
somaLista [x] = x
somaLista (x:xs) = x + somaLista xs

ssp::[Int]->Int
ssp (x:xs) = somaLista (map funcTest (x:xs))

-- Gabriel Velasco Exercicio 2
separa::[Char]->([Char], [Char])
separa l = ((filter isAlpha l), (filter isDigit l))

-- Gabriel Velasco Exercicio 3
const x _ = x -- pega valor do primeiro parametro
swap (x,y) = (y,x) -- troca ordem da tupla
apply f x = f x -- aplica funcao f em x equivalente a ==> map f [x]

-- Gabriel Velasco Exercicio 4
type Nome = String
type Quantidade = Int
type PrecoUn = Float

data ShopItem = Ind Nome Quantidade PrecoUn
    deriving (Show)

tmp::ShopItem
tmp = Ind "bolacha" 5 2.5

somaCompras::[Float]->Float
somaCompras [x] = x
somaCompras (x:xs) = x + somaCompras xs

pegaPreco::ShopItem->PrecoUn
pegaPreco (Ind _ _ precoUn) = precoUn

calculaLista::[ShopItem]->Float
calculaLista (x:xs) = somaCompras (map pegaPreco (x:xs))