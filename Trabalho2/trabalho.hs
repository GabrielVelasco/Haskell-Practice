type Codigo = Int
type Nome = String
type Preco = Float

type Produto = (Codigo, Nome, Preco)

tabelaProdutos::[Produto]
tabelaProdutos = [(001, "Chocolate", 5.25),
                  (002, "Biscoito", 10.10),
                  (003, "Laranja", 4.6),
                  (004, "Sabao", 2.1),
                  (005, "Batata", 6.9),
                  (006, "Doritos", 8.9)]

isCodigo::Codigo->Produto->Bool
isCodigo cod (c,_, _)
    | cod == c = True
    | otherwise = False

getPreco::Produto->Preco
getPreco (_, _, preco) = preco

getNome::Produto->Nome
getNome (_, nome, _) = nome

-- func q recebe um cod e uma listaProdutos e procura por 'cod' em 'listaProdutos'
-- e retorna o Produto
buscaCodTmp::Codigo->[Produto]->Produto
buscaCodTmp cod (x:xs) = head (filter (isCodigo cod) (x:xs))

buscaPrecoPorCodigo::Codigo->Preco
buscaPrecoPorCodigo cod = getPreco (buscaCodTmp cod tabelaProdutos)

buscaNomePorCodigo::Codigo->Nome
buscaNomePorCodigo cod = getNome (buscaCodTmp cod tabelaProdutos)

-- recebe lista de 'Codigos' de cada produto e retorna a soma total dos precos destes produtos
calculaPrecos::[Codigo]->Preco
calculaPrecos [cod] = buscaPrecoPorCodigo cod

calculaPrecos (x:xs) = buscaPrecoPorCodigo x + calculaPrecos xs


-- qntdPontos = 30 - length Produto
-- recebe o cod, cria a linha e add na notaFiscal
formataStrProduto::Codigo->IO()
formataStrProduto cod  = do
    let nome = buscaNomePorCodigo cod
    let preco = show (buscaPrecoPorCodigo cod)
    let qntdPontos = 30 - length nome
    let strPontos = replicate qntdPontos '.'
    let linhaNotaFiscal = (nome ++ strPontos ++ preco ++ "\n")
    appendFile "notaFiscal.txt" linhaNotaFiscal

-- recebe lista de 'Codigos', para cada um desses codigos c
-- criar a linha da nota fiscal para c
-- e dar append no arquivo .txt

geraNotaTmp::[Codigo]->[Codigo]->IO()
geraNotaTmp [] listaCompras = appendFile "notaFiscal.txt" (show (calculaPrecos listaCompras))
geraNotaTmp (x:xs) listaCompras = do 
    formataStrProduto x 
    geraNotaTmp xs listaCompras
    
geraNotaFiscal::[Codigo]->IO()
geraNotaFiscal listaCompras = geraNotaTmp listaCompras listaCompras