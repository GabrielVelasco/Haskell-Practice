module Main (main) where

-- import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import System.IO
import Control.Monad (when)

imprime::FilePath->IO()
imprime fPath = do 
          s <- readFile fPath
          putStrLn s

saque::Float->IO()
saque valorSaque = do 
          saldoAtual <- readFile "saldo.txt"
          let novoSaldo = (read saldoAtual::Float) - valorSaque
          when (novoSaldo >= 0) $
              writeFile "saldo.txt" (show novoSaldo)
          appendFile "extrato.txt" (" \n-"++(show valorSaque))

saqueTmp::IO()
saqueTmp = do 
          putStr "Digite o valor do saque: "
          valorSaque <- getLine
          saque (read valorSaque::Float)


deposito::Float->IO()
deposito valorDeposito = do 
          saldoAtual <- readFile "saldo.txt"
          let novoSaldo = (read saldoAtual::Float) + valorDeposito
          when (novoSaldo >= 0) $
              writeFile "saldo.txt" (show novoSaldo)
          appendFile "extrato.txt" (" \n+"++(show valorDeposito))

depositoTmp::IO()
depositoTmp = do
          putStr "Digite o valor de deposito: "
          valorDeposito <- getLine
          deposito (read valorDeposito::Float)


main::IO()
main = do 
          putStrLn "\n=============================="
          putStrLn "Banco Gabriel Araujo Velasco"
          putStrLn "Clube Atlético Mineiro, Galo Forte Vingador"
          putStrLn "=============================="
          putStrLn "Opcoes:"
          putStrLn "1. Saldo"
          putStrLn "2. Extrato"
          putStrLn "3. Deposito"
          putStrLn "4. Saque"
          putStrLn "5. Fim"
          putStr "Escolha uma opcacao: "
          op <- getLine
          case op of
                    "1" -> do {imprime "saldo.txt"; main} 
                    "2" -> do {imprime "extrato.txt"; main}
                    "3" -> do {depositoTmp; main}
                    "4" -> do {saqueTmp; main}
                    "5" -> putStrLn "Obrigado por usar o banco."
                    _ -> do {putStrLn "Opcao invalida, tente novamente\n"; main}