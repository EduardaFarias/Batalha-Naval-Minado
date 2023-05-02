import Control.Concurrent (threadDelay)
import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import System.Random

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

-- definição dos tipos de dados
type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Vez = Int
type Tabuleiro = [[Char]]
data Jogador = Jogador Nome Pontuacao
                    deriving (Show, Read)

-- função que inicia o programa
main :: IO ()
main = do
          {catch (ler_arquivo) tratar_error;}
          where
            ler_arquivo = do
            {
              arq <- openFile "./text/dados.txt" ReadMode;
              dados <- hGetLine arq;
              hClose arq;
              inicio_apresentacao (read dados);
            }
            tratar_error erro = if isDoesNotExistError erro then do
            {
              arq <- openFile "./text/dados.txt" WriteMode;
              hPutStrLn arq "[]";
              hClose arq;
              inicio_apresentacao []
            }
            else
              ioError erro

-- Função que apresenta uma história introduzindo ao jogo e chama o menu
inicio_apresentacao :: [Jogador] -> IO()
inicio_apresentacao dados = do
                 -- imprimiIntroducao
                  putStrLn ("\nPressione <Enter> para continuar")
                  getChar
                  menu dados;
                  return()
                  
-- função que irá imprimir a introdução do jogo
imprimiIntroducao :: IO ()
imprimiIntroducao = do
                      hSetBuffering stdout NoBuffering -- disable output buffering
                      system "clear" -- limpa a tela
                      handle <- openFile "./text/introducao.txt" ReadMode
                      imprimiTextoLentamente handle
                      hClose handle

imprimiTextoLentamente :: Handle -> IO ()
imprimiTextoLentamente handle = do
  texto <- hGetContents handle
  imprimiTextoLentamenteAux texto

imprimiTextoLentamenteAux :: String -> IO ()
imprimiTextoLentamenteAux [] = return ()
imprimiTextoLentamenteAux (x:xs) = do
  putStr [x]
  Control.Concurrent.threadDelay 50000
  imprimiTextoLentamenteAux xs


-- função que exibe o Menu
menu :: Jogadores -> IO Jogadores
menu dados = do
                system "clear" -- limpa a tela
                putStrLn "-------------------- Batalha Naval --------------------"
                putStrLn "\n ● Digite 1 para cadastrar jogador"
                putStrLn "\n ● Digite 2 para jogar"
                putStrLn "\n ● Digite 3 para ver o modo história"
                putStrLn "\n ● Digite 0 para sair "
                putStr "\n→ Opção: \n\n"
                op <- getChar
                getChar
                executarOpcao dados op


-- função para manipular a opção escolhida pelo usuário
executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '0' = do
                            putStr("\nA água esquece o nome dos afogados...\n")
                            return dados

executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '2' = prepararJogo dados
executarOpcao dados '3' = menu dados
executarOpcao dados _ = do
                          putStrLn ("\nOpção inválida! Tente novamente...")
                          putStrLn ("\nPressione <Enter> para voltar ao menu")
                          getChar
                          menu dados

-- função responsável pelo cadastro de jogadores
cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
                            system "clear" -- limpa a tela
                            putStrLn "                  Cadastro de jogadores                  "
                            putStrLn "\nDigite um nome de usuário: \n"
                            nome <- getLine
                            if(existeJogador dados nome) then do
                              putStrLn "\nEsse nome já existe, escolha outro."
                              putStrLn ("\nPressione <Enter> para continuar...")
                              getChar
                              cadastrarJogador dados
                            else do
                              arq <- openFile "./text/dados.txt" WriteMode
                              hPutStrLn arq(show ((Jogador nome 0):dados))
                              hClose arq
                              putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso!")
                              putStrLn ("\nPressione <Enter> para continuar...")
                              getChar
                              menu ((Jogador nome 0):dados)


getString :: String -> IO String
getString str = do
                  putStr str
                  res <- getLine
                  return res

existeJogador :: Jogadores -> String -> Bool
existeJogador [] _ = False
existeJogador ((Jogador nomeCadastrado pontuacao):xs) nome
                | (nomeCadastrado == nome) = True
                | otherwise = existeJogador xs nome

prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
                      system "clear"
                      putStrLn "-------------------- Batalha Naval --------------------"
                      putStrLn "\n • Digite 1 para jogar com a máquina"
                      putStrLn "\n • Digite 2 para jogar com dois jogadores"
                      putStrLn "\n • Digite 3 para redimensionar o tabuleiro"
                      putStrLn "\n • Digite 0 para voltar ao menu"
                      putStr "\n→ Opção: \n\n"
                      op <- getChar
                      getChar
                      executarOpcaoJogo dados op

executarOpcaoJogo :: Jogadores -> Char -> IO Jogadores
executarOpcaoJogo dados '0' = menu dados
executarOpcaoJogo dados '1' = doWhile True executaJogoComMaquina dados

executarOpcaoJogo dados '2' = menu dados
executarOpcaoJogo dados '3' = menu dados

-- executa um loop do jogo
doWhile :: Bool -> (function -> IO Jogadores) -> Jogadores -> IO Jogadores
doWhile condition function dados
  | condition = do 
                system "clear"
                tabuleiro_Jogador <- criaMatriz 10
                tabuleiro_Bot <- criaMatrizBot 10

                putStr $ unlines tabuleiro_Bot


               -- iniciaTabuleiro tabuleiro_Jogador 10 0
              --  tabuleiro2 <- configuraNavios 3 10 tabuleiro_Jogador
              --  iniciaTabuleiro tabuleiro2 10 0
                putStrLn "\nVocê quer jogar novamente? [1 para sim, outro número para sair]"
                putStr "\n→ Opção: \n\n"
                op <- getChar
                getChar
                if(op == '1') then doWhile True executaJogoComMaquina dados
                else doWhile False executaJogoComMaquina dados
  | otherwise = menu dados


-- função para criar a matriz com um tamnho determinado pelo usuário ou com tamanho 10 caso o usuário não redefina o tamanho do tabuleiro
criaMatriz :: Int -> IO [[Char]]
criaMatriz n = return $ replicate n (intercalate "" (replicate (n) "."))

criaMatrizBot :: Int -> IO [[Char]]
criaMatrizBot n = do
                  tabuleiro_Bot <- return $ replicate n (intercalate "" (replicate (n) "."))
                  adicionaNavioNoBot tabuleiro_Bot 5 n -- O tamanho do navio começa com 5

adicionaNavioNoBot :: [String] -> Int -> Int -> IO [String]
adicionaNavioNoBot tabuleiro 1 _ = return tabuleiro
adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro = do
                  gen <- newStdGen
                  let (posI, _) = randomR (0, (tamTabuleiro-1)) gen
                  let (posJ, _) = randomR (0, (tamTabuleiro-1)) gen
                  let (orientacao, _) = randomR (0, 1) gen :: (Int, StdGen)

                  if orientacao == 0 then
                    if ((posI + tamNavio <= 10) && (verificaTemNavioNaLinha tabuleiro posI posJ tamNavio)) then do
                        tab <- adicionaNavio tabuleiro posI posJ tamNavio
                        adicionaNavioNoBot tab (tamNavio-1) tamTabuleiro
                    else
                      adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro
                  else
                      if ((posJ + tamNavio <= 10) && (verificaTemNavioNaLinha (transpose tabuleiro) posI posJ tamNavio)) then do
                          tab <- fmap transpose (adicionaNavio (transpose tabuleiro) posJ posI tamNavio)
                          adicionaNavioNoBot tab (tamNavio-1) tamTabuleiro
                      else
                          adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro


adicionaNavio :: [[Char]] -> Int -> Int -> Int -> IO [[Char]]
adicionaNavio tabuleiro posI posJ tamNavio = return (adicionaNavioNaLinha tabuleiro tamNavio posI posJ)



adicionaNavioNaLinha :: [[Char]] -> Int -> Int -> Int -> [[Char]]
adicionaNavioNaLinha (h:t) tamNavio posI posJ
    | posI == 0 && t == [] = [take posJ h] ++ replicate (tamNavio) "#" ++ [drop (posJ + tamNavio) h]
    | posI == 0 = ((take posJ h) ++ ['#' | _  <- [posJ..(posJ + tamNavio - 1)]] ++ (drop (posJ + tamNavio) h)) : t
    | null t = [h]
    | otherwise = h : adicionaNavioNaLinha t tamNavio (posI - 1) posJ


verificaTemNavioNaLinha :: [[Char]] -> Int -> Int -> Int -> Bool
verificaTemNavioNaLinha tabuleiro posI posJ tamNavio =
    not (temNavio(take tamNavio (drop posJ (tabuleiro !! posI))))


temNavio :: [Char] -> Bool
temNavio tab = '#' `elem` tab


executaJogoComMaquina :: Jogadores -> IO Jogadores
executaJogoComMaquina dados = menu dados
