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
                let tabuleiro = criaMatriz 10
                iniciaTabuleiro tabuleiro 10 0
                tabuleiro2 <- configuraNavios 3 10 tabuleiro
                putStr (concat tabuleiro2)
                putStrLn "\nVocê quer jogar novamente? [1 para sim, outro número para sair]"
                putStr "\n→ Opção: \n\n"
                op <- getChar
                getChar
                if(op == '1') then doWhile True executaJogoComMaquina dados
                else doWhile False executaJogoComMaquina dados
  | otherwise = menu dados


-- função para criar a matriz com um tamnho determinado pelo usuário ou com tamanho 10 caso o usuário não redefina o tamanho do tabuleiro
criaMatriz :: Int -> [[Char]]
criaMatriz n = replicate n (intercalate " " (replicate (n) "."))

-- função para inicializar o tabuleiro
iniciaTabuleiro :: [[Char]] -> Int -> Int -> IO ()
iniciaTabuleiro matriz 0 i = do
                              putStrLn $ replicate 3 ' ' ++ replicate (i*2) '-' 
                              putStrLn $ replicate 3 ' ' ++ unwords (map show [0..(i-1)])
iniciaTabuleiro matriz n i = do
                          putStr $ (show i) ++ replicate 1 ' '
                          (\linha -> putStrLn $ "|" ++ linha ++ "|") (matriz !! (i))
                          iniciaTabuleiro matriz (n-1) (i+1)

configuraNavios :: Int -> Int -> [[Char]] -> IO [[Char]]
configuraNavios 0 _ tabuleiro = return tabuleiro
configuraNavios n i tabuleiro = do
                    gen <- newStdGen
                    let (posI, _) = randomR (0, (i-1)) gen
                    let (posJ, _) = randomR (0, (i-1)) gen
                    if (not (temNavioNaColuna posI posJ tabuleiro) || not (temNavioNaLinha posI posJ tabuleiro)) then do
                                                                    novoTabuleiro <- insereNavio posI posJ tabuleiro
                                                                    configuraNavios (n-1) i novoTabuleiro
                    else configuraNavios n i tabuleiro
                       

insereNavio :: Int -> Int -> [[Char]] -> IO [[Char]]
insereNavio posI posJ tabuleiro
                    | (posI + 3) < 10 = if not (temNavioNaColuna posI posJ tabuleiro) then adicionaNavioNaColuna posI posJ tabuleiro 3
                                         else return tabuleiro
                    | (posJ + 3) < 10 = if not (temNavioNaLinha posI posJ tabuleiro) then adicionaNavioNaLinha posI posJ tabuleiro 3
                                         else return tabuleiro
                    | otherwise = return tabuleiro


temNavioNaColuna :: Int -> Int -> [[Char]] -> Bool
temNavioNaColuna posI posJ tabuleiro = tabuleiro !! posI !! posJ == '#' ||
                                       tabuleiro !! (posI + 1) !! posJ == '#' ||
                                       tabuleiro !! (posI + 2) !! posJ == '#'

temNavioNaLinha :: Int -> Int -> [[Char]] -> Bool
temNavioNaLinha posI posJ tabuleiro = tabuleiro !! posI !! posJ == '#' ||
                                      tabuleiro !! posI !! (posJ + 1) == '#' ||
                                      tabuleiro !! posI !! (posJ + 2) == '#'

adicionaNavioNaColuna :: Int -> Int -> [[Char]] -> Int -> IO [[Char]]
adicionaNavioNaColuna _ _ [] _ = return []
adicionaNavioNaColuna _ _ l 0 = return l
adicionaNavioNaColuna posI posJ (x:xs) n
                                    | x == show posI = do
                                          navio <- adicionaNavio ([x !! posJ])
                                          restante <- adicionaNavioNaColuna (posI + 1) posJ xs (n-1)
                                          return (navio : restante)
                                    | otherwise = do
                                          restante <- adicionaNavioNaColuna posI posJ xs n
                                          return (x : restante)

adicionaNavio :: [Char] ->  IO [Char]
adicionaNavio [x] = return ['#']

adicionaNavioNaLinha :: Int -> Int -> [[Char]] -> Int -> IO [[Char]]
adicionaNavioNaLinha posI posJ tabuleiro n = return tabuleiro

executaJogoComMaquina :: Jogadores -> IO Jogadores
executaJogoComMaquina dados = menu dados
