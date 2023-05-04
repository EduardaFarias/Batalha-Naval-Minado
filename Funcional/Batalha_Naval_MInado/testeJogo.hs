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
executarOpcao dados '2' = prepararJogo dados 10
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

prepararJogo :: Jogadores -> Int -> IO Jogadores
prepararJogo dados tamTabuleiro = do
                      system "clear"
                      putStrLn "-------------------- Batalha Naval --------------------"
                      putStrLn "\n • Digite 1 para jogar com a máquina"
                      putStrLn "\n • Digite 2 para jogar com dois jogadores"
                      putStrLn "\n • Digite 3 para redimensionar o tabuleiro"
                      putStrLn "\n • Digite 0 para voltar ao menu"
                      putStr "\n→ Opção: \n\n"
                      op <- getChar
                      getChar
                      executarOpcaoJogo dados op tamTabuleiro

executarOpcaoJogo :: Jogadores -> Char -> Int -> IO Jogadores
executarOpcaoJogo dados '0' tamTabuleiro = menu dados
executarOpcaoJogo dados '1' tamTabuleiro = doWhile True executaJogoComMaquina dados tamTabuleiro

executarOpcaoJogo dados '2' tamTabuleiro = menu dados
executarOpcaoJogo dados '3' tamTabuleiro = do
                  system "clear"
                  putStrLn "-------------------- Batalha Naval --------------------"
                  putStrLn "\n • Qual o novo tamanho do tabuleiro? (Digite somente um valor ex: 10)"
                  putStr "\n→ Opção: \n\n"
                  op <- readLn :: IO Int
                  prepararJogo dados op

-- executa um loop do jogo
doWhile :: Bool -> (function -> IO Jogadores) -> Jogadores -> Int -> IO Jogadores
doWhile condition function dados tamTabuleiro
  | condition = do 
                system "clear"
                -- Esses são os tabueiros que irão guardar os navios
                tabuleiro_Jogador <- criaMatriz tamTabuleiro
                tabuleiro_Bot <- criaMatrizBot tamTabuleiro

                -- Esses são os tabuleiro que o usuário irá enxergar durante o jogo
                tabuleiro_Jogador_Aux <- criaMatriz tamTabuleiro
                tabuleiro_Bot_Aux <- criaMatriz tamTabuleiro

                tabuleiro_Jogador <- montaTabuleiroJogador tabuleiro_Jogador (round (fromIntegral tamTabuleiro / 2)) tamTabuleiro


                iniciaJogo tabuleiro_Jogador tabuleiro_Jogador_Aux tabuleiro_Bot tabuleiro_Bot_Aux tamTabuleiro

                putStrLn "\nVocê quer jogar novamente? [1 para sim, outro número para sair]"
                putStr "\n→ Opção: \n\n"
                op <- getChar
                getChar
                if(op == '1') then doWhile True executaJogoComMaquina dados tamTabuleiro
                else doWhile False executaJogoComMaquina dados tamTabuleiro
  | otherwise = menu dados


-- função para criar a matriz com um tamanho determinado pelo usuário ou com tamanho 10 caso o usuário não redefina o tamanho do tabuleiro
criaMatriz :: Int -> IO [[Char]]
criaMatriz n = return $ replicate n (intercalate " " (replicate (n) "~"))

criaMatrizBot :: Int -> IO [[Char]]
criaMatrizBot n = do
                  tabuleiro_Bot <- return $ replicate n (intercalate " " (replicate (n) "~"))
                  adicionaNavioNoBot tabuleiro_Bot (round (fromIntegral n / 2)) n

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
    | posI == 0 && t == [] = [concat [take posJ h, replicate tamNavio '#', drop (posJ + tamNavio) h]]
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

montaTabuleiroJogador :: [[Char]] -> Int -> Int -> IO [[Char]]
montaTabuleiroJogador tabuleiro 1 _ = return tabuleiro
montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro = do
                      --system "clear"
                      printaTabuleiro tabuleiro  tamTabuleiro "Esse é o seu tabuleiro:"

                      putStrLn ("\nO navio tem tamanho: " ++ show tamNavio)
                      orientacao <- pegaOrientacaoTabuleiro
                      
                      putStrLn "Insira as posicoes X (de 1 a 9) Y (de 1 a 9) para posicionar seu navio."
                      valorX <- pegaValor 'X' tamTabuleiro
                      valorY <- pegaValor 'Y' tamTabuleiro

                      if (orientacao == 'H') then
                        if ((valorY + tamNavio - 1) < tamTabuleiro) then
                          if(verificaTemNavioNaLinha tabuleiro valorX valorY tamNavio) then do
                            novoTab <- adicionaNavio tabuleiro valorX valorY tamNavio
                            montaTabuleiroJogador novoTab (tamNavio-1) tamTabuleiro
                          else do
                            putStrLn "Já tem um navio nesta posicao, insira outro valor novamente."
                            montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro
                        else do
                          putStrLn "O tamanho do navio esta fora dos limites, escolha outra posicao"
                          montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro
                      else do
                        if ((valorX + tamNavio - 1) < tamTabuleiro) then
                          if(verificaTemNavioNaLinha (transpose tabuleiro) valorX valorY tamNavio) then do
                            novoTab <- fmap transpose (adicionaNavio (transpose tabuleiro) valorY valorX tamNavio)
                            montaTabuleiroJogador novoTab (tamNavio-1) tamTabuleiro
                          else do
                            putStrLn "Já tem um navio nesta posicao, insira outro valor novamente."
                            montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro
                        else do
                          putStrLn "O tamanho do navio esta fora dos limites, escolha outra posicao"
                          montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro


imprimeTabuleiro :: [[Char]] -> Int -> IO ()
imprimeTabuleiro tabuleiro tamTabuleiro = do
    putStr "     "
    mapM_ (\i -> putStr $ show i ++ "   ") [1..9]
    if(tamTabuleiro > 9) then mapM_ (\i -> putStr $ show i ++ "  ") [10..tamTabuleiro]
    else putStr ""
    putStr "\n"
    imprimeLinhas tabuleiro 1 tamTabuleiro

imprimeLinhas :: [[Char]] -> Int -> Int -> IO ()
imprimeLinhas [] _ _ = return ()
imprimeLinhas (h:t) numLinha tamTabuleiro = do
    putStr (if numLinha < 10 then " " ++ show numLinha else show numLinha)
    putStr " "
    mapM_ (\x -> if x == 'X' then putStr "  X " else if x == '*' then putStr "  * " else if x == '#' then putStr "  # " else putStr "  ~ ") (take tamTabuleiro h)
    putStr "\n"
    imprimeLinhas t (numLinha + 1) tamTabuleiro



printaTabuleiro :: [[Char]] -> Int -> String -> IO()
printaTabuleiro tabuleiro tam msg = do
      putStrLn msg 
      imprimeTabuleiro tabuleiro tam

pegaOrientacaoTabuleiro :: IO Char
pegaOrientacaoTabuleiro = do
      putStrLn "Em que orientação você que posicioná-lo? (Digite H para horizontal e V para vertical) "
      ori <- getChar
      getChar

      if ((ori /= 'H') && (ori /= 'V')) then do
        putStrLn "O valor digitado é invalido"
        pegaOrientacaoTabuleiro
      else return ori
                      
pegaValor :: Char -> Int -> IO Int
pegaValor char tamTabuleiro = do 
                  putStrLn (char : ": ")
                  valor <- readLn :: IO Int

                  if((valor-1) < 0 || (valor-1) > tamTabuleiro) then do
                    putStrLn ("O valor de " ++ [char] ++ " é invalido, insira um valor entre 0 e 9.")
                    pegaValor char tamTabuleiro
                  else return (valor-1)

iniciaJogo :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> Int -> IO ()
iniciaJogo tabJogador tabJogo tabBot tabBotJogo tamTabuleiro= do
           system "clear"
           printaTabuleiro tabJogo tamTabuleiro "Tabuleiro do Jogador:"
           printaTabuleiro tabBotJogo tamTabuleiro "Tabuleiro do Bot:"

           let naviosJog = contaNavios tabJogador
           let naviosBot = contaNavios tabBot

           putStrLn ("Número de navios restantes do jogador: " ++ show naviosJog)
           putStrLn ("Número de navios restantes do bot: " ++ show naviosBot)

           if (naviosJog == 0) then
              putStrLn "Que pena você perdeu! Seus navios foram para o fundo do mar!"
           else if (naviosBot == 0) then
              putStrLn "Você é um verdadeiro almirante! Parabéns pela vitória na batalha naval."
           else do
              (tabBot_2, tabBotJogo_2) <- disparaNoTabuleiroBot tabBot tabBotJogo tamTabuleiro
              (tabJogador_2, tabJogo_2) <- disparaNoTabuleiroJogador tabJogador tabJogo tamTabuleiro

              iniciaJogo tabJogador_2 tabJogo_2 tabBot_2 tabBotJogo_2 tamTabuleiro
  

disparaNoTabuleiroBot :: [[Char]] -> [[Char]] -> Int -> IO ([[Char]], [[Char]])
disparaNoTabuleiroBot tabBot tabBotJogo tamTabuleiro = do
    putStrLn "Escolha as posições de X (de 0 a 9) e as posições Y (de 0 a 9)"
    valorX <- pegaValor 'X' tamTabuleiro
    valorY <- pegaValor 'Y' tamTabuleiro

    if((tabBotJogo !! valorX !! valorY) `elem` ['X', '*']) then do
      putStrLn "Voce já disparou nesta posição. Escolha uma outra posicao."
      disparaNoTabuleiroBot tabBot tabBotJogo tamTabuleiro
    else if((tabBot !! valorX !! valorY) == '#') then do
      putStrLn "Voce acertou um navio!"
      let simbolo = 'X'
      let tabBotFinal = disparaEmNavio tabBot valorX valorY simbolo
      let tabBotJogoFinal = disparaEmNavio tabBotJogo valorX valorY simbolo
      return (tabBotFinal, tabBotJogoFinal)
   
    else do
      putStrLn "Voce acertou na água!"
      let simbolo = '*'
      let tabBotJogoFinal = disparaEmNavio tabBotJogo valorX valorY simbolo
      return (tabBot, tabBotJogoFinal)


-- MONTAR ESSA FUNÇÃO
disparaNoTabuleiroJogador :: [[Char]] -> [[Char]] -> Int -> IO ([[Char]], [[Char]])
disparaNoTabuleiroJogador tabBot tabBotJogo tamTabuleiro = return (tabBot, tabBotJogo)


-- AJEITAR ESSA FUNÇÃO
disparaEmNavio :: [[Char]] -> Int -> Int -> Char -> [[Char]]
disparaEmNavio (h:t) valorX valorY simbolo
  --  | valorX == 0 && t == [] = [take valorY h] ++ [[simbolo]] ++ [drop (valorY + 1) h]
    | valorX == 0 && t == [] = [concat [take valorY h, [simbolo], drop (valorY + 1) h]]
    | valorX == 0 = (take valorY h ++ [simbolo] ++ drop (valorY + 1) h) : disparaEmNavio t (valorX - 1) valorY simbolo
    | null t = [h]
    | otherwise = h : disparaEmNavio t (valorX - 1) valorY simbolo

contaNavios :: [[Char]] -> Int
contaNavios tabuleiro =
    let navios = [verificaTemNavioNaLinha tabuleiro x y 1 | x <- [0..9], y <- [0..9]]
    in length (filter not navios)



