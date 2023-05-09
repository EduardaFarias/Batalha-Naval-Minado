import Control.Concurrent (threadDelay)
import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import System.Random
import Data.Function
import Data.Maybe (fromJust)


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

---------- INTRODUÇÃO E HISTORIA ----------

-- Função que apresenta uma introdução ao jogo e chama o menu
inicio_apresentacao :: [Jogador] -> IO()
inicio_apresentacao dados = do
                  imprimiIntroducao
                  putStrLn ("\n\nPressione <Enter> para continuar")
                  getChar
                  menu dados;
                  return()

-- função que irá imprimir a introdução do jogo
imprimiIntroducao :: IO ()
imprimiIntroducao = do
                      hSetBuffering stdout NoBuffering
                      system "clear" -- limpa a tela
                      handle <- openFile "./text/introducao.txt" ReadMode
                      imprimiTextoLentamente handle
                      hClose handle
                  
-- função que irá imprimir a historia do jogo
imprimiHistoria :: IO Jogadores
imprimiHistoria = do
                      hSetBuffering stdout NoBuffering
                      system "clear" -- limpa a tela
                      handle <- openFile "./text/historia.txt" ReadMode
                      imprimiTextoLentamente handle
                      hClose handle
                      return [] -- retorna uma lista vazia de jogadores

-- função para imprimir lentamente a introdução do jogo
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

---------- MENU ----------

-- função que exibe o Menu
menu :: Jogadores -> IO Jogadores
menu dados = do
                system "clear" -- limpa a tela
                putStrLn "-------------------- Batalha Naval --------------------"
                putStrLn "\n ● Digite 1 para cadastrar jogador"
                putStrLn "\n ● Digite 2 para jogar"
                putStrLn "\n ● Digite 3 para visualizar o ranking"
                putStrLn "\n ● Digite 4 para ver o modo história"
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
executarOpcao dados '3' = do
                            system "clear" -- limpa a tela
                            putStrLn "\nRanking dos jogadores:\n"
                            if (null dados) then do
                              putStrLn ("Não há jogadores cadastrados!")
                            else
                              -- a função ordenar crescentemente pela pontuação
                              exibirRanking (reverse (ordenar dados))
                            putStrLn "\nPressione <Enter> para voltar ao menu..."
                            getChar
                            menu dados
executarOpcao dados '4' = do
                            imprimiHistoria
                            putStrLn "\nPressione <Enter> para voltar ao menu..."
                            getLine -- Aguarda a entrada do usuário
                            menu dados
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

-- função que recebe uma string e retorna uma IO String
getString :: String -> IO String
getString str = do
                  putStr str
                  res <- getLine
                  return res

-- função que prepara o segundo menu do jogo
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

-- função para manipular a opção escolhida pelo usuário do segundo menu
executarOpcaoJogo :: Jogadores -> Char -> Int -> IO Jogadores
executarOpcaoJogo dados '0' tamTabuleiro = menu dados
executarOpcaoJogo dados '1' tamTabuleiro = doWhile True dados tamTabuleiro
executarOpcaoJogo dados '2' tamTabuleiro = doWhileJogoCom2 True dados tamTabuleiro
executarOpcaoJogo dados '3' tamTabuleiro = do
                  system "clear"
                  putStrLn "-------------------- Batalha Naval --------------------"
                  putStrLn "\n • Qual o novo tamanho do tabuleiro? (Digite somente um valor ex: 10)"
                  putStr "\n→ Opção: \n\n"
                  op <- readLn :: IO Int
                  prepararJogo dados op

-- executa um loop do jogo
doWhile :: Bool -> Jogadores -> Int -> IO Jogadores
doWhile condition dados tamTabuleiro
  | condition = do 
                system "clear"
                jogador1 <- chamaJogador dados "" "1"

                if(jogador1 == "JogadorNaoExiste") then doWhile True dados tamTabuleiro
                else do
                  system "clear"
                  -- Esses são os tabueiros que irão guardar os navios
                  tabuleiro_Jogador <- criaMatriz tamTabuleiro
                  tabuleiro_Bot <- criaMatrizBot tamTabuleiro

                  -- Esses são os tabuleiro que o usuário irá enxergar durante o jogo
                  tabuleiro_Jogador_Aux <- criaMatriz tamTabuleiro
                  tabuleiro_Bot_Aux <- criaMatriz tamTabuleiro

                  tabuleiro_Jogador <- montaTabuleiroJogador tabuleiro_Jogador (round (fromIntegral tamTabuleiro / 2)) tamTabuleiro jogador1
                  tabuleiro_Jogador <- jogaBombas tabuleiro_Jogador (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro
                  tabuleiro_Bot <- jogaBombas tabuleiro_Bot (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro
                  tabuleiro_Jogador <- jogaBombasBonus tabuleiro_Jogador (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro
                  tabuleiro_Bot <- jogaBombasBonus tabuleiro_Bot (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro
                  
                  dadosAtualizado <- iniciaJogoComMaquina tabuleiro_Jogador tabuleiro_Jogador_Aux tabuleiro_Bot tabuleiro_Bot_Aux tamTabuleiro jogador1 dados
                  
                  putStrLn "\nVocê quer jogar novamente? [1 para sim, outro número para sair]"
                  putStr "\n→ Opção: \n\n"
                  op <- getChar
                  getChar
                  if(op == '1') then doWhile True dadosAtualizado tamTabuleiro
                  else doWhile False dadosAtualizado tamTabuleiro
  | otherwise = menu dados

-- Define um nome para um jogador
chamaJogador :: Jogadores -> String -> String -> IO String
chamaJogador dados nomeJogador jogadorNum = do
  let mensagem = case jogadorNum of
                   "1" -> "Você deseja jogar com o primeiro jogador já cadastrado? (Digite S para sim e N para não)"
                   "2" -> "Você deseja jogar com o segundo jogador já cadastrado? (Digite S para sim e N para não)"
                   _   -> "Opção inválida"

  putStrLn mensagem
  putStr "\n→ Opção: \n\n"
  op <- getChar
  getChar


  if(op == 'S') then do
    putStrLn "\nDigite o nome do primeiro jogador: \n"
    jogador <- getLine
    if not(existeJogador dados jogador) then do
      putStrLn "\nEsse jogador não existe!"
      threadDelay 1000000
      return "JogadorNaoExiste"
    else if (jogador == nomeJogador) then do
      putStrLn "\nEsse jogador ja foi escolhido."
      threadDelay 1000000
      return "JogadorNaoExiste"
    else return jogador
  else if op == 'N' then do 
    if (nomeJogador == "Jogador 1") then return "Jogador 2"
    else return "Jogador 1"
  else do
    putStrLn "Opção Inválida"
    threadDelay 1000000
    return "JogadorNaoExiste"

-- função que verifica se um jogador existe (o nome do jogador é único)
existeJogador :: Jogadores -> String -> Bool
existeJogador [] _ = False
existeJogador ((Jogador nomeCadastrado pontuacao):xs) nome
                | (nomeCadastrado == nome) = True
                | otherwise = existeJogador xs nome

---------- TABULEIRO ----------

-- função para criar a matriz com um tamanho determinado pelo usuário ou com tamanho 10 caso o usuário não redefina o tamanho do tabuleiro
criaMatriz :: Int -> IO [[Char]]
criaMatriz n = return $ replicate n (intercalate " " (replicate (n) "~"))

-- função para criar a matriz do bot com um tamanho determinado pelo usuário ou com tamanho 10 caso o usuário não redefina o tamanho do tabuleiro
criaMatrizBot :: Int -> IO [[Char]]
criaMatrizBot n = do
                  tabuleiro_Bot <- return $ replicate n (intercalate " " (replicate (n) "~"))
                  adicionaNavioNoBot tabuleiro_Bot (round (fromIntegral n / 2)) n

-- função que vai permitir ao usuario posicionar seus navios
montaTabuleiroJogador :: [[Char]] -> Int -> Int -> String -> IO [[Char]]
montaTabuleiroJogador tabuleiro 1 _ _ = return tabuleiro
montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro nomeJogador = do
                      system "clear"
                      printaTabuleiro tabuleiro tamTabuleiro ("Esse é o seu tabuleiro " ++ nomeJogador ++ ":\n")

                      putStrLn ("\nO navio tem tamanho: "  ++ show tamNavio)
                      orientacao <- pegaOrientacaoTabuleiro
                      
                      putStrLn ("Insira as posicoes X (de 1 a " ++ show tamTabuleiro ++ ") Y (de 1 a " ++ show tamTabuleiro ++ ") para posicionar seu navio.")
                      valorX <- pegaValor 'X' tamTabuleiro
                      valorY <- pegaValor 'Y' tamTabuleiro

                      if (orientacao == 'H') then
                        if ((valorY + tamNavio - 1) < tamTabuleiro) then
                          if(verificaTemNavioNaLinha tabuleiro valorX valorY tamNavio) then do
                            novoTab <- adicionaNavio tabuleiro valorX valorY tamNavio
                            montaTabuleiroJogador novoTab (tamNavio-1) tamTabuleiro nomeJogador
                          else do
                            putStrLn "Já tem um navio nesta posicao, insira outro valor novamente."
                            montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro nomeJogador
                        else do
                          putStrLn "O tamanho do navio esta fora dos limites, escolha outra posicao"
                          montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro nomeJogador
                      else do
                        if ((valorX + tamNavio - 1) < tamTabuleiro) then
                          if(verificaTemNavioNaLinha (transpose tabuleiro) valorY valorX tamNavio) then do
                            novoTab <- fmap transpose (adicionaNavio (transpose tabuleiro) valorY valorX tamNavio)
                            montaTabuleiroJogador novoTab (tamNavio-1) tamTabuleiro nomeJogador
                          else do
                            putStrLn "Já tem um navio nesta posicao, insira outro valor novamente."
                            montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro nomeJogador
                        else do
                          putStrLn "O tamanho do navio esta fora dos limites, escolha outra posicao"
                          montaTabuleiroJogador tabuleiro tamNavio tamTabuleiro nomeJogador

-- função para imprimir o tabuleiro
printaTabuleiro :: [[Char]] -> Int -> String -> IO()
printaTabuleiro tabuleiro tam msg = do
      putStrLn msg 
      imprimeTabuleiro tabuleiro tam

-- função para pegar a orientação em que o usuário quer posicionar os navios
pegaOrientacaoTabuleiro :: IO Char
pegaOrientacaoTabuleiro = do
      putStrLn "\nEm que orientação você que posicioná-lo? (Digite H para horizontal e V para vertical) "
      ori <- getChar
      getChar

      if ((ori /= 'H') && (ori /= 'V')) then do
        putStrLn "\nO valor digitado é invalido"
        pegaOrientacaoTabuleiro
      else return ori

-- função para pegar as posições do tabuleiro que o usuário digitar
pegaValor :: Char -> Int -> IO Int
pegaValor char tamTabuleiro = do 
                  putStrLn (char : ": ")
                  valor <- readLn :: IO Int

                  if((valor-1) < 0 || (valor-1) >= tamTabuleiro) then do
                    putStrLn ("O valor de " ++ [char] ++ " é invalido, insira um valor entre 0 e 9.")
                    pegaValor char tamTabuleiro
                  else return (valor-1)

-- função que adiciona os navios para o bot
adicionaNavioNoBot :: [String] -> Int -> Int -> IO [String]
adicionaNavioNoBot tabuleiro 1 _ = return tabuleiro
adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro = do
                  posI <- randomRIO (0, fromIntegral (tamTabuleiro - 1))
                  posJ <- randomRIO (0, fromIntegral (tamTabuleiro - 1))
                  orientacao <- randomRIO (0, 1) :: IO Int

                  if (orientacao == 0) then
                    if ((posJ + tamNavio - 1) < tamTabuleiro) then
                      if(verificaTemNavioNaLinha tabuleiro posI posJ tamNavio) then do
                        tab <- adicionaNavio tabuleiro posI posJ tamNavio
                        adicionaNavioNoBot tab (tamNavio-1) tamTabuleiro
                      else do
                        adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro
                    else
                      adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro
                  else do
                    if ((posI + tamNavio - 1) < tamTabuleiro) then
                      if(verificaTemNavioNaLinha (transpose tabuleiro) posJ posI tamNavio) then do
                        tab <- fmap transpose (adicionaNavio (transpose tabuleiro) posJ posI tamNavio)
                        adicionaNavioNoBot tab (tamNavio-1) tamTabuleiro
                      else
                        adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro
                    else
                      adicionaNavioNoBot tabuleiro tamNavio tamTabuleiro

adicionaNavio :: [[Char]] -> Int -> Int -> Int -> IO [[Char]]
adicionaNavio tabuleiro posI posJ tamNavio = return (adicionaNavioNaLinha tabuleiro tamNavio posI posJ)


-- função para add o navio
adicionaNavioNaLinha :: [[Char]] -> Int -> Int -> Int -> [[Char]]
adicionaNavioNaLinha (h:t) tamNavio posI posJ
    | posI == 0 && t == [] = [concat [take posJ h, replicate tamNavio '#', drop (posJ + tamNavio) h]]
    | posI == 0 = ((take posJ h) ++ ['#' | _  <- [posJ..(posJ + tamNavio - 1)]] ++ (drop (posJ + tamNavio) h)) : t
    | null t = [h]
    | otherwise = h : adicionaNavioNaLinha t tamNavio (posI - 1) posJ

-- função que verifica se há navio na coordenada
verificaTemNavioNaLinha :: [[Char]] -> Int -> Int -> Int -> Bool
verificaTemNavioNaLinha tabuleiro posI posJ tamNavio =
    not (temNavio (take tamNavio (drop posJ (tabuleiro !! posI))))


temNavio :: [Char] -> Bool
temNavio tab = '#' `elem` tab

-- função para imprimir no terminal o tabuleiro
imprimeTabuleiro :: [[Char]] -> Int -> IO ()
imprimeTabuleiro tabuleiro tamTabuleiro = do
    putStr "     "
    if (tamTabuleiro < 10) then mapM_ (\i -> putStr $ show i ++ "   ") [1..tamTabuleiro]
    else do
      mapM_ (\i -> putStr $ show i ++ "   ") [1..9]
      if (tamTabuleiro > 9) then mapM_ (\i -> putStr $ show i ++ "  ") [10..tamTabuleiro]
      else putStr ""
    putStr "\n"
    imprimeLinhas tabuleiro 1 tamTabuleiro


imprimeLinhas :: [[Char]] -> Int -> Int -> IO ()
imprimeLinhas [] _ _ = return ()
imprimeLinhas (h:t) numLinha tamTabuleiro = do
    putStr (if numLinha < 10 then " " ++ show numLinha else show numLinha)
    putStr " "
    mapM_ (\x -> if x == 'X' then putStr "  X " else if x == '*' then putStr "  * " else if x == '#' then putStr "  # " else if x == 'o' then putStr "  o " else if x == '^' then putStr "  ^ " else putStr "  ~ ") (take tamTabuleiro h)
    putStr "\n"
    imprimeLinhas t (numLinha + 1) tamTabuleiro

---------- JOGO ----------

-- função responsavel por iniciar a partida com o bot
iniciaJogoComMaquina :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> Int -> String -> Jogadores -> IO Jogadores
iniciaJogoComMaquina tabJogador tabJogo tabBot tabBotJogo tamTabuleiro nomeJogador dados = do
          -- system "clear"
           
           printaTabuleiro tabJogo tamTabuleiro ("Tabuleiro de " ++ nomeJogador ++ ":\n")
           printaTabuleiro tabBotJogo tamTabuleiro "Tabuleiro do Bot:\n"

           let naviosJog = contaNavios tabJogador tamTabuleiro
           let naviosBot = contaNavios tabBot tamTabuleiro

           putStrLn ("Número de navios restantes do jogador: " ++ show naviosJog)
           putStrLn ("Número de navios restantes do bot: " ++ show naviosBot)

           if (naviosJog == 0) then do
              putStrLn "Que pena você perdeu! Seus navios foram para o fundo do mar!"
              return dados
           else if (naviosBot == 0) then do
              putStrLn "Você é um verdadeiro almirante! Parabéns pela vitória na batalha naval."
              atualizaPontuacao dados nomeJogador
              
           else do
              (tabBot_2, tabBotJogo_2, tabJogador2, tabJogo2) <- disparaNoTabuleiroBot tabBot tabBotJogo tabJogador tabJogo tamTabuleiro
              (tabJogador_2, tabJogo_2, tabBot_2, tabBotJogo_2) <- disparaNoTabuleiroJogador tabJogador2 tabJogo2 tabBot_2 tabBotJogo_2 tamTabuleiro

              iniciaJogoComMaquina tabJogador_2 tabJogo_2 tabBot_2 tabBotJogo_2 tamTabuleiro nomeJogador dados
  
-- função que atualiza a pontuação do vencedor
-- recebe a lista (Jogadores), o nome do vencedor e retorna uma nova lista atualizada
atualizaPontos :: Jogadores -> String -> Jogadores
atualizaPontos [] _ = []
atualizaPontos ((Jogador nome pontuacao):xs) vencedor
                | (nome == vencedor) = [(Jogador nome (pontuacao + 1))] ++ xs
                | otherwise = (Jogador nome pontuacao):(atualizaPontos xs vencedor)


atualizaPontuacao :: Jogadores -> String -> IO Jogadores
atualizaPontuacao dados nomeJogador = do 
              -- abre o arquivo para escrita para atualizá-lo
              arq_escrita <- openFile "./text/dados.txt" WriteMode
              hPutStrLn arq_escrita (show (atualizaPontos dados nomeJogador))
              hClose arq_escrita

              -- abre o arquivo para leitura
              arq_leitura <- openFile "./text/dados.txt" ReadMode
              dados_atualizados <- hGetLine arq_leitura
              hClose arq_leitura

              return (read dados_atualizados)


disparaNoTabuleiroBot :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> Int -> IO ([[Char]], [[Char]], [[Char]], [[Char]])
disparaNoTabuleiroBot tabBot tabBotJogo tabJogador tabJogadorJogo tamTabuleiro = do
    putStrLn "Escolha as posições de X (de 0 a 9) e as posições Y (de 0 a 9)"
    valorX <- pegaValor 'X' tamTabuleiro
    valorY <- pegaValor 'Y' tamTabuleiro

    if((tabBotJogo !! valorX !! valorY) `elem` ['X', '*', 'o', '^']) then do
      putStrLn "Voce já disparou nesta posição. Escolha uma outra posicao."
      disparaNoTabuleiroBot tabBot tabBotJogo tabJogador tabJogadorJogo tamTabuleiro
    else if((tabBot !! valorX !! valorY) == '#') then do
      putStrLn "Voce acertou um navio!"
      let simbolo = 'X'
      let tabBotFinal = disparaEmNavio tabBot valorX valorY simbolo
      let tabBotJogoFinal = disparaEmNavio tabBotJogo valorX valorY simbolo
      return (tabBotFinal, tabBotJogoFinal, tabJogador, tabJogadorJogo)
    else if((tabBot !! valorX !! valorY) == 'o') then do
      putStrLn "Voce acertou uma bomba!"
      let simbolo = 'X'
      let (posI, posJ) = procuraNaMatriz '#' tabJogador

      let tabJogadorFinal = disparaEmNavio tabJogador posI posJ simbolo
      let tabJogadorJogoFinal = disparaEmNavio tabJogadorJogo posI posJ simbolo

      let simbolo2 = 'o'
      let tabBotFinal = disparaEmNavio tabBot valorX valorY simbolo2
      let tabBotJogoFinal = disparaEmNavio tabBotJogo valorX valorY simbolo2
      return (tabBotFinal, tabBotJogoFinal, tabJogadorFinal, tabJogadorJogoFinal)
    
    else if((tabBot !! valorX !! valorY) == '^') then do
      putStrLn "Voce acertou uma bomba bonus!"
      let simbolo = 'X'

      let (posI, posJ) = procuraNaMatriz '#' tabBot
      let tabBot2 = disparaEmNavio tabBot posI posJ simbolo
      let tabBotJogo2 = disparaEmNavio tabBotJogo posI posJ simbolo

      let simbolo2 = '^'
      let tabBotFinal = disparaEmNavio tabBot2 valorX valorY simbolo2
      let tabBotJogoFinal = disparaEmNavio tabBotJogo2 valorX valorY simbolo2
      return (tabBotFinal, tabBotJogoFinal, tabJogador, tabJogadorJogo)
    else do
      putStrLn "Voce acertou na água!"
      let simbolo = '*'
      let tabBotJogoFinal = disparaEmNavio tabBotJogo valorX valorY simbolo
      return (tabBot, tabBotJogoFinal, tabJogador, tabJogadorJogo)

procuraNaMatriz :: Char -> [[Char]] -> (Int, Int)
procuraNaMatriz c mat = case findIndex (elem c) mat of
    Nothing -> (-1, -1)
    Just i -> case findIndex (== c) (mat !! i) of
        Nothing -> (-1, -1)
        Just j -> (i, j)

disparaNoTabuleiroJogador :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> Int -> IO ([[Char]], [[Char]], [[Char]], [[Char]])
disparaNoTabuleiroJogador tabJogador tabJogo tabBot tabBotJogo tamTabuleiro = do
    valorX <- randomRIO (0, fromIntegral (tamTabuleiro - 1))
    valorY <- randomRIO (0, fromIntegral (tamTabuleiro - 1))

    putStrLn ("O valor de x e y é : "++ show valorX ++ " " ++ show valorY)

    if(tabJogo !! valorX !! valorY `elem` ['X', '*', 'o', '^']) then do
      disparaNoTabuleiroJogador tabJogador tabJogo tabBot tabBotJogo tamTabuleiro
    else if(tabJogador !! valorX !! valorY == '#') then do
        let simbolo = 'X'
        let tabJogadorFinal = disparaEmNavio tabJogador valorX valorY simbolo
        let tabJogoFinal = disparaEmNavio tabJogo valorX valorY simbolo
        return (tabJogadorFinal, tabJogoFinal, tabBot, tabBotJogo)
    else if((tabJogador !! valorX !! valorY) == 'o') then do
      let simbolo = 'X'
      let (posI, posJ) = procuraNaMatriz '#' tabBot

      let tabBotFinal = disparaEmNavio tabBot posI posJ simbolo
      let tabBotJogoFinal = disparaEmNavio tabBotJogo posI posJ simbolo

      let simbolo2 = 'o'
      let tabJogadorFinal = disparaEmNavio tabJogador valorX valorY simbolo2
      let tabJogadorJogoFinal = disparaEmNavio tabJogo valorX valorY simbolo2
      return (tabJogadorFinal, tabJogadorJogoFinal, tabBotFinal, tabBotJogoFinal)
    
    else if((tabJogador !! valorX !! valorY) == '^') then do
      let simbolo = 'X'

      let (posI, posJ) = procuraNaMatriz '#' tabJogador
      let tabJogador2 = disparaEmNavio tabJogador posI posJ simbolo
      let tabJogo2 = disparaEmNavio tabJogo posI posJ simbolo

      let simbolo2 = '^'
      let tabJogadorFinal = disparaEmNavio tabJogo2 valorX valorY simbolo2
      let tabJogoFinal = disparaEmNavio tabJogador2 valorX valorY simbolo2
      return (tabJogadorFinal, tabJogoFinal, tabBot, tabBotJogo)
    
    else do
      let simbolo = '*'
      let tabJogoFinal = disparaEmNavio tabJogo valorX valorY simbolo
      return (tabJogador, tabJogoFinal, tabBot, tabBotJogo)

-- função para ataques em navios
disparaEmNavio :: [[Char]] -> Int -> Int -> Char -> [[Char]]
disparaEmNavio (h:t) valorX valorY simbolo
    | valorX == 0 && t == [] = [concat [take valorY h, [simbolo], drop (valorY + 1) h]]
    | valorX == 0 = (take valorY h ++ [simbolo] ++ drop (valorY + 1) h) : t
    | null t = [h]
    | otherwise = h : disparaEmNavio t (valorX - 1) valorY simbolo

-- função para contagem de navios no tabuleiro
contaNavios :: [[Char]] -> Int -> Int
contaNavios tabuleiro tamTabuleiro =
    let navios = [verificaTemNavioNaLinha tabuleiro x y 1 | x <- [0..(tamTabuleiro-1)], y <- [0..(tamTabuleiro-1)]]
    in length (filter not navios)


-- exibir ranking dos jogadores
exibirRanking :: Jogadores -> IO ()
exibirRanking [] = return ()
exibirRanking (x:xs) = do
                            putStrLn ((obterNome x) ++ " possui " ++ (show (obterPontuacao x)) ++ " pontos")
                            exibirRanking xs
  
-- função que recebe um jogador e retorna o nome
obterNome :: Jogador -> Nome
obterNome (Jogador nome _) = nome

-- função que recebe um jogador e retorna a pontuação
obterPontuacao :: Jogador -> Pontuacao
obterPontuacao (Jogador _ pontuacao) = pontuacao

-- função que define o critério de ordenação
ordenar :: Jogadores -> Jogadores
ordenar dados = sortBy (compare `on` obterPontuacao) dados

-- executa um loop do jogo
doWhileJogoCom2 :: Bool -> Jogadores -> Int -> IO Jogadores
doWhileJogoCom2 condition dados tamTabuleiro
  | condition = do 
                system "clear"
                jogador1 <- chamaJogador dados "" "1"
                jogador2 <- chamaJogador dados jogador1 "2"

                if(jogador1 == "JogadorNaoExiste" || jogador2 == "JogadorNaoExiste") then doWhileJogoCom2 True dados tamTabuleiro
                else do
                  system "clear"
                  -- Esses são os tabueiros que irão guardar os navios
                  tabuleiro_Jogador1 <- criaMatriz tamTabuleiro
                  tabuleiro_Jogador2 <- criaMatriz tamTabuleiro

                  -- Esses são os tabuleiro que o usuário irá enxergar durante o jogo
                  tabuleiro_Jogador1_Aux <- criaMatriz tamTabuleiro
                  tabuleiro_Jogador2_Aux <- criaMatriz tamTabuleiro

                  tabuleiro_Jogador1 <- montaTabuleiroJogador tabuleiro_Jogador1 (round (fromIntegral tamTabuleiro / 2)) tamTabuleiro jogador1
                  tabuleiro_Jogador2 <- montaTabuleiroJogador tabuleiro_Jogador2 (round (fromIntegral tamTabuleiro / 2)) tamTabuleiro jogador2

                  -- joga bombas nos tabuleiros
                  tabuleiro_Jogador1 <- jogaBombas tabuleiro_Jogador1 (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro
                  tabuleiro_Jogador2 <- jogaBombas tabuleiro_Jogador2 (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro
                  tabuleiro_Jogador1 <- jogaBombasBonus tabuleiro_Jogador1 (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro
                  tabuleiro_Jogador2 <- jogaBombasBonus tabuleiro_Jogador2 (round (fromIntegral tamTabuleiro / 5)) tamTabuleiro

                  dadosAtualizado <- iniciaJogoComJogadores tabuleiro_Jogador1 tabuleiro_Jogador1_Aux tabuleiro_Jogador2 tabuleiro_Jogador2_Aux tamTabuleiro jogador1 jogador2 dados

                  putStrLn "\nVocê quer jogar novamente? [1 para sim, outro número para sair]"
                  putStr "\n→ Opção: \n\n"
                  op <- getChar
                  getChar
                  if(op == '1') then doWhileJogoCom2 True dadosAtualizado tamTabuleiro
                  else doWhileJogoCom2 False dadosAtualizado tamTabuleiro
  | otherwise = menu dados

-- função para iniciar partida com outro jogador
iniciaJogoComJogadores :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> Int -> String -> String-> Jogadores -> IO Jogadores
iniciaJogoComJogadores tabJogador1 tabJogo1 tabJogador2 tabJogo2 tamTabuleiro nomeJogador1 nomeJogador2 dados = do
           system "clear"
           printaTabuleiro tabJogo1 tamTabuleiro ("Tabuleiro de " ++ nomeJogador1 ++ ":\n")
           printaTabuleiro tabJogo2 tamTabuleiro ("Tabuleiro de " ++ nomeJogador2 ++ ":\n")

           let naviosJog1 = contaNavios tabJogador1 tamTabuleiro
           let naviosJog2 = contaNavios tabJogador2 tamTabuleiro

           putStrLn ("Número de navios restantes do " ++ nomeJogador1 ++ " :" ++ show naviosJog1)
           putStrLn ("Número de navios restantes do " ++ nomeJogador2 ++ " :" ++ show naviosJog2)

           if (naviosJog1 == 0) then do
              putStrLn ("Parabéns! " ++ nomeJogador2 ++ " é o vencedor.")
              atualizaPontuacao dados nomeJogador2
           else if (naviosJog2 == 0) then do
              putStrLn ("Parabéns! " ++ nomeJogador1 ++ " é o vencedor.")
              atualizaPontuacao dados nomeJogador1
              
           else do
             putStrLn ("É a vez de " ++ nomeJogador1 ++ ":\n")
             (tabJogador2, tabJogo2, tabJogador1, tabJogo1) <- disparaNoTabuleiroBot tabJogador2 tabJogo2 tabJogador1 tabJogo1 tamTabuleiro
             system "clear"
             printaTabuleiro tabJogo1 tamTabuleiro ("Tabuleiro de " ++ nomeJogador1 ++ ":\n")
             printaTabuleiro tabJogo2 tamTabuleiro ("Tabuleiro de " ++ nomeJogador2 ++ ":\n")
              
             putStrLn ("É a vez de " ++ nomeJogador2 ++ ":\n")
             (tabJogador1, tabJogo1, tabJogador2, tabJogo2) <- disparaNoTabuleiroBot tabJogador1 tabJogo1 tabJogador2 tabJogo2 tamTabuleiro
             system "clear"
             printaTabuleiro tabJogo1 tamTabuleiro ("Tabuleiro de " ++ nomeJogador1 ++ ":\n")
             printaTabuleiro tabJogo2 tamTabuleiro ("Tabuleiro de " ++ nomeJogador2 ++ ":\n")

             iniciaJogoComJogadores tabJogador1 tabJogo1 tabJogador2 tabJogo2 tamTabuleiro nomeJogador1 nomeJogador2 dados


-- função que gera bombas aleatoriamente no tabuleiro
jogaBombas :: [[Char]] -> Int -> Int -> IO [[Char]]
jogaBombas tab 0 tamTabuleiro = return tab
jogaBombas tab qtdBombas tamTabuleiro = do
  putStrLn "Espalhando bombas ..."
  posI <- randomRIO (0, fromIntegral (tamTabuleiro - 1))
  posJ <- randomRIO (0, fromIntegral (tamTabuleiro - 1))
      
  putStrLn (show posI ++ " " ++ show posJ)

  if (verificaPosicaoValida tab posI posJ) then do
    tab_Final <- return (adicionaBomba tab posI posJ 'o')
    jogaBombas tab_Final (qtdBombas-1) tamTabuleiro
  else do 
    jogaBombas tab qtdBombas tamTabuleiro

adicionaBomba :: [[Char]] -> Int -> Int -> Char -> [[Char]]
adicionaBomba (h:t) valorX valorY simbolo
    | valorX == 0 && t == [] = [concat [take valorY h, [simbolo], drop (valorY + 1) h]]
    | valorX == 0 = (take valorY h ++ [simbolo] ++ drop (valorY + 1) h) : t
    | null t = [h]
    | otherwise = h : adicionaBomba t (valorX - 1) valorY simbolo

verificaTemElemento :: [[Char]] -> Int -> Int -> Bool
verificaTemElemento tabuleiro posI posJ = ((tabuleiro !! posI !! posJ) `elem` ['X', '*', 'o', '#', '^'])

verificaPosicaoValida :: [[Char]] -> Int -> Int -> Bool
verificaPosicaoValida tab posI posJ = 
 (posI < length tab &&
 posJ < length tab &&
 not (verificaTemElemento tab posI posJ) &&
 not (temBombaAdjacente tab posI posJ))


temBombaAdjacente :: [[Char]] -> Int -> Int -> Bool
temBombaAdjacente tab posI posJ =
  (((posI-1) >= 0 && verificaTemElemento tab (posI-1) posJ)) || -- verifica a posição de cima
  (((posJ-1) >= 0 && verificaTemElemento tab posI (posJ-1))) || -- verifica a posição da esquerda
  ((posI+1) < length tab && verificaTemElemento tab (posI+1) posJ) || -- verifica a posição de baixo
  (((posJ+1) < length (head tab) && verificaTemElemento tab posI (posJ+1)))


jogaBombasBonus :: [[Char]] -> Int -> Int -> IO [[Char]]
jogaBombasBonus tab 0 tamTabuleiro = return tab
jogaBombasBonus tab qtdBombas tamTabuleiro = do
  putStrLn "Espalhando bombas bônus..."
  posI <- randomRIO (0, fromIntegral (tamTabuleiro - 1))
  posJ <- randomRIO (0, fromIntegral (tamTabuleiro - 1))
      
  putStrLn (show posI ++ " " ++ show posJ)

  if (verificaPosicaoValida tab posI posJ) then do
    tab_Final <- return (adicionaBomba tab posI posJ '^')
    jogaBombasBonus tab_Final (qtdBombas-1) tamTabuleiro
  else do 
    jogaBombasBonus tab qtdBombas tamTabuleiro


