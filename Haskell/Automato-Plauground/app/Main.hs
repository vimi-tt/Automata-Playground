module Main where

import Automato
  ( Automato,
    criarAutomato,
    imprimirAutomato,
    testePalavra,
    salvarResultados,
    ResultadoTeste (..) -- Importa o construtor e os campos de ResultadoTeste
  )
import Control.Monad (forM, forM_) -- Importa forM e forM_
import System.IO (hFlush, stdout)

-- Caminho padrão para o arquivo JSON do autômato
defaultJsonPath :: FilePath
defaultJsonPath = "automato.json"

-- Caminho padrão para o arquivo de palavras (.txt)
defaultPalavrasPath :: FilePath
defaultPalavrasPath = "palavras.txt"

-- Função principal
main :: IO ()
main = do
  putStrLn "=== Simulador de Autômatos ==="
  putStrLn "Bem-vindo ao simulador de autômatos!"
  putStrLn "Carregando arquivo JSON padrão..."
  mainLoop Nothing

-- Loop principal do menu
mainLoop :: Maybe Automato -> IO ()
mainLoop mAutomato = do
  putStrLn "\n=== Menu ==="
  putStrLn "1. Carregar autômato (arquivo padrão)"
  putStrLn "2. Visualizar informações do autômato"
  putStrLn "3. Testar palavra(s)"
  putStrLn "4. Salvar resultados dos testes em JSON"
  putStrLn "5. Sair"
  putStr "Escolha uma opção: "
  hFlush stdout -- Força a exibição imediata do prompt
  opcao <- getLine
  case opcao of
    "1" -> carregarJSON >>= mainLoop
    "2" -> visualizarInfo mAutomato >>= mainLoop
    "3" -> testarPalavras mAutomato >>= mainLoop
    "4" -> salvarTestes mAutomato >>= mainLoop
    "5" -> putStrLn "Saindo..."
    _   -> do
      putStrLn "Opção inválida. Tente novamente."
      mainLoop mAutomato

-- Carrega o arquivo JSON padrão e retorna o autômato
carregarJSON :: IO (Maybe Automato)
carregarJSON = do
  putStrLn $ "Carregando autômato do arquivo: " ++ defaultJsonPath
  automato <- criarAutomato defaultJsonPath
  putStrLn "Autômato carregado com sucesso!"
  return (Just automato)

-- Visualiza informações do autômato
visualizarInfo :: Maybe Automato -> IO (Maybe Automato)
visualizarInfo Nothing = do
  putStrLn "Nenhum autômato carregado. Por favor, carregue o autômato primeiro."
  return Nothing
visualizarInfo (Just automato) = do
  putStrLn $ imprimirAutomato automato
  return (Just automato)

-- Testa palavras individualmente ou em lote
testarPalavras :: Maybe Automato -> IO (Maybe Automato)
testarPalavras Nothing = do
  putStrLn "Nenhum autômato carregado. Por favor, carregue o autômato primeiro."
  return Nothing
testarPalavras (Just automato) = do
  putStrLn "Deseja testar uma palavra individual ou um arquivo de palavras?"
  putStrLn "1. Palavra individual"
  putStrLn "2. Arquivo de palavras (.txt fixo)"
  putStr "Escolha uma opção: "
  hFlush stdout
  opcao <- getLine
  case opcao of
    "1" -> do
      putStr "Digite a palavra a ser testada: "
      hFlush stdout
      palavraTestada <- getLine
      resultado <- testePalavra automato palavraTestada
      putStrLn $ formatarResultado resultado
      return (Just automato)
    "2" -> do
      putStrLn $ "Carregando palavras do arquivo .txt: " ++ defaultPalavrasPath
      conteudo <- readFile defaultPalavrasPath
      let palavras = lines conteudo -- Divide o conteúdo em uma lista de palavras
      resultados <- forM palavras (\palavraAtual -> testePalavra automato palavraAtual)
      forM_ resultados (\resultado -> putStrLn $ formatarResultado resultado)
      return (Just automato)
    _ -> do
      putStrLn "Opção inválida."
      return (Just automato)

-- Formata o resultado de um teste para exibição
formatarResultado :: ResultadoTeste -> String
formatarResultado resultado =
  "Palavra: " ++ palavra resultado
    ++ "\nAceita: "
    ++ show (aceita resultado)
    ++ "\nEstados Percorridos: "
    ++ show (estadosPercorridos resultado)
    ++ "\n"

-- Salva os resultados dos testes em um arquivo JSON
salvarTestes :: Maybe Automato -> IO (Maybe Automato)
salvarTestes Nothing = do
  putStrLn "Nenhum autômato carregado. Por favor, carregue o autômato primeiro."
  return Nothing
salvarTestes (Just automato) = do
  putStrLn "Digite as palavras a serem testadas (separadas por vírgula): "
  hFlush stdout
  entrada <- getLine
  let palavras = words entrada -- Divide as palavras por espaço
  resultados <- forM palavras (\palavraAtual -> testePalavra automato palavraAtual)
  putStr "Digite o nome do arquivo para salvar os resultados: "
  hFlush stdout
  arquivo <- getLine
  salvarResultados arquivo resultados
  return (Just automato)