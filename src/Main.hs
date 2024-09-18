module Main
  ( main
  , mainLoop
  ) where

import qualified Types
import qualified API
import qualified Grid
import Control.Monad.State (runStateT)
import Data.Maybe (fromMaybe)
import Parser ()
import Util (clearScreen)

-- Função principal que baixa as disciplinas e inicia o loop de interação
main :: IO ()
main = do
  let url = "https://matricula.ufabc.edu.br/cache/todasDisciplinas.js" -- URL da API que contém os dados das disciplinas
  result <- API.downloadAndParseDisciplinas url -- Baixa e faz o parsing das disciplinas da URL
  case result of
    Right disciplinas -> mainLoop disciplinas Nothing -- Se o download for bem-sucedido, inicia o loop principal com a lista de disciplinas
    Left err -> do
      clearScreen
      putStrLn err

-- Função que implementa o loop principal de interação com o usuário
mainLoop :: [Types.Disciplina] -> Maybe Grid.GridState -> IO ()
mainLoop disciplinas maybeGridState = do
  clearScreen -- Limpa a tela antes de exibir o menu
  putStrLn "HaskGradeMaker - O Montador de Grades Favorito da UFABC!"
  putStrLn "\n1. Criar nova grade acadêmica"
  putStrLn "2. Visualizar grade acadêmica"
  putStrLn "3. Limpar grade acadêmica"
  putStrLn "0. Sair"
  choice <- getLine -- Lê a escolha do usuário
  case choice of
  
    "1" -> do
      -- Se o usuário escolhe "1", inicia a criação da grade acadêmica
      (_, newGridState) <- runStateT (Grid.buildAcademicGrid disciplinas) (fromMaybe Grid.initialState maybeGridState)
      clearScreen
      mainLoop disciplinas (Just newGridState) -- Reinicia o loop com o novo estado da grade
      
    "2" -> do
      -- Se o usuário escolhe "2", exibe a grade acadêmica criada, se houver
      case maybeGridState of
        Just gridState -> do -- Se existe uma grade acadêmica
          clearScreen
          putStrLn $ Grid.viewAcademicGrid gridState -- Exibe a grade acadêmica formatada
          putStrLn "Pressione qualquer tecla para continuar..."
        Nothing -> do
          -- Caso ainda não tenha uma grade criada
          clearScreen
          putStrLn "Nenhuma grade acadêmica criada ainda."
      _ <- getLine -- Pausa para o usuário continuar
      mainLoop disciplinas maybeGridState -- Reinicia o loop sem alterar o estado
      
    "3" -> do -- Se o usuário escolhe "3", limpa a grade acadêmica
      clearScreen
      putStrLn "Grade acadêmica limpa."
      putStrLn "Pressione qualquer tecla para continuar..."
      _ <- getLine 
      mainLoop disciplinas (Just Grid.initialState) -- Reinicia o loop com o estado inicial da grade (limpo)
      
    "0" -> do
      -- Se o usuário escolhe "0", encerra o programa
      clearScreen
      putStrLn "Programa encerrado."
      
    _   -> do
      -- Se o usuário insere uma opção inválida
      clearScreen
      putStrLn "Opção inválida. Tente novamente."
      _ <- getLine 
      mainLoop disciplinas maybeGridState -- Reinicia o loop sem alterar o estado
