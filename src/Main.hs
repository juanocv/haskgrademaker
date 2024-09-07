module Main where

import qualified Types
import qualified Parser
import qualified API
import qualified Util
import qualified Grid
import Control.Monad.State (runStateT)
import Data.Maybe (fromMaybe)
import System.Process (system)
import System.Info (os)

main :: IO ()
main = do
  let url = "https://matricula.ufabc.edu.br/cache/todasDisciplinas.js"
  result <- API.downloadAndParseDisciplinas url
  case result of
    Right disciplinas -> mainLoop disciplinas Nothing
    Left err -> do
      clearScreen
      putStrLn err

mainLoop :: [Types.Disciplina] -> Maybe Grid.GridState -> IO ()
mainLoop disciplinas maybeGridState = do
  clearScreen
  putStrLn "HaskGradeMaker - O Montador de Grades Favorito da UFABC!"
  putStrLn "\n1. Criar nova grade acadêmica"
  putStrLn "2. Visualizar grade acadêmica"
  putStrLn "3. Limpar grade acadêmica"
  putStrLn "0. Sair"
  choice <- getLine
  case choice of
    "1" -> do
      (_, newGridState) <- runStateT (Grid.buildAcademicGrid disciplinas clearScreen) (fromMaybe Grid.initialState maybeGridState)
      clearScreen
      mainLoop disciplinas (Just newGridState)
    "2" -> do
      case maybeGridState of
        Just gridState -> do
          clearScreen
          Grid.viewAcademicGrid gridState
          putStrLn "Pressione qualquer tecla para continuar..."
        Nothing -> do
          clearScreen
          putStrLn "Nenhuma grade acadêmica criada ainda."
      _ <- getLine 
      mainLoop disciplinas maybeGridState
    "3" -> do
      clearScreen
      putStrLn "Grade acadêmica limpa."
      putStrLn "Pressione qualquer tecla para continuar..."
      _ <- getLine 
      mainLoop disciplinas (Just Grid.initialState)
    "0" -> do
      clearScreen
      putStrLn "Programa encerrado."
    _   -> do
      clearScreen
      putStrLn "Opção inválida. Tente novamente."
      _ <- getLine 
      mainLoop disciplinas maybeGridState

clearScreen :: IO ()
clearScreen = do
  _ <- system clearCommand
  return ()
  where
    clearCommand
      | os == "mingw32" = "cls"
      | otherwise       = "clear"
