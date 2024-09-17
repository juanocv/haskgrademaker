{-# LANGUAGE OverloadedStrings #-}
module Grid
  ( buildAcademicGrid
  , GridState(..)
  , GridBuilder
  , initialState
  , viewAcademicGrid
  ) where

import qualified Util
import Control.Monad.State
import Control.Monad (forM_, void)
import Types
import Data.Char (toLower)

data GridState = GridState
  { selectedDisciplinas :: [Disciplina]
  }

type GridBuilder a = StateT GridState IO a

initialState :: GridState
initialState = GridState { selectedDisciplinas = [] }

buildAcademicGrid :: [Disciplina] -> GridBuilder [Disciplina]
buildAcademicGrid allDisciplinas = do
  gridBuilderLoop allDisciplinas
  gets selectedDisciplinas

gridBuilderLoop :: [Disciplina] -> GridBuilder ()
gridBuilderLoop allDisciplinas = do
  liftIO Util.clearScreen
  liftIO $ putStrLn "1. Adicionar disciplina\n0. Finalizar grade"
  choice <- liftIO getLine
  case choice of
    "1" -> do
      addDisciplina allDisciplinas
      gridBuilderLoop allDisciplinas
    "0" -> return ()
    _   -> do
      liftIO Util.clearScreen
      liftIO $ putStrLn "Opção iválida. Tente novamente."
      _ <- liftIO getLine
      gridBuilderLoop allDisciplinas

addDisciplina :: [Disciplina] -> GridBuilder ()
addDisciplina allDisciplinas = do
  liftIO Util.clearScreen
  liftIO $ putStrLn "Digite o nome da disciplina:"
  query <- liftIO getLine
  case Util.searchDisciplina query allDisciplinas of
    Left err -> do
      liftIO Util.clearScreen
      liftIO $ putStrLn err
      void $ liftIO getLine
    Right matches -> do
      selected <- selectDisciplina matches
      case selected of
        Just disciplina -> do
          alreadyInGrade <- isDisciplinaInGrade disciplina
          if alreadyInGrade
            then do
              liftIO Util.clearScreen
              liftIO $ putStrLn $ "A disciplina '" ++ nome disciplina ++ "' já está na sua grade."
            else do
              confirmed <- confirmDisciplina disciplina
              if confirmed
                then do
                  canAdd <- checkConcurrency disciplina
                  liftIO Util.clearScreen
                  if canAdd
                    then do
                      modify (\s -> s { selectedDisciplinas = disciplina : selectedDisciplinas s })
                      liftIO $ putStrLn $ "Disciplina '" ++ nome disciplina ++ "' adicionada com sucesso."
                    else liftIO $ putStrLn "Não é possível adicionar esta disciplina devido a conflitos de horário."
                else do
                  liftIO Util.clearScreen
                  liftIO $ putStrLn "Adição da disciplina cancelada."
          liftIO $ putStrLn "Pressione qualquer tecla para continuar..."
          void $ liftIO getLine
        Nothing -> return ()


selectDisciplina :: [Disciplina] -> GridBuilder (Maybe Disciplina)
selectDisciplina [] = do
  liftIO Util.clearScreen
  liftIO $ putStrLn "Nenhuma disciplina encontrada."
  _ <- liftIO getLine
  return Nothing
selectDisciplina [d] = return (Just d)
selectDisciplina disciplinas = do
  liftIO Util.clearScreen
  liftIO $ putStrLn "Múltiplas disciplinas encontradas. Escolha uma:"
  liftIO $ putStrLn "0. Voltar ao menu anterior"
  
  currentSelectedDisciplinas <- gets selectedDisciplinas
  forM_ (zip [1 :: Int ..] disciplinas) $ \(i, d) -> do
    inGrade <- isDisciplinaInGrade d
    let hasConflict = any (Util.hasConcurrency d) currentSelectedDisciplinas
    let cannotSelect = inGrade || hasConflict
    let colorCode = if cannotSelect then Util.redColor else ""
    let resetCode = if cannotSelect then Util.resetColor else ""
    let reason = if inGrade then " (já na grade)" 
                 else if hasConflict then " (conflito de horário)"
                 else ""
    liftIO $ putStrLn $ colorCode ++ show i ++ ". " ++ nome d ++ reason ++ resetCode
  
  choice <- liftIO getLine
  case reads choice of
    [(i, "")] 
      | i > 0 && i <= length disciplinas -> do
          let selectedDisciplina = disciplinas !! (i - 1)
          inGrade <- isDisciplinaInGrade selectedDisciplina
          let hasConflict = any (Util.hasConcurrency selectedDisciplina) currentSelectedDisciplinas
          if inGrade || hasConflict
            then do
              liftIO Util.clearScreen
              liftIO $ putStrLn $ "Não é possível selecionar esta disciplina." ++
                                  if inGrade then " Ela já está na sua grade."
                                  else " Há conflito de horário com disciplinas já selecionadas."
              _ <- liftIO getLine
              selectDisciplina disciplinas
            else return $ Just selectedDisciplina
      | i == 0 -> return Nothing
    _ -> do
      liftIO Util.clearScreen
      liftIO $ putStrLn "Escolha inválida. Tente novamente."
      _ <- liftIO getLine
      selectDisciplina disciplinas

confirmDisciplina :: Disciplina -> GridBuilder Bool
confirmDisciplina disciplina = do
  liftIO Util.clearScreen
  liftIO $ putStrLn "Informações da disciplina selecionada:"
  liftIO $ putStrLn $ Util.showDisciplina disciplina
  liftIO $ putStrLn "\nDeseja adicionar esta disciplina à sua grade? (S/N)"
  response <- liftIO getLine
  return $ map toLower response `elem` ["s", "sim", "y", "yes"]

checkConcurrency :: Disciplina -> GridBuilder Bool
checkConcurrency newDisciplina = do
  selected <- gets selectedDisciplinas
  return $ all (not . Util.hasConcurrency newDisciplina) selected
  
isDisciplinaInGrade :: Disciplina -> GridBuilder Bool
isDisciplinaInGrade disciplina = do
  selected <- gets selectedDisciplinas
  return $ any (\d -> codigo d == codigo disciplina) selected
  
viewAcademicGrid :: GridState -> String
viewAcademicGrid gridState =
  let disciplinas = selectedDisciplinas gridState
  in if null disciplinas
       then "A grade acadêmica está vazia."
       else "Grade acadêmica:\n" ++ unlines (map Util.formatDisciplina disciplinas)
