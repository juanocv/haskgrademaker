{-# LANGUAGE OverloadedStrings #-}
module Grid
  ( buildAcademicGrid
  , GridState(..)
  , GridBuilder
  , initialState
  , viewAcademicGrid
  ) where

import Control.Monad.State
import Control.Monad (forM_, void)
import Data.List (sort, intersect)
import Types
import Util (showDisciplina, searchDisciplina)
import Data.Char (toLower)

data GridState = GridState
  { selectedDisciplinas :: [Disciplina]
  }

type GridBuilder a = StateT GridState IO a

initialState :: GridState
initialState = GridState { selectedDisciplinas = [] }

buildAcademicGrid :: [Disciplina] -> IO () -> GridBuilder [Disciplina]
buildAcademicGrid allDisciplinas clearScreen = do
  gridBuilderLoop allDisciplinas clearScreen
  gets selectedDisciplinas

gridBuilderLoop :: [Disciplina] -> IO () -> GridBuilder ()
gridBuilderLoop allDisciplinas clearScreen = do
  liftIO clearScreen
  liftIO $ putStrLn "1. Adicionar disciplina\n0. Finalizar grade"
  choice <- liftIO getLine
  case choice of
    "1" -> do
      addDisciplina allDisciplinas clearScreen
      gridBuilderLoop allDisciplinas clearScreen
    "0" -> return ()
    _   -> do
      liftIO clearScreen
      liftIO $ putStrLn "Opção inválida. Tente novamente."
      liftIO getLine
      gridBuilderLoop allDisciplinas clearScreen

addDisciplina :: [Disciplina] -> IO () -> GridBuilder ()
addDisciplina allDisciplinas clearScreen = do
  liftIO clearScreen
  liftIO $ putStrLn "Digite o nome da disciplina:"
  query <- liftIO getLine
  case searchDisciplina query allDisciplinas of
    Left err -> do
      liftIO clearScreen
      liftIO $ putStrLn err
      void $ liftIO getLine
    Right matches -> do
      selected <- selectDisciplina matches clearScreen
      case selected of
        Just disciplina -> do
          confirmed <- confirmDisciplina disciplina clearScreen
          if confirmed
            then do
              canAdd <- checkConcurrency disciplina
              liftIO clearScreen
              if canAdd
                then do
                  modify (\s -> s { selectedDisciplinas = disciplina : selectedDisciplinas s })
                  liftIO $ putStrLn $ "Disciplina '" ++ nome disciplina ++ "' adicionada com sucesso."
                else liftIO $ putStrLn "Não é possível adicionar esta disciplina devido a conflitos de horário."
            else do
              liftIO clearScreen
              liftIO $ putStrLn "Adição da disciplina cancelada."
          liftIO $ putStrLn "Pressione qualquer tecla para continuar..."
          void $ liftIO getLine
        Nothing -> return ()

selectDisciplina :: [Disciplina] -> IO () -> GridBuilder (Maybe Disciplina)
selectDisciplina [] clearScreen = do
  liftIO clearScreen
  liftIO $ putStrLn "Nenhuma disciplina encontrada."
  liftIO getLine
  return Nothing
selectDisciplina [d] _ = return (Just d)
selectDisciplina disciplinas clearScreen = do
  liftIO clearScreen
  liftIO $ putStrLn "Múltiplas disciplinas encontradas. Escolha uma:"
  forM_ (zip [1..] disciplinas) $ \(i, d) ->
    liftIO $ putStrLn $ show i ++ ". " ++ nome d
  choice <- liftIO getLine
  case reads choice of
    [(i, "")] | i > 0 && i <= length disciplinas ->
      return $ Just $ disciplinas !! (i - 1)
    _ -> do
      liftIO clearScreen
      liftIO $ putStrLn "Escolha inválida. Tente novamente."
      liftIO getLine
      selectDisciplina disciplinas clearScreen

confirmDisciplina :: Disciplina -> IO () -> GridBuilder Bool
confirmDisciplina disciplina clearScreen = do
  liftIO clearScreen
  liftIO $ putStrLn "Informações da disciplina selecionada:"
  liftIO $ putStrLn $ showDisciplina disciplina
  liftIO $ putStrLn "\nDeseja adicionar esta disciplina à sua grade? (S/N)"
  response <- liftIO getLine
  return $ map toLower response `elem` ["s", "sim", "y", "yes"]

checkConcurrency :: Disciplina -> GridBuilder Bool
checkConcurrency newDisciplina = do
  selected <- gets selectedDisciplinas
  return $ all (not . hasConcurrency newDisciplina) selected

hasConcurrency :: Disciplina -> Disciplina -> Bool
hasConcurrency d1 d2 = any (uncurry coincidePeriodo) [(h1, h2) | h1 <- horarios d1, h2 <- horarios d2]
  where
    coincidePeriodo h1 h2 =
      semana h1 == semana h2 &&
      periodicidadeExtenso h1 == periodicidadeExtenso h2 &&
      sobrepoe (horas h1) (horas h2)
    sobrepoe :: [String] -> [String] -> Bool
    sobrepoe hs1 hs2 =
      let hs1Sorted = sort hs1
          hs2Sorted = sort hs2
      in (last hs1Sorted > head hs2Sorted) && (last hs2Sorted > head hs1Sorted)

viewAcademicGrid :: GridState -> IO ()
viewAcademicGrid gridState = do
  let disciplinas = selectedDisciplinas gridState
  if null disciplinas
    then putStrLn "A grade acadêmica está vazia."
    else do
      putStrLn "Grade acadêmica:"
      forM_ disciplinas $ \d -> do
        putStrLn $ "- " ++ nome d
        forM_ (horarios d) $ \h -> do
          putStrLn $ "  " ++ showHorarioG h
        putStrLn ""

showHorarioG :: Horario -> String
showHorarioG h = unwords
  [ diaSemana (semana h)
  , periodicidadeExtenso h
  , head (horas h) ++ "-" ++ last (horas h)
  ]

diaSemana :: Int -> String
diaSemana 1 = "Segunda-feira"
diaSemana 2 = "Terça-feira"
diaSemana 3 = "Quarta-feira"
diaSemana 4 = "Quinta-feira"
diaSemana 5 = "Sexta-feira"
diaSemana 6 = "Sábado"
diaSemana 7 = "Domingo"
diaSemana _ = "Dia inválido"
