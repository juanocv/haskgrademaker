module Util
  ( redColor
  , resetColor
  , showDisciplinas
  , showDisciplina
  , showHorarios
  , showHorario
  , searchDisciplina
  , formatDisciplina
  , hasConcurrency
  , clearScreen
  ) where

import System.Process (system)
import System.Info (os)
import Data.List (isPrefixOf, sort)
import Data.Char (toLower)
import Types

-- As funções abaixo foram feitas com auxílio da documentação e exemplos disponíveis em:
-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html

redColor :: String
redColor = "\ESC[31m"

resetColor :: String
resetColor = "\ESC[0m"

showDisciplinas :: [Disciplina] -> [String]
showDisciplinas = map showDisciplina

showDisciplina :: Disciplina -> String
showDisciplina d = unlines
  [ "Nome: " ++ nome d
  , "Vagas: " ++ show (vagas d)
  , "TPI: " ++ show (tpi d)
  , "Créditos: " ++ show (creditos d)
  , "Código: " ++ codigo d
  , nomeCampus d
  , "Horários: " ++ showHorarios (horarios d)
  ]

showHorarios :: [Horario] -> String
showHorarios = unlines . map showHorario

showHorario :: Horario -> String
showHorario h = unwords
  [ "\n\tDia:", diaSemana (semana h)
  , "\n\tPeriodicidade" ++ periodicidadeExtenso h
  , "\n\tPeríodo:", head (horas h), "-", last (horas h)
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

searchDisciplina :: String -> [Disciplina] -> Either String [Disciplina]
searchDisciplina s ds =
    let matches = filter (\d -> map toLower s `isPrefixOf` map toLower (nome d)) ds
    in if null matches
       then Left "Nenhuma disciplina encontrada com esse nome."
       else Right matches

formatDisciplina :: Disciplina -> String
formatDisciplina d =
  let nomeDisciplina = "- " ++ nome d
      horariosDisciplina = unlines (map showHorario (horarios d))
  in nomeDisciplina ++ "\n" ++ horariosDisciplina
  
hasConcurrency :: Disciplina -> Disciplina -> Bool
hasConcurrency d1 d2 = any (uncurry coincidePeriodo) [(h1, h2) | h1 <- horarios d1, h2 <- horarios d2]
  where
    coincidePeriodo h1 h2 =
      semana h1 == semana h2 &&
      periodicidadeExtenso h1 == periodicidadeExtenso h2 &&
      overlap (horas h1) (horas h2)
    overlap :: [String] -> [String] -> Bool
    overlap hs1 hs2 =
      let hs1Sorted = sort hs1
          hs2Sorted = sort hs2
      in (last hs1Sorted > head hs2Sorted) && (last hs2Sorted > head hs1Sorted)
  
clearScreen :: IO ()
clearScreen = do
  _ <- system clearCommand
  return ()
  where
    clearCommand
      | os == "mingw32" = "cls"
      | otherwise       = "clear"
