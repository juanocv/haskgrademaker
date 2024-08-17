module Util
  ( showDisciplinas
  , showDisciplina
  , showHorarios
  , showHorario
  , searchDisciplina
  ) where

import Data.List (isPrefixOf)
import Data.Char (toLower)

import Types

-- As funções abaixo foram feitas com auxílio da documentação e exemplos disponíveis em:
-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html
-- Basicamente para compreender o uso de funções como show, unline e unwords

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
  [ "\n\tDia:", evenSemana
  , "\n\tPeriodicidade" ++ periodicidadeExtenso h
  , "\n\tPeríodo:", head (horas h), "-", last (horas h)
  ]
  where
    evenSemana
      | semana h == 1 = "Segunda-feira"
      | semana h == 2 = "Terça-feira"
      | semana h == 3 = "Quarta-feira"
      | semana h == 4 = "Quinta-feira"
      | semana h == 5 = "Sexta-feira"
      | semana h == 6 = "Sábado"
      | otherwise     = "Domingo"

searchDisciplina :: String -> [Disciplina] -> Either String [Disciplina]
searchDisciplina s ds =
    let matches = filter (\d -> map toLower s `isPrefixOf` map toLower (nome d)) ds
    in if null matches
       then Left "Nenhuma disciplina encontrada com esse nome."
       else Right matches
