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
