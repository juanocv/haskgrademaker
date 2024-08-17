module Util where

import Data.List (isPrefixOf)
import Data.Char (toLower)

import Types

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
