module Processing (searchDisciplina, showDisciplinas) where

import Data.Char (toLower)
import Data.List (isPrefixOf)
import DataStructures (Disciplina(..), Horario(..))

-- Function to show disciplinas
showDisciplinas :: [Disciplina] -> [String]
showDisciplinas []     = []
showDisciplinas (d:ds) = showDisciplina d : showDisciplinas ds

showDisciplina :: Disciplina -> String
showDisciplina d = unlines
  [ "Nome: " ++ (nome d)
  , "Vagas: " ++ show (vagas d)
  --, "Vagas Ingressantes: " ++ maybe "N/A" show (vagasIngressantes d)
  , "TPI: " ++ show (tpi d)
  , "Créditos: " ++ show (creditos d)
  , "Código: " ++ codigo d
  --, "Campus: " ++ show (campus d)
  , nomeCampus d
  --, "Recomendações: " ++ maybe "N/A" (T.unpack . T.unwords) (recomendacoes d)
  , "Horários: " ++ showHorarios (horarios d)
  --, "Categoria: " ++ showObrigatoriedades (obrigatoriedades d)
  ]

showHorarios :: [Horario] -> String
showHorarios hs = unlines $ map showHorario hs

showHorario :: Horario -> String
showHorario h = unwords
  [ "\n\tDia:", evenSemana
  , "\n\tPeriodicidade" ++ periodicidadeExtenso h
  , "\n\tPeríodo:", head(horas h), "-", last(horas h)
  ]
  where
    evenSemana
        | (semana h == 1) = "Segunda-feira"
        | (semana h == 2) = "Terça-feira"
        | (semana h == 3) = "Quarta-feira"
        | (semana h == 4) = "Quinta-feira"
        | (semana h == 5) = "Sexta-feira"
        | (semana h == 6) = "Sábado"
        | otherwise       = "Domingo"

{-
showObrigatoriedades :: [Obrigatoriedade] -> String
showObrigatoriedades obs = unlines $ map showObrigatoriedade obs

showObrigatoriedade :: Obrigatoriedade -> String
showObrigatoriedade o = unwords
  [ obrigatoriedade o
  --, "\nCurso Id:", show (cursoId o)
  ]
-}

-- Function to search for a given disciplina
searchDisciplina :: String -> [Disciplina] -> Either String [Disciplina]
searchDisciplina s ds =
    let matches = filter (\d -> map toLower s `isPrefixOf` map toLower (nome d)) ds
    in if null matches
       then Left "Nenhuma disciplina encontrada com esse nome."
       else Right matches