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

{- Material de apoio para as funções abaixo:
   1. https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html
   2. https://hackage.haskell.org/package/process-1.6.23.0/docs/System-Process.html
   3. https://hackage.haskell.org/package/base-4.18.1.0/docs/System-Info.html
   4. https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-List.html
   5. https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Char.html
-}

-- Define as sequências de escape ANSI para colorir o texto de vermelho
redColor :: String
redColor = "\ESC[31m"

-- Restaura a cor padrão do texto no terminal
resetColor :: String
resetColor = "\ESC[0m"

-- Converte uma lista de disciplinas em uma lista de strings que representam suas informações
showDisciplinas :: [Disciplina] -> [String]
showDisciplinas = map showDisciplina

-- Exibe as informações detalhadas de uma disciplina (nome, vagas, TPI, créditos, código, campus, horários)
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

-- Concatena a exibição de uma lista de horários, chamando `showHorario` para cada um
showHorarios :: [Horario] -> String
showHorarios = unlines . map showHorario

-- Exibe detalhes de um horário (dia da semana, periodicidade e período)
showHorario :: Horario -> String
showHorario h = unwords
  [ "\n\tDia:", diaSemana (semana h) -- Converte o número do dia em texto (segunda-feira, etc.)
  , "\n\tPeriodicidade" ++ periodicidadeExtenso h -- Exibe a periodicidade (semanal, quinzenal, etc.)
  , "\n\tPeríodo:", head (horas h), "-", last (horas h) -- Exibe o período de tempo
  ]

-- Converte um número para o nome do dia da semana (1 = Segunda-feira, etc.)
diaSemana :: Int -> String
diaSemana 1 = "Segunda-feira"
diaSemana 2 = "Terça-feira"
diaSemana 3 = "Quarta-feira"
diaSemana 4 = "Quinta-feira"
diaSemana 5 = "Sexta-feira"
diaSemana 6 = "Sábado"
diaSemana 7 = "Domingo"
diaSemana _ = "Dia inválido"

-- Busca uma disciplina pelo nome. Se não encontrar nenhuma, retorna um erro.
searchDisciplina :: String -> [Disciplina] -> Either String [Disciplina]
searchDisciplina s ds =
    let matches = filter (\d -> map toLower s `isPrefixOf` map toLower (nome d)) ds -- Exibe todos os horários da disciplina
    in if null matches
       then Left "Nenhuma disciplina encontrada com esse nome."
       else Right matches

-- Formata uma disciplina em uma string que exibe seu nome e horários
formatDisciplina :: Disciplina -> String
formatDisciplina d =
  let nomeDisciplina = "- " ++ nome d
      horariosDisciplina = unlines (map showHorario (horarios d))
  in nomeDisciplina ++ "\n" ++ horariosDisciplina
  
hasConcurrency :: Disciplina -> Disciplina -> Bool
hasConcurrency d1 d2 = any (uncurry coincidePeriodo) [(h1, h2) | h1 <- horarios d1, h2 <- horarios d2] -- Verifica todos os pares de horários
  where
    -- Define se dois horários coincidem no mesmo dia, com a mesma periodicidade e com sobreposição de horas
    coincidePeriodo h1 h2 =
      semana h1 == semana h2 &&
      periodicidadeExtenso h1 == periodicidadeExtenso h2 && -- Mesma periodicidade
      overlap (horas h1) (horas h2)  -- Se as horas dos dois horários se sobrepõem
    overlap :: [String] -> [String] -> Bool
    overlap hs1 hs2 =
      let hs1Sorted = sort hs1 -- Ordena os horários para facilitar a comparação
          hs2Sorted = sort hs2
      in (last hs1Sorted > head hs2Sorted) && (last hs2Sorted > head hs1Sorted) -- Verifica se há sobreposição entre os intervalos de tempo

-- Limpa a tela do terminal, adaptando o comando de acordo com o sistema operacional
clearScreen :: IO ()
clearScreen = do
  _ <- system clearCommand -- Executa o comando do sistema
  return ()
  where
    -- Define o comando de limpeza de tela de acordo com o sistema operacional
    clearCommand
      | os == "mingw32" = "cls" -- No Windows, o comando é 'cls'
      | otherwise       = "clear" -- Em outros sistemas (Linux, macOS), o comando é 'clear'
