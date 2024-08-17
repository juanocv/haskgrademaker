module Main 
  ( main
  ) where

import API
import Util

main :: IO ()
main = do
  let url = "https://matricula.ufabc.edu.br/cache/todasDisciplinas.js"
  result <- API.downloadAndParseDisciplinas url
  case result of
    Right disciplinas -> do
      putStrLn $ "Download com sucesso de " ++ show (length disciplinas) ++ " disciplinas"
      putStrLn "Insira o nome da disciplina que deseja verificar: "
      entry <- getLine
      case Util.searchDisciplina entry disciplinas of
        Right matchedDisciplinas -> do
          putStrLn $ concat $ Util.showDisciplinas matchedDisciplinas
        Left err -> putStrLn err
    Left err -> putStrLn err
