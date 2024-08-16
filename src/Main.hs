{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Parsing (downloadAndParseDisciplinas)
import Processing (searchDisciplina, showDisciplinas)

-- Main function
main :: IO ()
main = do
  let url = "https://matricula.ufabc.edu.br/cache/todasDisciplinas.js"
  result <- downloadAndParseDisciplinas url
  case result of
    Right disciplinas -> do
      putStrLn $ "Download com sucesso de " ++ show (length disciplinas) ++ " disciplinas"
      putStrLn "Insira o nome da disciplina que deseja verificar: "
      entry <- getLine
      case searchDisciplina entry disciplinas of
        Right matchedDisciplinas -> do
          putStrLn $ concat $ showDisciplinas matchedDisciplinas
        Left err -> putStrLn err
    Left err -> do
      putStrLn "Falha ao realizar o parse do JSON. Detalhes do erro:"
      putStrLn err
      putStrLn "Isso pode ter ocorrido devido à valores null inesperados ou tipos incompatíveis no JSON."
