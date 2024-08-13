{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest_)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), eitherDecode)
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf)
import Data.Char (toLower)
import qualified Data.ByteString.Lazy as BSL

-- Data structures
data Disciplina = Disciplina
  { nome :: String
  , vagas :: Int
  , vagasIngressantes :: Maybe Int
  , tpi :: [Int]
  , creditos :: Int
  , id :: Int
  , codigo :: String
  , campus :: Int
  , horarios :: [Horario]
  , obrigatoriedades :: [Obrigatoriedade]
  , nomeCampus :: String
  , recomendacoes :: Maybe [String]
  } deriving (Show)

data Horario = Horario
  { semana :: Int
  , periodicidadeExtenso :: String
  , horas :: [String]
  } deriving (Show)

data Obrigatoriedade = Obrigatoriedade
  { obrigatoriedade :: String
  , cursoId :: Int
  } deriving (Show)

-- Updated FromJSON instance for Disciplina
instance FromJSON Disciplina where
  parseJSON = withObject "Disciplina" $ \v -> Disciplina
    <$> v .: "nome"
    <*> v .: "vagas"
    <*> v .:? "vagas_ingressantes"
    <*> v .: "tpi"
    <*> v .: "creditos"
    <*> v .: "id"
    <*> v .: "codigo"
    <*> v .: "campus"
    <*> v .: "horarios"
    <*> v .: "obrigatoriedades"
    <*> v .: "nome_campus"
    <*> v .:? "recomendacoes"

-- FromJSON instances for Horario and Obrigatoriedade
instance FromJSON Horario where
  parseJSON = withObject "Horario" $ \v -> Horario
    <$> v .: "semana"
    <*> v .: "periodicidade_extenso"
    <*> v .: "horas"

instance FromJSON Obrigatoriedade where
  parseJSON = withObject "Obrigatoriedade" $ \v -> Obrigatoriedade
    <$> v .: "obrigatoriedade"
    <*> v .: "curso_id"

-- Preprocessing function
preprocessJSON :: BSL.ByteString -> BSL.ByteString
preprocessJSON input =
  let content = BSL.takeWhile (/= 59) $ BSL.dropWhile (/= 91) input
  in "{\"todasDisciplinas\":" <> content <> "}"

-- Function to download JSON, preprocess and parse
downloadAndParseDisciplinas :: String -> IO (Either String [Disciplina])
downloadAndParseDisciplinas url = do
  response <- httpBS $ parseRequest_ url
  let rawData = BSL.fromStrict $ getResponseBody response
  let processedData = preprocessJSON rawData
  return $ eitherDecode processedData >>= parseEither (.: "todasDisciplinas")

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
