{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest_)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), eitherDecode)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)

-- Data structures remain the same
data Disciplina = Disciplina
  { nome :: Text
  , vagas :: Int
  , vagasIngressantes :: Maybe Int  -- This is already Maybe Int, which is correct
  , tpi :: [Int]
  , creditos :: Int
  , id :: Int
  , codigo :: Text
  , campus :: Int
  , horarios :: [Horario]
  , obrigatoriedades :: [Obrigatoriedade]
  , nomeCampus :: Text
  , recomendacoes :: Maybe [Text]
  } deriving (Show)

data Horario = Horario
  { semana :: Int
  , periodicidadeExtenso :: Text
  , horas :: [Text]
  } deriving (Show)

data Obrigatoriedade = Obrigatoriedade
  { obrigatoriedade :: Text
  , cursoId :: Int
  } deriving (Show)

-- Updated FromJSON instance for Disciplina
instance FromJSON Disciplina where
  parseJSON = withObject "Disciplina" $ \v -> Disciplina
    <$> v .: "nome"
    <*> v .: "vagas"
    <*> v .:? "vagas_ingressantes"  -- This correctly parses null as Nothing
    <*> v .: "tpi"
    <*> v .: "creditos"
    <*> v .: "id"
    <*> v .: "codigo"
    <*> v .: "campus"
    <*> v .: "horarios"
    <*> v .: "obrigatoriedades"
    <*> v .: "nome_campus"
    <*> v .:? "recomendacoes"

-- FromJSON instances for Horario and Obrigatoriedade remain the same
instance FromJSON Horario where
  parseJSON = withObject "Horario" $ \v -> Horario
    <$> v .: "semana"
    <*> v .: "periodicidade_extenso"
    <*> v .: "horas"

instance FromJSON Obrigatoriedade where
  parseJSON = withObject "Obrigatoriedade" $ \v -> Obrigatoriedade
    <$> v .: "obrigatoriedade"
    <*> v .: "curso_id"

-- Preprocessing function remains the same
preprocessJSON :: BSL.ByteString -> BSL.ByteString
preprocessJSON input =
  let content = BSL8.dropWhile (/= '[') $ BSL8.dropWhile (/= '=') input
      cleanedContent = BSL8.takeWhile (/= ';') content
  in "{\"todasDisciplinas\":" <> cleanedContent <> "}"

-- Function to download JSON, preprocess, and parse it remains the same
downloadAndParseDisciplinas :: String -> IO (Either String [Disciplina])
downloadAndParseDisciplinas url = do
  response <- httpBS $ parseRequest_ url
  let rawData = BSL.fromStrict $ getResponseBody response
  let processedData = preprocessJSON rawData
  return $ eitherDecode processedData >>= parseEither (.: "todasDisciplinas")

-- Main function now includes more detailed error reporting
main :: IO ()
main = do
  let url = "https://matricula.ufabc.edu.br/cache/todasDisciplinas.js"
  result <- downloadAndParseDisciplinas url
  case result of
    Right disciplinas -> do
      putStrLn $ "Successfully downloaded " ++ show (length disciplinas) ++ " disciplinas"
      mapM_ (putStrLn . show) (take 1 disciplinas)  -- Print the first disciplina as an example
    Left err -> do
      putStrLn "Failed to parse JSON. Error details:"
      putStrLn err
      putStrLn "This could be due to unexpected null values or mismatched types in the JSON data."
