{-# LANGUAGE OverloadedStrings #-}

module Parsing (downloadAndParseDisciplinas) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest_,Response,HttpException)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), eitherDecode)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BSL
import DataStructures (Disciplina(..), Horario(..), Obrigatoriedade(..))

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
{--
downloadAndParseDisciplinas :: String -> IO (Either String [Disciplina])
downloadAndParseDisciplinas url = do
  response <- httpBS $ parseRequest_ url
  let rawData = BSL.fromStrict $ getResponseBody response
  let processedData = preprocessJSON rawData
  return $ eitherDecode processedData >>= parseEither (.: "todasDisciplinas")
--}

-- Function to download JSON, preprocess and parse
downloadAndParseDisciplinas :: String -> IO (Either String [Disciplina])
downloadAndParseDisciplinas url = do
  result <- try (httpBS $ parseRequest_ url) :: IO (Either HttpException (Response BS.ByteString))
  case result of
    Left ex -> return $ Left $ "Erro ao tentar acessar a URL: " ++ show ex
    Right response -> do
      let rawData = BSL.fromStrict $ getResponseBody response
      let processedData = preprocessJSON rawData
      return $ eitherDecode processedData >>= parseEither (.: "todasDisciplinas")