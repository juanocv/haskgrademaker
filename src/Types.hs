-- Para a definição dos data types e instances abaixo contamos com o auxílio da documentação e exemplos disponíveis em:
-- https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html#g:11

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))

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

instance FromJSON Horario where
  parseJSON = withObject "Horario" $ \v -> Horario
    <$> v .: "semana"
    <*> v .: "periodicidade_extenso"
    <*> v .: "horas"

instance FromJSON Obrigatoriedade where
  parseJSON = withObject "Obrigatoriedade" $ \v -> Obrigatoriedade
    <$> v .: "obrigatoriedade"
    <*> v .: "curso_id"
