{-# LANGUAGE OverloadedStrings #-} -- Habilita o uso de literais de string para facilitar a manipulação de JSON
{-# LANGUAGE RecordWildCards #-} -- Permite o uso de sintaxe curta para acessar campos de registros

module Types 
  ( Disciplina(..)
  , Horario(..)
  , Obrigatoriedade(..)
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))

{- Para a definição dos data types e instances abaixo contamos com o auxílio da documentação e exemplos disponíveis em:
   https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html#g:11
-}


-- Definição da estrutura de dados `Disciplina`, que representa as informações sobre uma disciplina acadêmica
data Disciplina = Disciplina
  { nome :: String
  , vagas :: Int
  , vagasIngressantes :: Maybe Int -- Vagas para ingressantes (pode ser `Nothing` se não houver essa informação)
  , tpi :: [Int]
  , creditos :: Int
  , id :: Int
  , codigo :: String
  , campus :: Int
  , horarios :: [Horario]
  , obrigatoriedades :: [Obrigatoriedade]
  , nomeCampus :: String
  , recomendacoes :: Maybe [String] -- Recomendações associadas à disciplina (podem ser inexistentes (`Nothing`))
  } deriving (Show)

-- Definição da estrutura `Horario`, que representa os horários de uma disciplina
data Horario = Horario
  { semana :: Int
  , periodicidadeExtenso :: String
  , horas :: [String]
  } deriving (Show)

-- Definição da estrutura `Obrigatoriedade`, que descreve se uma disciplina é obrigatória em um curso específico
data Obrigatoriedade = Obrigatoriedade
  { obrigatoriedade :: String
  , cursoId :: Int
  } deriving (Show)

-- Instância da classe `FromJSON` para a estrutura `Disciplina`, permitindo que o tipo seja decodificado de um JSON
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

-- Instância da classe `FromJSON` para a estrutura `Horario`, permitindo que o tipo seja decodificado de um JSON
instance FromJSON Horario where
  parseJSON = withObject "Horario" $ \v -> Horario
    <$> v .: "semana"
    <*> v .: "periodicidade_extenso"
    <*> v .: "horas"

-- Instância da classe `FromJSON` para a estrutura `Obrigatoriedade`, permitindo que o tipo seja decodificado de um JSON
instance FromJSON Obrigatoriedade where
  parseJSON = withObject "Obrigatoriedade" $ \v -> Obrigatoriedade
    <$> v .: "obrigatoriedade"
    <*> v .: "curso_id"
