module DataStructures (Disciplina(..), Horario(..), Obrigatoriedade(..)) where

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