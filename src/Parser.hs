{-# LANGUAGE OverloadedStrings #-} -- Habilita o uso de literais de string para facilitar a manipulação de tipos Bytestring

module Parser
  ( preprocessJSON
  , parseJSON
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode, (.:))
import Data.Aeson.Types (parseEither)

import Types

{- Função que pré-processa o JSON, extraindo o conteúdo relevante do arquivo bruto
   Material de apoio:
   https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Lazy.html
-}
preprocessJSON :: BSL.ByteString -> BSL.ByteString
preprocessJSON input =
  -- Toma os dados entre o primeiro "[" (começo da lista de disciplinas) até o ";", removendo o conteúdo indesejado
  let content = BSL.takeWhile (/= 59) $ BSL.dropWhile (/= 91) input
  in "{\"todasDisciplinas\":" <> content <> "}"
  -- Envolve o conteúdo extraído em um novo objeto JSON, adicionando a chave "todasDisciplinas"


{- Função que faz o parsing do JSON pré-processado e retorna uma lista de disciplinas
   Material de apoio:
   https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html
-}
parseJSON :: BSL.ByteString -> Either String [Disciplina]
parseJSON processedData =
   -- Usa `eitherDecode` para transformar o ByteString em um objeto JSON, e depois extrai o valor da chave "todasDisciplinas"
  eitherDecode processedData >>= parseEither (.: "todasDisciplinas")
  -- Caso o parsing falhe, retorna um erro em forma de `Left String`
