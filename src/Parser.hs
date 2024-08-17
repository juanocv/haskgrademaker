{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( preprocessJSON
  , parseJSON
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode, (.:))
import Data.Aeson.Types (parseEither)

import Types

-- A função abaixo foi feita com o auxílio da documentação e exemplos disponíveis em:
-- https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Lazy.html
preprocessJSON :: BSL.ByteString -> BSL.ByteString
preprocessJSON input =
  let content = BSL.takeWhile (/= 59) $ BSL.dropWhile (/= 91) input
  in "{\"todasDisciplinas\":" <> content <> "}"

-- A função abaixo foi feita com o auxílio da documentação e exemplos disponíveis em:
-- https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html
parseJSON :: BSL.ByteString -> Either String [Disciplina]
parseJSON processedData = 
  eitherDecode processedData >>= parseEither (.: "todasDisciplinas") -- Para esta linha também contamos com ajuda do ClaudeAI
