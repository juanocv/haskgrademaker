{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( preprocessJSON
  , parseJSON
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode, (.:))
import Data.Aeson.Types (parseEither)

import Types

preprocessJSON :: BSL.ByteString -> BSL.ByteString
preprocessJSON input =
  let content = BSL.takeWhile (/= 59) $ BSL.dropWhile (/= 91) input
  in "{\"todasDisciplinas\":" <> content <> "}"

parseJSON :: BSL.ByteString -> Either String [Disciplina]
parseJSON processedData = 
  eitherDecode processedData >>= parseEither (.: "todasDisciplinas")
