{-# LANGUAGE OverloadedStrings #-}

module API
  ( downloadAndParseDisciplinas
  ) where

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest_)
import qualified Data.ByteString.Lazy as BSL
import Control.Exception (catch)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import Network.HTTP.Types.Status (statusCode)

import Types
import Parser

downloadAndParseDisciplinas :: String -> IO (Either String [Disciplina])
downloadAndParseDisciplinas url = catch fetchAndParse handleHttpException
  where
    fetchAndParse = do
      response <- httpBS $ parseRequest_ url
      let rawData = BSL.fromStrict $ getResponseBody response
      let processedData = preprocessJSON rawData
      return $ parseJSON processedData

    handleHttpException :: HttpException -> IO (Either String [Disciplina])
    handleHttpException (HttpExceptionRequest _ (ConnectionFailure _)) =
      return $ Left "Erro: Não foi possível conectar ao servidor. Por favor, verifique sua conexão com a internet e tente novamente."
    handleHttpException (HttpExceptionRequest _ (StatusCodeException resp _))
      | statusCode (responseStatus resp) == 404 =
          return $ Left "Erro: A página solicitada não foi encontrada (404). Por favor, verifique se o URL está correto."
    handleHttpException e =
      return $ Left $ "Erro ao acessar o servidor: " ++ show e
