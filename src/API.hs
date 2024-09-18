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

{- Função principal que baixa e faz o parsing das disciplinas
   Material de apoio:
   https://hackage.haskell.org/package/http-conduit-2.3.8.3/docs/Network-HTTP-Simple.html
-}
downloadAndParseDisciplinas :: String -> IO (Either String [Disciplina])
downloadAndParseDisciplinas url = catch fetchAndParse handleHttpException
-- `catch` é utilizado para lidar com exceções que podem ocorrer durante a requisição HTTP
  where
    -- Função auxiliar que faz o download e processa os dados
    fetchAndParse = do
      response <- httpBS $ parseRequest_ url -- Faz uma requisição HTTP GET ao URL fornecido
      let rawData = BSL.fromStrict $ getResponseBody response -- Obtém o corpo da resposta e converte para `ByteString` lazy
      let processedData = preprocessJSON rawData -- Aplica a função de pré-processamento nos dados brutos (normaliza o JSON)
      return $ parseJSON processedData -- Faz o parsing dos dados JSON para obter a lista de disciplinas


{- Função que lida com exceções HTTP específicas
   Material de apoio:
   https://hackage.haskell.org/package/http-client-0.7.17/docs/Network-HTTP-Client
-}
    handleHttpException :: HttpException -> IO (Either String [Disciplina])
    
    -- Trata exceção de falha de conexão
    handleHttpException (HttpExceptionRequest _ (ConnectionFailure _)) =
      return $ Left "Erro: Não foi possível conectar ao servidor. Por favor, verifique sua conexão com a internet e tente novamente."
      
    -- Trata exceção de código de status 404 (página não encontrada)
    handleHttpException (HttpExceptionRequest _ (StatusCodeException resp _))
      | statusCode (responseStatus resp) == 404 =
          return $ Left "Erro: A página solicitada não foi encontrada (404). Por favor, verifique se o URL está correto."
          
    -- Captura qualquer outra exceção HTTP e exibe uma mensagem de erro genérica
    handleHttpException e =
      return $ Left $ "Erro ao acessar o servidor: " ++ show e
