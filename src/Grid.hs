module Grid
  ( buildAcademicGrid
  , GridState(..)
  , GridBuilder
  , initialState
  , viewAcademicGrid
  ) where

import qualified Util
import Control.Monad.State
import Control.Monad (forM_, void)
import Types
import Data.Char (toLower)

-- Definição do estado da grade, armazenando as disciplinas selecionadas
data GridState = GridState
  { selectedDisciplinas :: [Disciplina]
  }

{- Material de apoio para os tipos e funções abaixo:
   1. https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-State-Lazy.html#g:2; 
   2. https://stackoverflow.com/questions/43438875/confusion-about-statet-state-and-monadstate
   3. https://hackage.haskell.org/package/transformers-0.4.2.0/docs/Control-Monad-IO-Class.html
   4. https://hackage.haskell.org/package/process-1.6.23.0/docs/System-Process.html (também em Util.hs)
   5. https://gist.github.com/JBlond/2fea43a3049b38287e5e9cefc87b2124
-}


-- Mônada que combina a habilidade de manipular o estado (GridState) com a capacidade de realizar operações de IO sob um type alias GridBuilder
type GridBuilder a = StateT GridState IO a

-- Estado inicial da grade, começando com uma lista vazia de disciplinas
initialState :: GridState
initialState = GridState { selectedDisciplinas = [] }

buildAcademicGrid :: [Disciplina] -> GridBuilder [Disciplina]
buildAcademicGrid allDisciplinas = do
  gridBuilderLoop allDisciplinas -- Inicia o loop principal para seleção de disciplinas
  gets selectedDisciplinas -- Retorna a lista de disciplinas selecionadas ao final

gridBuilderLoop :: [Disciplina] -> GridBuilder ()
gridBuilderLoop allDisciplinas = do
  liftIO Util.clearScreen -- Limpa a tela
  liftIO $ putStrLn "1. Adicionar disciplina\n0. Finalizar grade" -- Mostra opções ao usuário
  choice <- liftIO getLine -- Lê a escolha do usuário
  case choice of
    "1" -> do
      addDisciplina allDisciplinas -- Adiciona uma disciplina
      gridBuilderLoop allDisciplinas -- Continua o loop
    "0" -> return () -- Finaliza o processo
    _   -> do
      liftIO Util.clearScreen -- Para opções inválidas, mostra mensagem de erro
      liftIO $ putStrLn "Opção iválida. Tente novamente." 
      _ <- liftIO getLine
      gridBuilderLoop allDisciplinas -- Reinicia o loop

-- Função para adicionar uma disciplina à grade
addDisciplina :: [Disciplina] -> GridBuilder ()
addDisciplina allDisciplinas = do
  liftIO Util.clearScreen
  liftIO $ putStrLn "Digite o nome da disciplina:" -- Solicita o nome da disciplina
  query <- liftIO getLine
  case Util.searchDisciplina query allDisciplinas of -- Busca a disciplina pelo nome
    Left err -> do
      liftIO Util.clearScreen -- Se não for encontrada, exibe erro
      liftIO $ putStrLn err
      void $ liftIO getLine
    Right matches -> do
      selected <- selectDisciplina matches -- Caso existam correspondências, permite a seleção
      case selected of
        Just disciplina -> do
          alreadyInGrid <- isDisciplinaInGrid disciplina -- Verifica se a disciplina já está na grade
          if alreadyInGrid
            then do
              liftIO Util.clearScreen -- Se já estiver, avisa o usuário
              liftIO $ putStrLn $ "A disciplina '" ++ nome disciplina ++ "' já está na sua grade."
            else do
              confirmed <- confirmDisciplina disciplina -- Confirmação de nova adição
              if confirmed
                then do
                  canAdd <- checkConcurrency disciplina -- Verifica conflitos de horário
                  liftIO Util.clearScreen
                  if canAdd
                    then do
                      modify (\s -> s { selectedDisciplinas = disciplina : selectedDisciplinas s }) -- Adiciona a disciplina
                      liftIO $ putStrLn $ "Disciplina '" ++ nome disciplina ++ "' adicionada com sucesso."
                    else liftIO $ putStrLn "Não é possível adicionar esta disciplina devido a conflitos de horário."
                else do
                  liftIO Util.clearScreen
                  liftIO $ putStrLn "Adição da disciplina cancelada."
          liftIO $ putStrLn "Pressione qualquer tecla para continuar..."
          void $ liftIO getLine
        Nothing -> return () -- Se o usuário não selecionar nenhuma, retorna

-- Função que permite ao usuário selecionar uma disciplina dentre várias encontradas
selectDisciplina :: [Disciplina] -> GridBuilder (Maybe Disciplina)
selectDisciplina [] = do
  liftIO Util.clearScreen -- Se nenhuma disciplina for encontrada
  liftIO $ putStrLn "Nenhuma disciplina encontrada."
  _ <- liftIO getLine
  return Nothing
selectDisciplina [d] = return (Just d) -- Se apenas uma disciplina for encontrada, seleciona-a automaticamente
selectDisciplina disciplinas = do
  liftIO Util.clearScreen -- Mostra lista de disciplinas encontradas
  liftIO $ putStrLn "Múltiplas disciplinas encontradas. Escolha uma:"
  liftIO $ putStrLn "0. Voltar ao menu anterior"
  
  currentSelectedDisciplinas <- gets selectedDisciplinas -- Pega as disciplinas já selecionadas
  forM_ (zip [1 :: Int ..] disciplinas) $ \(i, d) -> do
    inGrid <- isDisciplinaInGrid d
    let hasConflict = any (Util.hasConcurrency d) currentSelectedDisciplinas -- Verifica conflito de horários
    let cannotSelect = inGrid || hasConflict
    let colorCode = if cannotSelect then Util.redColor else "" -- Adiciona cor vermelha para indicar não selecionáveis
    let resetCode = if cannotSelect then Util.resetColor else ""
    let reason = if inGrid then " (já na grade)" 
                 else if hasConflict then " (conflito de horário)"
                 else ""
    liftIO $ putStrLn $ colorCode ++ show i ++ ". " ++ nome d ++ reason ++ resetCode -- Mostra opções ao usuário
  
  choice <- liftIO getLine
  case reads choice of
    [(i, "")] 
      | i > 0 && i <= length disciplinas -> do
          let selectedDisciplina = disciplinas !! (i - 1)
          inGrid <- isDisciplinaInGrid selectedDisciplina
          let hasConflict = any (Util.hasConcurrency selectedDisciplina) currentSelectedDisciplinas
          if inGrid || hasConflict
            then do
              liftIO Util.clearScreen
              liftIO $ putStrLn $ "Não é possível selecionar esta disciplina." ++
                                  if inGrid then " Ela já está na sua grade."
                                  else " Há conflito de horário com disciplinas já selecionadas."
              _ <- liftIO getLine
              selectDisciplina disciplinas -- Retorna ao menu de seleção
            else return $ Just selectedDisciplina
      | i == 0 -> return Nothing -- Volta ao menu anterior
    _ -> do
      liftIO Util.clearScreen
      liftIO $ putStrLn "Escolha inválida. Tente novamente."
      _ <- liftIO getLine
      selectDisciplina disciplinas -- Reinicia a seleção

-- Função que confirma com o usuário se deseja adicionar uma disciplina à grade
confirmDisciplina :: Disciplina -> GridBuilder Bool
confirmDisciplina disciplina = do
  liftIO Util.clearScreen
  liftIO $ putStrLn "Informações da disciplina selecionada:"
  liftIO $ putStrLn $ Util.showDisciplina disciplina -- Mostra detalhes da disciplina
  liftIO $ putStrLn "\nDeseja adicionar esta disciplina à sua grade? (S/N)"
  response <- liftIO getLine
  return $ map toLower response `elem` ["s", "sim", "y", "yes"] -- Confirma a adição com base na resposta

-- Verifica se há conflito de horários com disciplinas já adicionadas
checkConcurrency :: Disciplina -> GridBuilder Bool
checkConcurrency newDisciplina = do
  selected <- gets selectedDisciplinas
  return $ all (not . Util.hasConcurrency newDisciplina) selected

-- Verifica se a disciplina já está na grade
isDisciplinaInGrid :: Disciplina -> GridBuilder Bool
isDisciplinaInGrid disciplina = do
  selected <- gets selectedDisciplinas
  return $ any (\d -> codigo d == codigo disciplina) selected

-- Função para visualizar a grade acadêmica criada
viewAcademicGrid :: GridState -> String
viewAcademicGrid gridState =
  let disciplinas = selectedDisciplinas gridState
  in if null disciplinas
       then "A grade acadêmica está vazia."
       else "Grade acadêmica:\n" ++ unlines (map Util.formatDisciplina disciplinas)
