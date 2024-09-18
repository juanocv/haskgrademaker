# Hask Grade Maker

Este projeto consiste na criação de um sistema que auxilia estudantes universitários na montagem de suas grades acadêmicas para o período letivo vigente. O sistema captura informações diretamente da web, extraindo dados sobre disciplinas, horários e outras informações relevantes disponíveis no JSON da universidade. 

Após a coleta, o sistema realiza um pré-processamento e parsing dessas informações, transformando os dados brutos em estruturas organizadas e compreensíveis.

Uma vez processadas as informações, o sistema oferece ao usuário uma interface intuitiva onde ele pode selecionar as disciplinas que deseja cursar. O sistema então monta visualmente a grade acadêmica, exibindo os horários das aulas de forma clara, evitando conflitos e sobreposições. Esse recurso é particularmente útil para estudantes que precisam otimizar seus horários de acordo com suas necessidades e preferências pessoais.

Este é um projeto da disciplina de **Programação Funcional** do 2º quadrimestre de 2024 da Universidade Federal do ABC.

Realizado por:\
**- JUAN OLIVEIRA DE CARVALHO** [(juanocv)](https://github.com/juanocv)**\
**- FERNANDO GABRIEL CHACON FERNANDES TERUEL DO PRADO** [(gabriel-chacon)](https://github.com/gabriel-chacon)**

## TO-DOs (Parte 01)
- [X] Definir as estruturas de dados
- [X] Realizar o download do JSON do site da UFABC
- [X] Fazer o tratamento de dados do JSON
- [X] Construir as estruturas de dados
- [X] Implementar busca básica em tela de disciplinas
- [X] Imprimir disciplina buscada em tela
- [X] Implementar modularização
- [X] Fazer o tratamento de exceções http
- [X] Adicionar comentários ao código

## TO-DOs (Parte 02)
- [X] Criar menu interativo para usuário
- [X] Permitir ao usuário criar nova grade
- [X] Permitir ao usuário buscar disciplinas
- [X] Permitir ao usuário limpar grade existente
- [X] Permitir ao usuário visualizar nova grade
- [X] Atualizar o estado da grade sucessivamente
- [X] Tratamento de erros e exceções

## Dependências
  * ghcup - The Haskell toolchain installer
  * ghc   - The Glasgow Haskell Compiler
  * cabal - The Cabal build tool for managing Haskell software
  * stack - A cross-platform program for developing Haskell projects (similar to cabal)
  * http-conduit - HTTP client package with conduit interface and HTTPS support
  * http-client - An HTTP client engine
  * http-types - Generic HTTP types for Haskell (for both client and server code)
  * aeson - Fast JSON parsing and encoding
  * bytestring - Fast, compact, strict and lazy byte strings with a list interface
  * text - An efficient packed Unicode text type
  * mtl - Monad classes for transformers, using functional dependencies
  * process - Libraries for dealing with system processes
    
## Como executar?
Em um ambiente LINUX:
1. Abra uma janela do terminal
2. Acesse o diretório em que deseja clonar o repo usando `cd caminho_do_diretorio_desejado`
3. Clone este repositório através do comando `git clone https://github.com/juanocv/haskgrademaker.git`
4. Acesse a pasta do projeto através do comando `cd haskgrademaker`
5. Execute utilizando `stack run`
