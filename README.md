# Hask Grade Maker

Projeto da disciplina de **Programação Funcional** do 2º quadrimestre de 2024 da Universidade Federal do ABC.

Realizado por:\
**- JUAN OLIVEIRA DE CARVALHO** [(juanocv)](https://github.com/juanocv)**\
**- FERNANDO GABRIEL CHACON FERNANDES TERUEL DO PRADO** [(gabriel-chacon)](https://github.com/gabriel-chacon)**

## TO-DOs (Parte 01)
- [X] Definir as estruturas de dados
- [X] Realizar o download do JSON do site da UFABC
- [X] Fazer o tratamento de dados do JSON
- [X] Construir as estruturas de dados
- [X] Implementar busca básica em tela de disciplinas
- [X] Imprimir disciplina(s) buscada(s) em tela
- [X] Implementar modularização
- [X] Fazer o tratamento de exceções http
- [X] Adicionar comentários ao código

## TO-DOs (Parte 02)
- [ ] Criar menu interativo para usuário
- [ ] Permitir usuário criar nova grade
- [ ] Permitir usuário selecionar disciplinas
- [ ] Armazenar nova grade do usuário
- [ ] Imprimir nova grade em tela
- [ ] Permitir ao usuário alterar a grade atual
- [ ] Permitir ao usuário remover a grade atual
- [ ] Tratamento de erros e exceções

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
    
## Como executar?
Em um ambiente LINUX com :
1. Abra uma janela do terminal
2. Acesse ao diretório em que deseja clonar o projeto usando `cd caminho_do_diretorio_desejado`
3. Clone este repositório através do comando `git clone https://github.com/juanocv/haskgrademaker.git`
4. Acesse a pasta do projeto através do comando `cd haskgrademaker`
5. Execute utilizando `stack run`
