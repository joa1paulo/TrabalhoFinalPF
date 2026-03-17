module Arquivos where

import Estruturas
import System.IO
import System.Directory (doesFileExist)

-- Carrega o banco de dados do arquivo biblioteca.txt
-- Se o arquivo nao existir, retorna um banco vazio
carregar_banco :: IO BancoDeDados
carregar_banco = do
    let arquivo = "biblioteca.txt"
    exists <- doesFileExist arquivo
    if exists
        then do
            conteudo <- readFile arquivo
            return (read conteudo :: BancoDeDados)
        else do
            return banco_vazio

-- Salva o banco de dados no arquivo biblioteca.txt
-- Comportamento conforme especificado no PDF do trabalho
salvar_banco :: BancoDeDados -> IO ()
salvar_banco bd = do
    let arquivo = "biblioteca.txt"
    exists <- doesFileExist arquivo
    if not exists
        then do
            putStrLn "\narquivo biblioteca.txt nao existe. Deseja criar um novo? (S/N)"
            resposta <- getLine
            case resposta of
                "S" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Ate a proxima."
                "s" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Ate a proxima."
                _ ->
                    putStrLn "Ate a proxima."
        else do
            putStrLn "\nArquivo biblioteca.txt ja existe. Deseja sobresceve-lo? (S/N)"
            resposta <- getLine
            case resposta of
                "S" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Ate a proxima."
                "s" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Ate a proxima."
                _ ->
                    putStrLn "Ate a proxima."
