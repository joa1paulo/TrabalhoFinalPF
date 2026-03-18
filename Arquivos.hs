module Arquivos where

import Estruturas
import System.IO
import System.Directory (doesFileExist)

-- Carregar o banco de dados 
carregar_banco :: IO BancoDeDados
carregar_banco = do
    let arquivo = "biblioteca.txt" -- Trouei "backup.txt" para o exigido no PDF
    exists <- doesFileExist arquivo  
    if exists
        then do
            conteudo <- readFile arquivo  -- ler arquivo           
            return (read conteudo :: BancoDeDados)  -- string volta a ser BancoDeDados
        else do
            return banco_vazio

salvar_banco :: BancoDeDados -> IO ()
salvar_banco bd = do
    let arquivo = "biblioteca.txt" 
    exists <- doesFileExist arquivo
    if not exists
        then do
            putStrLn "\narquivo biblioteca.txt não existe. Deseja criar um novo? (S/N)"
            resposta <- getLine 
            
            case resposta of
                "S" -> do
                    writeFile arquivo (show bd)  -- cria e escreve o backup
                    putStrLn "Até a próxima."
                "s" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Até a próxima."
                _ -> do 
                    putStrLn "Até a próxima."
        else do
            putStrLn "\nArquivo biblioteca.txt já existe. Deseja sobrescrevê-lo? (S/N)"
            resposta <- getLine
            
            case resposta of
                "S" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Até a próxima."
                "s" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Até a próxima."
                _ -> do 
                    putStrLn "Até a próxima."