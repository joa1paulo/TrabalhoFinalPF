module Arquivos where

import Estruturas
import System.IO
import System.Directory (doesFileExist)

-- Carregar o banco de dados (Lógica do colega adaptada para o arquivo correto)
carregar_banco :: IO BancoDeDados
carregar_banco = do
    let arquivo = "biblioteca.txt" -- Trocado "backup.txt" para o exigido no PDF
    exists <- doesFileExist arquivo  -- Usando a variavel 'exists' do colega
    if exists
        then do
            conteudo <- readFile arquivo  -- lê o conteúdo do arquivo
            -- Omitimos o print de sucesso aqui para não sujar a tela inicial do menu
            return (read conteudo :: BancoDeDados)  -- converte string de volta para BancoDeDados
        else do
            -- Retorna banco vazio se não existe (como o colega fez)
            return banco_vazio

-- Salvar o banco de dados (Estrutura do colega com os textos obrigatorios do PDF)
salvar_banco :: BancoDeDados -> IO ()
salvar_banco bd = do
    let arquivo = "biblioteca.txt" -- PDF exige esse nome
    exists <- doesFileExist arquivo
    if not exists
        then do
            -- Texto EXATAMENTE igual ao PDF (inicia em minuscula e com acentos)
            putStrLn "\narquivo biblioteca.txt não existe. Deseja criar um novo? (S/N)"
            resposta <- getLine  -- lê a resposta do usuário
            
            -- Usando o 'case' que o colega implementou, adaptado para aceitar S e s
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
            -- Texto EXATAMENTE igual ao PDF (inicia em maiuscula e com acentos)
            putStrLn "\nArquivo biblioteca.txt já existe. Deseja sobrescrevê-lo? (S/N)"
            resposta <- getLine
            
            -- Usando o 'case' do colega aqui também
            case resposta of
                "S" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Até a próxima."
                "s" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Até a próxima."
                _ -> do 
                    putStrLn "Até a próxima."