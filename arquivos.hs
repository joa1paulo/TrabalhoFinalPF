module Arquivos where

import Estruturas
import System.IO
import Data.List (intercalate)
import System.Directory (doesFileExist)


-- Função auxiliar para converter Item para linha CSV
itemParaCSV :: Item -> String
itemParaCSV item = intercalate "," [
    show (id_item item), --converte int para string
    nome_titulo item, --nao usa show pq ja é string
    autor_diretor item, --nao usa show pq ja é string
    show (ano_lancamento item),
    show (tipo_midia item),
    show (ta_disponivel item),
    show (fila_espera item)
    ]

-- Função auxiliar para converter Usuario para linha CSV
usuarioParaCSV :: Usuario -> String
usuarioParaCSV user = intercalate "," [
    show (matricula_user user),
    nome_user user, --nao usa show pq ja é string
    email_user user, --nao usa show pq ja é string
    show (meus_emprestimos user)
    ] -- a função intercalate junta os elementos os separando por ,

-- Exportar itens e usuários para CSV
ArquivarCSV :: BancoDeDados -> IO () -- usa IO pois vai escrever em arquivos, gerando efeitos colaterais
ArquivarCSV bd = do
    let itensCSV = map itemParaCSV (lista_itens bd) --transforma cada item  da lista de itens em uma linha CSV
    let usuariosCSV = map usuarioParaCSV (lista_usuarios bd) --transforma cada usuario da lista de usuarios em uma linha CSV
    writeFile "itens.csv" (unlines ("id,nome,autor,ano,tipo,disponivel,fila" : itensCSV))
    writeFile "usuarios.csv" (unlines ("matricula,nome,email,emprestimos" : usuariosCSV))

-- Exportar itens e usuários para CSV com usuario escolhendo o nome do arquivo
-- ArquivarCSV :: BancoDeDados -> IO () -- usa IO pois vai escrever em arquivos, gerando efeitos colaterais
-- ArquivarCSV bd = do
--   let itensCSV = map itemParaCSV (lista_itens bd) --transforma cada item  da lista de itens em uma linha CSV
--   let usuariosCSV = map usuarioParaCSV (lista_usuarios bd) --transforma cada usuario da lista de usuarios em uma linha CSV
--   putStrLn "Digite o nome do arquivo para salvar os itens (ex: itens.csv):"
--   nomeArquivoItem <- getLine
--   WriteFile nomeArquivoItem (unlines ("id,nome,autor,ano,tipo,disponivel,fila" : itensCSV))
--   putStrLn "Digite o nome do arquivo para salvar os usuários (ex: usuarios.csv):"
--   nomeArquivoUsuario <- getLine
--   writeFile nomeArquivoUsuario (unlines ("matricula,nome,email,emprestimos" : usuariosCSV))

-- Função auxiliar para parsear linha CSV para Item
csvParaItem :: String -> Item -- converte cada string que está ordenada, no formato CSV, para um item.
csvParaItem linha = case splitOn ',' linha of --a função spliton faz o contrário da intercalate e o case é para garantir que a linha toda tem o numero "certo" de espaços 
    [idStr, nome, autor, anoStr, tipoStr, dispStr, filaStr] ->
        Item (read idStr) nome autor (read anoStr) (read tipoStr) (read dispStr) (read filaStr) -- converte cada string separarada para seu devido tipo.
    _ -> error "Formato CSV inválido para Item" -- se o case falhar , a linha está no formato errado.

-- Função auxiliar para parsear linha CSV para Usuario
csvParaUsuario :: String -> Usuario -- converte cada string que está ordenada, no formato CSV, para um usuario.
csvParaUsuario linha = case splitOn ',' linha of --a função spliton faz o contrário da intercalate e o case é para garantir que a linha toda tem o numero "certo" de espaços 
    [matStr, nome, email, empStr] ->
        Usuario (read matStr) nome email (read empStr)
    _ -> error "Formato CSV inválido para Usuario" -- se o case falhar , a linha está no formato errado.

-- Importar de CSV
importarCSV :: IO BancoDeDados
importarCSV = do
    itensLinhas <- readFile "itens.csv" >>= return . tail . lines  -- pula header
    usuariosLinhas <- readFile "usuarios.csv" >>= return . tail . lines
    let itens = map csvParaItem itensLinhas -- transforma csv em itens com tipos adequados
    let usuarios = map csvParaUsuario usuariosLinhas -- transforma csv em usuarios com tipos adequados
    return (BancoDeDados itens usuarios [] [])  -- emprestimos e log vazios devem ser colocado aqui, quando estiver pronto

-- Salvar logs de auditoria em um arquivo separado (Função de log não implementada ainda)
-- salvarLogs :: BancoDeDados -> IO ()
-- salvarLogs bd = writeFile "auditoria.txt" (unlines (historico_log bd))

-- Salvar o banco de dados (estado atual do sistema) no arquivo backup.txt
salvarBanco :: BancoDeDados -> IO ()
salvarBanco bd = do
    let arquivo = "backup.txt"
    exists <- doesFileExist arquivo
    if exists
        then do
            putStrLn "Backup atualizado com sucesso!"
            writeFile arquivo (show bd) --converte o banco de dados para string e salva no arquivo
        else do
            putStrLn "O arquivo de backup não existe. Deseja criar um novo backup? (s/n)"
            resposta <- getLine
            case resposta of
                "s" -> do
                    writeFile arquivo (show bd)
                    putStrLn "Backup criado com sucesso!"
                "n" -> putStrLn "Operação de backup cancelada."
                 _  -> putStrLn "Resposta inválida.Cancelando Operação de Backup."


-- Carregar o banco de dados do arquivo backup.txt
carregarBanco :: IO BancoDeDados
carregarBanco = do
    let arquivo = "backup.txt"
    exists <- doesFileExist arquivo
    if exists
        then do
            conteudo <- readFile arquivo
            putStrLn "Banco de dados carregado com sucesso!"
            return (read conteudo)
            
        else do
            putStrLn "O arquivo de backup não existe."
            return (BancoDeDados [] [] [] [])