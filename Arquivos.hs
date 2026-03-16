module Arquivos where

import Estruturas
import System.IO
import System.Directory (doesFileExist)
import Text.read (readMaybe)

-- função auxiliar para substituir Intercalate que vem de uma biblioteca externa.
meuIntercalate :: String -> [String] -> String
meuIntercalate _ [] = ""  -- caso base: lista vazia retorna string vazia
meuIntercalate _ [x] = x  -- caso base: lista com um elemento retorna o elemento
meuIntercalate sep (x:xs) = x ++ sep ++ meuIntercalate sep xs  -- recursão: concatena elemento, separador e recursão no resto

-- função auxiliar para substituir SplitOn que vem de uma biblioteca externa.
meuSplitOn :: Char -> String -> [String]
meuSplitOn _ "" = []  -- caso base: string vazia retorna lista vazia
meuSplitOn c s = 
    let (antes, depois) = break (== c) s  -- quebra a string no primeiro caractere separador
    in antes : case depois of
                 [] -> []  -- se não há resto, termina a lista
                 (_:resto) -> meuSplitOn c resto  -- recursão no resto da string

-- Função auxiliar para converter Item para linha CSV
itemParaCSV :: Item -> String
itemParaCSV item = meuIntercalate "," [
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
usuarioParaCSV user = meuIntercalate "," [
    show (matricula_user user),
    nome_user user, --nao usa show pq ja é string
    email_user user, --nao usa show pq ja é string
    show (meus_emprestimos user)
    ] -- a função meuIntercalate junta os elementos os separando por ,

-- Exportar itens e usuários para CSV
ArquivarCSV :: BancoDeDados -> IO () -- usa IO pois vai escrever em arquivos, gerando efeitos colaterais
ArquivarCSV bd = do
    let itensCSV = map itemParaCSV (lista_itens bd) --transforma cada item  da lista de itens em uma linha CSV
    let usuariosCSV = map usuarioParaCSV (lista_usuarios bd) --transforma cada usuario da lista de usuarios em uma linha CSV
    writeFile "itens.csv" (unlines ("id,nome,autor,ano,tipo,disponivel,fila" : itensCSV))  -- escreve header e itens no arquivo CSV
    writeFile "usuarios.csv" (unlines ("matricula,nome,email,emprestimos" : usuariosCSV))  -- escreve header e usuários no arquivo CSV

-- Exportar itens e usuários para CSV com usuario escolhendo o nome do arquivo
ArquivarCSVcustom :: BancoDeDados -> IO () -- usa IO pois vai escrever em arquivos, gerando efeitos colaterais
ArquivarCSVcustom bd = do
    let itensCSV = map itemParaCSV (lista_itens bd) --transforma cada item  da lista de itens em uma linha CSV
    let usuariosCSV = map usuarioParaCSV (lista_usuarios bd) --transforma cada usuario da lista de usuarios em uma linha CSV
    putStrLn "Digite o nome do arquivo para salvar os itens (ex: itens.csv):"
    nomeArquivoItem <- getLine  -- lê o nome do arquivo para itens do usuário
    writeFile nomeArquivoItem (unlines ("id,nome,autor,ano,tipo,disponivel,fila" : itensCSV))  -- escreve itens no arquivo escolhido
    putStrLn "Digite o nome do arquivo para salvar os usuários (ex: usuarios.csv):"
    nomeArquivoUsuario <- getLine  -- lê o nome do arquivo para usuários do usuário
    writeFile nomeArquivoUsuario (unlines ("matricula,nome,email,emprestimos" : usuariosCSV))  -- escreve usuários no arquivo escolhido

-- Função auxiliar para parsear linha CSV para Item
csvParaItem :: String -> Maybe Item -- converte cada string que está ordenada, no formato CSV, para um item.
csvParaItem linha = case meuSplitOn ',' linha of --a função meuSplitOn faz o contrário da meuIntercalate e o case é para garantir que a linha toda tem o numero "certo" de espaços 
    [idStr, nome, autor, anoStr, tipoStr, dispStr, filaStr] ->
        Just (Item (readMaybe idStr >>= \x -> Just x) nome autor (readMaybe anoStr >>= \x -> Just x) (readMaybe tipoStr >>= \x -> Just x) (readMaybe dispStr >>= \x -> Just x) (readMaybe filaStr >>= \x -> Just x  )) -- converte cada string separarada para seu devido tipo.
    _ -> Nothing -- se o case falhar , a linha está no formato errado.

-- Função auxiliar para parsear linha CSV para Usuario
csvParaUsuario :: String -> Maybe Usuario -- converte cada string que está ordenada, no formato CSV, para um usuario.
csvParaUsuario linha = case meuSplitOn ',' linha of --a função meuSplitOn faz o contrário da meuIntercalate e o case é para garantir que a linha toda tem o numero "certo" de espaços 
    [matStr, nome, email, empStr] ->
        Just (Usuario (readMaybe matStr >>= \x -> Just x) nome email (readMaybe empStr >>= \x -> Just x))
    _ -> Nothing -- se o case falhar , a linha está no formato errado.

-- Importar de CSV
importarCSV :: IO BancoDeDados
importarCSV = do
    itensLinhas <- readFile "itens.csv" >>= return . tail . lines  -- pula header
    usuariosLinhas <- readFile "usuarios.csv" >>= return . tail . lines
    let itens = map csvParaItem itensLinhas -- transforma csv em itens com tipos adequados
    let usuarios = map csvParaUsuario usuariosLinhas -- transforma csv em usuarios com tipos adequados
    return (BancoDeDados itens usuarios [] [])  -- emprestimos e log vazios devem ser colocado aqui, quando estiver pronto

-- Salvar o banco de dados (estado atual do sistema) no arquivo backup.txt
salvarBanco :: BancoDeDados -> IO ()
salvarBanco bd = do
    let arquivo = "backup.txt"
    exists <- doesFileExist arquivo  -- verifica se o arquivo de backup existe
    if exists
        then do
            putStrLn "Backup atualizado com sucesso!"
            writeFile arquivo (show bd) --converte o banco de dados para string e salva no arquivo
        else do
            putStrLn "O arquivo de backup não existe. Deseja criar um novo backup? (s/n)"
            resposta <- getLine  -- lê a resposta do usuário
            case resposta of
                "s" -> do
                    writeFile arquivo (show bd)  -- cria e escreve o backup
                    putStrLn "Backup criado com sucesso!"
                "n" -> putStrLn "Operação de backup cancelada."
                 _  -> putStrLn "Resposta inválida.Cancelando Operação de Backup."


-- Carregar o banco de dados do arquivo backup.txt
carregarBanco :: IO BancoDeDados
carregarBanco = do
    let arquivo = "backup.txt"
    exists <- doesFileExist arquivo  -- verifica se o arquivo existe
    if exists
        then do
            conteudo <- readFile arquivo  -- lê o conteúdo do arquivo
            putStrLn "Banco de dados carregado com sucesso!"
            return (read conteudo)  -- converte string de volta para BancoDeDados
            
        else do
            putStrLn "O arquivo de backup não existe."
            return (BancoDeDados [] [] [] [])  -- retorna banco vazio se não existe

-- Função para o usuario escolher qual arquivo especifico quer exportar (itens, usuarios, emprestimos ou log) em um arquivo específico
ExportarCSVespecifico :: BancoDeDados -> IO ()
ExportarCSVespecifico bd = do
    putStrLn "O que você deseja exportar? (itens/usuarios/emprestimos/log)"
    escolha <- getLine  -- lê a escolha do usuário
    case escolha of
        "itens" -> do
            let itensCSV = map itemParaCSV (lista_itens bd)  -- converte itens para CSV
            putStrLn "Digite o nome do arquivo para salvar os itens (ex: itens.csv):"
            nomeArquivoItem <- getLine  -- lê nome do arquivo
            writeFile nomeArquivoItem (unlines ("id,nome,autor,ano,tipo,disponivel,fila" : itensCSV))  -- escreve itens
            putStrLn "Itens exportados com sucesso!"
        "usuarios" -> do
            let usuariosCSV = map usuarioParaCSV (lista_usuarios bd)  -- converte usuários para CSV
            putStrLn "Digite o nome do arquivo para salvar os usuários (ex: usuarios.csv):"
            nomeArquivoUsuario <- getLine  -- lê nome do arquivo
            writeFile nomeArquivoUsuario (unlines ("matricula,nome,email,emprestimos" : usuariosCSV))  -- escreve usuários
            putStrLn "Usuários exportados com sucesso!"
        "emprestimos" -> do
            let emprestimosCSV = map emprestimoParaCSV (lista_emprestimos bd)  -- converte empréstimos para CSV
            putStrLn "Digite o nome do arquivo para salvar os empréstimos (ex:  emprestimos.csv):"
            nomeArquivoEmprestimo <- getLine  -- lê nome do arquivo
            writeFile nomeArquivoEmprestimo (unlines ("id_usuario,id_item,data_emprestimo,data_devolucao" : emprestimosCSV))  -- escreve empréstimos
            putStrLn "Empréstimos exportados com sucesso!"
        "log" -> do
            let logCSV = map logEntryParaCSV (lista_log bd)  -- converte log para CSV
            putStrLn "Digite o nome do arquivo para salvar o log (ex: log.csv):"
            nomeArquivoLog <- getLine  -- lê nome do arquivo
            writeFile nomeArquivoLog (unlines ("timestamp,acao,detalhes" : logCSV))  -- escreve log
            putStrLn "Log exportado com sucesso!"
        _ -> putStrLn "Opção inválida. Por favor, escolha entre itens, usuarios, emprestimos ou log."
