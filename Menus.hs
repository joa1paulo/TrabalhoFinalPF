-- TESTE !!!!! AINDA VOU ALTERAR

module Menus where

import Emprestimos
import Estruturas
import Cadastros 
import Arquivos (salvar_banco)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

exibir_opcoes :: String -> [String] -> IO ()
exibir_opcoes titulo opcoes = do
    putStrLn "\n=============================="
    putStrLn titulo
    putStrLn "=============================="
    putStrLn (unlines opcoes)

prompt_str :: String -> IO String
prompt_str msg = do 
    putStr msg
    getLine

submenu_itens :: BancoDeDados -> IO ()
submenu_itens banco = do
    exibir_opcoes "Cadastro de Itens" [
        "1 - Adicionar novo item",
        "2 - Remover item",
        "3 - Listar itens cadastrados",
        "4 - Voltar ao menu principal"
        ]
    opcao <- prompt_str "Opcao: "
    
    case opcao of
        "1" -> do
            id_str <- prompt_str "Informe o codigo unico: "
            titulo_str <- prompt_str "Informe o titulo: "
            autor_str <- prompt_str "Informe o autor/diretor: "
            ano_str <- prompt_str "Informe o ano: "
            
            exibir_opcoes "Qual o tipo de midia?" ["1 - Livro | 2 - Filme | 3 - Jogo"]
            tipo_str <- prompt_str "Opcao: "
            
            let tipo_escolhido = case tipo_str of
                                    "2" -> Filme
                                    "3" -> Jogo
                                    _   -> Livro
            
            let id_int = read id_str :: Int
            let ano_int = read ano_str :: Int
            let novo_item = Item id_int titulo_str autor_str ano_int tipo_escolhido True []
            
            let banco_novo = cadastrar_item novo_item banco
            putStrLn "Sucesso! Item cadastrado."
            submenu_itens banco_novo
            
        "2" -> do
            id_str <- prompt_str "ID para remover: "
            let banco_novo = remover_item (read id_str) banco
            putStrLn "Operacao concluida."
            submenu_itens banco_novo
            
        "3" -> do
            putStrLn "\n--- Lista de Itens ---"
            mapM_ (\i -> putStrLn $ show (id_item i) ++ " - " ++ nome_titulo i) (lista_itens banco)
            submenu_itens banco
            
        "4" -> menu_principal banco
        _   -> putStrLn "Opcao invalida!" >> submenu_itens banco

submenu_usuarios :: BancoDeDados -> IO ()
submenu_usuarios banco = do
    exibir_opcoes "Cadastro de Usuarios" [
        "1 - Adicionar novo usuario",
        "2 - Remover usuario",
        "3 - Listar usuarios",
        "4 - Voltar ao menu principal"
        ]
    opcao <- prompt_str "Opcao: "
    
    case opcao of
        "1" -> do
            mat_str <- prompt_str "Matricula: "
            nome_str <- prompt_str "Nome: "
            email_str <- prompt_str "E-mail: "
            
            let novo_user = Usuario (read mat_str) nome_str email_str []
            let banco_novo = cadastrar_usuario novo_user banco
            putStrLn "Usuario cadastrado."
            submenu_usuarios banco_novo
            
        "2" -> do
            mat_str <- prompt_str "Matricula para remover: "
            let banco_novo = remover_usuario (read mat_str) banco
            submenu_usuarios banco_novo
            
        "3" -> do
            mapM_ (\u -> putStrLn $ show (matricula_user u) ++ " - " ++ nome_user u) (lista_usuarios banco)
            submenu_usuarios banco
            
        "4" -> menu_principal banco
        _   -> submenu_usuarios banco

submenu_emprestimos :: BancoDeDados -> IO ()
submenu_emprestimos banco = do
    exibir_opcoes "Emprestimos e Devolucoes" [
        "1 - Registrar emprestimo",
        "2 - Registrar devolucao",
        "3 - Voltar ao menu principal"
        ]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            id_i <- prompt_str "ID Item: "
            mat_u <- prompt_str "Matricula: "
            data_h <- prompt_str "Data: "
            let banco_novo = fazer_emprestimo (read id_i) (read mat_u) data_h banco
            putStrLn "Operacao realizada."
            submenu_emprestimos banco_novo
        "2" -> do
            id_i <- prompt_str "ID Item: "
            mat_u <- prompt_str "Matricula: "
            let banco_novo = fazer_devolucao (read id_i) (read mat_u) banco
            putStrLn "Devolucao realizada."
            submenu_emprestimos banco_novo
        "3" -> menu_principal banco
        _   -> submenu_emprestimos banco

submenu_busca :: BancoDeDados -> IO ()
submenu_busca banco = do
    exibir_opcoes "Busca e Listagem" ["1 - Buscar por titulo ou autor", "2 - Voltar ao menu principal"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            termo <- prompt_str "Informe o termo para busca: "
            let termo_min = para_minusculo termo
            let resultados = filter (\i -> contem_substring termo_min (para_minusculo (nome_titulo i)) || 
                                           contem_substring termo_min (para_minusculo (autor_diretor i))) (lista_itens banco)
            putStrLn "\n--- Resultados ---"
            if null resultados
                then putStrLn "Nenhum item encontrado."
                else mapM_ (\i -> putStrLn $ show (id_item i) ++ " - " ++ nome_titulo i) resultados
            submenu_busca banco
        "2" -> menu_principal banco
        _   -> submenu_busca banco

submenu_relatorios :: BancoDeDados -> IO ()
submenu_relatorios banco = do
    exibir_opcoes "Relatorios e Estatisticas" ["1 - Emprestimos ativos", "2 - Voltar ao menu principal"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            let emprestados = filter (\i -> not (ta_disponivel i)) (lista_itens banco)
            putStrLn "\n--- Itens Atualmente Emprestados ---"
            if null emprestados
                then putStrLn "Nenhum item emprestado."
                else mapM_ (\i -> putStrLn $ "ID: " ++ show (id_item i) ++ " | Titulo: " ++ nome_titulo i) emprestados
            submenu_relatorios banco
        "2" -> menu_principal banco
        _   -> submenu_relatorios banco

submenu_edicao :: BancoDeDados -> IO ()
submenu_edicao banco = do
    exibir_opcoes "Edicao de Dados" [
        "1 - Editar e-mail de usuario",
        "2 - Editar ano de um item",
        "3 - Voltar ao menu principal"
        ]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            mat_str <- prompt_str "Matricula do usuario: "
            novo_email <- prompt_str "Novo e-mail: "
            let mat_int = read mat_str :: Int
            let users_atualizados = map (\u -> if matricula_user u == mat_int then u { email_user = novo_email } else u) (lista_usuarios banco)
            let novo_log = historico_log banco ++ ["Editou email de " ++ mat_str]
            putStrLn "Sucesso!"
            submenu_edicao (banco { lista_usuarios = users_atualizados, historico_log = novo_log })
        "2" -> do
            id_str <- prompt_str "ID do item: "
            novo_ano_str <- prompt_str "Novo ano: "
            let id_int = read id_str :: Int
            let itens_atualizados = map (\i -> if id_item i == id_int then i { ano_lancamento = read novo_ano_str } else i) (lista_itens banco)
            let novo_log = historico_log banco ++ ["Editou ano do item " ++ id_str]
            putStrLn "Sucesso!"
            submenu_edicao (banco { lista_itens = itens_atualizados, historico_log = novo_log })
        "3" -> menu_principal banco
        _   -> submenu_edicao banco

submenu_exportacao :: BancoDeDados -> IO ()
submenu_exportacao banco = do
    exibir_opcoes "Exportacao de Dados" ["1 - Exportar banco para CSV", "2 - Voltar ao menu principal"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            let linhas_itens = map (\i -> "Item," ++ show (id_item i) ++ "," ++ nome_titulo i) (lista_itens banco)
            writeFile "exportacao.csv" (unlines linhas_itens)
            putStrLn "Sucesso! Dados exportados."
            submenu_exportacao banco
        "2" -> menu_principal banco
        _   -> submenu_exportacao banco

submenu_auditoria :: BancoDeDados -> IO ()
submenu_auditoria banco = do
    exibir_opcoes "Auditoria" ["1 - Exibir logs", "2 - Voltar"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            putStrLn "\n--- Log de Operacoes ---"
            mapM_ putStrLn (historico_log banco)
            _ <- prompt_str "\nEnter para continuar..."
            submenu_auditoria banco
        "2" -> menu_principal banco
        _   -> submenu_auditoria banco

menu_principal :: BancoDeDados -> IO ()
menu_principal banco = do
    exibir_opcoes "Menu Principal" [
        "1 - Itens", "2 - Usuarios", "3 - Emprestimos", 
        "4 - Busca", "5 - Relatorios", "6 - Edicao", 
        "7 - Exportar", "8 - Auditoria", "0 - Salvar e Sair"
        ]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> submenu_itens banco
        "2" -> submenu_usuarios banco
        "3" -> submenu_emprestimos banco
        "4" -> submenu_busca banco
        "5" -> submenu_relatorios banco
        "6" -> submenu_edicao banco
        "7" -> submenu_exportacao banco
        "8" -> submenu_auditoria banco
        "0" -> salvar_banco banco
        _   -> menu_principal banco

eh_prefixo :: String -> String -> Bool
eh_prefixo [] _ = True
eh_prefixo _ [] = False
eh_prefixo (x:xs) (y:ys) = x == y && eh_prefixo xs ys

contem_substring :: String -> String -> Bool
contem_substring sub texto@(t:ts)
    | eh_prefixo sub texto = True
    | otherwise            = contem_substring sub ts
contem_substring _ [] = False

para_minusculo :: String -> String
para_minusculo s = map (\c -> if fromEnum c >= 65 && fromEnum c <= 90 then toEnum (fromEnum c + 32) else c) s
