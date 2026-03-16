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
            id_str <- prompt_str "Informe o codigo unico do item: "
            titulo_str <- prompt_str "Informe o titulo: "
            autor_str <- prompt_str "Informe o autor: "
            ano_str <- prompt_str "Informe o ano: "
            
            exibir_opcoes "Tipo de Midia" ["1 - Livro | 2 - Filme | 3 - Jogo"]
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
            putStrLn "Item removido."
            submenu_itens banco_novo
            
        "3" -> do
            putStrLn "\n--- Lista de Itens ---"
            mapM_ (\i -> putStrLn $ show (id_item i) ++ " - " ++ titulo i) (lista_itens banco)
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
            mapM_ (\u -> putStrLn $ show (matricula u) ++ " - " ++ nome u) (lista_usuarios banco)
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
    exibir_opcoes "Busca" ["1 - Buscar", "2 - Voltar"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            termo <- prompt_str "Termo: "
            let t_min = para_minusculo termo
            let res = filter (\i -> contem_substring t_min (para_minusculo (titulo i)) || 
                                    contem_substring t_min (para_minusculo (autor i))) (lista_itens banco)
            mapM_ (\i -> putStrLn $ show (id_item i) ++ " - " ++ titulo i) res
            submenu_busca banco
        "2" -> menu_principal banco
        _   -> submenu_busca banco

submenu_relatorios :: BancoDeDados -> IO ()
submenu_relatorios banco = do
    exibir_opcoes "Relatorios" ["1 - Emprestados", "2 - Voltar"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            let emp = filter (\i -> not (disponivel i)) (lista_itens banco)
            mapM_ (\i -> putStrLn $ titulo i) emp
            submenu_relatorios banco
        "2" -> menu_principal banco
        _   -> submenu_relatorios banco

submenu_edicao :: BancoDeDados -> IO ()
submenu_edicao banco = do
    exibir_opcoes "Edicao" ["1 - Email Usuario", "2 - Ano Item", "3 - Voltar"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            m <- prompt_str "Matricula: "
            e <- prompt_str "Novo Email: "
            let m_int = read m
            let u_at = map (\u -> if matricula u == m_int then u { email = e } else u) (lista_usuarios banco)
            let log = historico_log banco ++ ["Editou email de " ++ m]
            putStrLn "Email alterado."
            submenu_edicao (banco { lista_usuarios = u_at, historico_log = log })
        "2" -> do
            i <- prompt_str "ID: "
            a <- prompt_str "Novo Ano: "
            let i_int = read i
            let i_at = map (\it -> if id_item it == i_int then it { ano = read a } else it) (lista_itens banco)
            let log = historico_log banco ++ ["Editou ano de " ++ i]
            putStrLn "Ano alterado."
            submenu_edicao (banco { lista_itens = i_at, historico_log = log })
        "3" -> menu_principal banco
        _   -> submenu_edicao banco

submenu_exportacao :: BancoDeDados -> IO ()
submenu_exportacao banco = do
    exibir_opcoes "Exportar" ["1 - Gerar CSV", "2 - Voltar"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> do
            let l_i = map (\i -> "Item," ++ titulo i) (lista_itens banco)
            writeFile "export.csv" (unlines l_i)
            putStrLn "Exportado."
            submenu_exportacao banco
        "2" -> menu_principal banco
        _   -> submenu_exportacao banco

submenu_auditoria :: BancoDeDados -> IO ()
submenu_auditoria banco = do
    exibir_opcoes "Auditoria" ["1 - Logs", "2 - Voltar"]
    opcao <- prompt_str "Opcao: "
    case opcao of
        "1" -> mapM_ putStrLn (historico_log banco) >> submenu_auditoria banco
        "2" -> menu_principal banco
        _   -> submenu_auditoria banco

menu_principal :: BancoDeDados -> IO ()
menu_principal banco = do
    exibir_opcoes "Menu Principal" [
        "1 - Itens", "2 - Usuarios", "3 - Emprestimos", 
        "4 - Busca", "5 - Relatorios", "6 - Edicao", 
        "7 - Exportar", "8 - Auditoria", "0 - Sair"
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
contem_substring [] _ = True
contem_substring _ [] = False
contem_substring sub texto@(t:ts)
    | eh_prefixo sub texto = True
    | otherwise            = contem_substring sub ts

minuscula :: Char -> Char
minuscula c
    | fromEnum c >= 65 && fromEnum c <= 90 = toEnum (fromEnum c + 32)
    | otherwise = c

para_minusculo :: String -> String
para_minusculo s = map minuscula s
