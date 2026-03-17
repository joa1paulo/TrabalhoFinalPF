module Edicao where

import Estruturas
import Data.List (sortOn)


-- FUNÇÕES DE EDIÇÃO DE ITENS


editar_titulo_item :: String -> Int -> String -> BancoDeDados -> BancoDeDados
editar_titulo_item momento id_alvo novo_titulo banco =
    let itens_atualizados = map (\i -> if id_item i == id_alvo then i { titulo = novo_titulo } else i) (lista_itens banco)
        item_antigo = head (filter (\i -> id_item i == id_alvo) (lista_itens banco))
        
        log_ed = LogEdicao momento ("Item ID " ++ show id_alvo) ("Titulo: " ++ titulo item_antigo) ("Titulo: " ++ novo_titulo) "Sistema"
        novo_hist_ed = historico_edicoes banco ++ [log_ed]
        
        -- salvar no Log Geral
        log_op = LogOperacao momento ("Edição item: código \"" ++ show id_alvo ++ "\"") "Sistema" Sucesso ""
        novo_hist_op = historico_operacoes banco ++ [log_op]
        
    in banco { lista_itens = itens_atualizados, historico_edicoes = novo_hist_ed, historico_operacoes = novo_hist_op }

editar_autor_item :: String -> Int -> String -> BancoDeDados -> BancoDeDados
editar_autor_item momento id_alvo novo_autor banco =
    let itens_atualizados = map (\i -> if id_item i == id_alvo then i { autor = novo_autor } else i) (lista_itens banco)
        item_antigo = head (filter (\i -> id_item i == id_alvo) (lista_itens banco))
        
        log_ed = LogEdicao momento ("Item ID " ++ show id_alvo) ("Autor: " ++ autor item_antigo) ("Autor: " ++ novo_autor) "Sistema"
        novo_hist_ed = historico_edicoes banco ++ [log_ed]
        
        log_op = LogOperacao momento ("Edição item: código \"" ++ show id_alvo ++ "\"") "Sistema" Sucesso ""
        novo_hist_op = historico_operacoes banco ++ [log_op]
        
    in banco { lista_itens = itens_atualizados, historico_edicoes = novo_hist_ed, historico_operacoes = novo_hist_op }

editar_ano_item :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
editar_ano_item momento id_alvo novo_ano banco =
    let itens_atualizados = map (\i -> if id_item i == id_alvo then i { ano = novo_ano } else i) (lista_itens banco)
        item_antigo = head (filter (\i -> id_item i == id_alvo) (lista_itens banco))
        
        log_ed = LogEdicao momento ("Item ID " ++ show id_alvo) ("Ano: " ++ show (ano item_antigo)) ("Ano: " ++ show novo_ano) "Sistema"
        novo_hist_ed = historico_edicoes banco ++ [log_ed]
        
        log_op = LogOperacao momento ("Edição item: código \"" ++ show id_alvo ++ "\"") "Sistema" Sucesso ""
        novo_hist_op = historico_operacoes banco ++ [log_op]
        
    in banco { lista_itens = itens_atualizados, historico_edicoes = novo_hist_ed, historico_operacoes = novo_hist_op }



-- FUNÇÕES DE EDIÇÃO DE USUÁRIOS


editar_nome_usuario :: String -> Int -> String -> BancoDeDados -> BancoDeDados
editar_nome_usuario momento mat_alvo novo_nome banco =
    let users_atualizados = map (\u -> if matricula_user u == mat_alvo then u { nome_user = novo_nome } else u) (lista_usuarios banco)
        user_antigo = head (filter (\u -> matricula_user u == mat_alvo) (lista_usuarios banco))
        
        log_ed = LogEdicao momento ("Usuario mat " ++ show mat_alvo) ("Nome: " ++ nome_user user_antigo) ("Nome: " ++ novo_nome) "Sistema"
        novo_hist_ed = historico_edicoes banco ++ [log_ed]
        
        -- salvar no log geral
        log_op = LogOperacao momento ("Edição usuário: matrícula \"" ++ show mat_alvo ++ "\"") "Sistema" Sucesso ""
        novo_hist_op = historico_operacoes banco ++ [log_op]
        
    in banco { lista_usuarios = users_atualizados, historico_edicoes = novo_hist_ed, historico_operacoes = novo_hist_op }

editar_email_usuario :: String -> Int -> String -> BancoDeDados -> BancoDeDados
editar_email_usuario momento mat_alvo novo_email banco =
    let users_atualizados = map (\u -> if matricula_user u == mat_alvo then u { email_user = novo_email } else u) (lista_usuarios banco)
        user_antigo = head (filter (\u -> matricula_user u == mat_alvo) (lista_usuarios banco))
        
        log_ed = LogEdicao momento ("Usuario mat " ++ show mat_alvo) ("Email: " ++ email_user user_antigo) ("Email: " ++ novo_email) "Sistema"
        novo_hist_ed = historico_edicoes banco ++ [log_ed]
        
        log_op = LogOperacao momento ("Edição usuário: matrícula \"" ++ show mat_alvo ++ "\"") "Sistema" Sucesso ""
        novo_hist_op = historico_operacoes banco ++ [log_op]
        
    in banco { lista_usuarios = users_atualizados, historico_edicoes = novo_hist_ed, historico_operacoes = novo_hist_op }


-- BUSCA BINARIA RECURSIVA DIRETA 


busca_binaria_item :: Int -> [Item] -> Int -> Int -> [Item]
busca_binaria_item alvo itens inicio fim
    | inicio > fim = []
    | id_item meio_item == alvo = [meio_item]
    | id_item meio_item < alvo  = busca_binaria_item alvo itens (meio + 1) fim
    | otherwise                 = busca_binaria_item alvo itens inicio (meio - 1)
    where
        meio = (inicio + fim) `div` 2
        meio_item = itens !! meio

busca_binaria_user :: Int -> [Usuario] -> Int -> Int -> [Usuario]
busca_binaria_user alvo users inicio fim
    | inicio > fim = []
    | matricula_user meio_user == alvo = [meio_user]
    | matricula_user meio_user < alvo  = busca_binaria_user alvo users (meio + 1) fim
    | otherwise                        = busca_binaria_user alvo users inicio (meio - 1)
    where
        meio = (inicio + fim) `div` 2
        meio_user = users !! meio

buscar_item_para_edicao :: Int -> BancoDeDados -> [Item]
buscar_item_para_edicao id_alvo banco =
    let itens_ordenados = sortOn id_item (lista_itens banco)
    in busca_binaria_item id_alvo itens_ordenados 0 (length itens_ordenados - 1)

buscar_user_para_edicao :: Int -> BancoDeDados -> [Usuario]
buscar_user_para_edicao mat_alvo banco =
    let users_ordenados = sortOn matricula_user (lista_usuarios banco)
    in busca_binaria_user mat_alvo users_ordenados 0 (length users_ordenados - 1)