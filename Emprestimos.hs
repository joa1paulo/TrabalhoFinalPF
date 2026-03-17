module Emprestimos where

import Estruturas

-- Auxiliar para pegar o nome do tipo em minusculo
tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo = "jogo"

ja_ta_na_fila :: Int -> [Int] -> Bool
ja_ta_na_fila _ [] = False
ja_ta_na_fila mat_alvo (x:xs)
    | mat_alvo == x = True
    | otherwise     = ja_ta_na_fila mat_alvo xs

fazer_emprestimo :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
fazer_emprestimo momento id_item_alvo mat_user banco =
    let
        -- pegar o titulo e o tipo pro log
        item_alvo = head (filter (\i -> id_item i == id_item_alvo) (lista_itens banco))
        titulo_item = titulo item_alvo
        tipo_item = tipo_str (tipo item_alvo)
        
        novo_emp = Emprestimo id_item_alvo mat_user momento
        nova_lista_emp = lista_emprestimos banco ++ [novo_emp]
        nova_lista_itens = map (\i -> if id_item i == id_item_alvo then i { ta_disponivel = False } else i) (lista_itens banco)
        nova_lista_users = map (\u -> if matricula_user u == mat_user then u { meus_emprestimos = meus_emprestimos u ++ [id_item_alvo] } else u) (lista_usuarios banco)
        
        -- Formatação 
        desc = "Empréstimo: " ++ tipo_item ++ " \"" ++ titulo_item ++ "\" para matrícula \"" ++ show mat_user ++ "\""
        log_op = LogOperacao momento desc (show mat_user) Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in 
        banco { lista_emprestimos = nova_lista_emp, lista_itens = nova_lista_itens, lista_usuarios = nova_lista_users, historico_operacoes = novo_log }

fazer_devolucao :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
fazer_devolucao momento id_item_alvo mat_user banco =
    let
        item_alvo = head (filter (\i -> id_item i == id_item_alvo) (lista_itens banco))
        titulo_item = titulo item_alvo
        tipo_item = tipo_str (tipo item_alvo)
        
        nova_lista_emp = filter (\e -> not (id_item_emp e == id_item_alvo && mat_user_emp e == mat_user)) (lista_emprestimos banco)
        nova_lista_itens = map (\i -> if id_item i == id_item_alvo then i { ta_disponivel = True } else i) (lista_itens banco)
        nova_lista_users = map (\u -> if matricula_user u == mat_user 
                                      then u { meus_emprestimos = filter (/= id_item_alvo) (meus_emprestimos u) } 
                                      else u) (lista_usuarios banco)
                                      
        desc = "Devolução: " ++ tipo_item ++ " \"" ++ titulo_item ++ "\" da matrícula \"" ++ show mat_user ++ "\""
        log_op = LogOperacao momento desc (show mat_user) Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in 
        banco { lista_emprestimos = nova_lista_emp, lista_itens = nova_lista_itens, lista_usuarios = nova_lista_users, historico_operacoes = novo_log }

renovar_emprestimo :: String -> Int -> Int -> String -> BancoDeDados -> BancoDeDados
renovar_emprestimo momento id_item_alvo mat_user nova_data banco =
    let
        item_alvo = head (filter (\i -> id_item i == id_item_alvo) (lista_itens banco))
        
        nova_lista_emp = map (\e -> if id_item_emp e == id_item_alvo && mat_user_emp e == mat_user
                                    then e { data_emp = nova_data } 
                                    else e) (lista_emprestimos banco)
                                    
        desc = "Renovação: " ++ tipo_str (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" para matrícula \"" ++ show mat_user ++ "\""
        log_op = LogOperacao momento desc (show mat_user) Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in
        banco { lista_emprestimos = nova_lista_emp, historico_operacoes = novo_log }

emprestimo_lote :: String -> [Int] -> Int -> BancoDeDados -> BancoDeDados
emprestimo_lote momento ids mat_user banco =
    foldl (\b id_alvo -> fazer_emprestimo momento id_alvo mat_user b) banco ids

devolucao_lote :: String -> [Int] -> Int -> BancoDeDados -> BancoDeDados
devolucao_lote momento ids mat_user banco =
    foldl (\b id_alvo -> fazer_devolucao momento id_alvo mat_user b) banco ids

    -- coloca na fila de espera
adicionar_fila_espera :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
adicionar_fila_espera momento id_item_alvo mat_user banco =
    let nova_lista_itens = map (\i -> if id_item i == id_item_alvo then i { fila_espera = fila_espera i ++ [mat_user] } else i) (lista_itens banco)
        
        --  nome do item par pro log
        item_alvo = head (filter (\i -> id_item i == id_item_alvo) (lista_itens banco))
        desc = "Entrou na fila de espera: " ++ tipo_str (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" (matrícula \"" ++ show mat_user ++ "\")"
        
        log_op = LogOperacao momento desc (show mat_user) Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in banco { lista_itens = nova_lista_itens, historico_operacoes = novo_log }