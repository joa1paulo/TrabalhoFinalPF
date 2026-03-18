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

fazer_emprestimo :: String -> String -> Int -> Int -> BancoDeDados -> BancoDeDados
fazer_emprestimo momento data_dev id_item_alvo mat_user banco =
    let user_busca = filter (\usuario -> matricula_user usuario == mat_user) (lista_usuarios banco)
        item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
    in if null user_busca || null item_busca
       then let 
           motivo = if null user_busca then "Usuário não encontrado" else "Item não encontrado"
           log_erro = LogOperacao momento ("Tentativa de empréstimo (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro motivo
           novo_log = historico_operacoes banco ++ [log_erro]
       in banco { historico_operacoes = novo_log }
       
       else let 
           item_alvo = head item_busca
       in if not (ta_disponivel item_alvo)
          then let 
              log_erro = LogOperacao momento ("Tentativa de empréstimo (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro "Item já está emprestado"
              novo_log = historico_operacoes banco ++ [log_erro]
          in banco { historico_operacoes = novo_log }
          
          else let
              titulo_item = titulo item_alvo
              tipo_item = tipo_str (tipo item_alvo)
              -- ATUALIZADO com data_dev
              novo_emp = Emprestimo id_item_alvo mat_user momento data_dev
              nova_lista_emp = lista_emprestimos banco ++ [novo_emp]
              nova_lista_itens = map (\item -> if id_item item == id_item_alvo then item { ta_disponivel = False } else item) (lista_itens banco)
              nova_lista_users = map (\usuario -> if matricula_user usuario == mat_user then usuario { meus_emprestimos = meus_emprestimos usuario ++ [id_item_alvo] } else usuario) (lista_usuarios banco)
              
              desc = "Empréstimo: " ++ tipo_item ++ " \"" ++ titulo_item ++ "\" para matrícula \"" ++ show mat_user ++ "\""
              log_op = LogOperacao momento desc (show mat_user) Sucesso ""
              novo_log = historico_operacoes banco ++ [log_op]
          in banco { lista_emprestimos = nova_lista_emp, lista_itens = nova_lista_itens, lista_usuarios = nova_lista_users, historico_operacoes = novo_log }


fazer_devolucao :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
fazer_devolucao momento id_item_alvo mat_user banco =
    let item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
        emp_busca = filter (\emprestimo -> id_item_emp emprestimo == id_item_alvo && mat_user_emp emprestimo == mat_user) (lista_emprestimos banco)
    
    in if null item_busca || null emp_busca
       then let
           -- LOGA O ERRO DE DEVOLUCAO FALSA
           motivo = if null item_busca then "Item não encontrado" else "Empréstimo não existe para este usuário"
           log_erro = LogOperacao momento ("Tentativa de devolução (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro motivo
           novo_log = historico_operacoes banco ++ [log_erro]
       in banco { historico_operacoes = novo_log }
       
       else let
           item_alvo = head item_busca
           titulo_item = titulo item_alvo
           tipo_item = tipo_str (tipo item_alvo)
           
           nova_lista_emp = filter (\emprestimo -> not (id_item_emp emprestimo == id_item_alvo && mat_user_emp emprestimo == mat_user)) (lista_emprestimos banco)
           nova_lista_itens = map (\item -> if id_item item == id_item_alvo then item { ta_disponivel = True } else item) (lista_itens banco)
           nova_lista_users = map (\usuario -> if matricula_user usuario == mat_user 
                                         then usuario { meus_emprestimos = filter (/= id_item_alvo) (meus_emprestimos usuario) } 
                                         else usuario) (lista_usuarios banco)
                                         
           desc = "Devolução: " ++ tipo_item ++ " \"" ++ titulo_item ++ "\" da matrícula \"" ++ show mat_user ++ "\""
           log_op = LogOperacao momento desc (show mat_user) Sucesso ""
           novo_log = historico_operacoes banco ++ [log_op]
       in banco { lista_emprestimos = nova_lista_emp, lista_itens = nova_lista_itens, lista_usuarios = nova_lista_users, historico_operacoes = novo_log }


renovar_emprestimo :: String -> Int -> Int -> String -> BancoDeDados -> BancoDeDados
renovar_emprestimo momento id_item_alvo mat_user nova_data banco =
    let item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
        emp_busca = filter (\emprestimo -> id_item_emp emprestimo == id_item_alvo && mat_user_emp emprestimo == mat_user) (lista_emprestimos banco)
    in if null item_busca || null emp_busca 
       then let
           motivo = if null item_busca then "Item não encontrado" else "Empréstimo não existe"
           log_erro = LogOperacao momento ("Tentativa de renovação (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro motivo
           novo_log = historico_operacoes banco ++ [log_erro]
       in banco { historico_operacoes = novo_log }
       else let
           item_alvo = head item_busca
           -- CORRIGIDO: Agora ele atualiza a data_devolucao em vez da data_emp!
           nova_lista_emp = map (\emprestimo -> if id_item_emp emprestimo == id_item_alvo && mat_user_emp emprestimo == mat_user
                                       then emprestimo { data_devolucao = nova_data } 
                                       else emprestimo) (lista_emprestimos banco)
                                       
           desc = "Renovação: " ++ tipo_str (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" para matrícula \"" ++ show mat_user ++ "\""
           log_op = LogOperacao momento desc (show mat_user) Sucesso ""
           novo_log = historico_operacoes banco ++ [log_op]
       in banco { lista_emprestimos = nova_lista_emp, historico_operacoes = novo_log }

-- (As funções de lote)

emprestimo_lote :: String -> String -> [Int] -> Int -> BancoDeDados -> BancoDeDados
emprestimo_lote momento data_dev ids mat_user banco =
    foldl (\banco_acumulado id_alvo -> fazer_emprestimo momento data_dev id_alvo mat_user banco_acumulado) banco ids

devolucao_lote :: String -> [Int] -> Int -> BancoDeDados -> BancoDeDados
devolucao_lote momento ids mat_user banco =
    foldl (\banco_acumulado id_alvo -> fazer_devolucao momento id_alvo mat_user banco_acumulado) banco ids

-- Adiciona o usuario na fila de espera do item
adicionar_fila_espera :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
adicionar_fila_espera momento id_item_alvo mat_user banco =
    let item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
        -- Validar se o usuario fantasma ta tentando entrar na fila
        user_busca = filter (\usuario -> matricula_user usuario == mat_user) (lista_usuarios banco)
    in if null item_busca || null user_busca
       then let
           -- CORRIGIDO: Adicionado o log de erro para a fila de espera também!
           motivo = if null item_busca then "Item não encontrado" else "Usuário não encontrado"
           log_erro = LogOperacao momento ("Tentativa de fila de espera (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro motivo
           novo_log = historico_operacoes banco ++ [log_erro]
       in banco { historico_operacoes = novo_log }
       
       else let
           item_alvo = head item_busca
       -- Protecao: usa a funcao de voces pra nao botar o cara duas vezes na fila
       in if ja_ta_na_fila mat_user (fila_espera item_alvo)
          then banco
          else let
              nova_lista_itens = map (\item -> if id_item item == id_item_alvo then item { fila_espera = fila_espera item ++ [mat_user] } else item) (lista_itens banco)
              desc = "Entrou na fila de espera: " ++ tipo_str (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" (matrícula \"" ++ show mat_user ++ "\")"
              
              log_op = LogOperacao momento desc (show mat_user) Sucesso ""
              novo_log = historico_operacoes banco ++ [log_op]
          in banco { lista_itens = nova_lista_itens, historico_operacoes = novo_log }

contar_atrasos :: Int -> String -> BancoDeDados -> Int
contar_atrasos mat_user momento_atual banco =
    length (filter (\emp -> mat_user_emp emp == mat_user && data_devolucao emp < momento_atual) (lista_emprestimos banco))