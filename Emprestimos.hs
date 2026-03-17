module Emprestimos where

import Estruturas

-- Auxiliar para pegar o nome do tipo em minusculo
tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo  = "jogo"

-- CORRIGIDO: Recursao direta para verificar duplicatas na fila (exigencia do PDF)
ja_ta_na_fila :: Int -> [Int] -> Bool
ja_ta_na_fila _ []     = False
ja_ta_na_fila mat (x:xs)
    | mat == x  = True
    | otherwise = ja_ta_na_fila mat xs

-- Registra um emprestimo
fazer_emprestimo :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
fazer_emprestimo momento id_item_alvo mat_user banco =
    let user_busca = filter (\usuario -> matricula_user usuario == mat_user) (lista_usuarios banco)
        item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
    in if null user_busca || null item_busca
       then let
           motivo   = if null user_busca then "Usuario nao encontrado" else "Item nao encontrado"
           log_erro = LogOperacao momento ("Tentativa de emprestimo (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro motivo
           novo_log = historico_operacoes banco ++ [log_erro]
       in banco { historico_operacoes = novo_log }

       else let
           item_alvo = head item_busca
       in if not (ta_disponivel item_alvo)
          then let
              log_erro = LogOperacao momento ("Tentativa de emprestimo (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro "Item ja esta emprestado"
              novo_log = historico_operacoes banco ++ [log_erro]
          in banco { historico_operacoes = novo_log }

          else let
              titulo_item     = titulo item_alvo
              tipo_item       = tipo_str (tipo item_alvo)
              novo_emp        = Emprestimo id_item_alvo mat_user momento
              nova_lista_emp  = lista_emprestimos banco ++ [novo_emp]
              nova_lista_itens = map (\item -> if id_item item == id_item_alvo
                                               then item { ta_disponivel = False }
                                               else item) (lista_itens banco)
              nova_lista_users = map (\usuario -> if matricula_user usuario == mat_user
                                                  then usuario { meus_emprestimos = meus_emprestimos usuario ++ [id_item_alvo] }
                                                  else usuario) (lista_usuarios banco)
              desc     = "Emprestimo: " ++ tipo_item ++ " \"" ++ titulo_item ++ "\" para matricula \"" ++ show mat_user ++ "\""
              log_op   = LogOperacao momento desc (show mat_user) Sucesso ""
              novo_log = historico_operacoes banco ++ [log_op]
          in banco { lista_emprestimos = nova_lista_emp, lista_itens = nova_lista_itens,
                     lista_usuarios = nova_lista_users, historico_operacoes = novo_log }


-- CORRIGIDO: Devolucao agora consome a fila de espera e avanca o proximo da fila
fazer_devolucao :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
fazer_devolucao momento id_item_alvo mat_user banco =
    let item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
        emp_busca  = filter (\emp -> id_item_emp emp == id_item_alvo && mat_user_emp emp == mat_user) (lista_emprestimos banco)

    in if null item_busca || null emp_busca
       then let
           motivo   = if null item_busca then "Item nao encontrado" else "Emprestimo nao existe para este usuario"
           log_erro = LogOperacao momento ("Tentativa de devolucao (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro motivo
           novo_log = historico_operacoes banco ++ [log_erro]
       in banco { historico_operacoes = novo_log }

       else let
           item_alvo    = head item_busca
           titulo_item  = titulo item_alvo
           tipo_item    = tipo_str (tipo item_alvo)
           fila_atual   = fila_espera item_alvo

           -- Remove o emprestimo atual
           nova_lista_emp = filter (\emp -> not (id_item_emp emp == id_item_alvo && mat_user_emp emp == mat_user)) (lista_emprestimos banco)

           -- Remove o item dos emprestimos do usuario que devolveu
           nova_lista_users_dev = map (\usuario -> if matricula_user usuario == mat_user
                                                    then usuario { meus_emprestimos = filter (/= id_item_alvo) (meus_emprestimos usuario) }
                                                    else usuario) (lista_usuarios banco)

           -- CORRIGIDO: Verifica se ha fila de espera para avançar o proximo automaticamente
           (nova_lista_itens, nova_lista_emp2, nova_lista_users2, log_fila) =
               if null fila_atual
               then
                   -- Sem fila: apenas marca como disponivel e limpa a fila (que ja esta vazia)
                   ( map (\item -> if id_item item == id_item_alvo
                                   then item { ta_disponivel = True, fila_espera = [] }
                                   else item) (lista_itens banco)
                   , nova_lista_emp
                   , nova_lista_users_dev
                   , []
                   )
               else
                   -- Com fila: empresta automaticamente para o proximo da fila
                   let proximo_mat  = head fila_atual
                       fila_restante = tail fila_atual
                       novo_emp_fila = Emprestimo id_item_alvo proximo_mat momento
                       -- Item continua indisponivel, mas a fila avanca
                       itens_fila   = map (\item -> if id_item item == id_item_alvo
                                                     then item { ta_disponivel = False, fila_espera = fila_restante }
                                                     else item) (lista_itens banco)
                       -- Adiciona o emprestimo do proximo da fila
                       emp_fila     = nova_lista_emp ++ [novo_emp_fila]
                       -- Adiciona o item nos emprestimos do proximo da fila
                       users_fila   = map (\usuario -> if matricula_user usuario == proximo_mat
                                                        then usuario { meus_emprestimos = meus_emprestimos usuario ++ [id_item_alvo] }
                                                        else usuario) nova_lista_users_dev
                       desc_fila    = "Emprestimo automatico (fila de espera): " ++ tipo_item ++ " \"" ++ titulo_item ++ "\" para matricula \"" ++ show proximo_mat ++ "\""
                       log_auto     = LogOperacao momento desc_fila (show proximo_mat) Sucesso ""
                   in (itens_fila, emp_fila, users_fila, [log_auto])

           desc     = "Devolucao: " ++ tipo_item ++ " \"" ++ titulo_item ++ "\" da matricula \"" ++ show mat_user ++ "\""
           log_op   = LogOperacao momento desc (show mat_user) Sucesso ""
           novo_log = historico_operacoes banco ++ [log_op] ++ log_fila

       in banco { lista_emprestimos = nova_lista_emp2, lista_itens = nova_lista_itens,
                  lista_usuarios = nova_lista_users2, historico_operacoes = novo_log }


-- Renova um emprestimo (atualiza a data)
renovar_emprestimo :: String -> Int -> Int -> String -> BancoDeDados -> BancoDeDados
renovar_emprestimo momento id_item_alvo mat_user nova_data banco =
    let item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
        emp_busca  = filter (\emp -> id_item_emp emp == id_item_alvo && mat_user_emp emp == mat_user) (lista_emprestimos banco)
    in if null item_busca || null emp_busca
       then let
           motivo   = if null item_busca then "Item nao encontrado" else "Emprestimo nao existe"
           log_erro = LogOperacao momento ("Tentativa de renovacao (ID: " ++ show id_item_alvo ++ ")") (show mat_user) Erro motivo
           novo_log = historico_operacoes banco ++ [log_erro]
       in banco { historico_operacoes = novo_log }
       else let
           item_alvo       = head item_busca
           nova_lista_emp  = map (\emp -> if id_item_emp emp == id_item_alvo && mat_user_emp emp == mat_user
                                           then emp { data_emp = nova_data }
                                           else emp) (lista_emprestimos banco)
           desc     = "Renovacao: " ++ tipo_str (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" para matricula \"" ++ show mat_user ++ "\""
           log_op   = LogOperacao momento desc (show mat_user) Sucesso ""
           novo_log = historico_operacoes banco ++ [log_op]
       in banco { lista_emprestimos = nova_lista_emp, historico_operacoes = novo_log }


-- Emprestimo em lote usando foldl (exigencia do PDF)
emprestimo_lote :: String -> [Int] -> Int -> BancoDeDados -> BancoDeDados
emprestimo_lote momento ids mat_user banco =
    foldl (\banco_acc id_alvo -> fazer_emprestimo momento id_alvo mat_user banco_acc) banco ids

-- Devolucao em lote usando foldl
devolucao_lote :: String -> [Int] -> Int -> BancoDeDados -> BancoDeDados
devolucao_lote momento ids mat_user banco =
    foldl (\banco_acc id_alvo -> fazer_devolucao momento id_alvo mat_user banco_acc) banco ids

-- Adiciona o usuario na fila de espera do item
adicionar_fila_espera :: String -> Int -> Int -> BancoDeDados -> BancoDeDados
adicionar_fila_espera momento id_item_alvo mat_user banco =
    let item_busca = filter (\item -> id_item item == id_item_alvo) (lista_itens banco)
        user_busca = filter (\usuario -> matricula_user usuario == mat_user) (lista_usuarios banco)
    in if null item_busca || null user_busca
       then banco
       else let
           item_alvo = head item_busca
       in if ja_ta_na_fila mat_user (fila_espera item_alvo)
          then banco
          else let
              nova_lista_itens = map (\item -> if id_item item == id_item_alvo
                                               then item { fila_espera = fila_espera item ++ [mat_user] }
                                               else item) (lista_itens banco)
              desc     = "Entrou na fila de espera: " ++ tipo_str (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" (matricula \"" ++ show mat_user ++ "\")"
              log_op   = LogOperacao momento desc (show mat_user) Sucesso ""
              novo_log = historico_operacoes banco ++ [log_op]
          in banco { lista_itens = nova_lista_itens, historico_operacoes = novo_log }
