module Cadastros where

import Estruturas

--  tipo em minusculo no log
tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo = "jogo"

-- === Usuarios 


usuarioExiste :: Int -> [Usuario] -> Bool
usuarioExiste _ [] = False
usuarioExiste mat (x:xs)
    | matricula_user x == mat = True
    | otherwise               = usuarioExiste mat xs

-- 1. Cadastra um novo usuario 
cadastrar_usuario :: String -> Usuario -> BancoDeDados -> BancoDeDados
cadastrar_usuario momento novo_user banco = 
    
    if usuarioExiste (matricula_user novo_user) (lista_usuarios banco)
    then banco
    else let 
        nova_lista = lista_usuarios banco ++ [novo_user]
        log_op = LogOperacao momento ("Cadastro usuário: \"" ++ nome_user novo_user ++ "\"") "Sistema" Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in banco { lista_usuarios = nova_lista, historico_operacoes = novo_log }

auxRemoverUsuario :: Int -> [Usuario] -> [Usuario]
auxRemoverUsuario _ [] = []
auxRemoverUsuario mat (x:xs)
    | matricula_user x == mat = xs
    | otherwise               = x : auxRemoverUsuario mat xs

-- 3. Remove um usuario 
remover_usuario :: String -> Int -> BancoDeDados -> BancoDeDados
remover_usuario momento mat banco =
    let nova_lista = auxRemoverUsuario mat (lista_usuarios banco)
        log_op = LogOperacao momento ("Remoção usuário matrícula: \"" ++ show mat ++ "\"") "Sistema" Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in banco { lista_usuarios = nova_lista, historico_operacoes = novo_log }

-- === Itens 


itemExiste :: Int -> [Item] -> Bool
itemExiste _ [] = False
itemExiste idBusca (x:xs)
    | id_item x == idBusca = True
    | otherwise            = itemExiste idBusca xs

-- 2. Cadastra um novo item 
cadastrar_item :: String -> Item -> BancoDeDados -> BancoDeDados
cadastrar_item momento novo_item banco = 
    if itemExiste (id_item novo_item) (lista_itens banco)
    then banco
    else let 
        nova_lista = lista_itens banco ++ [novo_item]
        log_op = LogOperacao momento ("Cadastro item: " ++ tipo_str (tipo novo_item) ++ " \"" ++ titulo novo_item ++ "\"") "Sistema" Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in banco { lista_itens = nova_lista, historico_operacoes = novo_log }


auxRemoverItem :: Int -> [Item] -> [Item]
auxRemoverItem _ [] = []
auxRemoverItem idBusca (x:xs)
    | id_item x == idBusca = xs
    | otherwise            = x : auxRemoverItem idBusca xs

-- 4. Remove um item 
remover_item :: String -> Int -> BancoDeDados -> BancoDeDados
remover_item momento id_remover banco =
    let nova_lista = auxRemoverItem id_remover (lista_itens banco)
        log_op = LogOperacao momento ("Remoção item código: \"" ++ show id_remover ++ "\"") "Sistema" Sucesso ""
        novo_log = historico_operacoes banco ++ [log_op]
    in banco { lista_itens = nova_lista, historico_operacoes = novo_log }
    


-- REGRAS DE VALIDAÇÃO 


-- O ano deve estar entre 1900 e o ano atual (2026)
validar_ano :: Int -> Bool
validar_ano a = a >= 1900 && a <= 2026

-- Funcao auxiliar para achar ".com"
contem_ponto_com :: String -> Bool
contem_ponto_com [] = False
contem_ponto_com (t:ts)
    | take 4 (t:ts) == ".com" = True
    | otherwise = contem_ponto_com ts

-- O e-mail deve ter texto antes do '@', o '@' e '.com' depois
validar_email :: String -> Bool
validar_email email =
    let (antes, depois_com_arroba) = break (== '@') email
    in if null depois_com_arroba
       then False 
       else let depois = tail depois_com_arroba 
            in (length antes > 0) && contem_ponto_com depois

-- O formato deve ser estritamente [YYYY-MM-DD HH:MM] (18 caracteres)
validar_data :: String -> Bool
validar_data d =
    length d == 18 &&
    (d !! 0 == '[') &&
    (d !! 5 == '-') &&
    (d !! 8 == '-') &&
    (d !! 11 == ' ') &&
    (d !! 14 == ':') &&
    (d !! 17 == ']')