module Cadastros where

import Estruturas

-- Auxiliar para printar o tipo em minusculo no log
tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo  = "jogo"

-------------------------------- Usuarios --------------------------------

-- Verificacao recursiva se o usuario ja existe
usuarioExiste :: Int -> [Usuario] -> Bool
usuarioExiste _ [] = False
usuarioExiste mat (x:xs)
    | matricula_user x == mat = True
    | otherwise               = usuarioExiste mat xs

-- Cadastra um novo usuario
cadastrar_usuario :: String -> Usuario -> BancoDeDados -> BancoDeDados
cadastrar_usuario momento novo_user banco =
    if usuarioExiste (matricula_user novo_user) (lista_usuarios banco)
    then banco
    else let
        nova_lista = lista_usuarios banco ++ [novo_user]
        log_op     = LogOperacao momento ("Cadastro usuario: \"" ++ nome_user novo_user ++ "\"") "Sistema" Sucesso ""
        novo_log   = historico_operacoes banco ++ [log_op]
    in banco { lista_usuarios = nova_lista, historico_operacoes = novo_log }

-- Recursao manual para remover usuario
auxRemoverUsuario :: Int -> [Usuario] -> [Usuario]
auxRemoverUsuario _ [] = []
auxRemoverUsuario mat (x:xs)
    | matricula_user x == mat = xs
    | otherwise               = x : auxRemoverUsuario mat xs

-- Remove um usuario
remover_usuario :: String -> Int -> BancoDeDados -> BancoDeDados
remover_usuario momento mat banco =
    let nova_lista = auxRemoverUsuario mat (lista_usuarios banco)
        log_op     = LogOperacao momento ("Remocao usuario matricula: \"" ++ show mat ++ "\"") "Sistema" Sucesso ""
        novo_log   = historico_operacoes banco ++ [log_op]
    in banco { lista_usuarios = nova_lista, historico_operacoes = novo_log }

---------------------------------- Itens ----------------------------------

-- Verificacao recursiva se o item ja existe
itemExiste :: Int -> [Item] -> Bool
itemExiste _ [] = False
itemExiste idBusca (x:xs)
    | id_item x == idBusca = True
    | otherwise            = itemExiste idBusca xs

-- Cadastra um novo item
cadastrar_item :: String -> Item -> BancoDeDados -> BancoDeDados
cadastrar_item momento novo_item banco =
    if itemExiste (id_item novo_item) (lista_itens banco)
    then banco
    else let
        nova_lista = lista_itens banco ++ [novo_item]
        log_op     = LogOperacao momento ("Cadastro item: " ++ tipo_str (tipo novo_item) ++ " \"" ++ titulo novo_item ++ "\"") "Sistema" Sucesso ""
        novo_log   = historico_operacoes banco ++ [log_op]
    in banco { lista_itens = nova_lista, historico_operacoes = novo_log }

-- Recursao manual para remover item
auxRemoverItem :: Int -> [Item] -> [Item]
auxRemoverItem _ [] = []
auxRemoverItem idBusca (x:xs)
    | id_item x == idBusca = xs
    | otherwise            = x : auxRemoverItem idBusca xs

-- Remove um item
remover_item :: String -> Int -> BancoDeDados -> BancoDeDados
remover_item momento id_remover banco =
    let nova_lista = auxRemoverItem id_remover (lista_itens banco)
        log_op     = LogOperacao momento ("Remocao item codigo: \"" ++ show id_remover ++ "\"") "Sistema" Sucesso ""
        novo_log   = historico_operacoes banco ++ [log_op]
    in banco { lista_itens = nova_lista, historico_operacoes = novo_log }
