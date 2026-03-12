module Cadastros where

import Estruturas

-------------------------------- Usuarios --------------------------------

usuarioExiste :: Int -> [Usuario] -> Bool
usuarioExiste _ [] = False
usuarioExiste mat (x:xs)
    | matricula_user x == mat = True
    | otherwise               = usuarioExiste mat xs

adicionarUsuario :: Usuario -> BancoDeDados -> BancoDeDados
adicionarUsuario novoUser bd
    | usuarioExiste (matricula_user novoUser) (lista_usuarios bd) = bd
    | otherwise = bd { 
        lista_usuarios = novoUser : lista_usuarios bd,
        historico_log = "Usuario adicionado: " ++ nome_user novoUser : historico_log bd
    }

auxRemoverUsuario :: Int -> [Usuario] -> [Usuario]
auxRemoverUsuario _ [] = []
auxRemoverUsuario mat (x:xs)
    | matricula_user x == mat = xs
    | otherwise               = x : auxRemoverUsuario mat xs

removerUsuario :: Int -> BancoDeDados -> BancoDeDados
removerUsuario mat bd = bd {
    lista_usuarios = auxRemoverUsuario mat (lista_usuarios bd),
    historico_log = "Usuario removido: " ++ show mat : historico_log bd
}

auxEditarUsuario :: Int -> Usuario -> [Usuario] -> [Usuario]
auxEditarUsuario _ _ [] = []
auxEditarUsuario mat novoUser (x:xs)
    | matricula_user x == mat = novoUser : xs
    | otherwise               = x : auxEditarUsuario mat novoUser xs

editarUsuario :: Int -> Usuario -> BancoDeDados -> BancoDeDados
editarUsuario mat novoUser bd = bd {
    lista_usuarios = auxEditarUsuario mat novoUser (lista_usuarios bd),
    historico_log = "Usuario editado: " ++ show mat : historico_log bd
}

---------------------------------- Itens ----------------------------------
-- (filmes, livros, jogos)

itemExiste :: Int -> [Item] -> Bool
itemExiste _ [] = False
itemExiste idBusca (x:xs)
    | id_item x == idBusca = True
    | otherwise            = itemExiste idBusca xs

adicionarItem :: Item -> BancoDeDados -> BancoDeDados
adicionarItem novoItem bd
    | itemExiste (id_item novoItem) (lista_itens bd) = bd
    | otherwise = bd { 
        lista_itens = novoItem : lista_itens bd,
        historico_log = "Item adicionado: " ++ nome_titulo novoItem : historico_log bd
    }

auxRemoverItem :: Int -> [Item] -> [Item]
auxRemoverItem _ [] = []
auxRemoverItem idBusca (x:xs)
    | id_item x == idBusca = xs
    | otherwise            = x : auxRemoverItem idBusca xs

removerItem :: Int -> BancoDeDados -> BancoDeDados
removerItem idBusca bd = bd {
    lista_itens = auxRemoverItem idBusca (lista_itens bd),
    historico_log = "Item removido com ID: " ++ show idBusca : historico_log bd
}

auxEditarItem :: Int -> Item -> [Item] -> [Item]
auxEditarItem _ _ [] = []
auxEditarItem idBusca novoItem (x:xs)
    | id_item x == idBusca = novoItem : xs
    | otherwise            = x : auxEditarItem idBusca novoItem xs

editarItem :: Int -> Item -> BancoDeDados -> BancoDeDados
editarItem idBusca novoItem bd = bd {
    lista_itens = auxEditarItem idBusca novoItem (lista_itens bd),
    historico_log = "Item editado com ID: " ++ show idBusca : historico_log bd
}