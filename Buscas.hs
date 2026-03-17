module Buscas where

import Estruturas
import Relatorios (contem_substring)
import Data.List (sortOn)
import Data.Ord (Down(..))


-- FORMATAÇÃO (ignorar Maiúsculas/Minúsculas)


minuscula :: Char -> Char
minuscula c
    | fromEnum c >= 65 && fromEnum c <= 90 = toEnum (fromEnum c + 32)
    | otherwise = c

para_minusculo :: String -> String
para_minusculo = map minuscula


-- BUSCAR E FILTRAR


-- 1. Buscar por titulo
buscar_titulo :: String -> BancoDeDados -> [Item]
buscar_titulo termo banco =
    let termo_min = para_minusculo termo
    in filter (\i -> contem_substring termo_min (para_minusculo (titulo i))) (lista_itens banco)

-- 2. Buscar por autor/diretor
buscar_autor :: String -> BancoDeDados -> [Item]
buscar_autor termo banco =
    let termo_min = para_minusculo termo
    in filter (\i -> contem_substring termo_min (para_minusculo (autor i))) (lista_itens banco)

-- 3. Busca combinada (titulo E autor)
busca_combinada :: String -> String -> BancoDeDados -> [Item]
busca_combinada termo_tit termo_aut banco =
    let tit_min = para_minusculo termo_tit
        aut_min = para_minusculo termo_aut
    in filter (\i -> contem_substring tit_min (para_minusculo (titulo i)) && 
                     contem_substring aut_min (para_minusculo (autor i))) (lista_itens banco)

-- 4. Filtrar por categoria
filtrar_categoria :: TipoMidia -> BancoDeDados -> [Item]
filtrar_categoria cat banco =
    filter (\i -> tipo i == cat) (lista_itens banco)

-- 5. Ordenar resultados (com o sortOn)
ordenar_itens :: String -> String -> [Item] -> [Item]
ordenar_itens criterio ordem itens =
    case (criterio, ordem) of
        ("titulo", "asc")  -> sortOn titulo itens
        ("titulo", "desc") -> sortOn (Down . titulo) itens
        ("ano", "asc")     -> sortOn ano itens
        ("ano", "desc")    -> sortOn (Down . ano) itens
        ("autor", "asc")   -> sortOn autor itens
        ("autor", "desc")  -> sortOn (Down . autor) itens
        _                  -> itens -- pra evitar falhas