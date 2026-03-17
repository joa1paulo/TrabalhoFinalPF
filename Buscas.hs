module Buscas where

import Estruturas
import Relatorios (contem_substring)
import Data.List (sortOn)

-- ==========================================================
-- FUNÇÕES DE FORMATAÇÃO (Para ignorar Maiúsculas/Minúsculas)
-- ==========================================================

minuscula :: Char -> Char
minuscula c
    | fromEnum c >= 65 && fromEnum c <= 90 = toEnum (fromEnum c + 32)
    | otherwise = c

-- CORRIGIDO: Variável 'texto' explicitamente declarada (evitando estilo point-free)
para_minusculo :: String -> String
para_minusculo texto = map minuscula texto

-- ==========================================================
-- FUNÇÕES DE BUSCA E FILTRAGEM
-- ==========================================================

-- 1. Buscar por titulo
buscar_titulo :: String -> BancoDeDados -> [Item]
buscar_titulo termo banco =
    let termo_min = para_minusculo termo
    in filter (\item -> contem_substring termo_min (para_minusculo (titulo item))) (lista_itens banco)

-- 2. Buscar por autor/diretor
buscar_autor :: String -> BancoDeDados -> [Item]
buscar_autor termo banco =
    let termo_min = para_minusculo termo
    in filter (\item -> contem_substring termo_min (para_minusculo (autor item))) (lista_itens banco)

-- 3. Busca combinada (titulo E autor)
busca_combinada :: String -> String -> BancoDeDados -> [Item]
busca_combinada termo_tit termo_aut banco =
    let tit_min = para_minusculo termo_tit
        aut_min = para_minusculo termo_aut
    in filter (\item -> contem_substring tit_min (para_minusculo (titulo item)) && 
                        contem_substring aut_min (para_minusculo (autor item))) (lista_itens banco)

-- 4. Filtrar por categoria
filtrar_categoria :: TipoMidia -> BancoDeDados -> [Item]
filtrar_categoria cat banco =
    filter (\item -> tipo item == cat) (lista_itens banco)

-- 5. Ordenar resultados (Usando funções de ordem superior sortOn)
ordenar_itens :: String -> String -> [Item] -> [Item]
ordenar_itens criterio ordem itens =
    case (criterio, ordem) of
        ("titulo", "asc")  -> sortOn (\item -> titulo item) itens
        ("titulo", "desc") -> reverse (sortOn (\item -> titulo item) itens)
        ("ano", "asc")     -> sortOn (\item -> ano item) itens
        ("ano", "desc")    -> reverse (sortOn (\item -> ano item) itens)
        ("autor", "asc")   -> sortOn (\item -> autor item) itens
        ("autor", "desc")  -> reverse (sortOn (\item -> autor item) itens)
        _                  -> itens -- Se digitar errado, devolve a lista normal