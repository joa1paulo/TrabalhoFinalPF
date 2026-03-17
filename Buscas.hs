module Buscas where

import Estruturas
import Relatorios (contem_substring)
import Data.List (sortOn)

-- ==========================================================
-- FUNCOES DE NORMALIZACAO (ignorar maiusculas/minusculas)
-- ==========================================================

-- Converte um caractere para minusculo usando codigo ASCII
minuscula :: Char -> Char
minuscula c
    | fromEnum c >= 65 && fromEnum c <= 90 = toEnum (fromEnum c + 32)
    | otherwise = c

-- Aplica minuscula a toda a string usando map (funcao de ordem superior)
para_minusculo :: String -> String
para_minusculo texto = map minuscula texto

-- ==========================================================
-- FUNCOES DE BUSCA E FILTRAGEM (filter - exigencia do PDF)
-- ==========================================================

-- 1. Busca por titulo (case-insensitive)
buscar_titulo :: String -> BancoDeDados -> [Item]
buscar_titulo termo banco =
    let termo_min = para_minusculo termo
    in filter (\item -> contem_substring termo_min (para_minusculo (titulo item))) (lista_itens banco)

-- 2. Busca por autor/diretor (case-insensitive)
buscar_autor :: String -> BancoDeDados -> [Item]
buscar_autor termo banco =
    let termo_min = para_minusculo termo
    in filter (\item -> contem_substring termo_min (para_minusculo (autor item))) (lista_itens banco)

-- 3. Busca combinada: titulo E autor (case-insensitive)
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

-- 5. Ordenar resultados por criterio e ordem (sortOn - funcao de ordem superior)
ordenar_itens :: String -> String -> [Item] -> [Item]
ordenar_itens criterio ordem itens =
    case (criterio, ordem) of
        ("titulo", "asc")  -> sortOn (\item -> titulo item) itens
        ("titulo", "desc") -> reverse (sortOn (\item -> titulo item) itens)
        ("ano",    "asc")  -> sortOn (\item -> ano item) itens
        ("ano",    "desc") -> reverse (sortOn (\item -> ano item) itens)
        ("autor",  "asc")  -> sortOn (\item -> autor item) itens
        ("autor",  "desc") -> reverse (sortOn (\item -> autor item) itens)
        _                  -> itens
