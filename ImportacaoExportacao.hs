module ImportacaoExportacao where

import Estruturas
import Text.Read (readMaybe)

-- ==========================================================
-- FUNCOES AUXILIARES RECURSIVAS PROPRIAS
-- (evita dependencia de bibliotecas externas)
-- ==========================================================

-- Substitui Data.List.intercalate: une lista de strings com separador
meuIntercalate :: String -> [String] -> String
meuIntercalate _ []     = ""
meuIntercalate _ [x]    = x
meuIntercalate sep (x:xs) = x ++ sep ++ meuIntercalate sep xs

-- Substitui Data.List.Split.splitOn: divide string pelo caractere separador
meuSplitOn :: Char -> String -> [String]
meuSplitOn _ "" = []
meuSplitOn c s  =
    let (antes, depois) = break (== c) s
    in antes : case depois of
                 []       -> []
                 (_:resto) -> meuSplitOn c resto

-- ==========================================================
-- EXPORTACAO (Struct -> linha CSV)
-- ==========================================================

tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo  = "jogo"

tipo_de_str :: String -> TipoMidia
tipo_de_str "filme" = Filme
tipo_de_str "jogo"  = Jogo
tipo_de_str _       = Livro

colocar_aspas :: String -> String
colocar_aspas s = "\"" ++ s ++ "\""

-- Serializa um Item em formato CSV
itemParaCSV :: Item -> String
itemParaCSV item = meuIntercalate ";" [
    colocar_aspas (tipo_str (tipo item)),
    colocar_aspas (titulo item),
    colocar_aspas (autor item),
    colocar_aspas (show (ano item)),
    colocar_aspas (show (id_item item))
    ]

-- Serializa um Usuario em formato CSV
userParaCSV :: Usuario -> String
userParaCSV usuario = meuIntercalate ";" [
    colocar_aspas (nome_user usuario),
    colocar_aspas (email_user usuario),
    colocar_aspas (show (matricula_user usuario))
    ]

-- Serializa um Emprestimo em formato CSV
empParaCSV :: Emprestimo -> String
empParaCSV emprestimo = meuIntercalate ";" [
    colocar_aspas (show (id_item_emp emprestimo)),
    colocar_aspas (show (mat_user_emp emprestimo)),
    colocar_aspas (data_emp emprestimo)
    ]

-- ==========================================================
-- IMPORTACAO E VALIDACAO (linha CSV -> Maybe Struct)
-- ==========================================================

-- Remove aspas duplas de uma string
remover_aspas :: String -> String
remover_aspas linha = filter (\c -> c /= '"') linha

-- Tenta montar um Item a partir de uma linha CSV; retorna Nothing se invalida
montar_item_da_linha :: String -> Maybe Item
montar_item_da_linha linha =
    let partes = meuSplitOn ';' (remover_aspas linha)
    in if length partes == 5
       then case (readMaybe (partes !! 3) :: Maybe Int, readMaybe (partes !! 4) :: Maybe Int) of
                (Just ano_int, Just id_int) ->
                    Just (Item id_int (partes !! 1) (partes !! 2) ano_int (tipo_de_str (partes !! 0)) True [])
                _ -> Nothing
       else Nothing

-- Tenta montar um Usuario a partir de uma linha CSV
montar_usuario_da_linha :: String -> Maybe Usuario
montar_usuario_da_linha linha =
    let partes = meuSplitOn ';' (remover_aspas linha)
    in if length partes == 3
       then case readMaybe (partes !! 2) :: Maybe Int of
                Just mat_int -> Just (Usuario mat_int (partes !! 0) (partes !! 1) [])
                _            -> Nothing
       else Nothing

-- Tenta montar um Emprestimo a partir de uma linha CSV
montar_emprestimo_da_linha :: String -> Maybe Emprestimo
montar_emprestimo_da_linha linha =
    let partes = meuSplitOn ';' (remover_aspas linha)
    in if length partes == 3
       then case (readMaybe (partes !! 0) :: Maybe Int, readMaybe (partes !! 1) :: Maybe Int) of
                (Just id_int, Just mat_int) -> Just (Emprestimo id_int mat_int (partes !! 2))
                _                           -> Nothing
       else Nothing

-- Recursao direta para filtrar apenas os valores validos (Just) de uma lista
pegar_apenas_validos :: [Maybe a] -> [a]
pegar_apenas_validos []             = []
pegar_apenas_validos (Just x  : xs) = x : pegar_apenas_validos xs
pegar_apenas_validos (Nothing : xs) = pegar_apenas_validos xs
