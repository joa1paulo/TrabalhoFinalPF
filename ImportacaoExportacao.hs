module ImportacaoExportacao where

import Estruturas
import Text.Read (readMaybe)

-- ==========================================================
-- EXPORTAÇÃO (Struct -> CSV)
-- ==========================================================

tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo = "jogo"

tipo_de_str :: String -> TipoMidia
tipo_de_str "filme" = Filme
tipo_de_str "jogo" = Jogo
tipo_de_str _       = Livro

-- Trocado 'i' por 'item' para manter o padrao didatico e claro
itemParaCSV :: Item -> String
itemParaCSV item = "\"" ++ tipo_str (tipo item) ++ "\";\"" ++ titulo item ++ "\";\"" ++ autor item ++ "\";\"" ++ show (ano item) ++ "\";\"" ++ show (id_item item) ++ "\""

-- Trocado 'u' por 'usuario'
userParaCSV :: Usuario -> String
userParaCSV usuario = "\"" ++ nome_user usuario ++ "\";\"" ++ email_user usuario ++ "\";\"" ++ show (matricula_user usuario) ++ "\""

-- Trocado 'e' por 'emprestimo'
empParaCSV :: Emprestimo -> String
empParaCSV emprestimo = "\"" ++ show (id_item_emp emprestimo) ++ "\";\"" ++ show (mat_user_emp emprestimo) ++ "\";\"" ++ data_emp emprestimo ++ "\""

-- ==========================================================
-- IMPORTAÇÃO E VALIDAÇÃO (CSV -> Struct)
-- ==========================================================

-- Remove as aspas duplas de uma string (escrito de forma explicita)
remover_aspas :: String -> String
remover_aspas linha = filter (\c -> c /= '\"') linha

-- Corta a string toda vez que acha um ponto e virgula (;)
split_csv :: String -> [String]
split_csv "" = [""]
split_csv (c:cs)
    | c == ';'  = "" : resto
    | otherwise = (c : head resto) : tail resto
    where resto = split_csv cs

-- Tenta montar um Item a partir do texto
montar_item_da_linha :: String -> Maybe Item
montar_item_da_linha linha =
    let partes = split_csv (remover_aspas linha)
    in if length partes == 5
       then case (readMaybe (partes !! 3) :: Maybe Int, readMaybe (partes !! 4) :: Maybe Int) of
                (Just ano_int, Just id_int) -> Just (Item id_int (partes !! 1) (partes !! 2) ano_int (tipo_de_str (partes !! 0)) True [])
                _ -> Nothing
       else Nothing

-- Tenta montar um Usuario a partir do texto
montar_usuario_da_linha :: String -> Maybe Usuario
montar_usuario_da_linha linha =
    let partes = split_csv (remover_aspas linha)
    in if length partes == 3
       then case readMaybe (partes !! 2) :: Maybe Int of
                Just mat_int -> Just (Usuario mat_int (partes !! 0) (partes !! 1) [])
                _ -> Nothing
       else Nothing

-- Tenta montar um Emprestimo a partir do texto
montar_emprestimo_da_linha :: String -> Maybe Emprestimo
montar_emprestimo_da_linha linha =
    let partes = split_csv (remover_aspas linha)
    in if length partes == 3
       then case (readMaybe (partes !! 0) :: Maybe Int, readMaybe (partes !! 1) :: Maybe Int) of
                (Just id_int, Just mat_int) -> Just (Emprestimo id_int mat_int (partes !! 2))
                _ -> Nothing
       else Nothing

-- Funcao manual e recursiva para limpar os erros (Nothing) da lista
pegar_apenas_validos :: [Maybe a] -> [a]
pegar_apenas_validos [] = []
pegar_apenas_validos (Just x : xs) = x : pegar_apenas_validos xs
pegar_apenas_validos (Nothing : xs) = pegar_apenas_validos xs