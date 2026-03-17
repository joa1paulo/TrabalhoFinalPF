module ImportacaoExportacao where

import Estruturas
import Text.Read (readMaybe)

 
-- EXPORTAÇÃO (Struct -> CSV)


tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo = "jogo"

tipo_de_str :: String -> TipoMidia
tipo_de_str "filme" = Filme
tipo_de_str "jogo" = Jogo
tipo_de_str _       = Livro

itemParaCSV :: Item -> String
itemParaCSV i = "\"" ++ tipo_str (tipo i) ++ "\";\"" ++ titulo i ++ "\";\"" ++ autor i ++ "\";\"" ++ show (ano i) ++ "\";\"" ++ show (id_item i) ++ "\""

userParaCSV :: Usuario -> String
userParaCSV u = "\"" ++ nome_user u ++ "\";\"" ++ email_user u ++ "\";\"" ++ show (matricula_user u) ++ "\""

empParaCSV :: Emprestimo -> String
empParaCSV e = "\"" ++ show (id_item_emp e) ++ "\";\"" ++ show (mat_user_emp e) ++ "\";\"" ++ data_emp e ++ "\""


-- IMPORTAÇÃO E VALIDAÇÃO (CSV -> Struct)


-- Remover as aspas duplas das strings 
remover_aspas :: String -> String
remover_aspas linha = filter (\c -> c /= '\"') linha

-- Corta a string em todo ponto virgula (;)
split_csv :: String -> [String]
split_csv "" = [""]
split_csv (c:cs)
    | c == ';'  = "" : resto
    | otherwise = (c : head resto) : tail resto
    where resto = split_csv cs

-- Tenta montar um Item 
montar_item_da_linha :: String -> Maybe Item
montar_item_da_linha linha =
    let partes = split_csv (remover_aspas linha)
    in if length partes == 5
       then case (readMaybe (partes !! 3) :: Maybe Int, readMaybe (partes !! 4) :: Maybe Int) of
                (Just ano_int, Just id_int) -> Just (Item id_int (partes !! 1) (partes !! 2) ano_int (tipo_de_str (partes !! 0)) True [])
                _ -> Nothing
       else Nothing

-- Tenta montar um Usuario 
montar_usuario_da_linha :: String -> Maybe Usuario
montar_usuario_da_linha linha =
    let partes = split_csv (remover_aspas linha)
    in if length partes == 3
       then case readMaybe (partes !! 2) :: Maybe Int of
                Just mat_int -> Just (Usuario mat_int (partes !! 0) (partes !! 1) [])
                _ -> Nothing
       else Nothing

-- Tenta montar um Emprestimo 
montar_emprestimo_da_linha :: String -> Maybe Emprestimo
montar_emprestimo_da_linha linha =
    let partes = split_csv (remover_aspas linha)
    in if length partes == 3
       then case (readMaybe (partes !! 0) :: Maybe Int, readMaybe (partes !! 1) :: Maybe Int) of
                (Just id_int, Just mat_int) -> Just (Emprestimo id_int mat_int (partes !! 2))
                _ -> Nothing
       else Nothing

-- Funcao para tirarr os (Nothing) da lista e nao dar erro
pegar_apenas_validos :: [Maybe a] -> [a]
pegar_apenas_validos [] = []
pegar_apenas_validos (Just x : xs) = x : pegar_apenas_validos xs
pegar_apenas_validos (Nothing : xs) = pegar_apenas_validos xs