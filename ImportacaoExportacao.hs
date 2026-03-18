module ImportacaoExportacao where

import Cadastros (validar_ano, validar_email, validar_data)
import Estruturas
import Text.Read (readMaybe)



-- função aux pra substituir o Intercalate que vem de uma biblioteca externa.
meuIntercalate :: String -> [String] -> String
meuIntercalate _ [] = ""  
meuIntercalate _ [x] = x  
meuIntercalate sep (x:xs) = x ++ sep ++ meuIntercalate sep xs  

-- função aux pra substituir o SplitOn que vem de uma biblioteca externa.
meuSplitOn :: Char -> String -> [String]
meuSplitOn _ "" = []  
meuSplitOn c s = 
    let (antes, depois) = break (== c) s  -- quebra string no primeiro caractere separador
    in antes : case depois of
                 [] -> []  
                 (_:resto) -> meuSplitOn c resto  


-- EXPORTAÇÃO (Struct -> CSV)


tipo_str :: TipoMidia -> String
tipo_str Livro = "livro"
tipo_str Filme = "filme"
tipo_str Jogo = "jogo"

tipo_de_str :: String -> TipoMidia
tipo_de_str "filme" = Filme
tipo_de_str "jogo" = Jogo
tipo_de_str _       = Livro

colocar_aspas :: String -> String
colocar_aspas s = "\"" ++ s ++ "\""

-- Usando 'meuIntercalate' 
itemParaCSV :: Item -> String
itemParaCSV item = meuIntercalate ";" [
    colocar_aspas (tipo_str (tipo item)),
    colocar_aspas (titulo item),
    colocar_aspas (autor item),
    colocar_aspas (show (ano item)),
    colocar_aspas (show (id_item item))
    ]

userParaCSV :: Usuario -> String
userParaCSV usuario = meuIntercalate ";" [
    colocar_aspas (nome_user usuario),
    colocar_aspas (email_user usuario),
    colocar_aspas (show (matricula_user usuario))
    ]

empParaCSV :: Emprestimo -> String
empParaCSV emprestimo = meuIntercalate ";" [
    colocar_aspas (show (id_item_emp emprestimo)),
    colocar_aspas (show (mat_user_emp emprestimo)),
    colocar_aspas (data_emp emprestimo),
    colocar_aspas (data_devolucao emprestimo)
    ]


-- IMPORTAÇÃO E VALIDAÇÃO (CSV -> Struct)


-- Remove as aspas duplas de string
remover_aspas :: String -> String
remover_aspas linha = filter (\c -> c /= '\"') linha



montar_item_da_linha :: String -> Maybe Item
montar_item_da_linha linha =
    let partes = meuSplitOn ';' (remover_aspas linha)
    in if length partes == 5
       then case (readMaybe (partes !! 3) :: Maybe Int, readMaybe (partes !! 4) :: Maybe Int) of
                (Just ano_int, Just id_int) -> 
                    if validar_ano ano_int 
                    then Just (Item id_int (partes !! 1) (partes !! 2) ano_int (tipo_de_str (partes !! 0)) True [])
                    else Nothing
                _ -> Nothing
       else Nothing

montar_usuario_da_linha :: String -> Maybe Usuario
montar_usuario_da_linha linha =
    let partes = meuSplitOn ';' (remover_aspas linha)
    in if length partes == 3
       then case readMaybe (partes !! 2) :: Maybe Int of
                Just mat_int -> 
                    let email_csv = partes !! 1
                    in if validar_email email_csv 
                       then Just (Usuario mat_int (partes !! 0) email_csv [])
                       else Nothing
                _ -> Nothing
       else Nothing

montar_emprestimo_da_linha :: String -> Maybe Emprestimo
montar_emprestimo_da_linha linha =
    let partes = meuSplitOn ';' (remover_aspas linha)
    in if length partes == 4
       then case (readMaybe (partes !! 0) :: Maybe Int, readMaybe (partes !! 1) :: Maybe Int) of
                (Just id_int, Just mat_int) -> 
                    let data_emp_csv = partes !! 2
                        data_dev_csv = partes !! 3
                    
                    in if validar_data data_emp_csv && validar_data data_dev_csv
                       then Just (Emprestimo id_int mat_int data_emp_csv data_dev_csv)
                       else Nothing
                _ -> Nothing
       else Nothing

--  tirar os Nothing da lista
pegar_apenas_validos :: [Maybe a] -> [a]
pegar_apenas_validos [] = []
pegar_apenas_validos (Just x : xs) = x : pegar_apenas_validos xs
pegar_apenas_validos (Nothing : xs) = pegar_apenas_validos xs