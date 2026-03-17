module Relatorios where

import Estruturas
import Data.List (sort, group, sortOn)

-- ==========================================================
-- ESTATISTICAS UTILIZANDO MAP, FILTER E FOLDL (exigencia do PDF)
-- ==========================================================

-- 1. Emprestimos ativos por categoria
ativos_por_categoria :: TipoMidia -> BancoDeDados -> [Item]
ativos_por_categoria cat banco =
    filter (\item -> tipo item == cat && not (ta_disponivel item)) (lista_itens banco)

-- 2. Usuarios mais ativos (Ranking Geral)
-- Usa map, filter, group, sort e sortOn sobre as listas de log
usuarios_mais_ativos :: BancoDeDados -> [(String, Int)]
usuarios_mais_ativos banco =
    let usuarios_mat  = map usuario_envolvido (historico_operacoes banco)
        usuarios_reais = filter (\mat -> mat /= "Sistema") usuarios_mat
        pegar_nome m_str =
            let encontrados = filter (\usuario -> show (matricula_user usuario) == m_str) (lista_usuarios banco)
            in if null encontrados then "Matricula " ++ m_str else nome_user (head encontrados)
        nomes    = map pegar_nome usuarios_reais
        contagem = map (\grupo -> (head grupo, length grupo)) (group (sort nomes))
    in reverse (sortOn (\tupla -> snd tupla) contagem)

-- 3. Itens mais emprestados
-- CORRIGIDO: a string de busca agora usa "Emprestimo:" sem acento,
-- igual ao que e gravado em Emprestimos.hs
itens_mais_emprestados :: BancoDeDados -> [(String, Int)]
itens_mais_emprestados banco =
    let logsEmp   = filter (\lg -> contem_substring "Emprestimo:" (descricao_op lg)) (historico_operacoes banco)
        descricoes = map descricao_op logsEmp
        contagem  = map (\grupo -> (head grupo, length grupo)) (group (sort descricoes))
    in reverse (sortOn (\tupla -> snd tupla) contagem)

-- 4. Frequencia de emprestimos por periodo (foldl - exigencia do PDF)
-- CORRIGIDO: a string de busca agora usa "Emprestimo:" sem acento
frequencia_periodo :: String -> BancoDeDados -> Int
frequencia_periodo periodo banco =
    foldl (\acc lg -> contar_emprestimo_no_periodo lg periodo acc) 0 (historico_operacoes banco)
  where
    contar_emprestimo_no_periodo lg per acc =
        if contem_substring "Emprestimo:" (descricao_op lg) && contem_substring per (data_hora_op lg)
        then acc + 1
        else acc

-- 5. Itens com lista de espera
itens_com_espera :: BancoDeDados -> [Item]
itens_com_espera banco =
    filter (\item -> not (null (fila_espera item))) (lista_itens banco)

-- 6. Relatorio de operacoes filtrado por termo (matricula ou tipo de operacao)
relatorio_operacoes :: String -> BancoDeDados -> [LogOperacao]
relatorio_operacoes termo banco =
    filter (\lg -> contem_substring termo (usuario_envolvido lg) || contem_substring termo (descricao_op lg)) (historico_operacoes banco)

-- ==========================================================
-- FUNCOES AUXILIARES RECURSIVAS (exigencia do PDF: recursao direta)
-- ==========================================================

-- Verifica se 'sub' e prefixo de 'texto'
eh_prefixo :: String -> String -> Bool
eh_prefixo [] _          = True
eh_prefixo _  []         = False
eh_prefixo (x:xs) (y:ys)
    | x == y    = eh_prefixo xs ys
    | otherwise = False

-- Verifica se 'sub' esta contido em 'texto' (busca recursiva por janela deslizante)
contem_substring :: String -> String -> Bool
contem_substring []  _           = True
contem_substring _   []          = False
contem_substring sub texto@(_:ts)
    | eh_prefixo sub texto = True
    | otherwise            = contem_substring sub ts
