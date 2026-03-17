module Relatorios where

import Estruturas
import Data.List (sort, group, sortOn)
import Data.Ord (Down(..))


-- ESTATÍSTICAS COM MAP, FILTER E FOLDL


-- 1. Empréstimos ativos (por categoria)
ativos_por_categoria :: TipoMidia -> BancoDeDados -> [Item]
ativos_por_categoria cat banco =
    filter (\i -> tipo i == cat && not (ta_disponivel i)) (lista_itens banco)

-- 2. Usuarios mais ativos (Ranking Geral)
usuarios_mais_ativos :: BancoDeDados -> [(String, Int)]
usuarios_mais_ativos banco =
    let usuarios_mat = map usuario_envolvido (historico_operacoes banco)
        usuarios_reais = filter (/= "Sistema") usuarios_mat
        
        --  pegar a string da matricula e buscar o nome  na lista de usuarios
        pegar_nome m_str = 
            let encontrados = filter (\u -> show (matricula_user u) == m_str) (lista_usuarios banco)
            in if null encontrados then "Matricula " ++ m_str else nome_user (head encontrados)
            
        nomes = map pegar_nome usuarios_reais
        contagem = map (\g -> (head g, length g)) (group (sort nomes))
    in sortOn (Down . snd) contagem

-- 3. Itens mais emprestados
itens_mais_emprestados :: BancoDeDados -> [(String, Int)]
itens_mais_emprestados banco =
    -- Filtra so as operacoes de "Emprestimo"
    let logsEmp = filter (\log -> contem_substring "Emprestimo:" (descricao_op log)) (historico_operacoes banco)
        -- Pega a descricao (c/ o ID do item)
        descricoes = map descricao_op logsEmp
        contagem = map (\g -> (head g, length g)) (group (sort descricoes))
    in sortOn (Down . snd) contagem

-- 4. Frequência de empréstimos por período com foldl
frequencia_periodo :: String -> BancoDeDados -> Int
frequencia_periodo periodo banco =
    foldl (\acc log -> se_foi_emprestimo_no_periodo log periodo acc) 0 (historico_operacoes banco)
  where
    se_foi_emprestimo_no_periodo log per acc =
        if contem_substring "Emprestimo:" (descricao_op log) && contem_substring per (data_hora_op log)
        then acc + 1
        else acc

-- 5. Itens com lista de espera
itens_com_espera :: BancoDeDados -> [Item]
itens_com_espera banco =
    filter (\i -> not (null (fila_espera i))) (lista_itens banco)

-- 6. Relatório de operações (Rdevolve a lista de logs completa que bater com a busca)
relatorio_operacoes :: String -> BancoDeDados -> [LogOperacao]
relatorio_operacoes termo banco =
    filter (\log -> contem_substring termo (usuario_envolvido log) || contem_substring termo (descricao_op log)) (historico_operacoes banco)


-- FUNÇÕES AUXILIARES


eh_prefixo :: String -> String -> Bool
eh_prefixo [] _ = True
eh_prefixo _ [] = False
eh_prefixo (x:xs) (y:ys)
    | x == y    = eh_prefixo xs ys
    | otherwise = False

contem_substring :: String -> String -> Bool
contem_substring [] _ = True
contem_substring _ [] = False
contem_substring sub texto@(t:ts)
    | eh_prefixo sub texto = True
    | otherwise            = contem_substring sub ts