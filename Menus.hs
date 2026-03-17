module Menus where

import Arquivos (salvar_banco)
import Buscas
import Cadastros
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Edicao
import Emprestimos
import Estruturas
import ImportacaoExportacao
import Relatorios
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- ==========================================================
-- FUNCOES AUXILIARES DE INTERFACE
-- ==========================================================

-- Imprime cabecalho e opcoes de menu de forma padronizada
exibir_opcoes :: String -> [String] -> IO ()
exibir_opcoes titulo opcoes = do
    putStrLn "\n=============================="
    putStrLn titulo
    putStrLn "=============================="
    putStrLn (unlines opcoes)

-- Obtem data e hora atual formatada para logs e historico
pegar_tempo_atual :: IO String
pegar_tempo_atual = do
    agora <- getCurrentTime
    return (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M]" agora)

-- Lê uma string do terminal com prompt, garantindo flush do buffer
ler_string :: String -> IO String
ler_string texto = do
    putStr texto
    hFlush stdout
    getLine

-- CORRIGIDO: le um Int com protecao contra entrada invalida,
-- repetindo a pergunta ate receber um numero valido
ler_int :: String -> IO Int
ler_int texto = do
    entrada <- ler_string texto
    case readMaybe entrada :: Maybe Int of
        Just n  -> return n
        Nothing -> do
            putStrLn "Erro: digite apenas numeros inteiros."
            ler_int texto

-- ==========================================================
-- FUNCOES DE VALIDACAO (exigencia do PDF)
-- ==========================================================

-- CORRIGIDO: valida formato de e-mail (exige '@' e ponto apos o '@')
email_valido :: String -> Bool
email_valido email =
    let (antes, depois) = break (== '@') email
    in not (null antes) && length depois > 2 && '.' `elem` tail depois

-- CORRIGIDO: valida ano entre 1900 e 2026 (ano atual fixo para Haskell puro)
ano_valido :: Int -> Bool
ano_valido a = a >= 1900 && a <= 2026

-- Verifica se um usuario possui emprestimos ativos (para aviso de atraso)
-- O sistema usa a data do emprestimo como referencia: se a data informada
-- for anterior ao momento atual, considera em atraso.
-- Como o sistema nao tem data limite separada, verifica se ha emprestimos
-- registrados com data anterior ao periodo atual do log.
usuario_tem_emprestimos_ativos :: Int -> BancoDeDados -> Bool
usuario_tem_emprestimos_ativos mat banco =
    any (\emp -> mat_user_emp emp == mat) (lista_emprestimos banco)

-- ==========================================================
-- SUBMENUS
-- ==========================================================

-- Submenu 1: Cadastro de Itens
submenu_itens :: BancoDeDados -> IO ()
submenu_itens banco = do
    exibir_opcoes "Cadastro de Itens" [
        "1 - Adicionar novo item",
        "2 - Remover item",
        "3 - Listar itens cadastrados",
        "4 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            id_int <- ler_int "Informe o codigo unico do item (numeros): "

            if any (\item -> id_item item == id_int) (lista_itens banco)
            then do
                putStrLn ("Erro: codigo \"" ++ show id_int ++ "\" ja cadastrado.")
                submenu_itens banco
            else do
                titulo_str <- ler_string "Informe o titulo: "
                autor_str  <- ler_string "Informe o autor/diretor/criador: "

                -- CORRIGIDO: validacao de ano com loop ate valor valido
                ano_int <- ler_ano_valido

                exibir_opcoes "Qual o tipo de midia?" ["1 - Livro | 2 - Filme | 3 - Jogo"]
                tipo_str_op <- ler_string "Opcao de tipo: "
                let tipo_escolhido = case tipo_str_op of
                                        "2" -> Filme
                                        "3" -> Jogo
                                        _   -> Livro

                let novo_item = Item id_int titulo_str autor_str ano_int tipo_escolhido True []
                momento <- pegar_tempo_atual
                let banco_novo = cadastrar_item momento novo_item banco
                putStrLn "Sucesso! Item cadastrado."
                submenu_itens banco_novo

        "2" -> do
            id_int <- ler_int "Informe o ID do item para remover: "
            momento <- pegar_tempo_atual
            let banco_novo = remover_item momento id_int banco
            putStrLn "Sucesso! Item removido (se existia)."
            submenu_itens banco_novo

        "3" -> do
            putStrLn "\n--- Lista de Itens ---"
            if null (lista_itens banco)
                then putStrLn "Nenhum item cadastrado."
                else mapM_ (\item -> putStrLn (show (id_item item) ++ " - " ++ titulo item ++ " (" ++ show (tipo item) ++ ") | Disponivel: " ++ if ta_disponivel item then "Sim" else "Nao")) (lista_itens banco)
            submenu_itens banco

        "4" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_itens banco

-- Auxiliar: le um ano com validacao de intervalo (1900-2026)
ler_ano_valido :: IO Int
ler_ano_valido = do
    a <- ler_int "Informe o ano de publicacao (1900-2026): "
    if ano_valido a
        then return a
        else do
            putStrLn ("Erro: ano \"" ++ show a ++ "\" invalido. Informe um valor entre 1900 e 2026.")
            ler_ano_valido

-- Auxiliar: le um e-mail com validacao de formato
ler_email_valido :: IO String
ler_email_valido = do
    email <- ler_string "Informe o e-mail: "
    if email_valido email
        then return email
        else do
            putStrLn ("Erro: e-mail \"" ++ email ++ "\" esta mal formatado. Use o formato usuario@dominio.com")
            ler_email_valido

-- Submenu 2: Cadastro de Usuarios
submenu_usuarios :: BancoDeDados -> IO ()
submenu_usuarios banco = do
    exibir_opcoes "Cadastro de Usuarios" [
        "1 - Adicionar novo usuario",
        "2 - Remover usuario",
        "3 - Listar usuarios cadastrados",
        "4 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            mat_int <- ler_int "Informe a matricula (apenas numeros): "

            if any (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            then do
                putStrLn ("Erro: matricula \"" ++ show mat_int ++ "\" ja cadastrada.")
                submenu_usuarios banco
            else do
                nome_str  <- ler_string "Informe o nome: "
                -- CORRIGIDO: validacao de e-mail com loop
                email_str <- ler_email_valido
                let novo_user = Usuario mat_int nome_str email_str []
                momento <- pegar_tempo_atual
                let banco_novo = cadastrar_usuario momento novo_user banco
                putStrLn "Sucesso! Usuario cadastrado."
                submenu_usuarios banco_novo

        "2" -> do
            mat_int <- ler_int "Informe a matricula para remover: "
            momento <- pegar_tempo_atual
            let banco_novo = remover_usuario momento mat_int banco
            putStrLn "Sucesso! Usuario removido (se existia)."
            submenu_usuarios banco_novo

        "3" -> do
            putStrLn "\n--- Lista de Usuarios ---"
            if null (lista_usuarios banco)
                then putStrLn "Nenhum usuario cadastrado."
                else mapM_ (\usuario -> putStrLn (show (matricula_user usuario) ++ " - " ++ nome_user usuario ++ " | " ++ email_user usuario)) (lista_usuarios banco)
            submenu_usuarios banco

        "4" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_usuarios banco

-- Submenu 3: Emprestimos e Devolucoes
submenu_emprestimos :: BancoDeDados -> IO ()
submenu_emprestimos banco = do
    exibir_opcoes "Emprestimos e Devolucoes" [
        "1 - Registrar emprestimo",
        "2 - Registrar devolucao",
        "3 - Visualizar emprestimos ativos",
        "4 - Renovar emprestimo",
        "5 - Emprestimo/devolucao em lote",
        "6 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            id_int  <- ler_int "Informe o ID do item: "
            mat_int <- ler_int "Informe a matricula do usuario: "

            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca
            then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                momento <- pegar_tempo_atual
                let log_erro = LogOperacao momento ("Tentativa de emprestimo (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Usuario inexistente"
                submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
            else do
                -- CORRIGIDO: aviso de emprestimos ativos antes de novo emprestimo
                let emprestimos_usuario = filter (\emp -> mat_user_emp emp == mat_int) (lista_emprestimos banco)
                if not (null emprestimos_usuario)
                then do
                    putStrLn ("\nAtencao: O usuario possui " ++ show (length emprestimos_usuario) ++ " emprestimo(s) ativo(s).")
                    putStrLn "Regularize sua situacao se houver atrasos antes de novos emprestimos."
                    continuar <- ler_string "Deseja continuar com o emprestimo? (S/N): "
                    if continuar == "S" || continuar == "s"
                    then processar_emprestimo id_int mat_int banco
                    else do
                        putStrLn "Operacao cancelada."
                        submenu_emprestimos banco
                else processar_emprestimo id_int mat_int banco

        "2" -> do
            id_int  <- ler_int "Informe o ID do item devolvido: "
            mat_int <- ler_int "Informe a matricula do usuario: "

            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca
            then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                momento <- pegar_tempo_atual
                let log_erro = LogOperacao momento ("Tentativa de devolucao (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Usuario inexistente"
                submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
            else do
                let emp_busca = filter (\emp -> id_item_emp emp == id_int && mat_user_emp emp == mat_int) (lista_emprestimos banco)
                if null emp_busca
                then do
                    putStrLn "Erro: Nao ha registro desse emprestimo para este usuario."
                    momento <- pegar_tempo_atual
                    let log_erro = LogOperacao momento ("Tentativa de devolucao (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Emprestimo nao existe"
                    submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
                else do
                    momento <- pegar_tempo_atual
                    let banco_novo = fazer_devolucao momento id_int mat_int banco
                    -- Informa se o proximo da fila foi atendido automaticamente
                    let item_busca = filter (\item -> id_item item == id_int) (lista_itens banco)
                    if not (null item_busca) && not (null (fila_espera (head item_busca)))
                    then putStrLn "Sucesso! Devolucao registrada. O proximo da fila de espera foi atendido automaticamente."
                    else putStrLn "Sucesso! Devolucao registrada."
                    submenu_emprestimos banco_novo

        "3" -> do
            putStrLn "\n--- Lista de Emprestimos Ativos ---"
            if null (lista_emprestimos banco)
            then putStrLn "Nenhum emprestimo ativo."
            else mapM_ (\emp -> do
                    let item_b = filter (\item -> id_item item == id_item_emp emp) (lista_itens banco)
                    let user_b = filter (\usuario -> matricula_user usuario == mat_user_emp emp) (lista_usuarios banco)
                    let nome_item   = if null item_b then show (id_item_emp emp) else titulo (head item_b)
                    let nome_pessoa = if null user_b then "Mat: " ++ show (mat_user_emp emp) else nome_user (head user_b)
                    putStrLn ("ID: " ++ show (id_item_emp emp) ++ " | Titulo: \"" ++ nome_item ++ "\" | Usuario: " ++ nome_pessoa ++ " | Data: " ++ data_emp emp)
                ) (lista_emprestimos banco)
            submenu_emprestimos banco

        "4" -> do
            id_int   <- ler_int "Informe o ID do item para renovar: "
            mat_int  <- ler_int "Informe a matricula do usuario: "
            nova_data <- ler_string "Informe a nova data de devolucao: "

            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca
            then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                submenu_emprestimos banco
            else do
                momento <- pegar_tempo_atual
                let banco_novo = renovar_emprestimo momento id_int mat_int nova_data banco
                putStrLn "Operacao processada (verifique a auditoria em caso de falha)."
                submenu_emprestimos banco_novo

        "5" -> do
            putStrLn "Qual operacao em lote deseja realizar?"
            putStrLn "1 - Emprestimos em lote"
            putStrLn "2 - Devolucoes em lote"
            op_lote <- ler_string "Opcao do lote: "

            mat_int <- ler_int "Informe a matricula do usuario: "

            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca
            then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                submenu_emprestimos banco
            else do
                ids_str <- ler_string "Informe os IDs dos itens separados por espaco (ex: 101 102 103): "
                let ids_ints = map read (words ids_str) :: [Int]
                momento <- pegar_tempo_atual

                case op_lote of
                    "1" -> do
                        let banco_novo = emprestimo_lote momento ids_ints mat_int banco
                        putStrLn "Emprestimos em lote efetuados (IDs invalidos geraram log de Erro na auditoria)."
                        submenu_emprestimos banco_novo
                    "2" -> do
                        let banco_novo = devolucao_lote momento ids_ints mat_int banco
                        putStrLn "Devolucoes em lote processadas (IDs invalidos geraram log de Erro na auditoria)."
                        submenu_emprestimos banco_novo
                    _ -> do
                        putStrLn "Opcao invalida."
                        submenu_emprestimos banco

        "6" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_emprestimos banco

-- Auxiliar: processa o emprestimo apos validacoes do submenu
processar_emprestimo :: Int -> Int -> BancoDeDados -> IO ()
processar_emprestimo id_int mat_int banco = do
    let item_busca = filter (\item -> id_item item == id_int) (lista_itens banco)
    if null item_busca
    then do
        putStrLn "Erro: Item nao encontrado no sistema."
        submenu_emprestimos banco
    else do
        let item_alvo = head item_busca
        if not (ta_disponivel item_alvo)
        then do
            putStrLn ("\nAviso: O " ++ show (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" ja esta emprestado.")
            escolha <- ler_string "Deseja entrar na lista de espera? (S/N): "
            if escolha == "S" || escolha == "s"
            then do
                momento <- pegar_tempo_atual
                let banco_novo = adicionar_fila_espera momento id_int mat_int banco
                putStrLn "Sucesso! Voce foi adicionado a fila de espera."
                submenu_emprestimos banco_novo
            else do
                putStrLn "Operacao cancelada."
                submenu_emprestimos banco
        else do
            momento <- pegar_tempo_atual
            let banco_novo = fazer_emprestimo momento id_int mat_int banco
            putStrLn "Sucesso! Emprestimo registrado."
            submenu_emprestimos banco_novo

-- Submenu 4: Busca e Listagem Avancada
submenu_busca :: BancoDeDados -> IO ()
submenu_busca banco = do
    exibir_opcoes "Busca Avancada" [
        "1 - Buscar por titulo",
        "2 - Buscar por autor/diretor",
        "3 - Busca combinada (multiplos campos)",
        "4 - Filtrar por categoria",
        "5 - Ordenar resultados (titulo, ano, autor/diretor)",
        "6 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            termo <- ler_string "Informe o titulo para busca: "
            let resultados = buscar_titulo termo banco
            imprimir_resultados resultados
            submenu_busca banco

        "2" -> do
            termo <- ler_string "Informe o autor/diretor para busca: "
            let resultados = buscar_autor termo banco
            imprimir_resultados resultados
            submenu_busca banco

        "3" -> do
            tit <- ler_string "Informe o titulo: "
            aut <- ler_string "Informe o autor/diretor: "
            let resultados = busca_combinada tit aut banco
            imprimir_resultados resultados
            submenu_busca banco

        "4" -> do
            exibir_opcoes "Qual categoria deseja filtrar?" ["1 - Livro | 2 - Filme | 3 - Jogo"]
            cat_str <- ler_string "Opcao de tipo: "
            let categoria = case cat_str of
                                "2" -> Filme
                                "3" -> Jogo
                                _   -> Livro
            let resultados = filtrar_categoria categoria banco
            imprimir_resultados resultados
            submenu_busca banco

        "5" -> do
            putStrLn "Qual criterio de ordenacao? (titulo / ano / autor)"
            criterio <- ler_string "Criterio: "
            putStrLn "Qual a ordem? (asc / desc)"
            ordem <- ler_string "Ordem: "
            let resultados = ordenar_itens criterio ordem (lista_itens banco)
            imprimir_resultados resultados
            submenu_busca banco

        "6" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_busca banco

-- Submenu 5: Relatorios e Estatisticas
submenu_relatorios :: BancoDeDados -> IO ()
submenu_relatorios banco = do
    exibir_opcoes "Relatorios e Estatisticas" [
        "1 - Emprestimos ativos (por categoria)",
        "2 - Usuarios mais ativos",
        "3 - Itens mais emprestados",
        "4 - Frequencia de emprestimos por periodo",
        "5 - Itens com lista de espera",
        "6 - Relatorio de operacoes (por usuario/tipo de item)",
        "7 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            exibir_opcoes "Qual categoria deseja visualizar?" ["1 - Livro | 2 - Filme | 3 - Jogo"]
            cat_str <- ler_string "Opcao de tipo: "
            let categoria = case cat_str of
                                "2" -> Filme
                                "3" -> Jogo
                                _   -> Livro
            let ativos = ativos_por_categoria categoria banco
            putStrLn ("\n--- Emprestimos Ativos (" ++ show categoria ++ ") ---")
            if null ativos
            then putStrLn "Nenhum item desta categoria esta emprestado no momento."
            else mapM_ (\item -> do
                    let emp_busca = filter (\emp -> id_item_emp emp == id_item item) (lista_emprestimos banco)
                    if not (null emp_busca)
                    then do
                        let emp_atual = head emp_busca
                        let user_busca = filter (\usuario -> matricula_user usuario == mat_user_emp emp_atual) (lista_usuarios banco)
                        let nome = if null user_busca then "Mat: " ++ show (mat_user_emp emp_atual) else nome_user (head user_busca)
                        putStrLn ("Codigo: " ++ show (id_item item) ++ " | Titulo: \"" ++ titulo item ++ "\" | Usuario: " ++ nome ++ " | Data: " ++ data_emp emp_atual)
                    else putStrLn ("Codigo: " ++ show (id_item item) ++ " | Titulo: \"" ++ titulo item ++ "\"")
                ) ativos
            submenu_relatorios banco

        "2" -> do
            putStrLn "\n--- Ranking: Usuarios Mais Ativos ---"
            let ranking = usuarios_mais_ativos banco
            if null ranking
            then putStrLn "Nenhum usuario realizou operacoes ainda."
            else mapM_ (\(pos, (nome, qtd)) -> putStrLn (show pos ++ ". " ++ nome ++ " (" ++ show qtd ++ " operacoes)")) (zip [1..] ranking)
            submenu_relatorios banco

        "3" -> do
            putStrLn "\n--- Ranking: Itens Mais Emprestados ---"
            let ranking = itens_mais_emprestados banco
            if null ranking
            then putStrLn "Nenhum item foi emprestado ainda."
            else mapM_ (\(nome_item, qtd) -> putStrLn (nome_item ++ " (" ++ show qtd ++ " vez(es))")) ranking
            submenu_relatorios banco

        "4" -> do
            periodo <- ler_string "Informe o periodo (ex: 2026-03 ou 2026-03-16): "
            let freq = frequencia_periodo periodo banco
            putStrLn ("\nFrequencia de emprestimos no periodo '" ++ periodo ++ "': " ++ show freq ++ " operacao(es).")
            submenu_relatorios banco

        "5" -> do
            putStrLn "\n--- Itens Com Lista de Espera ---"
            let itens_espera = itens_com_espera banco
            if null itens_espera
            then putStrLn "Nenhuma lista de espera ativa no momento."
            else mapM_ (\item -> do
                    putStrLn (show (tipo item) ++ ": \"" ++ titulo item ++ "\" (Codigo " ++ show (id_item item) ++ ")")
                    putStrLn "Lista de espera:"
                    mapM_ (\(pos, mat) -> do
                            let user_b = filter (\u -> matricula_user u == mat) (lista_usuarios banco)
                            let nome_u = if null user_b then "Matricula " ++ show mat else nome_user (head user_b)
                            putStrLn ("  " ++ show pos ++ ". " ++ nome_u ++ " (mat. " ++ show mat ++ ")")
                        ) (zip [1..] (fila_espera item))
                ) itens_espera
            submenu_relatorios banco

        "6" -> do
            termo <- ler_string "Informe a matricula do usuario ou tipo de operacao (ex: 12345 ou Emprestimo): "
            putStrLn ("\n--- Relatorio de Operacoes para '" ++ termo ++ "' ---")
            let relatorio = relatorio_operacoes termo banco
            if null relatorio
            then putStrLn "Nenhuma operacao encontrada com este termo."
            else mapM_ (\lg -> putStrLn (data_hora_op lg ++ " " ++ descricao_op lg ++ " | Envolvido: " ++ usuario_envolvido lg ++ " | Status: " ++ show (status_op lg))) relatorio
            submenu_relatorios banco

        "7" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_relatorios banco

-- Submenu 6: Edicao de Dados
submenu_edicao :: BancoDeDados -> IO ()
submenu_edicao banco = do
    exibir_opcoes "Edicao de Dados" [
        "1 - Editar item",
        "2 - Editar usuario",
        "3 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            id_int <- ler_int "Informe o codigo do item: "
            let item_encontrado = buscar_item_para_edicao id_int banco

            if null item_encontrado
            then do
                putStrLn "Erro: Item nao encontrado!"
                submenu_edicao banco
            else do
                let item = head item_encontrado
                putStrLn ("\nItem encontrado: \"" ++ titulo item ++ "\"")
                putStrLn "Dados atuais:"
                putStrLn ("1 - Titulo: "       ++ titulo item)
                putStrLn ("2 - Autor/Diretor: " ++ autor item)
                putStrLn ("3 - Ano: "           ++ show (ano item))

                campo <- ler_string "\nEscolha campo para editar (1/2/3): "

                case campo of
                    "1" -> do
                        novo_val <- ler_string "Informe novo titulo: "
                        confirma <- ler_string "Confirma edicao? (S/N): "
                        if confirma == "S" || confirma == "s"
                        then do
                            momento <- pegar_tempo_atual
                            let banco_novo = editar_titulo_item momento id_int novo_val banco
                            putStrLn "Sucesso! Titulo alterado."
                            submenu_edicao banco_novo
                        else do
                            putStrLn "Edicao cancelada."
                            submenu_edicao banco

                    "2" -> do
                        novo_val <- ler_string "Informe novo autor/diretor: "
                        confirma <- ler_string "Confirma edicao? (S/N): "
                        if confirma == "S" || confirma == "s"
                        then do
                            momento <- pegar_tempo_atual
                            let banco_novo = editar_autor_item momento id_int novo_val banco
                            putStrLn "Sucesso! Autor alterado."
                            submenu_edicao banco_novo
                        else do
                            putStrLn "Edicao cancelada."
                            submenu_edicao banco

                    "3" -> do
                        -- CORRIGIDO: validacao de ano tambem na edicao
                        ano_int  <- ler_ano_valido
                        confirma <- ler_string "Confirma edicao? (S/N): "
                        if confirma == "S" || confirma == "s"
                        then do
                            momento <- pegar_tempo_atual
                            let banco_novo = editar_ano_item momento id_int ano_int banco
                            putStrLn ("Sucesso! Ano alterado para " ++ show ano_int ++ ".")
                            submenu_edicao banco_novo
                        else do
                            putStrLn "Edicao cancelada."
                            submenu_edicao banco

                    _ -> do
                        putStrLn "Opcao invalida!"
                        submenu_edicao banco

        "2" -> do
            mat_int <- ler_int "Informe a matricula: "
            let user_encontrado = buscar_user_para_edicao mat_int banco

            if null user_encontrado
            then do
                putStrLn "Erro: Usuario nao encontrado!"
                submenu_edicao banco
            else do
                let usuario_atual = head user_encontrado
                putStrLn ("\nUsuario encontrado: " ++ nome_user usuario_atual)
                putStrLn "Dados atuais:"
                putStrLn ("1 - Nome: "  ++ nome_user  usuario_atual)
                putStrLn ("2 - E-mail: " ++ email_user usuario_atual)

                campo <- ler_string "\nEscolha campo para editar (1/2): "

                case campo of
                    "1" -> do
                        novo_val <- ler_string "Informe novo nome: "
                        confirma <- ler_string "Confirma edicao? (S/N): "
                        if confirma == "S" || confirma == "s"
                        then do
                            momento <- pegar_tempo_atual
                            let banco_novo = editar_nome_usuario momento mat_int novo_val banco
                            putStrLn "Sucesso! Nome alterado."
                            submenu_edicao banco_novo
                        else do
                            putStrLn "Edicao cancelada."
                            submenu_edicao banco

                    "2" -> do
                        -- CORRIGIDO: validacao de e-mail tambem na edicao
                        novo_val <- ler_email_valido
                        confirma <- ler_string "Confirma edicao? (S/N): "
                        if confirma == "S" || confirma == "s"
                        then do
                            momento <- pegar_tempo_atual
                            let banco_novo = editar_email_usuario momento mat_int novo_val banco
                            putStrLn "Sucesso! E-mail atualizado."
                            submenu_edicao banco_novo
                        else do
                            putStrLn "Edicao cancelada."
                            submenu_edicao banco

                    _ -> do
                        putStrLn "Opcao invalida!"
                        submenu_edicao banco

        "3" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_edicao banco

-- Submenu 7: Exportacao e Importacao
submenu_exportacao :: BancoDeDados -> IO ()
submenu_exportacao banco = do
    exibir_opcoes "Exportacao/Importacao" [
        "1 - Exportar dados para CSV",
        "2 - Importar dados de CSV",
        "3 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            exibir_opcoes "O que deseja exportar?" [
                "1 - Lista completa de Itens",
                "2 - Lista completa de Usuarios",
                "3 - Registros de Emprestimos"
                ]
            op_exp <- ler_string "Opcao de exportacao: "

            case op_exp of
                "1" -> do
                    let linhas = map itemParaCSV (lista_itens banco)
                    writeFile "export_itens.csv" (unlines linhas)
                    putStrLn "Sucesso! Itens exportados para 'export_itens.csv'."
                    submenu_exportacao banco
                "2" -> do
                    let linhas = map userParaCSV (lista_usuarios banco)
                    writeFile "export_usuarios.csv" (unlines linhas)
                    putStrLn "Sucesso! Usuarios exportados para 'export_usuarios.csv'."
                    submenu_exportacao banco
                "3" -> do
                    let linhas = map empParaCSV (lista_emprestimos banco)
                    writeFile "export_emprestimos.csv" (unlines linhas)
                    putStrLn "Sucesso! Emprestimos exportados para 'export_emprestimos.csv'."
                    submenu_exportacao banco
                _ -> do
                    putStrLn "Opcao invalida!"
                    submenu_exportacao banco

        "2" -> do
            exibir_opcoes "O que deseja importar?" [
                "1 - Lista completa de Itens",
                "2 - Lista completa de Usuarios",
                "3 - Registros de Emprestimos"
                ]
            op_imp <- ler_string "Opcao de importacao: "
            arquivo <- ler_string "Digite o nome do arquivo (ex: export_itens.csv): "

            existe <- doesFileExist arquivo
            if not existe
            then do
                putStrLn "Erro: Arquivo nao encontrado! Verifique o nome e tente novamente."
                submenu_exportacao banco
            else do
                conteudo <- readFile arquivo
                let linhas = lines conteudo

                case op_imp of
                    "1" -> do
                        let itens_lidos  = pegar_apenas_validos (map montar_item_da_linha linhas)
                        let erros_fmt    = length linhas - length itens_lidos
                        let (lista_final, duplicatas) = foldl (\(acc, qtd) novo ->
                                if any (\ex -> id_item ex == id_item novo) acc
                                then (acc, qtd + 1)
                                else (acc ++ [novo], qtd)
                              ) (lista_itens banco, 0) itens_lidos
                        let inseridos = length itens_lidos - duplicatas
                        putStrLn ("Importacao concluida: " ++ show inseridos ++ " itens importados com sucesso.")
                        if (erros_fmt + duplicatas) > 0
                        then putStrLn ("Aviso: " ++ show (erros_fmt + duplicatas) ++ " linhas ignoradas (formato invalido ou ID repetido).")
                        else return ()
                        submenu_exportacao (banco { lista_itens = lista_final })

                    "2" -> do
                        let users_lidos  = pegar_apenas_validos (map montar_usuario_da_linha linhas)
                        let erros_fmt    = length linhas - length users_lidos
                        let (lista_final, duplicatas) = foldl (\(acc, qtd) novo ->
                                if any (\ex -> matricula_user ex == matricula_user novo) acc
                                then (acc, qtd + 1)
                                else (acc ++ [novo], qtd)
                              ) (lista_usuarios banco, 0) users_lidos
                        let inseridos = length users_lidos - duplicatas
                        putStrLn ("Importacao concluida: " ++ show inseridos ++ " usuarios importados com sucesso.")
                        if (erros_fmt + duplicatas) > 0
                        then putStrLn ("Aviso: " ++ show (erros_fmt + duplicatas) ++ " linhas ignoradas (formato invalido ou matricula repetida).")
                        else return ()
                        submenu_exportacao (banco { lista_usuarios = lista_final })

                    "3" -> do
                        let emps_lidos = pegar_apenas_validos (map montar_emprestimo_da_linha linhas)
                        let erros_fmt  = length linhas - length emps_lidos
                        putStrLn ("Importacao concluida: " ++ show (length emps_lidos) ++ " emprestimos importados com sucesso.")
                        if erros_fmt > 0
                        then putStrLn ("Aviso: " ++ show erros_fmt ++ " linhas com formato invalido foram ignoradas.")
                        else return ()
                        submenu_exportacao (banco { lista_emprestimos = lista_emprestimos banco ++ emps_lidos })

                    _ -> do
                        putStrLn "Opcao invalida!"
                        submenu_exportacao banco

        "3" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_exportacao banco

-- Submenu 8: Auditoria e Historico
submenu_auditoria :: BancoDeDados -> IO ()
submenu_auditoria banco = do
    exibir_opcoes "Auditoria e Historico" [
        "1 - Exibir log de operacoes",
        "2 - Exibir historico de alteracoes",
        "3 - Voltar ao menu principal"
        ]
    opcao <- ler_string "Opcao: "

    case opcao of
        "1" -> do
            putStrLn "\n--- Log de Operacoes ---"
            if null (historico_operacoes banco)
            then putStrLn "Nenhuma operacao registrada."
            else mapM_ (\lg -> putStrLn (data_hora_op lg ++ " " ++ descricao_op lg ++ " (" ++ show (status_op lg) ++ ")")) (historico_operacoes banco)
            putStrLn "\nPressione Enter para continuar..."
            _ <- getLine
            submenu_auditoria banco

        "2" -> do
            putStrLn "\n--- Historico de Alteracoes ---"
            if null (historico_edicoes banco)
            then putStrLn "Nenhuma alteracao registrada."
            else mapM_ (\ed -> putStrLn (data_hora_ed ed ++ " Entidade: " ++ entidade_alterada ed ++ "\nAntes: " ++ estado_antes ed ++ "\nDepois: " ++ estado_depois ed ++ "\nAlterado por: " ++ alterado_por ed ++ "\n-")) (historico_edicoes banco)
            putStrLn "\nPressione Enter para continuar..."
            _ <- getLine
            submenu_auditoria banco

        "3" -> menu_principal banco

        _ -> do
            putStrLn "Opcao invalida!"
            submenu_auditoria banco

-- Menu principal do sistema
menu_principal :: BancoDeDados -> IO ()
menu_principal banco = do
    exibir_opcoes "Sistema de Midias - Menu Principal" [
        "1 - Cadastro de Itens",
        "2 - Cadastro de Usuarios",
        "3 - Emprestimos e Devolucoes",
        "4 - Busca e Listagem Avancada",
        "5 - Relatorios e Estatisticas",
        "6 - Edicao de Dados",
        "7 - Exportacao/Importacao de Dados",
        "8 - Auditoria e Historico",
        "0 - Salvar e Sair"
        ]
    opcao <- ler_string "Digite uma opcao: "

    case opcao of
        "1" -> submenu_itens banco
        "2" -> submenu_usuarios banco
        "3" -> submenu_emprestimos banco
        "4" -> submenu_busca banco
        "5" -> submenu_relatorios banco
        "6" -> submenu_edicao banco
        "7" -> submenu_exportacao banco
        "8" -> submenu_auditoria banco
        "0" -> do
            salvar_banco banco
            return ()
        _   -> do
            putStrLn "\nOpcao invalida. Tente novamente."
            menu_principal banco

-- Funcao auxiliar para imprimir lista de itens no terminal
imprimir_resultados :: [Item] -> IO ()
imprimir_resultados resultados = do
    putStrLn "\n--- Resultados da Busca ---"
    if null resultados
    then putStrLn "Nenhum item encontrado."
    else mapM_ (\item -> putStrLn ("ID: " ++ show (id_item item) ++ " | Titulo: " ++ titulo item ++ " | Autor: " ++ autor item ++ " | Ano: " ++ show (ano item) ++ " | Tipo: " ++ show (tipo item))) resultados
