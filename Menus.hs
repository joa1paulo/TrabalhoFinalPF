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
import System.IO (stdout)
import Text.Read (readMaybe)

-- ==========================================================
-- FUNCOES AUXILIARES DE INTERFACE (Contribuição do colega!)
-- ==========================================================

-- Imprime os menus de forma modular e limpa
exibir_opcoes :: String -> [String] -> IO ()
exibir_opcoes titulo opcoes = do
    putStrLn "\n=============================="
    putStrLn titulo
    putStrLn "=============================="
    putStrLn (unlines opcoes)

-- Função para pegar tempo para log e historico
pegar_tempo_atual :: IO String
pegar_tempo_atual = do
    agora <- getCurrentTime
    return (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M]" agora)

-- Funcao auxiliar pra printar e ler na mesma linha
ler_string :: String -> IO String
ler_string texto = do
    putStr texto
    getLine

-- ==========================================================
-- SUBMENUS
-- ==========================================================

-- Submenu 1: Itens
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
            id_str <- ler_string "Informe o codigo unico do item (numeros): "
            titulo_str <- ler_string "Informe o titulo: "
            autor_str <- ler_string "Informe o autor/diretor/criador: "
            ano_str <- ler_string "Informe o ano de publicacao (numeros): "
            
            exibir_opcoes "Qual o tipo de midia?" ["1 - Livro | 2 - Filme | 3 - Jogo"]
            tipo_str <- ler_string "Opcao de tipo: "
            
            let tipo_escolhido = case tipo_str of
                                    "2" -> Filme
                                    "3" -> Jogo
                                    _   -> Livro
            
            let id_int = read id_str :: Int
            
            if any (\item -> id_item item == id_int) (lista_itens banco) then do
                putStrLn ("Erro: codigo \"" ++ show id_int ++ "\" ja cadastrado.")
                submenu_itens banco
            else do
                let ano_int = read ano_str :: Int
                if not (validar_ano ano_int) then do
                    let nome_cat = if tipo_escolhido == Livro then "livros" else if tipo_escolhido == Filme then "filmes" else "jogos"
                    let msg_erro = "Erro: ano \"" ++ show ano_int ++ "\" inválido para " ++ nome_cat ++ "."
                    putStrLn msg_erro
                    
                    momento <- pegar_tempo_atual
                    let log_erro = LogOperacao momento ("Tentativa de cadastro de item (ID: " ++ show id_int ++ ")") "Sistema" Erro msg_erro
                    submenu_itens (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
                else do
                    let novo_item = Item id_int titulo_str autor_str ano_int tipo_escolhido True []
                    
                    momento <- pegar_tempo_atual 
                    let banco_novo = cadastrar_item momento novo_item banco
                    putStrLn "Sucesso! Item cadastrado."
                    submenu_itens banco_novo
            
        "2" -> do
            id_str <- ler_string "Informe o ID do item para remover: "
            let id_int = read id_str :: Int
            
            momento <- pegar_tempo_atual 
            
            let banco_novo = remover_item momento id_int banco
            putStrLn "Sucesso! Item removido (se existia)."
            submenu_itens banco_novo
            
        "3" -> do
            putStrLn "\n--- Lista de Itens ---"
            mapM_ (\item -> putStrLn (show (id_item item) ++ " - " ++ titulo item ++ " (" ++ show (tipo item) ++ ")")) (lista_itens banco)
            submenu_itens banco
            
        "4" -> do
            menu_principal banco
            
        _ -> do
            putStrLn "Opcao invalida!"
            submenu_itens banco

-- Submenu 2: Usuarios
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
            mat_str <- ler_string "Informe a matricula (apenas numeros): "
            case readMaybe mat_str :: Maybe Int of
                Nothing -> do
                    putStrLn "Erro: A matricula deve conter APENAS numeros inteiros."
                    submenu_usuarios banco
                Just mat_int -> do
                    if any (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco) then do
                        putStrLn ("Erro: matricula \"" ++ show mat_int ++ "\" ja cadastrada.")
                        submenu_usuarios banco
                    else do
                        nome_str <- ler_string "Informe o nome: "
                        email_str <- ler_string "Informe o e-mail: "
                        
                        -- NOVO ESCUDO DE E-MAIL AQUI!
                        if not (validar_email email_str) then do
                            let msg_erro = "Erro: e-mail \"" ++ email_str ++ "\" está mal formatado."
                            putStrLn msg_erro
                            
                            momento <- pegar_tempo_atual
                            let log_erro = LogOperacao momento ("Tentativa de cadastro de usuário (Mat: " ++ show mat_int ++ ")") "Sistema" Erro msg_erro
                            submenu_usuarios (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
                        else do
                            let novo_user = Usuario mat_int nome_str email_str []
                            
                            momento <- pegar_tempo_atual
                            let banco_novo = cadastrar_usuario momento novo_user banco
                            
                            putStrLn "Sucesso! Usuario cadastrado."
                            submenu_usuarios banco_novo
                    
        "2" -> do
            mat_str <- ler_string "Informe a matricula para remover: "
            let mat_int = read mat_str :: Int
            momento <- pegar_tempo_atual
            let banco_novo = remover_usuario momento mat_int banco
            putStrLn "Sucesso! Usuario removido (se existia)."
            submenu_usuarios banco_novo
            
        "3" -> do
            putStrLn "\n--- Lista de Usuarios ---"
            mapM_ (\usuario -> putStrLn (show (matricula_user usuario) ++ " - " ++ nome_user usuario)) (lista_usuarios banco)
            submenu_usuarios banco
            
        "4" -> do
            menu_principal banco
            
        _ -> do
            putStrLn "Opcao invalida!"
            submenu_usuarios banco

-- Submenu 3: Empréstimos e Devoluções 
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
            id_str <- ler_string "Informe o ID do item: "
            mat_str <- ler_string "Informe a matricula do usuario: "
            
            let id_int = read id_str :: Int
            let mat_int = read mat_str :: Int
            
            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                momento <- pegar_tempo_atual
                let log_erro = LogOperacao momento ("Tentativa de empréstimo (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Usuário inexistente"
                submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
            else do
                let item_busca = filter (\item -> id_item item == id_int) (lista_itens banco)
                if null item_busca then do
                    putStrLn "Erro: Item nao encontrado no sistema."
                    momento <- pegar_tempo_atual
                    let log_erro = LogOperacao momento ("Tentativa de empréstimo (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Item inexistente"
                    submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
                else do
                    let item_alvo = head item_busca
                    if not (ta_disponivel item_alvo) then do
                        putStrLn ("\nAviso: O " ++ show (tipo item_alvo) ++ " \"" ++ titulo item_alvo ++ "\" ja esta emprestado.")
                        escolha <- ler_string "Deseja entrar na lista de espera? (S/N): "
                        
                        if escolha == "S" || escolha == "s" then do
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
            
        "2" -> do
            id_str <- ler_string "Informe o ID do item devolvido: "
            mat_str <- ler_string "Informe a matricula do usuario: "
            
            let id_int = read id_str :: Int
            let mat_int = read mat_str :: Int
            
            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                momento <- pegar_tempo_atual
                let log_erro = LogOperacao momento ("Tentativa de devolução (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Usuário inexistente"
                submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
            else do
                let emp_busca = filter (\emprestimo -> id_item_emp emprestimo == id_int && mat_user_emp emprestimo == mat_int) (lista_emprestimos banco)
                if null emp_busca then do
                    putStrLn "Erro: Nao ha registro desse emprestimo para este usuario."
                    momento <- pegar_tempo_atual
                    let log_erro = LogOperacao momento ("Tentativa de devolução (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Empréstimo não existe"
                    submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
                else do
                    momento <- pegar_tempo_atual 
                    let banco_novo = fazer_devolucao momento id_int mat_int banco
                    putStrLn "Sucesso! Devolucao registrada."
                    submenu_emprestimos banco_novo
            
        "3" -> do
            putStrLn "\n--- Lista de Emprestimos Ativos ---"
            if null (lista_emprestimos banco) then
                putStrLn "Nenhum emprestimo ativo."
            else
                mapM_ (\emprestimo -> do
                    let item_b = filter (\item -> id_item item == id_item_emp emprestimo) (lista_itens banco)
                    let user_b = filter (\usuario -> matricula_user usuario == mat_user_emp emprestimo) (lista_usuarios banco)
                    let nome_item = if null item_b then show (id_item_emp emprestimo) else titulo (head item_b)
                    let nome_pessoa = if null user_b then show (mat_user_emp emprestimo) else nome_user (head user_b)
                    
                    putStrLn ("ID Item: " ++ show (id_item_emp emprestimo) ++ " | Titulo: \"" ++ nome_item ++ "\" | Mat: " ++ show (mat_user_emp emprestimo) ++ " | Usuario: " ++ nome_pessoa ++ " | Data: " ++ data_emp emprestimo)
                ) (lista_emprestimos banco)
            submenu_emprestimos banco
            
        "4" -> do
            id_str <- ler_string "Informe o ID do item para renovar: "
            mat_str <- ler_string "Informe a matricula do usuario: "
            nova_data <- ler_string "Informe a NOVA data de devolucao: "
            
            let id_int = read id_str :: Int
            let mat_int = read mat_str :: Int
            
            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                momento <- pegar_tempo_atual
                let log_erro = LogOperacao momento ("Tentativa de renovação (ID: " ++ show id_int ++ ")") (show mat_int) Erro "Usuário inexistente"
                submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
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
            
            mat_str <- ler_string "Informe a matricula do usuario: "
            let mat_int = read mat_str :: Int
            
            let user_busca = filter (\usuario -> matricula_user usuario == mat_int) (lista_usuarios banco)
            if null user_busca then do
                putStrLn "Erro: Usuario nao encontrado no sistema."
                momento <- pegar_tempo_atual
                let log_erro = LogOperacao momento "Tentativa de lote" (show mat_int) Erro "Usuário inexistente"
                submenu_emprestimos (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
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
            
        "6" -> do
            menu_principal banco
            
        _ -> do
            putStrLn "Opcao invalida!"
            submenu_emprestimos banco

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
                        let emp_busca = filter (\emprestimo -> id_item_emp emprestimo == id_item item) (lista_emprestimos banco)
                        if not (null emp_busca) then do
                            let emprestimo_atual = head emp_busca
                            let user_busca = filter (\usuario -> matricula_user usuario == mat_user_emp emprestimo_atual) (lista_usuarios banco)
                            let nome = if null user_busca then "Mat: " ++ show (mat_user_emp emprestimo_atual) else nome_user (head user_busca)
                            putStrLn ("Codigo: " ++ show (id_item item) ++ " | Titulo: \"" ++ titulo item ++ "\" | Usuario: " ++ nome ++ " | Data: " ++ data_emp emprestimo_atual)
                        else putStrLn ("Codigo: " ++ show (id_item item) ++ " | Titulo: \"" ++ titulo item ++ "\"")
                    ) ativos
            submenu_relatorios banco
            
        "2" -> do
            putStrLn "\n--- Ranking: Usuarios Mais Ativos ---"
            let ranking_users = usuarios_mais_ativos banco
            if null ranking_users
                then putStrLn "Nenhum usuario realizou operacoes ainda."
                else do
                    mapM_ (\(pos, (nome, qtd)) -> putStrLn (show pos ++ ". " ++ nome ++ " (" ++ show qtd ++ " operacoes)")) (zip [1..] ranking_users)
            submenu_relatorios banco
            
        "3" -> do
            putStrLn "\n--- Ranking: Itens Mais Emprestados ---"
            let ranking_itens = itens_mais_emprestados banco
            if null ranking_itens
                then putStrLn "Nenhum item foi emprestado ainda."
                else mapM_ (\(nome_item, qtd) -> putStrLn (nome_item ++ " (" ++ show qtd ++ " vezes)")) ranking_itens
            submenu_relatorios banco
            
        "4" -> do
            periodo <- ler_string "Informe o periodo de busca (ex: 2026-03 ou 2026-03-16): "
            let freq = frequencia_periodo periodo banco
            putStrLn ("\nFrequencia de emprestimos registrados contendo '" ++ periodo ++ "': " ++ show freq ++ " operacoes.")
            submenu_relatorios banco
            
        "5" -> do
            putStrLn "\n--- Itens Com Lista de Espera ---"
            let itens_espera = itens_com_espera banco
            if null itens_espera
                then putStrLn "Nenhuma lista de espera ativa no momento."
                else mapM_ (\item -> putStrLn ("ID: " ++ show (id_item item) ++ " | Titulo: " ++ titulo item ++ " | Fila: " ++ show (length (fila_espera item)) ++ " pessoa(s)")) itens_espera
            submenu_relatorios banco
            
        "6" -> do
            termo <- ler_string "Informe a matricula do usuario ou operacao (ex: 2025111 ou Emprestimo): "
            putStrLn ("\n--- Relatorio de Operacoes para '" ++ termo ++ "' ---")
            let relatorio = relatorio_operacoes termo banco
            if null relatorio
                then putStrLn "Nenhuma operacao encontrada com este termo."
                else mapM_ (\log -> putStrLn (data_hora_op log ++ " " ++ descricao_op log ++ " | Envolvido: " ++ usuario_envolvido log)) relatorio
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
            id_str <- ler_string "Informe o codigo do item: "
            let id_int = read id_str :: Int
            
            let item_encontrado = buscar_item_para_edicao id_int banco
            
            if null item_encontrado
                then do 
                    putStrLn "Erro: Item nao encontrado!"
                    submenu_edicao banco
                else do
                    let item = head item_encontrado
                    putStrLn ("\nItem encontrado: \"" ++ titulo item ++ "\"")
                    putStrLn "Dados atuais:"
                    putStrLn ("1 - Titulo: " ++ titulo item)
                    putStrLn ("2 - Autor/Diretor: " ++ autor item)
                    putStrLn ("3 - Ano: " ++ show (ano item))
                    
                    campo <- ler_string "\nEscolha campo para editar (1/2/3): "
                    
                    case campo of
                        "1" -> do
                            novo_val <- ler_string "Informe novo titulo: "
                            confirma <- ler_string "Confirma edicao? (S/N): "
                            if confirma == "S" || confirma == "s" then do
                                momento <- pegar_tempo_atual
                                let banco_novo = editar_titulo_item momento id_int novo_val banco
                                putStrLn "Sucesso! Titulo alterado."
                                submenu_edicao banco_novo
                            else do putStrLn "Edicao cancelada."; submenu_edicao banco
                            
                        "2" -> do
                            novo_val <- ler_string "Informe novo autor/diretor: "
                            confirma <- ler_string "Confirma edicao? (S/N): "
                            if confirma == "S" || confirma == "s" then do
                                momento <- pegar_tempo_atual
                                let banco_novo = editar_autor_item momento id_int novo_val banco
                                putStrLn "Sucesso! Autor alterado."
                                submenu_edicao banco_novo
                            else do putStrLn "Edicao cancelada."; submenu_edicao banco
                            
                        "3" -> do
                            novo_val <- ler_string "Informe novo ano: "
                            let ano_int = read novo_val :: Int
                            
                            if not (validar_ano ano_int) then do
                                let nome_cat = if tipo item == Livro then "livros" else if tipo item == Filme then "filmes" else "jogos"
                                let msg_erro = "Erro: ano \"" ++ show ano_int ++ "\" inválido para " ++ nome_cat ++ "."
                                putStrLn msg_erro
                                momento <- pegar_tempo_atual
                                let log_erro = LogOperacao momento ("Tentativa de edição de ano (ID: " ++ show id_int ++ ")") "Sistema" Erro msg_erro
                                submenu_edicao (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
                            else do
                                confirma <- ler_string "Confirma edicao? (S/N): "
                                if confirma == "S" || confirma == "s" then do
                                    momento <- pegar_tempo_atual
                                    let banco_novo = editar_ano_item momento id_int ano_int banco
                                    putStrLn "Sucesso! Ano alterado."
                                    submenu_edicao banco_novo
                                else do 
                                    putStrLn "Edicao cancelada."
                                    submenu_edicao banco
                            
                        _ -> do putStrLn "Opcao invalida!"; submenu_edicao banco

        "2" -> do
            mat_str <- ler_string "Informe a matricula: "
            let mat_int = read mat_str :: Int
            
            let user_encontrado = buscar_user_para_edicao mat_int banco
            
            if null user_encontrado
                then do 
                    putStrLn "Erro: Usuario nao encontrado!"
                    submenu_edicao banco
                else do
                    let usuario_atual = head user_encontrado
                    putStrLn ("\nUsuario encontrado: " ++ nome_user usuario_atual)
                    putStrLn "Dados atuais:"
                    putStrLn ("1 - Nome: " ++ nome_user usuario_atual)
                    putStrLn ("2 - E-mail: " ++ email_user usuario_atual)
                    
                    campo <- ler_string "\nEscolha campo para editar (1/2): "
                    
                    case campo of
                        "1" -> do
                            novo_val <- ler_string "Informe novo nome: "
                            confirma <- ler_string "Confirma edicao? (S/N): "
                            if confirma == "S" || confirma == "s" then do
                                momento <- pegar_tempo_atual
                                let banco_novo = editar_nome_usuario momento mat_int novo_val banco
                                putStrLn "Sucesso! Nome alterado."
                                submenu_edicao banco_novo
                            else do putStrLn "Edicao cancelada."; submenu_edicao banco
                            
                        "2" -> do
                            novo_val <- ler_string "Informe novo e-mail: "
                            
                            if not (validar_email novo_val) then do
                                let msg_erro = "Erro: e-mail \"" ++ novo_val ++ "\" está mal formatado."
                                putStrLn msg_erro
                                momento <- pegar_tempo_atual
                                let log_erro = LogOperacao momento ("Tentativa de edição de e-mail (Mat: " ++ show mat_int ++ ")") "Sistema" Erro msg_erro
                                submenu_edicao (banco { historico_operacoes = historico_operacoes banco ++ [log_erro] })
                            else do
                                confirma <- ler_string "Confirma edicao? (S/N): "
                                if confirma == "S" || confirma == "s" then do
                                    momento <- pegar_tempo_atual
                                    let banco_novo = editar_email_usuario momento mat_int novo_val banco
                                    putStrLn "Sucesso! E-mail atualizado."
                                    submenu_edicao banco_novo
                                else do 
                                    putStrLn "Edicao cancelada."
                                    submenu_edicao banco
                                    
                        _ -> do putStrLn "Opcao invalida!"; submenu_edicao banco
        "3" -> menu_principal banco
        
        _ -> do
            putStrLn "Opcao invalida!"
            submenu_edicao banco

-- Submenu 7: Exportação e Importação
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
            exibir_opcoes "O que deseja exportar?" ["1 - Lista completa de Itens", "2 - Lista completa de Usuarios", "3 - Registros de Emprestimos"]
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
                _ -> do putStrLn "Opcao invalida!"; submenu_exportacao banco

        "2" -> do
            exibir_opcoes "O que deseja importar?" ["1 - Lista completa de Itens", "2 - Lista completa de Usuarios", "3 - Registros de Emprestimos"]
            op_imp <- ler_string "Opcao de importacao: "
            
            arquivo <- ler_string "Digite o nome do arquivo (ex: export_itens.csv): "
            
            existe <- doesFileExist arquivo
            if not existe then do
                putStrLn "Erro: Arquivo nao encontrado! Verifique o nome e tente novamente."
                submenu_exportacao banco
            else do
                conteudo <- readFile arquivo
                let linhas = lines conteudo
                
                case op_imp of
                    "1" -> do
                        let itens_lidos = pegar_apenas_validos (map montar_item_da_linha linhas)
                        let erros_formato = length linhas - length itens_lidos
                        
                        let (lista_final, duplicatas) = foldl (\(lista_acumulada, qtd_ignorados) novo -> 
                                if any (\existente -> id_item existente == id_item novo) lista_acumulada
                                then (lista_acumulada, qtd_ignorados + 1)
                                else (lista_acumulada ++ [novo], qtd_ignorados)
                              ) (lista_itens banco, 0) itens_lidos
                        
                        let total_ignorados = erros_formato + duplicatas
                        let inseridos = length itens_lidos - duplicatas
                        
                        putStrLn ("Importacao concluida: " ++ show inseridos ++ " itens importados com sucesso.")
                        if total_ignorados > 0 then putStrLn ("Aviso: " ++ show total_ignorados ++ " linhas ignoradas (erro de formato ou ID repetido).") else return ()
                        
                        submenu_exportacao (banco { lista_itens = lista_final })
                        
                    "2" -> do
                        let usuarios_lidos = pegar_apenas_validos (map montar_usuario_da_linha linhas)
                        let erros_formato = length linhas - length usuarios_lidos
                        
                        let (lista_final, duplicatas) = foldl (\(lista_acumulada, qtd_ignorados) novo -> 
                                if any (\existente -> matricula_user existente == matricula_user novo) lista_acumulada
                                then (lista_acumulada, qtd_ignorados + 1)
                                else (lista_acumulada ++ [novo], qtd_ignorados)
                              ) (lista_usuarios banco, 0) usuarios_lidos
                        
                        let total_ignorados = erros_formato + duplicatas
                        let inseridos = length usuarios_lidos - duplicatas
                        
                        putStrLn ("Importacao concluida: " ++ show inseridos ++ " usuarios importados com sucesso.")
                        if total_ignorados > 0 then putStrLn ("Aviso: " ++ show total_ignorados ++ " linhas ignoradas (erro de formato ou matricula repetida).") else return ()
                        
                        submenu_exportacao (banco { lista_usuarios = lista_final })
                        
                    "3" -> do
                        let emprestimos_lidos = pegar_apenas_validos (map montar_emprestimo_da_linha linhas)
                        let erros_formato = length linhas - length emprestimos_lidos
                        
                        putStrLn ("Importacao concluida: " ++ show (length emprestimos_lidos) ++ " emprestimos importados com sucesso.")
                        if erros_formato > 0 then putStrLn ("Aviso: " ++ show erros_formato ++ " linhas com erro de formato foram ignoradas.") else return ()
                        
                        submenu_exportacao (banco { lista_emprestimos = lista_emprestimos banco ++ emprestimos_lidos })
                        
                    _ -> do
                        putStrLn "Opcao invalida!"
                        submenu_exportacao banco
                        
        "3" -> menu_principal banco
        
        _ -> do
            putStrLn "Opcao invalida!"
            submenu_exportacao banco

-- Submenu 8: Auditoria
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
            mapM_ (\operacao -> putStrLn (data_hora_op operacao ++ " " ++ descricao_op operacao ++ " (" ++ show (status_op operacao) ++ ")")) (historico_operacoes banco)
            
            putStrLn "\nPressione Enter para continuar..."
            _ <- getLine 
            submenu_auditoria banco
            
        "2" -> do
            putStrLn "\n--- Historico de Alteracoes ---"
            mapM_ (\edicao -> putStrLn (data_hora_ed edicao ++ " Entidade: " ++ entidade_alterada edicao ++ "\nAntes: " ++ estado_antes edicao ++ "\nDepois: " ++ estado_depois edicao ++ "\nAlterado por: " ++ alterado_por edicao ++ "\n-")) (historico_edicoes banco)
            
            putStrLn "\nPressione Enter para continuar..."
            _ <- getLine
            submenu_auditoria banco
            
        "3" -> do
            menu_principal banco
            
        _ -> do
            putStrLn "Opcao invalida!"
            submenu_auditoria banco

-- O nosso while(1) principal
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
        _ -> do
            putStrLn "\nOpcao invalida. Tente novamente."
            menu_principal banco

-- Funcao auxiliar simples pra printar a lista bonitinha no terminal
imprimir_resultados :: [Item] -> IO ()
imprimir_resultados resultados = do
    putStrLn "\n--- Resultados da Busca ---"
    if null resultados
        then putStrLn "Nenhum item encontrado."
        else mapM_ (\item -> putStrLn ("ID: " ++ show (id_item item) ++ " | Titulo: " ++ titulo item ++ " | Autor: " ++ autor item ++ " | Ano: " ++ show (ano item) ++ " | Tipo: " ++ show (tipo item))) resultados