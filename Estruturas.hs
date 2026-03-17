module Estruturas where

-- Equivalente a um enum para as categorias pedidas no PDF
data TipoMidia = Livro | Filme | Jogo 
    deriving (Show, Read, Eq)

-- A nossa "struct" de Item
data Item = Item {
    id_item :: Int,
    titulo :: String,
    autor :: String, 
    ano :: Int,
    tipo :: TipoMidia,
    ta_disponivel :: Bool,
    fila_espera :: [Int] 
} deriving (Show, Read, Eq)

-- A nossa "struct" de Usuário
data Usuario = Usuario {
    matricula_user :: Int,
    nome_user :: String,
    email_user :: String,
    meus_emprestimos :: [Int] 
} deriving (Show, Read, Eq)

-- Registro pra sabermos quem pegou o que e quando
data Emprestimo = Emprestimo {
    id_item_emp :: Int,
    mat_user_emp :: Int,
    data_emp :: String 
} deriving (Show, Read, Eq)

---------------------------------------------------------
-- NOVAS ESTRUTURAS DE AUDITORIA E HISTÓRICO
---------------------------------------------------------
data StatusLog = Sucesso | Erro deriving (Show, Read, Eq)

-- Struct para a Opção 1: Log de Operações (Cadastros, empréstimos, erros)
data LogOperacao = LogOperacao {
    data_hora_op :: String,
    descricao_op :: String,
    usuario_envolvido :: String,
    status_op :: StatusLog,
    detalhe_erro :: String -- Fica vazio se for Sucesso
} deriving (Show, Read, Eq)

-- Struct para a Opção 2: Histórico de Alterações (Edição de dados)
data LogEdicao = LogEdicao {
    data_hora_ed :: String,
    entidade_alterada :: String,
    estado_antes :: String,
    estado_depois :: String,
    alterado_por :: String
} deriving (Show, Read, Eq)

-- O nosso "banco de dados" em memória atualizado
data BancoDeDados = BancoDeDados {
    lista_itens :: [Item],
    lista_usuarios :: [Usuario],
    lista_emprestimos :: [Emprestimo],
    historico_operacoes :: [LogOperacao], -- Lista separada para operações
    historico_edicoes :: [LogEdicao]      -- Lista separada para edições
} deriving (Show, Read, Eq)

-- Estado inicial quando não tem nada cadastrado ainda
banco_vazio :: BancoDeDados
banco_vazio = BancoDeDados [] [] [] [] []