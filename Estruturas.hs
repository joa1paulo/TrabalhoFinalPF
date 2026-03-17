module Estruturas where

-- Equivalente a um enum para as categorias pedidas no PDF
data TipoMidia = Livro | Filme | Jogo
    deriving (Show, Read, Eq)

-- A nossa "struct" de Item
data Item = Item {
    id_item       :: Int,
    titulo        :: String,
    autor         :: String,
    ano           :: Int,
    tipo          :: TipoMidia,
    ta_disponivel :: Bool,
    fila_espera   :: [Int]
} deriving (Show, Read, Eq)

-- A nossa "struct" de Usuario
data Usuario = Usuario {
    matricula_user   :: Int,
    nome_user        :: String,
    email_user       :: String,
    meus_emprestimos :: [Int]
} deriving (Show, Read, Eq)

-- Registro para sabermos quem pegou o que e quando
data Emprestimo = Emprestimo {
    id_item_emp  :: Int,
    mat_user_emp :: Int,
    data_emp     :: String
} deriving (Show, Read, Eq)

---------------------------------------------------------
-- ESTRUTURAS DE AUDITORIA E HISTORICO
---------------------------------------------------------
data StatusLog = Sucesso | Erro deriving (Show, Read, Eq)

-- Log de Operacoes (Cadastros, emprestimos, erros)
data LogOperacao = LogOperacao {
    data_hora_op      :: String,
    descricao_op      :: String,
    usuario_envolvido :: String,
    status_op         :: StatusLog,
    detalhe_erro      :: String
} deriving (Show, Read, Eq)

-- Historico de Alteracoes (Edicao de dados)
data LogEdicao = LogEdicao {
    data_hora_ed      :: String,
    entidade_alterada :: String,
    estado_antes      :: String,
    estado_depois     :: String,
    alterado_por      :: String
} deriving (Show, Read, Eq)

-- O banco de dados em memoria
data BancoDeDados = BancoDeDados {
    lista_itens         :: [Item],
    lista_usuarios      :: [Usuario],
    lista_emprestimos   :: [Emprestimo],
    historico_operacoes :: [LogOperacao],
    historico_edicoes   :: [LogEdicao]
} deriving (Show, Read, Eq)

-- Estado inicial vazio
banco_vazio :: BancoDeDados
banco_vazio = BancoDeDados [] [] [] [] []
