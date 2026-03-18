module Estruturas where

-- categorias
data TipoMidia = Livro | Filme | Jogo 
    deriving (Show, Read, Eq)

--  Item
data Item = Item {
    id_item :: Int,
    titulo :: String,
    autor :: String, 
    ano :: Int,
    tipo :: TipoMidia,
    ta_disponivel :: Bool,
    fila_espera :: [Int] 
} deriving (Show, Read, Eq)

-- struct Usuário
data Usuario = Usuario {
    matricula_user :: Int,
    nome_user :: String,
    email_user :: String,
    meus_emprestimos :: [Int] 
} deriving (Show, Read, Eq)

-- Registro de quem pegou o que e quando
data Emprestimo = Emprestimo {
    id_item_emp :: Int,
    mat_user_emp :: Int,
    data_emp :: String,
    data_devolucao :: String 
} deriving (Show, Read, Eq)


--  AUDITORIA E HISTÓRICO

data StatusLog = Sucesso | Erro deriving (Show, Read, Eq)

-- Struct Log de Operações (Cadastros, empréstimos, erros)
data LogOperacao = LogOperacao {
    data_hora_op :: String,
    descricao_op :: String,
    usuario_envolvido :: String,
    status_op :: StatusLog,
    detalhe_erro :: String -- Fica vazio se for Sucesso
} deriving (Show, Read, Eq)

-- Struct Histórico de Alterações (Edição de dados)
data LogEdicao = LogEdicao {
    data_hora_ed :: String,
    entidade_alterada :: String,
    estado_antes :: String,
    estado_depois :: String,
    alterado_por :: String
} deriving (Show, Read, Eq)

-- O "banco de dados"
data BancoDeDados = BancoDeDados {
    lista_itens :: [Item],
    lista_usuarios :: [Usuario],
    lista_emprestimos :: [Emprestimo],
    historico_operacoes :: [LogOperacao], -- Lista para operações
    historico_edicoes :: [LogEdicao]      -- Lista para edições
} deriving (Show, Read, Eq)

-- Começa assim
banco_vazio :: BancoDeDados
banco_vazio = BancoDeDados [] [] [] [] []