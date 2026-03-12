module Estruturas where

-- Equivalente a um enum em C
data TipoMidia = Livro | Filme | Jogo 
    deriving (Show, Read, Eq)

-- A nossa "struct" de Item
data Item = Item {
    id_item :: Int,
    nome_titulo :: String,
    autor_diretor :: String,
    ano_lancamento :: Int,
    tipo_midia :: TipoMidia,
    ta_disponivel :: Bool,
    fila_espera :: [Int] -- vetor (lista) de matriculas de quem ta esperando
} deriving (Show, Read, Eq)

-- A "struct" de Usuario
data Usuario = Usuario {
    matricula_user :: Int,
    nome_user :: String,
    email_user :: String,
    meus_emprestimos :: [Int] -- guarda os id_item que o cara pegou
} deriving (Show, Read, Eq)

-- A "struct" para registrar quem pegou o que
data Emprestimo = Emprestimo {
    id_item_emp :: Int,
    mat_user_emp :: Int,
    data_emp :: String 
} deriving (Show, Read, Eq)

-- O "banco de dados" que vamos ficar passando de uma funcao pra outra
data BancoDeDados = BancoDeDados {
    lista_itens :: [Item],
    lista_usuarios :: [Usuario],
    lista_emprestimos :: [Emprestimo],
    historico_log :: [String] 
} deriving (Show, Read, Eq)

-- Inicia tudo zerado
banco_vazio :: BancoDeDados
banco_vazio = BancoDeDados [] [] [] []