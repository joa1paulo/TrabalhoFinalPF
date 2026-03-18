{-
INSTRUCOES PARA COMPILAR E EXECUTAR NO UBUNTU:
1. Abra o terminal na pasta dos arquivos.
2. Compile o programa usando o comando:
   ghc Main.hs -o sistema_midias
3. Execute o programa gerado com o comando:
   ./sistema_midias
-}

module Main where

import Menus (menu_principal)
import Arquivos (carregar_banco)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
    
    hSetBuffering stdout NoBuffering
    putStrLn "Iniciando o Sistema de Gerenciamento de Midias..."
    
    -- Le o arquivo biblioteca.txt ou cria um banco vazio se nao existir
    banco_inicial <- carregar_banco
    
    -- Passa os dados pro loop do menu
    menu_principal banco_inicial
