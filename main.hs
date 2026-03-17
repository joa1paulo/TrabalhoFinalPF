{-
INSTRUCOES PARA COMPILAR E EXECUTAR NO UBUNTU:
1. Abra o terminal na pasta dos arquivos.
2. Compile o programa usando o comando:
   ghc --make Main.hs -o sistema_midias
3. Execute o programa gerado com o comando:
   ./sistema_midias
-}

module Main where

import Menus (menu_principal)
import Arquivos (carregar_banco)

main :: IO ()
main = do
    putStrLn "Iniciando o Sistema de Gerenciamento de Midias..."

    -- Le o arquivo biblioteca.txt ou inicia com banco vazio
    banco_inicial <- carregar_banco

    -- Inicia o loop principal do menu
    menu_principal banco_inicial
