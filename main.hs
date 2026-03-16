-- TESTE!! AINDA VOU ALTERAR

module Main where

import Menus (menu_principal)
import Arquivos (carregar_banco)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Estruturas (BancoDeDados(..))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    putStrLn "Iniciando Sistema de Midias..."
    
    banco <- carregar_banco
    
    menu_principal banco
