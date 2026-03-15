import Arquivos
import Estruturas

main :: IO ()
main = do
    let bd = BancoDeDados [Item 1 "Livro A" "Autor A" 2020 Livro True []] [Usuario 123 "João" "joao@email.com" []] [] ["Log inicial"]
    salvarBanco bd
    putStrLn "Banco salvo."
    bdCarregado <- carregarBanco
    putStrLn ("Banco carregado: " ++ show bdCarregado)
    exportarCSV bd
    putStrLn "CSV exportado."
    bdImportado <- importarCSV
    putStrLn ("CSV importado: " ++ show bdImportado)
    salvarLogs bd
    putStrLn "Logs salvos."