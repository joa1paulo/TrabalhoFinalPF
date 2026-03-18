# Sistema de Gerenciamento de Inventário de Mídias

Este projeto consiste em um sistema funcional desenvolvido em **Haskell** para o gerenciamento de uma biblioteca de mídias (livros, filmes e jogos).O software permite o controle de inventário, cadastro de usuários, empréstimos com lista de espera e geração de relatórios estatísticos.

## 🚀 Requisitos do Sistema

Para rodar este programa, é necessário estar em um ambiente **Ubuntu** (ou derivado Debian) com o compilador Haskell instalado.

## Instalação do Compilador (GHC)

Caso não possua o GHC instalado, execute os seguintes comandos no terminal:

```bash
sudo apt update
sudo apt install ghc
```
## 📥 Como Baixar o Projeto
Abra o terminal do seu Ubuntu.

Clone este repositório:
```bash
git clone https://github.com/joa1paulo/TrabalhoFinalPF.git
```
Navegue até a pasta do projeto:
```bash
cd TrabalhoFinalPF
```

## 🛠️ Como Compilar e Executar
O sistema deve ser compilado e executado via linha de comando (CLI).

Opção 1: Compilação (Recomendado para Apresentação)
Esta opção gera um executável otimizado, garantindo que todos os módulos sejam carregados corretamente.

Compilar:
```
ghc --make Main.hs -o sistema_midias
```
Rodar:
```
./sistema_midias
```

Opção 2: Execução Direta (Interpretada)
Para rodar o código sem gerar arquivos binários extras:
```
runghc Main.hs
```
