# Sistema de Gerenciamento de Inventário de Mídias

[cite_start]Este projeto consiste em um sistema funcional desenvolvido em **Haskell** para o gerenciamento de uma biblioteca de mídias (livros, filmes e jogos)[cite: 4, 37]. [cite_start]O software permite o controle de inventário, cadastro de usuários, empréstimos com lista de espera e geração de relatórios estatísticos[cite: 41, 45, 49].

## 🚀 Requisitos do Sistema

[cite_start]Para rodar este programa, é necessário estar em um ambiente **Ubuntu** (ou derivado Debian) com o compilador Haskell instalado[cite: 11, 27].

### Instalação do Compilador (GHC)

Caso não possua o GHC instalado, execute os seguintes comandos no terminal:

```bash
sudo apt update
sudo apt install ghc
```
###📥 Como Baixar o Projeto
Abra o terminal do seu Ubuntu.

Clone este repositório:
```bash
git clone https://github.com/joa1paulo/TrabalhoFinalPF.git
```
Navegue até a pasta do projeto:
```bash
cd TrabalhoFinalPF
```

🛠️ Como Compilar e Executar
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
## 🤖 Uso de Inteligência Artificial no Projeto

Este projeto contou com o suporte de Inteligência Artificial (IA) como um co-piloto estratégico, otimizando o fluxo de trabalho desde a arquitetura lógica até a documentação final. Abaixo, detalhamos como a ferramenta foi integrada ao desenvolvimento:

### 🔍 Exploração e Arquitetura
* **Descoberta de Bibliotecas:** Auxílio na identificação de bibliotecas essenciais e bibliotecas padrão do ecossistema Haskell, garantindo o uso de ferramentas eficientes.
* **Mapeamento de Requisitos:** Comparação sistemática entre as exigências formais do trabalho e o estado atual do código, garantindo que todas as necessidades e funcionalidades solicitadas fossem atendidas.

### 🛠️ Desenvolvimento e Refatoração
* **Correção de Erros de Lógica:** Apoio na depuração de funções puras e no rastreamento de fluxos de dados, especialmente em estruturas de recursão e guardas.
* **Sintaxe e Indentação:** Correção automatizada de erros de formatação e ajuste de indentação, garantindo a legibilidade e o cumprimento das regras sintáticas rigorosas do Haskell.

### 🧪 Verificação e Documentação
* **Geração de Testes Unitários:** Criação de arquivos de teste para validação de casos de borda (*edge cases*), assegurando a robustez das funções implementadas.
* **Documentação (README):** A própria estruturação e redação deste arquivo Markdown foram geradas com auxílio de IA para garantir uma apresentação clara, técnica e organizada.

---

> [!IMPORTANT]
> **Nota de Integridade:** Embora a IA tenha atuado como facilitadora na sintaxe, testes e documentação, a lógica de negócio e a validação final de cada módulo foram realizadas manualmente pelos integrantes, garantindo o domínio dos conceitos de programação funcional e a integridade acadêmica do trabalho.

---
