# **Simulador de Autômatos**

O projeto `Simulador de Autômatos` é uma implementação em Haskell para manipulação e simulação de autômatos finitos determinísticos (AFD). Ele combina o módulo `Automato`, que fornece funcionalidades essenciais para criar, testar e gerenciar AFDs, com um programa principal (`Main`) que oferece uma interface interativa via console. O simulador suporta leitura de arquivos JSON, testes de palavras e exportação de resultados, sendo ideal para estudos ou experimentos com autômatos.

## **Índice**
1. [Descrição](#descrição)
2. [Funcionalidades](#funcionalidades)
3. [Estrutura do Projeto](#estrutura-do-projeto)
4. [Dependências](#dependências)

---

## **Descrição**

Este projeto implementa um simulador interativo para autômatos finitos determinísticos (AFD) em Haskell. O módulo `Automato` encapsula as operações principais, como criação de AFDs a partir de arquivos JSON, teste de palavras e salvamento de resultados, enquanto o módulo `Main` oferece uma interface de usuário simples e funcional. O simulador é projetado para ser modular, reutilizável e fácil de entender, com suporte a entrada/saída em formatos JSON e TXT.

---

## **Funcionalidades**

### **1. Carregar Autômato**
- Carrega a configuração de um AFD a partir de um arquivo JSON (`automato.json` por padrão).
- Componentes suportados:
  - Alfabeto (conjunto de símbolos).
  - Estado inicial.
  - Conjunto de estados finais.
  - Tabela de transições.

### **2. Testar Palavras**
- Permite testar palavras individualmente ou em lote (via arquivo `palavras.txt`).
- Resultados incluem:
  - Palavra testada.
  - Status de aceitação (`True` ou `False`).
  - Lista de estados percorridos.

### **3. Visualizar Informações**
- Exibe no console os detalhes do AFD carregado:
  - Alfabeto.
  - Estado inicial.
  - Estados finais.
  - Transições.

### **4. Salvar Resultados**
- Exporta os resultados dos testes para um arquivo JSON especificado pelo usuário.
- Resultados salvos contêm:
  - Palavra testada.
  - Status de aceitação.
  - Estados percorridos.

### **5. Interface Interativa**
- Menu de opções no console:
  - Carregar autômato.
  - Visualizar informações.
  - Testar palavras (individual ou em lote).
  - Salvar resultados.
  - Sair.

---

## **Estrutura do Projeto**

O projeto é composto por dois módulos principais:

### **1. Módulo `Automato`**
- Define as operações fundamentais para manipulação de AFDs.
- **Tipos**:
  - `Automato`: Estrutura que representa um AFD.
    - `alfabeto`: `Set String` (conjunto de símbolos).
    - `estadoInicial`: `String` (estado inicial).
    - `estadosFinais`: `Set String` (estados finais).
    - `transicoes`: `Map String (Map String String)` (tabela de transições).
  - `ResultadoTeste`: Estrutura para resultados de teste.
    - `palavra`: `String` (palavra testada).
    - `aceita`: `Bool` (aceitação).
    - `estadosPercorridos`: `[String]` (estados visitados).
- **Funções principais**:
  - `criarAutomato :: FilePath -> IO Automato`: Cria um AFD a partir de um arquivo JSON.
  - `imprimirAutomato :: Automato -> String`: Gera uma representação textual do AFD.
  - `testePalavra :: Automato -> String -> IO ResultadoTeste`: Testa uma palavra no AFD.
  - `salvarResultados :: FilePath -> [ResultadoTeste] -> IO ()`: Salva resultados em JSON.

### **2. Módulo `Main`**
- Implementa a interface interativa do simulador.
- **Funções principais**:
  - `main :: IO ()`: Inicia o simulador.
  - `mainLoop :: Maybe Automato -> IO ()`: Controla o loop do menu interativo.
  - `carregarJSON :: IO (Maybe Automato)`: Carrega o AFD do arquivo padrão.
  - `testarPalavras :: Maybe Automato -> IO (Maybe Automato)`: Testa palavras individualmente ou em lote.
  - `salvarTestes :: Maybe Automato -> IO (Maybe Automato)`: Salva resultados dos testes.
- **Arquivos padrão**:
  - `automato.json`: Configuração do AFD.
  - `palavras.txt`: Lista de palavras para teste (uma por linha).

---

## **Dependências**

O projeto requer as seguintes bibliotecas Haskell:
- `aeson`: Para manipulação de JSON.
- `bytestring`: Para operações com arquivos binários.
- `containers`: Para uso de `Set` e `Map`.
- `base`: Biblioteca padrão (inclui `Control.Monad` para `forM` e `forM_`).
