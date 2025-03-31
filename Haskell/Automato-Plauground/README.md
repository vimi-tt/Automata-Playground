# **Módulo Automato**

O módulo `Automato` é uma implementação em Haskell para trabalhar com autômatos finitos determinísticos (AFD). Ele permite criar, testar e visualizar autômatos, além de salvar os resultados dos testes em arquivos JSON. Este módulo foi projetado para ser modular e fácil de usar, com suporte a leitura de arquivos JSON e geração de saída formatada.

## **Índice**
1. [Descrição](#descrição)
2. [Funcionalidades](#funcionalidades)
3. [Estrutura do Módulo](#estrutura-do-módulo)
4. [Dependências](#dependências)

---

## **Descrição**

Este módulo implementa operações básicas para manipular autômatos finitos determinísticos (AFD). Ele lê as informações do autômato de um arquivo JSON, testa palavras no autômato, exibe informações sobre ele e salva os resultados dos testes em arquivos JSON.

---

## **Funcionalidades**

### **1. Carregar Autômato**
- O módulo carrega as informações do autômato a partir de um arquivo JSON.
- As informações incluem:
  - Alfabeto
  - Estado inicial
  - Estados finais
  - Transições

### **2. Testar Palavras**
- Permite testar palavras no autômato para verificar se elas são aceitas ou rejeitadas.
- Retorna os estados percorridos durante o teste.

### **3. Visualizar Informações**
- Exibe as informações do autômato no terminal, incluindo:
  - Alfabeto
  - Estado inicial
  - Estados finais
  - Transições

### **4. Salvar Resultados**
- Salva os resultados dos testes em um arquivo JSON.
- Os resultados incluem:
  - A palavra testada
  - Se ela foi aceita ou rejeitada
  - Os estados percorridos

---

## **Estrutura do Módulo**

O módulo `Automato` contém as seguintes funções principais:

### **1. Tipo `Automato`**
- Representa um autômato finito determinístico (AFD).
- Campos:
  - `alfabeto`: Conjunto de símbolos do alfabeto (`Set String`).
  - `estadoInicial`: Estado inicial do autômato (`String`).
  - `estadosFinais`: Conjunto de estados finais (`Set String`).
  - `transicoes`: Mapa de transições (`Map.Map String (Map.Map String String)`).

### **2. Função `criarAutomato`**
- **Descrição**: Cria um autômato a partir de um arquivo JSON.
- **Parâmetros**:
  - `FilePath`: Caminho para o arquivo JSON que contém as informações do autômato.
- **Retorno**: Um valor do tipo `IO Automato`.
- **Exemplo**:
  ```haskell
  criarAutomato "automato.json"
  ```

### **3. Função `imprimirAutomato`**
- **Descrição**: Gera uma representação textual das informações do autômato.
- **Parâmetros**:
  - `Automato`: O autômato a ser exibido.
- **Retorno**: Uma `String` contendo as informações formatadas.
- **Exemplo**:
  ```haskell
  imprimirAutomato automato
  ```

### **4. Função `testePalavra`**
- **Descrição**: Testa uma palavra no autômato.
- **Parâmetros**:
  - `Automato`: O autômato no qual a palavra será testada.
  - `String`: A palavra a ser testada.
- **Retorno**: Um valor do tipo `IO ResultadoTeste`.
- **Exemplo**:
  ```haskell
  testePalavra automato "00"
  ```

### **5. Função `salvarResultados`**
- **Descrição**: Salva os resultados dos testes em um arquivo JSON.
- **Parâmetros**:
  - `FilePath`: Nome do arquivo onde os resultados serão salvos.
  - `[ResultadoTeste]`: Lista de resultados dos testes.
- **Exemplo**:
  ```haskell
  salvarResultados "resultados.json" resultados
  ```

### **6. Tipo `ResultadoTeste`**
- Representa o resultado de um teste de palavra no autômato.
- Campos:
  - `palavra`: A palavra testada (`String`).
  - `aceita`: Indica se a palavra foi aceita (`Bool`).
  - `estadosPercorridos`: Lista dos estados percorridos durante o teste (`[String]`).

---

## **Dependências**

Este módulo depende das seguintes bibliotecas do Haskell:
- `aeson`: Para trabalhar com JSON.
- `bytestring`: Para manipular arquivos binários (JSON).
- `containers`: Para trabalhar com conjuntos (`Set`) e mapas (`Map`).