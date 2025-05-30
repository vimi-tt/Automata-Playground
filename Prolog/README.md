# **Automato Playground**

**Automato Playground** é uma aplicação em Prolog para simulação e manipulação de Autômatos Finitos Determinísticos (AFDs). O projeto inclui um módulo (`automato`) com operações essenciais sobre AFDs e um programa principal (`main`) que fornece uma interface interativa via console. É possível carregar autômatos a partir de arquivos JSON, testar palavras, visualizar detalhes e salvar os resultados em JSON.

---

## **Índice**

1. [Descrição](#descrição)  
2. [Funcionalidades](#funcionalidades)  
3. [Estrutura do Projeto](#estrutura-do-projeto)  
4. [Dependências](#dependências)  
5. [Como Usar](#como-usar)  

---

## **Descrição**

O projeto implementa um simulador interativo para AFDs usando Prolog. A lógica principal reside no módulo `automato`, que permite a criação e manipulação de autômatos, enquanto o programa `main` apresenta uma interface de linha de comando simples e intuitiva. A entrada e saída de dados ocorrem em formato JSON para facilitar a integração e visualização dos resultados.

---

## **Funcionalidades**

### 1. **Carregar Autômato**
- Carrega um autômato a partir de um arquivo JSON (`automato.json`, por padrão).
- Componentes:
  - Alfabeto  
  - Estado inicial  
  - Estados finais  
  - Transições  

### 2. **Testar Palavras**
- Testa palavras individualmente ou em lote (via `palavras.txt`).
- Exibe:
  - Palavra testada  
  - Aceitação (`true` ou `false`)  
  - Caminho percorrido nos estados  

### 3. **Visualizar Informações**
- Mostra os componentes do autômato:
  - Alfabeto  
  - Estado inicial  
  - Estados finais  
  - Transições  

### 4. **Salvar Resultados**
- Exporta os resultados dos testes para um arquivo JSON.
- Informações salvas:
  - Palavra testada  
  - Resultado da aceitação  
  - Sequência de estados visitados  

### 5. **Interface Interativa**
- Menu de opções:
  - Carregar autômato  
  - Visualizar informações  
  - Testar palavras  
  - Salvar resultados  
  - Sair  

---

## **Estrutura do Projeto**

O projeto é composto por dois arquivos principais:

### 1. **Módulo `automato`**
Contém as funcionalidades para criação, visualização e teste de autômatos.

- **Predicados principais**:
  - `criar_automato/2`: Lê um JSON e cria um AFD.  
    Exemplo: `criar_automato("automato.json", Automato).`
  - `imprimir_automato/1`: Exibe informações do AFD.  
    Exemplo: `imprimir_automato(Automato).`
  - `teste_palavra/3`: Testa se uma palavra é aceita pelo autômato.  
    Exemplo: `teste_palavra(Automato, "ab", Resultado).`
  - `salvar_resultados/2`: Salva os resultados em um JSON.  
    Exemplo: `salvar_resultados("resultados.json", Resultados).`

- **Representações**:
  - `automato(Alfabeto, EstadoInicial, EstadosFinais, Transicoes)`  
  - Transições: `de(Origem, Simbolo, Destino)`
  - Resultado do teste:
    ```prolog
    resultado_teste{
      palavra: "abc",
      aceita: true,
      estados_percorridos: [q0, q1, q2]
    }
    ```

### 2. **Programa Principal (`main`)**
Fornece a interface interativa do sistema.

- **Predicados principais**:
  - `main/0`: Inicia o simulador.  
  - `main_loop/1`: Gerencia o menu interativo.  
  - `carregar_json/1`: Lê o arquivo `automato.json`.  
  - `testar_palavras/1`: Testa palavras manualmente ou via arquivo.  
  - `salvar_testes/1`: Salva os resultados em JSON.

- **Arquivos esperados**:
  - `automato.json`: Definição do autômato.  
  - `palavras.txt`: Palavras para teste (uma por linha).  

---

## **Dependências**

Este projeto utiliza bibliotecas padrão do Prolog:

- `library(http/json)` – Leitura e escrita de arquivos JSON  
- `library(http/json_convert)` – Conversão entre termos Prolog e JSON  

---

## **Como Usar**

### 1. **Preparação**
- Crie um arquivo `automato.json` contendo a definição do AFD.  
- (Opcional) Crie um `palavras.txt` com as palavras a serem testadas.

### 2. **Executar o Programa**
No interpretador Prolog:

```prolog
?- [main].
?- main.
```

### 3. **Menu Interativo**
```
=== Simulador de Autômatos ===
Bem-vindo ao simulador de autômatos!

   === Menu ===
   1 Carregar automato arquivo padrao
   2 Visualizar informacoes do automato
   3 Testar palavra(s)
   4 Salvar resultados dos testes em JSON
   5 Sair
   Escolha uma opcao: 1
   Automato carregado com sucesso
   ```

=== Menu ===
1. Carregar autômato (arquivo padrão)
2. Visualizar informações do autômato
3. Testar palavra(s)
4. Salvar resultados dos testes em JSON
5. Sair
Escolha uma opção: 
```