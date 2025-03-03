### Automata-Playground

Este projeto é um simulador de Autômatos Finitos Determinísticos (AFD) para a cadeira de Paradigmas de Linguagem de Programação (PLP) da UFCG.

O objetivo do simulador é permitir que o usuário defina um AFD, insira sequências de entrada e veja se elas são aceitas ou rejeitadas pelo autômato. O simulador também exibe o caminho percorrido pelo autômato durante a simulação. O projeto terá duas versões em linguagens distintas: **Haskell** e **Prolog**.

---

### Funcionalidades

| ID  | Funcionalidade                          | Descrição                                                                                                                                      |
| --- | --------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | Definir Autômato por arquivo JSON       | Permite ao usuário definir a estrutura de autômato finito, incluindo estados, alfabeto, função de transição, estado inicial e estados finais, utilizando um arquivo JSON. |
| 2   | Testar sequência                        | Permite ao usuário testar uma sequência de entrada no autômato definido, retornando se a sequência foi aceita ou rejeitada.                    |
| 3   | Visualizar caminho percorrido           | Mostra o caminho percorrido pelo autômato ao processar a sequência de entrada, indicando os estados visitados e as transições realizadas.       |
| 4   | Exportar resultados para arquivo JSON   | Exporta os resultados da simulação (como aceitação ou rejeição da sequência de entrada e o caminho percorrido) para um arquivo no formato JSON.  |
| 5   | Interface de linha de comando           | Fornece uma interface de linha de comando para interagir com o simulador, permitindo a execução de comandos como leitura de arquivo JSON, teste e visualização. |

---

### Como Rodar o Projeto

#### Pré-requisitos
Antes de executar o projeto, certifique-se de que as seguintes ferramentas estão instaladas no seu sistema:

- **Haskell**: [Instalar GHC e Cabal](https://www.haskell.org/ghcup/)
- **Git**: [Instalar Git](https://git-scm.com/)

#### Passos para Executar

##### 1. Clonar o Repositório
Abra o terminal e clone o repositório usando o comando:
```bash
git clone https://github.com/seu-usuario/Automata-Playground.git
cd Automata-Playground
```

##### 2. Versão em Haskell
Para rodar a versão em Haskell, siga os passos abaixo:

1. **Instalar Dependências**  
   Certifique-se de que o GHC e o Cabal estão instalados. Em seguida, instale as dependências do projeto:
   ```bash
   cabal update
   cabal install --dependencies-only
   ```

2. **Compilar o Projeto**  
   Compile o código Haskell:
   ```bash
   cabal build
   ```

3. **Executar o Simulador**  
   Execute o simulador com o seguinte comando:
   ```bash
   cabal run -- <caminho-para-arquivo-json> <sequencia-de-entrada>
   ```
   Exemplo:
   ```bash
   cabal run -- examples/afd.json "0101"
   ```

##### 3. Arquivo JSON de Entrada
Certifique-se de fornecer um arquivo JSON válido para definir o autômato. Um exemplo de estrutura JSON é mostrado abaixo:
```json
{
  "estados": ["q0", "q1", "q2"],
  "alfabeto": ["0", "1"],
  "transicoes": {
    "q0": { "0": "q1", "1": "q0" },
    "q1": { "0": "q2", "1": "q0" },
    "q2": { "0": "q2", "1": "q2" }
  },
  "estado_inicial": "q0",
  "estados_finais": ["q2"]
}
```

---

### Alunos
1. [Vitor Miranda](https://github.com/VitorMI)
2. [Alisson Amarante](https://github.com/alissonramarante)
3. [Gabriel Vilar](https://github.com/Gakjvc)

---

### Observações
- Certifique-se de que os caminhos dos arquivos JSON e as sequências de entrada estejam corretos.
- Para mais informações sobre as funcionalidades, consulte a seção **Funcionalidades** acima.
