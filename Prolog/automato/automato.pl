:- module(automato, [
    criar_automato/2, % Cria um autômato a partir de um arquivo JSON
    imprimir_automato/1, % Exibe as informações do autômato
    teste_palavra/3, % Verifica se uma palavra é aceita pelo autômato
    salvar_resultados/2 % Exporta resultados de testes para um arquivo JSON
]).

:- use_module(library(http/json)). % Biblioteca para leitura e escrita de JSON
:- use_module(library(http/json_convert)). % Biblioteca para conversão Prolog-JSON


% Carrega um arquivo JSON e o converte em estrutura Prolog
ler_json(Arquivo, Json) :-
    open(Arquivo, read, Stream), % Abre o arquivo em modo leitura
    json_read(Stream, Json), % Converte o conteúdo JSON em Prolog
    close(Stream). % Fecha o fluxo de leitura

% Transforma as transições do formato JSON em uma lista de tuplas
extrair_transicoes(json(TransicoesJSON), Transicoes) :-
    findall(de(EstadoOrigem, Simbolo, EstadoDestino),
            ( member(EstadoOrigem=json(Simbolos), TransicoesJSON),
              member(Simbolo=EstadoDestino, Simbolos)
            ),
            Transicoes).

% Cria um autômato a partir de um arquivo JSON
criar_automato(Arquivo, Automato) :-
    ler_json(Arquivo, Json), % Carrega o conteúdo do arquivo JSON
    Json = json([alfabeto=Alfabeto, estadoInicial=EstadoInicial, estadosFinais=EstadosFinais, transicoes=TransicoesJSON]),
    extrair_transicoes(TransicoesJSON, Transicoes), % Converte transições do JSON para lista
    Automato = automato(Alfabeto, EstadoInicial, EstadosFinais, Transicoes). % Define a estrutura do autômato

% Exibe as informações do autômato no console
imprimir_automato(automato(Alfabeto, EstadoInicial, EstadosFinais, Transicoes)) :-
    format("=== Informacoes do Automato ===~n", []), % Cabeçalho da exibição
    format("Alfabeto: ~w~n", [Alfabeto]), % Lista os símbolos do alfabeto
    format("Estado Inicial: ~w~n", [EstadoInicial]), % Mostra o estado inicial
    format("Estado(s) Final(is): ~w~n", [EstadosFinais]), % Lista os estados finais
    format("Transicoes:~n"), % Cabeçalho das transições
    maplist(imprimir_transicao, Transicoes). % Exibe cada transição

% Formata e exibe uma transição no console
imprimir_transicao(de(EstadoOrigem, Simbolo, EstadoDestino)) :-
    format("  ~w --(~w)--> ~w~n", [EstadoOrigem, Simbolo, EstadoDestino]). % Mostra transição no formato origem-símbolo-destino

% Verifica se uma palavra é aceita pelo autômato
teste_palavra(automato(Alfabeto, EstadoInicial, EstadosFinais, Transicoes), Palavra, Resultado) :-
    string_chars(Palavra, Simbolos), % Divide a palavra em lista de símbolos
    (   forall(member(Simbolo, Simbolos), member(Simbolo, Alfabeto)) -> % Confere se os símbolos pertencem ao alfabeto
        simular_transicoes(Transicoes, EstadoInicial, Simbolos, EstadoFinal, EstadosPercorridos), % Executa a simulação
        (   member(EstadoFinal, EstadosFinais) -> % Verifica se o estado final é de aceitação
            Aceita = true
        ;   Aceita = false
        )
    ;   _EstadoFinal = "Fora do alfabeto", % Define mensagem para símbolos inválidos
        EstadosPercorridos = ["Fora do alfabeto"],
        Aceita = false
    ),
    Resultado = resultado_teste{palavra: Palavra, aceita: Aceita, estados_percorridos: EstadosPercorridos}. % Estrutura o resultado

% Simula o processamento da palavra no autômato
simular_transicoes(_, EstadoAtual, [], EstadoAtual, [EstadoAtual]). % Caso base: retorna o estado atual ao fim da palavra
simular_transicoes(Transicoes, EstadoAtual, [Simbolo|Restante], EstadoFinal, [EstadoAtual|EstadosPercorridos]) :-
    member(de(EstadoAtual, Simbolo, ProximoEstado), Transicoes), % Busca transição válida
    simular_transicoes(Transicoes, ProximoEstado, Restante, EstadoFinal, EstadosPercorridos). % Processa o restante da palavra

% Salva os resultados dos testes em um arquivo JSON
salvar_resultados(Arquivo, Resultados) :-
    NomePasta = 'pastaResultados/', % Define o diretório de saída
    (   exists_directory(NomePasta) -> % Verifica existência do diretório
        true
    ;   make_directory(NomePasta), % Cria o diretório se necessário
        writeln('Pasta "pastaResultados" criada.')
    ),
    atom_concat(NomePasta, Arquivo, CaminhoAtom), % Gera o caminho completo do arquivo
    atom_string(CaminhoAtom, CaminhoCompleto), % Converte o caminho para string
    maplist(converter_resultado_para_json, Resultados, ResultadosJSON), % Converte resultados para formato JSON
    open(CaminhoCompleto, write, Stream), % Abre o arquivo em modo escrita
    json_write_dict(Stream, json{resultados: ResultadosJSON}, [pretty(true)]), % Grava os resultados no arquivo
    close(Stream), % Fecha o fluxo de escrita
    format("Resultados salvos em ~w~n", [CaminhoCompleto]). % Confirma a gravação

% Converte um resultado de teste para o formato JSON
converter_resultado_para_json(resultado_teste{palavra: Palavra, aceita: Aceita, estados_percorridos: Estados}, 
                               json{palavra: Palavra, aceita: Aceita, estados_percorridos: Estados}). % Mapeia campos do resultado para JSON