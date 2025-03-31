:- module(automato, [
    criar_automato/2, % Cria um autômato a partir de um arquivo JSON
    imprimir_automato/1, % Imprime as informações do autômato
    teste_palavra/3, % Testa uma palavra no autômato
    salvar_resultados/2 % Salva os resultados dos testes em um arquivo JSON
]).

:- use_module(library(http/json)). % Importa a biblioteca JSON para manipulação de arquivos JSON
:- use_module(library(http/json_convert)). % Importa a biblioteca para conversão entre Prolog e JSON

% Cria um autômato a partir de um arquivo JSON
criar_automato(Arquivo, Automato) :-
    ler_json(Arquivo, Json), % Lê o arquivo JSON
    Json = json([alfabeto=Alfabeto, estadoInicial=EstadoInicial, estadosFinais=EstadosFinais, transicoes=TransicoesJSON]),
    extrair_transicoes(TransicoesJSON, Transicoes), % Extrai as transições do JSON
    Automato = automato(Alfabeto, EstadoInicial, EstadosFinais, Transicoes). % Cria o autômato

% Lê um arquivo JSON e converte seu conteúdo para uma estrutura Prolog
ler_json(Arquivo, Json) :-
    open(Arquivo, read, Stream), % Abre o arquivo para leitura
    json_read(Stream, Json), % Lê o conteúdo JSON do arquivo
    close(Stream). % Fecha o arquivo

% Extrai as transições do JSON para uma lista de tuplas (de/para)
extrair_transicoes(json(TransicoesJSON), Transicoes) :-
    findall(de(EstadoOrigem, Simbolo, EstadoDestino),
            ( member(EstadoOrigem=json(Simbolos), TransicoesJSON),
              member(Simbolo=EstadoDestino, Simbolos)
            ),
            Transicoes).

% Imprime as informações do autômato
imprimir_automato(automato(Alfabeto, EstadoInicial, EstadosFinais, Transicoes)) :-
    format("=== Informacoes do Automato ===~n", []), % Título da seção
    format("Alfabeto: ~w~n", [Alfabeto]), % Exibe o alfabeto
    format("Estado Inicial: ~w~n", [EstadoInicial]), % Exibe o estado inicial
    format("Estado(s) Final(is): ~w~n", [EstadosFinais]), % Exibe os estados finais
    format("Transicoes:~n"), % Título para as transições
    maplist(imprimir_transicao, Transicoes). % Imprime cada transição

% Imprime uma transição no formato "EstadoOrigem --(Simbolo)--> EstadoDestino"
imprimir_transicao(de(EstadoOrigem, Simbolo, EstadoDestino)) :-
    format("  ~w --(~w)--> ~w~n", [EstadoOrigem, Simbolo, EstadoDestino]). % Formata e exibe a transição