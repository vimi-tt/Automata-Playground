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

% Testa uma palavra no autômato
teste_palavra(automato(Alfabeto, EstadoInicial, EstadosFinais, Transicoes), Palavra, Resultado) :-
    string_chars(Palavra, Simbolos), % Converte a palavra para uma lista de símbolos
    (   forall(member(Simbolo, Simbolos), member(Simbolo, Alfabeto)) -> % Verifica se todos os símbolos estão no alfabeto
        simular_transicoes(Transicoes, EstadoInicial, Simbolos, EstadoFinal, EstadosPercorridos), % Simula as transições
        (   member(EstadoFinal, EstadosFinais) -> % Verifica se o estado final é um estado de aceitação
            Aceita = true
        ;   Aceita = false
        )
    ;   _EstadoFinal = "Fora do alfabeto", % Caso a palavra contenha símbolos inválidos
        EstadosPercorridos = ["Fora do alfabeto"],
        Aceita = false
    ),
    Resultado = resultado_teste{palavra: Palavra, aceita: Aceita, estados_percorridos: EstadosPercorridos}. % Cria o resultado

% Simula as transições do autômato para uma dada palavra
simular_transicoes(_, EstadoAtual, [], EstadoAtual, [EstadoAtual]). % Caso base: fim da palavra
simular_transicoes(Transicoes, EstadoAtual, [Simbolo|Restante], EstadoFinal, [EstadoAtual|EstadosPercorridos]) :-
    member(de(EstadoAtual, Simbolo, ProximoEstado), Transicoes), % Encontra a transição válida
    simular_transicoes(Transicoes, ProximoEstado, Restante, EstadoFinal, EstadosPercorridos). % Continua a simulação

% Salva os resultados dos testes em um arquivo JSON
salvar_resultados(Arquivo, Resultados) :-
    NomePasta = 'pastaResultados/', % Define o nome da pasta para salvar os resultados
    (   exists_directory(NomePasta) -> % Verifica se a pasta existe
        true
    ;   make_directory(NomePasta), % Cria a pasta se ela não existir
        writeln('Pasta "pastaResultados" criada.')
    ),
    atom_concat(NomePasta, Arquivo, CaminhoAtom), % Concatena o nome da pasta com o nome do arquivo
    atom_string(CaminhoAtom, CaminhoCompleto), % Converte o caminho para string
    maplist(converter_resultado_para_json, Resultados, ResultadosJSON), % Converte os resultados para JSON
    open(CaminhoCompleto, write, Stream), % Abre o arquivo para escrita
    json_write_dict(Stream, json{resultados: ResultadosJSON}, [pretty(true)]), % Escreve os resultados no arquivo
    close(Stream), % Fecha o arquivo
    format("Resultados salvos em ~w~n", [CaminhoCompleto]). % Exibe o caminho do arquivo salvo

% Converte um resultado de teste para o formato JSON
converter_resultado_para_json(resultado_teste{palavra: Palavra, aceita: Aceita, estados_percorridos: Estados}, 
                               json{palavra: Palavra, aceita: Aceita, estados_percorridos: Estados}). % Mapeia os campos para JSON