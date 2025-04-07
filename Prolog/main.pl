:- use_module(automato/automato). % Importa o módulo automato para manipular autômatos

% Define o caminho padrão para o arquivo JSON que contém as informações do autômato
default_json_path('automato.json').

% Define o caminho padrão para o arquivo TXT que contém as palavras a serem testadas
default_palavras_path('palavras.txt').

% Função principal: Inicia o simulador de autômatos
main :-
    writeln('=== Simulador de Automatos ==='),
    writeln('Bem vindo ao simulador de automatos'),
    main_loop(none). % Inicia o loop principal do programa com nenhum autômato carregado inicialmente

% Loop principal do programa: Exibe o menu e processa as opções do usuário
main_loop(Automato) :-
    writeln('\n=== Menu ==='),
    writeln('1 Carregar automato arquivo padrao'), % Opção para carregar o autômato de um arquivo JSON
    writeln('2 Visualizar informacoes do automato'), % Opção para visualizar informações do autômato carregado
    writeln('3 Testar palavra(s)'), % Opção para testar palavras no autômato
    writeln('4 Salvar resultados dos testes em JSON'), % Opção para salvar os resultados dos testes em um arquivo JSON
    writeln('5 Sair'), % Opção para sair do programa
    write('Escolha uma opcao: '),
    read_line_to_string(user_input, Opcao), % Lê a opção escolhida pelo usuário
    processar_opcao(Opcao, Automato, NovoAutomato), % Processa a opção escolhida
    continuar_ou_sair(NovoAutomato, Opcao). % Decide se continua no loop ou sai do programa

% Processa a opção escolhida pelo usuário
processar_opcao("1", _, Automato) :- % Carrega o autômato de um arquivo JSON
    carregar_json(Automato).
processar_opcao("2", none, _) :- % Verifica se há um autômato carregado antes de visualizar informações
    writeln('Nenhum automato carregado Por favor carregue o automato primeiro'),
    fail.
processar_opcao("2", Automato, Automato) :- % Visualiza informações do autômato carregado
    visualizar_info(Automato).
processar_opcao("3", none, _) :- % Verifica se há um autômato carregado antes de testar palavras
    writeln('Nenhum automato carregado Por favor carregue o automato primeiro'),
    fail.
processar_opcao("3", Automato, Automato) :- % Testa palavras no autômato carregado
    testar_palavras(Automato).
processar_opcao("4", none, _) :- % Verifica se há um autômato carregado antes de salvar resultados
    writeln('Nenhum automato carregado Por favor carregue o automato primeiro'),
    fail.
processar_opcao("4", Automato, Automato) :- % Salva os resultados dos testes em um arquivo JSON
    salvar_testes(Automato).
processar_opcao("5", _, _) :- % Sai do programa
    writeln('Saindo'),
    !.
processar_opcao(_, _, _) :- % Caso a opção seja inválida, exibe uma mensagem de erro
    writeln('Opcao invalida Tente novamente').

% Decide se continua no loop ou sai do programa
continuar_ou_sair(_, "5") :- !. % Se a opção for "5", sai do programa
continuar_ou_sair(Automato, _) :- % Caso contrário, continua no loop principal
    main_loop(Automato).

% Carrega o autômato de um arquivo JSON
carregar_json(Automato) :-
    default_json_path(Caminho), % Obtém o caminho padrão para o arquivo JSON
    criar_automato(Caminho, Automato), % Cria o autômato a partir do arquivo JSON
    writeln('Automato carregado com sucesso'). % Exibe uma mensagem de sucesso

% Visualiza informações do autômato carregado
visualizar_info(Automato) :-
    imprimir_automato(Automato). % Chama a função para imprimir as informações do autômato

% Testa palavras no autômato
testar_palavras(Automato) :-
    writeln('Deseja testar uma palavra individual ou um arquivo de palavras'),
    writeln('1 Palavra individual'), % Opção para testar uma única palavra
    writeln('2 Arquivo de palavras txt fixo'), % Opção para testar múltiplas palavras de um arquivo
    write('Escolha uma opcao: '),
    read_line_to_string(user_input, Opcao), % Lê a opção escolhida pelo usuário
    (   Opcao = "1" -> % Testa uma palavra individual
        write('Digite a palavra a ser testada: '),
        read_line_to_string(user_input, Palavra),
        teste_palavra(Automato, Palavra, Resultado), % Testa a palavra no autômato
        formatarResultado(Resultado) % Formata e exibe o resultado
    ;   Opcao = "2" -> % Testa múltiplas palavras de um arquivo
        default_palavras_path(Caminho), % Obtém o caminho padrão para o arquivo de palavras
        ler_palavras_arquivo(Caminho, Palavras), % Lê as palavras do arquivo
        maplist(testar_e_exibir(Automato), Palavras) % Testa e exibe os resultados para cada palavra
    ;   writeln('Opcao invalida') % Caso a opção seja inválida, exibe uma mensagem de erro
    ).

% Lê palavras de um arquivo TXT (uma palavra por linha)
ler_palavras_arquivo(Caminho, Palavras) :-
    open(Caminho, read, Stream), % Abre o arquivo para leitura
    read_lines(Stream, Linhas), % Lê todas as linhas do arquivo
    close(Stream), % Fecha o arquivo
    maplist(string_chars, Linhas, Palavras). % Converte as linhas em listas de caracteres

% Testa e exibe o resultado para uma palavra
testar_e_exibir(Automato, Palavra) :-
    teste_palavra(Automato, Palavra, Resultado), % Testa a palavra no autômato
    formatarResultado(Resultado). % Formata e exibe o resultado

% Formata e exibe o resultado de um teste
formatarResultado(resultado_teste{palavra: Palavra, aceita: Aceita, estados_percorridos: Estados}) :-
    format('Palavra: ~w~n', [Palavra]), % Exibe a palavra testada
    format('Aceita: ~w~n', [Aceita]), % Exibe se a palavra foi aceita ou não
    format('Estados Percorridos: ~w~n~n', [Estados]). % Exibe os estados percorridos durante o teste

% Salva os resultados dos testes em um arquivo JSON
salvar_testes(Automato) :-
    default_palavras_path(Caminho), % Obtém o caminho padrão para o arquivo de palavras
    writeln('Carregando palavras do arquivo: '), writeln(Caminho), % Exibe o caminho do arquivo
    ler_palavras_arquivo(Caminho, Palavras), % Lê as palavras do arquivo
    maplist(testar_palavra_com_aut(Automato), Palavras, Resultados), % Testa as palavras no autômato
    write('Digite o nome do arquivo para salvar os resultados: '),
    read_line_to_string(user_input, Arquivo), % Lê o nome do arquivo para salvar os resultados
    salvar_resultados(Arquivo, Resultados). % Salva os resultados no arquivo especificado

% Predicado auxiliar para testar uma palavra com o autômato
testar_palavra_com_aut(Automato, PalavraChars, Resultado) :-
    string_chars(PalavraString, PalavraChars), % Converte a lista de caracteres para string
    teste_palavra(Automato, PalavraString, Resultado). % Testa a palavra no autômato

% Lê todas as linhas de um stream
:- discontiguous read_lines/2. % Suprime o aviso de cláusulas descontínuas

read_lines(Stream, []) :- % Caso base: fim do stream
    at_end_of_stream(Stream).
read_lines(Stream, [Line|Lines]) :- % Lê uma linha e continua lendo recursivamente
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).