% Mariana Silva de Carvalho 109974
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo

/*---------------------------------------------------------------------*/
/*----------------------------4.1.Consultas----------------------------*/
/*---------------------------------------------------------------------*/
%----------------------------------------------------------------------
% vizinhanca(+(L,C), -Vizinhanca)
% Verdade se Vizinhanca for uma lista ordenada de cima
% para baixo e da esquerda para a direita, sem elementos repetidos, 
% com as coordenadas das posicoes imediatamente acima, imediatamente 
% a esquerda, imediatamente a direita e imediatamente abaixo 
% da coordenada (L, C).
%----------------------------------------------------------------------
%L de linha e C de coluna
vizinhanca((L,C), Vizinhanca):-
    L1 is L - 1, L2 is L + 1, C1 is C - 1, C2 is C + 1,
    Vizinhanca = [(L1,C), (L,C1), (L,C2), (L2,C)]. 
    %Obtenho a vizinhanca e uma lista com as intersecoes sem repeticoes.


%----------------------------------------------------------------------
% vizinhancaAlargada(+(L,C), -VizinhancaAlargada)
% Verdade se VizinhancaAlargada for uma lista ordenada de cima 
% para baixo e da esquerda para a direita, sem elementos repetidos,
% com as coordenadas anteriores e ainda as diagonais da coordenada (L, C).
%----------------------------------------------------------------------
vizinhancaAlargada((L,C), VizinhancaAlargada):-
    L1 is L - 1, L2 is L + 1, C1 is C - 1, C2 is C + 1,
    VizinhancaAlargada=[(L1, C1), (L1, C), (L1, C2), (L, C1), (L, C2), (L2, C1), (L2, C), (L2, C2)]. 
    %Obtenho a vizinhanca com a vizinhanca mais as diagonais.


%----------------------------------------------------------------------
% todasCelulas(+Tabuleiro, -TodasCelulas)
% Verdade se TodasCelulas for uma lista ordenada de cima para baixo e 
% da esquerda para a direita, sem elementos repetidos, com todas as
% coordenadas do tabuleiro Tabuleiro.
%----------------------------------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas):-
    findall((L, C), (nth1(L, Tabuleiro, Linha), nth1(C, Linha, _)), TodasCelulas).
    %O programa vai encontrar a Linha de indice L no Tabuleiro
    %e dentro dessa linha vai encontrar o elemento de indice C.


%----------------------------------------------------------------------
% todasCelulas(+Tabuleiro, -TodasCelulas, +Objecto)
% Verdade se TodasCelulas for uma lista ordenada de cima para baixo e 
% da esquerda para a direita, sem elementos repetidos, com todas
% as coordenadas do tabuleiro Tabuleiro em que existe 
% um objecto do tipo Objecto.
%----------------------------------------------------------------------
%Predicado principal:
todasCelulas(Tabuleiro, TodasCelulas, Objecto):-
    primeiraAuxiliar(Tabuleiro, TodasCelulas, [], Objecto, 1, 1).

    %Caso terminal:
    primeiraAuxiliar([], ProximaLst, ProximaLst, _, _, _):-!.
    %O programa vai de linha emm linha:
    primeiraAuxiliar([LinhaAtual|Resto], TodasCelulas, ListaAnterior, Objecto, Linhas, Colunas):-
        %O programa vai ver a linha em que estou:
        segundaAuxiliar(LinhaAtual, CelulasLinha, Objecto, Linhas, Colunas),
        LinhasAuxiliar is Linhas + 1,
        %Adiciona as celulas encontrada na linha na lista acumulada
        append(ListaAnterior, CelulasLinha, ProximaLst),
        primeiraAuxiliar(Resto, TodasCelulas, ProximaLst, Objecto, LinhasAuxiliar, Colunas),!.

    %Esta auxiliar vai visitar e entender uma linha do Tabuleiro:
    segundaAuxiliar([], [], _, _, _):-!.
    %Caso o elemento atual seja igual ao Objecto:
    segundaAuxiliar([El|Cauda], [(Linhas, Colunas)|T], Objecto, Linhas, Colunas):-
        El==Objecto,
        ColunasAuxiliar is Colunas + 1,
        segundaAuxiliar(Cauda, T, Objecto, Linhas, ColunasAuxiliar),!.
    %Caso o elemento e o Objecto sejam variaveis:
    segundaAuxiliar([El|Cauda], [(Linhas, Colunas)|T], Objecto, Linhas, Colunas):-
        var(El), var(Objecto),
        ColunasAuxiliar is Colunas + 1,
        segundaAuxiliar(Cauda, T, Objecto, Linhas, ColunasAuxiliar),!.
    %Caso o elemento atual nao seja igual ao Objecto:
    segundaAuxiliar([_|Cauda], T, Objecto, Linhas, Colunas):-
        ColunasAuxiliar is Colunas + 1,
        segundaAuxiliar(Cauda, T, Objecto, Linhas, ColunasAuxiliar),!.
    

%----------------------------------------------------------------------
% calculaObjectosTabuleiro(+Tabuleiro, -ContagemLinhas, -ContagemColunas, +Objecto)
% Verdade se Tabuleiro for um tabuleiro, Objecto for o tipo de objecto que 
% se procura, e ContagemLinhas e ContagemColunas forem, respectivamente, 
% listas com o numero desses objectos por linha e por coluna.
%----------------------------------------------------------------------
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto):-
    %Verifica se e um tabuleiro:
    length(Tabuleiro, ComprimentoDoTabuleiro),
    maplist(mesmoComprimento(ComprimentoDoTabuleiro), Tabuleiro),
    %Conto os objetos nas linhas:
    maplist(contaObjetos(Objecto), Tabuleiro, ContagemLinhas),
        %Transpoe o tabuleiro para contar objetos por coluna:
        transpose(Tabuleiro, TabuleiroTransposto),
    %Conto os objetos nas colunas:
    maplist(contaObjetos(Objecto), TabuleiroTransposto, ContagemColunas).

    %Funcoes auxiliares:
        %mesmoComprimento:
        mesmoComprimento(Comprimento, Lista):-
            length(Lista, Comprimento).
        %contaObjetos:
        contaObjetos(Objecto, Lista, Contagem) :-
            %Se o objeto for uma variavel, o Objecto sera um underscore e vamos contar o numero de variaveis por linha
            (var(Objecto) -> contarVariaveis(Lista, Contagem)
            %Se o objeto nao for uma variavel, conta o numero desses objetos(exatamente iguais) na Lista
            ; nonvar(Objecto), include(==(Objecto), Lista, Objetos), length(Objetos, Contagem)).

                %Predicado auxiliar para contar o numero de variaveis na Lista
                contarVariaveis(Lista, Contagem) :-
                    include(var, Lista, Variaveis),
                    length(Variaveis, Contagem).


%----------------------------------------------------------------------
% celulaVazia(+Tabuleiro, +(L,C))
% Verdade se Tabuleiro for um tabuleiro que nao tem
% nada ou tem relva nas coordenadas (L, C).
% Se as coordenadas nao fizerem parte do tabuleiro, 
% o predicado nao deve falhar.
%----------------------------------------------------------------------
    % Predicado principal
    celulaVazia(Tabuleiro, (L,C)):-
        %Para o caso das intersecoes estarem fora do tabuleiro
        %defini condicoes para as quais as intersecoes
        %estariam fora do tabuleiro
        length(Tabuleiro, ComprimentoDoTabuleiro),
        (L<1; L>ComprimentoDoTabuleiro;
        C<1; C>ComprimentoDoTabuleiro),!.
    celulaVazia(Tabuleiro, (L,C)):-   
        %Verificar se a intersecao e livre
        nth1(L, Tabuleiro, Linha),
        nth1(C, Linha, ElementoLivre),
        (ElementoLivre\==a, ElementoLivre\==t).
	
/*---------------------------------------------------------------------*/
/*--------------------4.2.Insercao de tendas e relva-------------------*/
/*---------------------------------------------------------------------*/
%----------------------------------------------------------------------
% insereObjectoCelula(+Tabuleiro, +TendaOuRelva, +(L, C))
% Verdade se Tabuleiro for um tabuleiro e (L, C) sao as coordenadas 
% onde queremos inserir o objecto TendaOuRelva.
%----------------------------------------------------------------------
%Se o objeto for uma variavel (com um underscore) coloco ai um t ou um r
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)):-
	nth1(L, Tabuleiro, LinhaAMudar),
    nth1(C, LinhaAMudar, ElementoAMudar),
    (var(ElementoAMudar), !, ElementoAMudar = TendaOuRelva).
%Se o objeto nao for uma variavel o tabuleiro mantem-se igual
insereObjectoCelula(Tabuleiro, _, (L, C)):-
    nth1(L, Tabuleiro, LinhaAMudar),
    nth1(C, LinhaAMudar, ElementoAMudar),
    nonvar(ElementoAMudar).


%----------------------------------------------------------------------
% insereObjectoEntrePosicoes(+Tabuleiro, +TendaOuRelva, +(L, C1), +(L, C2))
% Verdade se Tabuleiro for um tabuleiro, e (L, C1) e (L, C2) sao as 
% coordenadas, na Linha L, entre as quais (incluindo) 
% se insere o objecto TendaOuRelva.
%----------------------------------------------------------------------
%Predicado principal:
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    %Acrescenta sempre mais 1 para garantir que inclui a posicao final
    NovoC2 is C2+1,
    insereEntrePosicoesAux(Tabuleiro, TendaOuRelva, L, C1, NovoC2).
    
    %Predicado auxiliar
    %Caso terminal:
    insereEntrePosicoesAux(_, _, _, C, C):-!.
    insereEntrePosicoesAux(Tabuleiro, TendaOuRelva, L, C1, C2) :-
        %Insere o objeto na celula correspondente a posicao atual:
        insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C1)), 
        %Calcula a proxima coluna:
        ProximoC is C1 + 1,
        insereEntrePosicoesAux(Tabuleiro, TendaOuRelva, L, ProximoC, C2),!.

/*---------------------------------------------------------------------*/
/*--------------------------4.3.Estrategias----------------------------*/
/*---------------------------------------------------------------------*/

%----------------------------------------------------------------------
% relva(+Puzzle)
% Verdade se Puzzle for um puzzle que, apos a aplicacao do predicado, 
% tem relva em todas as linhas/colunas cujo numero de tendas ja 
% atingiu o numero de tendas possivel nessas linhas/colunas.
%----------------------------------------------------------------------
relva(Puzzle):-
    Puzzle=(Tabuleiro, TendasPorLinha, TendasPorColuna), %as obrigatorias pelo jogo
    calculaObjectosTabuleiro(Tabuleiro, TendasTabuleiroLinha, TendasTabuleiroColuna, t), %as existentes no tabuleiro
    length(Tabuleiro, ComprimentoDoTabuleiro), 
    %linhas(tabuleiro, tendas obrigatorias, tendas ja existentes, comprimento, linha onde estou):
    linhas(Tabuleiro, TendasPorLinha, TendasTabuleiroLinha, ComprimentoDoTabuleiro, 1), 
    %Transpor o tabuleiro para aplicar a mesma funcao auxiliar para as colunas:
    transpose(Tabuleiro, TabuleiroTransposto), 
    %Aplicar a mesma funcao auxiliar para as colunas:
    linhas(TabuleiroTransposto, TendasPorColuna, TendasTabuleiroColuna, ComprimentoDoTabuleiro, 1),
    transpose(TabuleiroTransposto, Tabuleiro). 


    %Caso terminal
    linhas(_, [], [], _, _):-!.
    %vou de linha em linha    vou de numero em numero obrigatorio      tendas ja existentes no tabuleiro      comprimento      linha em que estou
    linhas(Tabuleiro, [NumTendas|RestoTendas], [NumTendasExistentes|RestoTendasExistentes], ComprimentoDoTabuleiro, LinhaAtual):-
        %Caso uma linha tenha 0 tendas na solucao
        %ou se o numero de tendas obrigatorias por linhas for igual ao numero de tendas ja existentes:
        NumTendas == NumTendasExistentes, !,
        insereObjectoEntrePosicoes(Tabuleiro, r, (LinhaAtual, 1), (LinhaAtual, ComprimentoDoTabuleiro)),
        ProximaLinha is LinhaAtual + 1,
        linhas(Tabuleiro, RestoTendas,RestoTendasExistentes, ComprimentoDoTabuleiro, ProximaLinha),!.
    %Caso em que nao se faz nada (nao se pode colocar relva):
    linhas(Tabuleiro, [NumTendas|RestoTendas], [NumTendasExistentes|RestoTendasExistentes], ComprimentoDoTabuleiro, LinhaAtual):-
        NumTendas\==NumTendasExistentes,
        ProximaLinha is LinhaAtual + 1,
        linhas(Tabuleiro, RestoTendas,RestoTendasExistentes, ComprimentoDoTabuleiro, ProximaLinha),!.   


%----------------------------------------------------------------------
% inacessiveis(+Tabuleiro)
% Verdade se Tabuleiro for um tabuleiro que, apos a aplicacao do
% predicado, tem relva em todas as posicoes inacessiveis.
%----------------------------------------------------------------------
inacessiveis(Tabuleiro):-
    length(Tabuleiro, ComprimentoDoTabuleiro),
    %Vou obter todas as intersecoes do tabuleiro:
    todasCelulas(Tabuleiro, TodasAsIntersecoesDoTabuleiro),
    %Vou obter todas as intersecoes onde se encontram arvores:
    todasCelulas(Tabuleiro, TodasAsArvoresDoTabuleiro, a),
    %Vou obter todas as celulas das vizinhancas das arvores no tabuleiro:
    maplist(vizinhanca, TodasAsArvoresDoTabuleiro, VizinhancasDeTodasAsArvoresDoTabuleiro),
    %Coloco todas as listas com as vizinhancas numa lista apenas:
    flatten(VizinhancasDeTodasAsArvoresDoTabuleiro, VizinhancasDeTodasAsArvoresFinais),
    %Retiro todas as celulas que estejam fora do tabuleiro:
    filtrarCoordenadas(ComprimentoDoTabuleiro, VizinhancasDeTodasAsArvoresFinais, VizinhancasDeTodasAsArvoresFinaisFiltradas),
    %Obtenho as celulas onde vou colocar relva:
    subtract(TodasAsIntersecoesDoTabuleiro, VizinhancasDeTodasAsArvoresFinaisFiltradas, CelulasOndeColocoRelvaComArvores),
	subtract(CelulasOndeColocoRelvaComArvores, TodasAsArvoresDoTabuleiro, CelulasOndeColocoMesmoRelva),
    %Coloco relva:
    colocarRelva(Tabuleiro, r, CelulasOndeColocoMesmoRelva).


%----------------------------------------------------------------------
% aproveita(+Puzzle)
% Verdade se Puzzle for um puzzle que, apos a aplicacao do predicado,
% tem tendas em todas as linhas e colunas as quais faltavam colocar X 
% tendas e que tinham exatamente X posicoes livres. 
% Este predicado deve ser implementado resolvendo as
% linhas, fazendo novas contagens, e resolvendo as colunas.
%----------------------------------------------------------------------
aproveita(Puzzle):-
    %Isto fornece o numero de tendas obrigatorias no tabuleiro:
    Puzzle=(Tabuleiro, NumTendasObrigatoriasPorLinha, NumTendasObrigatoriasPorColuna),
    %Calculo o numero de espacos vazios:
    calculaObjectosTabuleiro(Tabuleiro, EspacosVaziosTabuleiroLinha, EspacosVaziosTabuleiroColuna, _),
    %Calculo o numero de tendas que ja tenho no tabuleiro:
    calculaObjectosTabuleiro(Tabuleiro, TendasExistentesPorLinha, TendasExistentesPorColuna, t),
    length(Tabuleiro, ComprimentoTab),
    %Analiso as linhas:
    analisar(Tabuleiro, NumTendasObrigatoriasPorLinha, EspacosVaziosTabuleiroLinha, TendasExistentesPorLinha, ComprimentoTab, 1), 
    %Transponho o tabuleiro para analisar as colunas:
    transpose(Tabuleiro, TabuleiroTransposto),
    %Analiso as colunas:
    analisar(TabuleiroTransposto, NumTendasObrigatoriasPorColuna, EspacosVaziosTabuleiroColuna, TendasExistentesPorColuna, ComprimentoTab, 1),
    %Transponho de novo para obter o resultado final:
    transpose(TabuleiroTransposto, Tabuleiro).

                         
    %Caso terminal:
    analisar(_, [], [], [], _, _):-!.
    %vou de linha em linha, vou de numero em numero obrigatorio, tendas ja existentes no tabuleiro, vou de numero em numero existente, comprimento, linha em que estou
    analisar(Tabuleiro, [NumTendas|RestoTendas], [PrimeiroEspacoVazio|RestoDosEspacosVazios], [NumTendasExistentes|RestoTendasExistentes], ComprimentoTab, LinhaAtual):-
        NumTendasQueFaltaColocarLinha is NumTendas - NumTendasExistentes,
    	NumTendasQueFaltaColocarLinha == PrimeiroEspacoVazio, !,
        insereObjectoEntrePosicoes(Tabuleiro, t, (LinhaAtual, 1), (LinhaAtual, ComprimentoTab)),
        ProximaLinha is LinhaAtual + 1,
        analisar(Tabuleiro, RestoTendas, RestoDosEspacosVazios, RestoTendasExistentes, ComprimentoTab, ProximaLinha),!.
    %Caso em que nao se faz nada (nao se pode colocar relva):
    analisar(Tabuleiro, [_|RestoTendas], [_|RestoDosEspacosVazios], [_|RestoTendasExistentes], ComprimentoTab, LinhaAtual):-
        ProximaLinha is LinhaAtual + 1,
        analisar(Tabuleiro, RestoTendas, RestoDosEspacosVazios, RestoTendasExistentes, ComprimentoTab, ProximaLinha),!. 


%----------------------------------------------------------------------
% limpaVizinhancas((+Tabuleiro, +Linhas, +Colunas))
% Verdade se Puzzle for um puzzle que, apos a aplicacao do
% predicado, tem relva em todas as posicoes a volta de uma tenda.
%----------------------------------------------------------------------
limpaVizinhancas((Tabuleiro, _, _)):-
    encontrarVizinhancasAlargadas(Tabuleiro, ListaDeCelulasFiltradas),
    colocarRelva(Tabuleiro, r, ListaDeCelulasFiltradas).
	

	encontrarVizinhancasAlargadas(Tabuleiro, ListaDeCelulasFiltradas):-
    	length(Tabuleiro, ComprimentoDoTabuleiro),
    	%Encontrar todas as intersecoes com tendas:
		findall((L, C), (nth1(L, Tabuleiro, Linha), nth1(C, Linha, ElementoAAnalisar), ElementoAAnalisar==t), TodasAsCelulasComTendas),
    	%Encontrar as vizinhancas alargadas de cada uma das tendas encontradas:
		maplist(vizinhancaAlargada, TodasAsCelulasComTendas, TodasAsVizinhancasAlargadas),
    	%Tiras as intersecoes que estao fora do tabuleiro das vizinhancas alargadas:
    	flatten(TodasAsVizinhancasAlargadas, ListaGrandeDeTodasAsVizinhancasAlargadas),
    	filtrarCoordenadas(ComprimentoDoTabuleiro, ListaGrandeDeTodasAsVizinhancasAlargadas, ListaDeCelulasFiltradas).
    

    %Esta funcao auxiliar filtra as celulas que estao fora do tabuleiro:
	filtrarCoordenadas(Tamanho, Lista, Resultado):-
    	exclude(coordenadasForaDoTabuleiro(Tamanho), Lista, Resultado).
		coordenadasForaDoTabuleiro(Tamanho, (L, C)) :-
    		(L > Tamanho ; L < 1 ; C >  Tamanho; C < 1).
    %Esta funcao auxiliar coloca relva:
	colocarRelva(_, _, []):-!.
	colocarRelva(Tabuleiro, Objecto, [PrimeiraCoordenada|RestoDasCoordenadas]):-
    	insereObjectoCelula(Tabuleiro, Objecto, PrimeiraCoordenada),
    	colocarRelva(Tabuleiro, Objecto, RestoDasCoordenadas),!.


%----------------------------------------------------------------------
% unicaHipotese(+Puzzle)
% Verdade se Puzzle for um puzzle que, apos a aplicacao do predicado, 
% todas as arvores que tinham apenas uma posicao livre na sua vizinhanca 
% que lhes permitia ficar ligadas a uma tenda, 
% tem agora uma tenda nessa posicao.
%----------------------------------------------------------------------
unicaHipotese(Puzzle):-
    Puzzle=(Tabuleiro, _, _),
    length(Tabuleiro, ComprimentoDoTabuleiro),
    %Vou obter todas as celulas em que se encontram as arvores:
    todasCelulas(Tabuleiro, TodasAsArvoresTabuleiro, a),
    %Vou encontrar todas as vizinhancas de todas as arvores:
    findall(Viz, (member(Arvore,TodasAsArvoresTabuleiro),vizinhanca(Arvore, Viz)), VizinhancasArvores),
    %Vou filtrar as tendas de modo a tirar as celulas que nao estao no tabuleiro:
    maplist(filtrarCoordenadas(ComprimentoDoTabuleiro), VizinhancasArvores, VizinhancasArvoresFiltradas),
    filtrarTendas(VizinhancasArvoresFiltradas, Tabuleiro).

    %Apos encontrar as listas com apenas celulas vazias
    %Se apenas existir uma celula vazia significa que apenas
    %existiria uma hipotese onde se coloca uma tenda:
    filtrarTendas([], _).
    filtrarTendas([PrimeiraViz|RestoDasViz], Tabuleiro):-
        findall((L, C), (member((L, C), PrimeiraViz), nth1(L, Tabuleiro, LinhaDoElemento), nth1(C, LinhaDoElemento, Elemento), var(Elemento)), ListaDeElementos),
        length(ListaDeElementos, 1),
        member(Coordenada, ListaDeElementos),
        insereObjectoCelula(Tabuleiro, t, Coordenada),
        filtrarTendas(RestoDasViz, Tabuleiro).
    filtrarTendas([_|RestoDasViz], Tabuleiro):-
        filtrarTendas(RestoDasViz, Tabuleiro).


/*---------------------------------------------------------------------*/
/*-----------------------4.4.Tentativa e Erro--------------------------*/
/*---------------------------------------------------------------------*/

%----------------------------------------------------------------------
% valida(+LArv, +LTen)
% Verdade se LArv e LTen forem listas com todas as coordenadas em
% que existem, respetivamente, arvores e tendas, e for avaliado para 
% verdade se for possivel estabelecer uma relacao em que existe uma 
% e uma unica tenda para cada arvore nas suas vizinhancas.
%----------------------------------------------------------------------
valida(LArv, LTen):-
    length(LArv, Comprimento1),
    length(LTen, Comprimento2),
    Comprimento1==Comprimento2,

    %Obter uma lista com todas as vizinhancas das arvores:
    findall(VizinhancaArv, (member((L, C), LArv), vizinhanca((L, C), VizinhancaArv)), ListaDeVizinhancas),
    %Vou de lista em lista na vizinhanca e relaciono uma tenda com cada arvore
    %Dentro da funcao relacionarTendas vou relacionar(comparando) e vou apagando 
    %a tenda e a vizinhanca que relaciono.
    %Como vou apagando, quando chego a duas listas vazias significa que todas as arvores
    %foram relacionadas com tendas e por isso esta disposicao seria valida.
    relacionarTendas(ListaDeVizinhancas, LTen).
        relacionarTendas([], []):-!.
        relacionarTendas([PrimeiraViz|RestoDasViz], LTen):-
            member(CTenda, LTen),
            member(CTenda, PrimeiraViz),
            delete(LTen, CTenda, LTenNovo),
            relacionarTendas(RestoDasViz, LTenNovo), !.


%----------------------------------------------------------------------
% resolve(+Puzzle)
% Verdade se Puzzle for um puzzle que, apos a aplicacao do predicado,
% fica resolvido.
%----------------------------------------------------------------------
%Caso terminal:
%Serve para fazer o ciclo parar quando todas as celulas
%forem preenchidas.
resolve((Tabuleiro, Linhas, Colunas)):-
    calculaObjectosTabuleiro(Tabuleiro, Linhas, Colunas, t),
    relva((Tabuleiro, Linhas, Colunas)),
    todasCelulas(Tabuleiro, LArv, a),
    todasCelulas(Tabuleiro, LTen, t),
    valida(LArv, LTen), !.
%Aplico os predicados todos de modo a resolver o tabuleiro:
resolve((Tabuleiro, Linhas, Colunas)):-
    calculaObjectosTabuleiro(Tabuleiro, VaziasInicialL, VaziasInicialC, _),
    relva((Tabuleiro, Linhas, Colunas)),
    inacessiveis(Tabuleiro),
    aproveita((Tabuleiro, Linhas, Colunas)),
    limpaVizinhancas((Tabuleiro, Linhas, Colunas)),
    unicaHipotese((Tabuleiro, Linhas, Colunas)),
    calculaObjectosTabuleiro(Tabuleiro, VaziasFinalL, VaziasFinalC, _),
    %Se o tabuleiro antes das alteracoes for igual ao tabuleiro depois das alteracoes
    %vou tentar inserir uma tenda noutra celula vazia com a
    %funcao auxiliar chamada hipotese.
    %Maneira de ver se os tabuleiros sao iguais: se a lista de celulas vazias no
    %inicio for igual a lista de celulas vazias no final, porque o facto de nao serem
    %iguais significa que nao se modificou nada e nao ocorreu nenhuma jogada.
    %Caso contrario, vai continuar a resolver com os predicados
    %de estrategia.
    ((igual(VaziasInicialL, VaziasFinalL),
    igual(VaziasInicialC, VaziasFinalC))->
    hipotese(Tabuleiro); true),
    resolve((Tabuleiro, Linhas, Colunas)), !.

    hipotese(Tabuleiro):-
        todasCelulas(Tabuleiro, TodasCelulasVazias, _),
        member(Celula, TodasCelulasVazias),
        insereObjectoCelula(Tabuleiro, t, Celula).
    igual([], []).
    igual([H1|R1], [H2|R2]):-
        H1 = H2,
        igual(R1, R2).