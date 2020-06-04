% sem distância
%
% Intem 1 do trabalho - grafo não direcionado
aresta(porto_alegre,florianopolis).
aresta(florianopolis,curitiba).
aresta(curitiba,sao_paulo).
aresta(curitiba,campo_grande).
aresta(sao_paulo,rio_de_janeiro).
aresta(sao_paulo,belo_horizonte).
aresta(sao_paulo,campo_grande).
aresta(campo_grande,belo_horizonte).
aresta(campo_grande,goiania).
aresta(campo_grande,cuiaba).
aresta(goiania,brasilia).
aresta(goiania,belo_horizonte).
aresta(goiania,cuiaba).
aresta(goiania,palmas).
aresta(goiania,salvador).
aresta(rio_de_janeiro,vitoria).
aresta(rio_de_janeiro,belo_horizonte).
aresta(vitoria,belo_horizonte).
aresta(vitoria,salvador).
aresta(belo_horizonte,salvador).
aresta(belo_horizonte,brasilia).
aresta(rio_branco,porto_velho).
aresta(rio_branco,manaus).
aresta(cuiaba,porto_velho).
aresta(cuiaba,manaus).
aresta(cuiaba,belem).
aresta(cuiaba,palmas).
aresta(manaus,boa_vista).
aresta(manaus,belem).
aresta(boa_vista,belem).
aresta(belem,macapa).
aresta(belem,palmas).
aresta(belem,sao_luis).
aresta(sao_luis,teresina).
aresta(teresina,palmas).
aresta(teresina,fortaleza).
aresta(teresina,recife).
aresta(teresina,salvador).
aresta(fortaleza,natal).
aresta(fortaleza,joao_pessoa).
aresta(fortaleza,recife).
aresta(fortaleza,salvador).
aresta(natal,joao_pessoa).
aresta(joao_pessoa,recife).
aresta(recife,maceio).
aresta(maceio,aracaju).
aresta(aracaju,salvador).

% set_prolog_flag(answer_write_options,[max_depth(0)]).

% Item 2 do trabalho - Predicado que percorre grafo em profundidade
% predicado que verifica se o estado de uma capital faz fronteira com estado de outra
conectado(X,Y):- aresta(X,Y).
conectado(X,Y):- aresta(Y,X).

/*
    predicado que 'avança' para a próxima capital e armazena o caminho
    em uma lista
*/
prox_node(Atual, Prox, Visitados):-
	conectado(Atual, Prox),
	not(member(Prox, Visitados))/*verifica se 'Prox' pertence ou não à lista*/.

%predicado de busca em profundidade
busca_profundidade(Meta,Meta,_,[Meta]):-!.
busca_profundidade(Inicio, Meta, Visitados, [Inicio|Caminho]):-
	prox_node(Inicio, Prox, Visitados),
	busca_profundidade(Prox, Meta, [Prox|Visitados], Caminho),!.


% com distância
aresta(porto_alegre,florianopolis,1).
aresta(florianopolis,curitiba,1).
aresta(curitiba,sao_paulo,1).
aresta(curitiba,campo_grande,1).
aresta(sao_paulo,rio_de_janeiro,1).
aresta(sao_paulo,belo_horizonte,1).
aresta(sao_paulo,campo_grande,1).
aresta(campo_grande,belo_horizonte,1).
aresta(campo_grande,goiania,1).
aresta(campo_grande,cuiaba,1).
aresta(goiania,brasilia,1).
aresta(goiania,belo_horizonte,1).
aresta(goiania,cuiaba,1).
aresta(goiania,palmas,1).
aresta(goiania,salvador,1).
aresta(rio_de_janeiro,vitoria,1).
aresta(rio_de_janeiro,belo_horizonte,1).
aresta(vitoria,belo_horizonte,1).
aresta(vitoria,salvador,1).
aresta(belo_horizonte,salvador,1).
aresta(belo_horizonte,brasilia,1).
aresta(rio_branco,porto_velho,1).
aresta(rio_branco,manaus,1).
aresta(cuiaba,porto_velho,1).
aresta(cuiaba,manaus,1).
aresta(cuiaba,belem,1).
aresta(cuiaba,palmas,1).
aresta(manaus,boa_vista,1).
aresta(manaus,belem,1).
aresta(boa_vista,belem,1).
aresta(belem,macapa,1).
aresta(belem,palmas,1).
aresta(belem,sao_luis,1).
aresta(belem,palmas,1).
aresta(sao_luis,teresina,1).
aresta(teresina,palmas,1).
aresta(teresina,fortaleza,1).
aresta(teresina,recife,1).
aresta(teresina,salvador,1).
aresta(fortaleza,natal,1).
aresta(fortaleza,joao_pessoa,1).
aresta(fortaleza,recife,1).
aresta(fortaleza,salvador,1).
aresta(natal,joao_pessoa,1).
aresta(joao_pessoa,recife,1).
aresta(recife,maceio,1).
aresta(maceio,aracaju,1).
aresta(aracaju,salvador,1).

/*
        Item 3 do trabalho - predicado que percorre grafo em
        profundidade e retorna, também, a distância
*/
% predicado que verifica se o estado de uma capital faz fronteira com estado de outra
conectado(X,Y,D):- aresta(X,Y,D).
conectado(X,Y,D):- aresta(Y,X,D).
/*
   predicado que 'avança' para a próxima capital e armazena o caminho em
   uma lista
*/
prox_node(Atual, Prox, Visitados, D):-
	conectado(Atual, Prox, D),
	not(member(Prox, Visitados)).

% predicado de busca
busca_profundidade(Meta,Meta,_,[Meta],Df,Df):-!.
busca_profundidade(Inicio, Meta, Visitados, [Inicio|Caminho], Di, Df):-
	prox_node(Inicio, Prox, Visitados, D),
        Dn is Di + D, % acumulador da distância percorrida
	busca_profundidade(Prox, Meta, [Prox|Visitados], Caminho, Dn, Df),!.

% Item 4 - predicado que retorna mais de um caminho
/*
    Este predicado usa os mesmos predicados 'prox_node' e 'conectado'
    que o item anterior
*/
busca_profundidades(Meta,Meta,_,[Meta],Df,Df):-!.
busca_profundidades(Inicio, Meta, Visitados, [Inicio|Caminho], Di, Df):-
	prox_node(Inicio, Prox, Visitados, D),
        Dn is Di + D,
	busca_profundidades(Prox, Meta, [Prox|Visitados], Caminho, Dn, Df)/**/. % ausência do 'cut'

% Item 5 do trabalho - busca em largura
consed(A,B,[B|A]).

busca_larg(Meta,[[Meta|Caminho]|_],[Meta|Caminho]):-!.
busca_larg(Meta,[Visitados|RestoVisitados],Caminho):-
	Visitados = [Inicio|_],
	Inicio \== Meta,
	findall(X,(conectado(X,Inicio,_),not(member(X,Visitados))),L),
	maplist(consed(Visitados),L,VisitadosExtendido),
	append(RestoVisitados,VisitadosExtendido,VisitadosAtualizado),
	write(VisitadosAtualizado),nl,
	busca_larg(Meta,VisitadosAtualizado,Caminho).

busca_largura(Inicio, Meta, Caminho):-
	busca_larg(Meta,[[Inicio]],Caminho).
