:- use_module(piezas).

% Ejercicio 1: Sublista
% sublista(+Descartar, +Tomar, +L, -R)
% sublista(Descartar, Tomar, L, R) :-
%     append(Prefijo, Sufijo, L),
%     length(Prefijo, Descartar),
%     append(R, _, Sufijo),
%     length(R, Tomar).

%! Lo veo bien, hay que preguntar cual esta mas acertada con respecto al paradigma

%* sublista(+Descartar, +Tomar, +L, -R)
sublista(0, Tomar, L, R) :- length(R,Tomar), append(R,_,L).
sublista(Descartar, Tomar, [_|LS], R) :- Descartar>0, D2 is Descartar-1, sublista(D2, Tomar, LS, R).

%* Ejercicio 2: Tablero

% tablero(+K, -T)
% tablero(K, T) :-
%     K > 0,
%     length(T, 5),
%     maplist(esFilaK(K), T). %! Se puede? Consultar.

%* tablero(+K, -T)
tablero(K,T):- K>0,length(T,5),todasListasMismoLargo(T,K). %!por si va sin metapredicados

% tablero(0,[]).
% tablero(K,[H|T]):-K>0,length(T,5),length(H,K),tablero(K,T).

%* todasListasMismoLargo(+T, -K):
todasListasMismoLargo([], _).
todasListasMismoLargo([H |T],K) :-
    length(H, K),
    todasListasMismoLargo(T, K).

%* Ejercicio 3: Tamaño

%* tamaño(+M, -F, -C)
tamano([[]], 0, 0).
tamano([H|T], F, C) :- length([H|T], F), length(H, C).

%* Ejercicio 4: Coordenadas

%*coordenadas(+T, -IJ) :-
coordenadas([H| _], (I, J)) :-
    length(H, K),
    between(1, 5, I),
    between(1, K, J).

%* Ejercicio 5: K-piezas
%kPiezas(0, []).
%kPiezas(K,P) :- K>0, nombrePiezas(L),length(L,NL),NL >= K,
%                    between(0,NL,N),
%                    sublista(N,K,L,P). % Va por aca pero todavia falta, ya que sublista saca solo los primeros elementos. sublistaBetween..

%kPiezas(+K,-PS)
% kPiezas(K, PS) :- K > 0, nombrePiezas(L), combinacionesEnOrden(K, L, PS).

%combinacionesPosibles(+KTomo,+Lista,-Combi)
% combinacionesEnOrden(0,_,[]).
% combinacionesEnOrden(K,[H|T],[H|C]) :- K > 0, length(T,NL), K1 is K-1, NL >= K1, combinacionesEnOrden(K1,T,C).
% combinacionesEnOrden(K,[_|T],C) :- K > 0, length(T,NL), NL >= K, combinacionesEnOrden(K,T,C).

%*kPiezas(+K,-PS)
kPiezas(K,PS) :- nombrePiezas(L), partesOrdenadas(L, PS), length(PS, K). 

% partesOrdenadas (+XS, -YS)
partesOrdenadas([], []).
partesOrdenadas([H|T], [H|R]) :- partesOrdenadas(T, R).
partesOrdenadas([_|T], R) :- partesOrdenadas(T, R).
% L es fija,

%* Ejercicio 6: Seccion Tablero

%seccionTablero(+T,+ALTO, +ANCHO, +IJ, ?ST)
% seccionTablero(T,ALTO, ANCHO, IJ, ST):-
