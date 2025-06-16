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
tablero(K,T):- K>0,length(T,5),sublistasMismoLargo(T,K). %!por si va sin metapredicados

% tablero(0,[]).
% tablero(K,[H|T]):-K>0,length(T,5),length(H,K),tablero(K,T).

%* sublistasMismoLargo(+T, -K):
sublistasMismoLargo([], _).
sublistasMismoLargo([H |T],K) :-
    length(H, K), %aca le digo que H es una lista..
    sublistasMismoLargo(T, K).

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
% seccionTablero([H|T],ALTO, ANCHO, (I,J), ST):- length(H,K),ANCHO =< K,sublista(A,ALTO,[H|T],SL1),anchoTablero(SL1,K,ANCHO,ST).
% sublista(+Descartar, +Tomar, +L, -R)
% seccionTablero(T, ALTO, ANCHO, (I,J), ST):- sublista(I, ALTO, T, ST1), anchoTablero(ST1, J, ANCHO, ST).


% anchoTablero([], _, _, []).
% anchoTablero([H|T], I, ANCHO, [L1|L2]) :- sublista(I, ANCHO, H, L1), anchoTablero(T, I, ANCHO, L2).

seccionTablero(T, ALTO, ANCHO, (I,J), ST):- I0 is I-1,J0 is J-1,sublista(I0, ALTO, T, ST1), anchoTablero(ST1, J0, ANCHO, ST).

%anchoTablero(+T, +J, +ANCHO, -L)
anchoTablero([], _, _, []).
anchoTablero([H|T], I, ANCHO, [L1|L2]) :- sublista(I, ANCHO, H, L1), anchoTablero(T, I, ANCHO, L2).


%* Ejercicio 7: Ubicar pieza
% ubicarPieza(+Tablero, +Identificador)
ubicarPieza(T, I) :-pieza(I,[H|RESTO]),coordenadas(T,C),
                    length([H|RESTO],ALTO),
                    length(H,ANCHO),
                    seccionTablero(T,ALTO,ANCHO,C,[H|RESTO]).


%* Ejercicio 8: Ubicar piezas
% ubicarPiezas(+Tablero, +Poda, +Identificadores)

ubicarPiezas(_, _, []).
ubicarPiezas(T, P, [H|R]):-ubicarPieza(T,H),ubicarPiezas(T,P,R).


% poda(+P,+T)
poda(sinPoda,_).
% poda(P,T)

%* Ejercicio 9: Llenar Tablero

% llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(P, C, T) :- tablero(C,T), not((maplist(ground,T))), kPiezas(C,L), poda(P, T), ubicarPiezas(T,P,L2).

%* Ejercicio 10: Medición

cantSoluciones(Poda, Columnas, N) :-
                                    findall(T, llenarTablero(Poda, Columnas, T), TS),
                                    length(TS, N).

% ?- time(cantSoluciones(sinPoda, 3, N)).
% 21,394,045 inferences, 0.771 CPU in 0.774 seconds (100% CPU, 27748693 Lips)
% N = 28.

% ?- time(cantSoluciones(sinPoda, 4, N)).
% 814,452,007 inferences, 28.989 CPU in 29.095 seconds (100% CPU, 28095442 Lips)
% N = 200.

%* Ejercicio 11: Optimización

poda(podaMod5, T) :- todosGruposLibresModulo5(T).

todosGruposLibresModulo5(T) :-  

%coordenadaLibre(+C, +T)
coordenadaLibre(C, T) :- 