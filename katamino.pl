:- use_module(piezas).

%* Ejercicio 1: Sublista

% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :-
    append(Prefijo, Sufijo, L),
    length(Prefijo, Descartar),
    append(R, _, Sufijo),
    length(R, Tomar).

%Revesibilidad
%El predicado sublista es reversible en sus argumentos Descartar y R, si es que Tomar y L están instanciados.
%Teniendo en cuenta que sublista usa predicados append y length, ambos reversibles, no hay problema si 
%Descartar y R se instancian o no.

%Caso) sublista(-Descartar, +Tomar, +L, -R)
%Cuando se ejecuta length(Prefijo, Descartar), se generan valores posibles de Prefijos de la lista L de longitud
%Descartar. Entonces con append(Prefijo, Sufijo, L) se obtienen los Sufijos que resultan de sacar esos 
%elementos del Prefijo. Luego, con append(R, _, Sufijo) se instancian todas las posibles sublistas R y se 
%corrobora con length(R, Tomar) que efectivamente esta sublista tome la cantidad elementos que se pasa como 
%argumento. Por lo tanto, cuando Descartar y R no estan instanciados se pueden obtener varias soluciones 
%posibles tomando de L la cantidad de elementos que pide Tomar.

%Caso) sublista(-Descartar, +Tomar, +L, +R) 
%Si R si se encuentra instanciado, el predicado append(R, _, Sufijo) verifica si R es un posible Sufijo de L 
%y si cumple con la longitud que pide Tomar mediante length(R, Tomar). A partir de ese Sufijo se obtiene el 
%Prefijo con append(Prefijo, Sufijo, L) y con length(Prefijo, Descartar) instancia en Descartar la cantidad de
%elementos que deben eliminarse al inicio de L para que R sea una sublista que cumple lo especificado. 
%Con esta instanciación de sublista, se obtiene un solo resultado que va a ser el valor de Descartar.

%Caso) sublista(+Descartar, +Tomar, +L, +R)
%Con Descartar y R instanciados, el predicado simplemente verifica que R sea resultado de Descartar y Tomar 
%elementos de la lista L. Primero, append(Prefijo, Sufijo, L) genera posibles particiones de L y 
%length(Prefijo, Descartar) asegura Prefijo que se va a eliminar sea del tamaño pedido. Luego, 
%append(R, _, Sufijo) se comprueba que R este contenido al inicio del Sufijo y length(R, Tomar) confirma 
%que la sublista sea de la longitud correspondiente a Tomar. Por lo tanto, tiene éxito si se cumplen con las 
%condiciones del argumento.


%* Ejercicio 2: Tablero

% tablero(+K, -T)
tablero(K, T) :-
    K > 0,
    length(T, 5),
    maplist(flipLength(K), T). 

flipLength(K,L):- length(L,K).

%* Ejercicio 3: Tamaño

% tamaño(+M, -F, -C)
tamano([H|T], F, C) :- length([H|T], F), length(H, C).

%* Ejercicio 4: Coordenadas

% coordenadas(+T, -IJ)
coordenadas([H| _], (I, J)) :-
    length(H, K),
    between(1, 5, I),
    between(1, K, J).

%* Ejercicio 5: K-piezas

%kPiezas(+K,-PS)
kPiezas(K, PS) :- K > 0, nombrePiezas(L), combinacionesEnOrden(K, L, PS).

% %combinacionesPosibles(+KTomo,+Lista,-Combi)
combinacionesEnOrden(0,_,[]).
combinacionesEnOrden(K,[H|T],[H|C]) :- K > 0, length(T,NL), K1 is K-1, NL >= K1, combinacionesEnOrden(K1,T,C).
combinacionesEnOrden(K,[_|T],C) :- K > 0, length(T,NL), NL >= K, combinacionesEnOrden(K,T,C).

%* Ejercicio 6: Seccion Tablero

%seccionTablero(+T,+ALTO, +ANCHO, +IJ, ?ST)
seccionTablero(T, ALTO, ANCHO, (I,J), ST):- I0 is I-1,J0 is J-1,sublista(I0, ALTO, T, ST1), anchoTablero(ST1, J0, ANCHO, ST). % maplist

%anchoTablero(+T, +J, +ANCHO, -L)
anchoTablero([], _, _, []).
anchoTablero([H|T], I, ANCHO, [L1|L2]) :- sublista(I, ANCHO, H, L1), anchoTablero(T, I, ANCHO, L2).


%* Ejercicio 7: Ubicar pieza

% ubicarPieza(+Tablero, +Identificador)
ubicarPieza(T, I) :-pieza(I,P),coordenadas(T,C), %!pasar a tamaño
                    tamano(P,ALTO,ANCHO),
                    seccionTablero(T,ALTO,ANCHO,C,P).


%* Ejercicio 8: Ubicar piezas

% ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(_, _, []).
ubicarPiezas(T, P, [H|R]):-ubicarPieza(T,H),poda(P,T),ubicarPiezas(T,P,R).

% poda(+P,+T)
poda(sinPoda,_).
% poda(P,T)

%* Ejercicio 9: Llenar Tablero

% llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(P, C, T) :- tablero(C,T), kPiezas(C,L), ubicarPiezas(T,P,L).

%* Ejercicio 10: Medición

cantSoluciones(Poda, Columnas, N) :-findall(T, llenarTablero(Poda, Columnas, T), TS),
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