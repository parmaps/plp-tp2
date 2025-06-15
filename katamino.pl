:- use_module(piezas).

% Ejercicio 1: Sublista
% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :-
    append(Prefijo, Sufijo, L),
    length(Prefijo, Descartar),
    append(R, _, Sufijo),
    length(R, Tomar). 


% Ejercicio 2: Tablero

% crear_fila(+K, -Fila)
crear_fila(K, Fila) :-    
    length(Fila, K). 

% tablero(+K, -T) 
tablero(K, T) :-
    K > 0,
    length(T, 5),               
    maplist(crear_fila(K), T).


% Ejercicio 3: Tamaño

% filas_mismo_largo(+M, -C):
filas_mismo_largo([], _).
filas_mismo_largo([PrimeraFila | RestoFilas], C) :-
    length(PrimeraFila, C),
    filas_mismo_largo(RestoFilas, C).

% tamaño(+M, -F, -C)
tamaño([], 0, 0).
tamaño(M, F, C) :-
    M = [PrimeraFila | _],
    length(M, F),    
    length(PrimeraFila, C),
    filas_mismo_largo(M, C).