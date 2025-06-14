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