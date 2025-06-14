:- use_module(piezas).

% Ejercicio 1: Sublista
% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :-
    append(Prefijo, Sufijo, L),
    length(Prefijo, Descartar),
    append(R, _, Sufijo),
    length(R, Tomar). 