#lang teachlog/ugly

rel parent/2.
rel ancestor/2.

% Facts
parent("maekar", "aegon-5").
parent("aegon-5", "aerys-2").
parent("aerys-2", "viserys").
parent("aerys-2", "daenerys").
parent("daenerys", "drogon").

% Rules
ancestor(X, Y) :- parent(X, Y).

ancestor(X, Z) :-
 parent(X, Y),
 ancestor(Y, Z).

% Queries
ancestor(X, "drogon")?
next.
next.
next.
next.

% Test cases
data zero.
data plus/2.
parent('x, 'y)?
parent(zero, 'y)?
parent(plus('x,'y), zero)?
