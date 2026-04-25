iesimo(0,[X],X).
iesimo(1,[X|Xs],Y) :- I\=0, I2 is I-1, iesimo(I2,XS,Y).


% Las var que usan operadadores aritmeticos tienen que estar instanciados

iesimo(I,L,X) :- novar(I)  I>=0, length(L1, [X|_],L).
iesimo(I,L,X) :- var(I), append(L1, [X|_],L, length(L1,I)).

desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

pmq(X,Y).
pmq(X,Y) :- between(0, X, Y) ,par(Y).

par(Y) :- par(Y), between(0,X,Y).


%@ importante:: 
% cada solucion infinita debe tener un unico generador infinito.

coprimos(X,Y) :- generarPares(X,Y), 1 =.= gcd(X,Y).


generarPares(X,Y) :- desde(0, N), paresQueSuman(N,X,Y).


paresQueSuman(N,X,Y) :- between(1,N,X), Y is N-X.


corteMasParejo(L,L1,L2) :- append(L1,L2,L), not(hayCorteMasParejo(I,D,L)).


hayCorteMasParejo(I,D,L) :- append(I2,D2,L) ,esMasParejo(I2,D2,I,D).

esMasParejo(I2,D2,I,D) :- sum_list(I2, SI2),
                          sum_list(D2,SD2), 
                          sum_list(I, SI),
                          sum_list(D, S0),
                          abs(SI-SD) > abs(SI2-SD2).

% arreglar esto

proximoPrimo(N, N2) :- N2 is N+1, esPrimo(N2).
proximoPrimo(N,P) :- N2 is N +1 , proximoPrimo(N2,P).

% -----

esPrimo(N) :- N > 1, not(tieneDivisorNoTrivial(N)).

tieneDivisorNoTrivial(N) :- N1 is N-1, between(2,N,D), 0=:=N mod D.




esTriangulo(tri(A,B,C)) :- valido(A,B,C), valido(B,C,A), valido(C,A,B).


valido(A,B,C) :- A < B + A.


% ground es true cuando no hay var libres

perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))) ,esTriangulo(tri(A,B,C)).

triplasQueSuman(P,A,B,C) :- desdeReversible(0,P),
                            between(1,P,B),C is P-A-B, 
                            C > 0.


triangulo(T) :- perimetro(T, _)




