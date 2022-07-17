is_prime(T, T, _, 1) :- !.
is_prime(C, T, N, -1) :- 0 is mod(N, C), !.
is_prime(C, T, N, R) :- Next is C + 1, is_prime(Next, T, N, R).

prime(N) :- \=(N, 1), T is div(sqrt(N), 1) + 1, is_prime(2, T, N, R), R == 1.

composite(N) :- \+ prime(N).

concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

factorize(C, N, Divs, Divs) :- C > N, !.
factorize(C, N, Divs, R) :- prime(C), 0 is mod(N, C), !, NextN is div(N, C), concat(Divs, [C], T), (0 is mod(N, C) -> NextC = C; NextC is C + 1), 
											 factorize(NextC, NextN, T, R).
factorize(C, N, Divs, R) :- NextC is C + 1, factorize(NextC, N, Divs, R).

prime_divisors(N, [N]) :- prime(N), !.
prime_divisors(N, R) :- factorize(2, N, [], R).

find_nth(N, S, S, R) :- !, R is N - 1.
find_nth(N, C, S, R) :- prime(N), !, NextC is C + 1, NextN is N + 1, find_nth(NextN, NextC, S, R).
find_nth(N, C, S, R) :- NextN is N + 1, find_nth(NextN, C, S, R).

nth_prime(S, R) :- find_nth(2, 0, S, R).
