-module(powers).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([nth_root/2, raise/2]).

-spec(nth_root(number(), number()) -> number()).

nth_root(X, N) ->
  nth_root(X, N, X/2.0).

-spec(nth_root(number(), number(), number()) -> number()).

nth_root(X, N, A) ->
  F = raise(A, N) - X,
  Fprime = N * raise(A, N - 1),
  Next = A - F/Fprime,
  Change = abs(Next - A),
  if
    Change < 1.0e-8 ->
      Next;
    true ->
      io:format("Current guess is ~.15g~n", [Next]),
      nth_root(X, N, Next)
  end.


-spec(raise(number(), number()) -> number()).

raise(_, 0) ->
  1;
raise(X, N) when N < 0 ->
  1.0 / raise(X, 0 - N);
raise(X, N) when N > 0 ->
  raise(X, N, 1).


-spec(raise(number(), number(), number()) -> number()).

raise(_, 0, Acc) ->
  Acc;
raise(X, N, Acc) ->
  raise(X, N - 1, X * Acc).
