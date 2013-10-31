-module(stats).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([maximum/1, minimum/1, range/1, mean/1, stdv/1]).

-spec(range(list()) -> list()).

range(List) ->
  [minimum(List), maximum(List)].

-spec(minimum(list()) -> number()).

minimum([Head|Tail]) ->
  max_or_min(fun erlang:'<'/2, Tail, Head).


-spec(maximum(list()) -> number()).

maximum([Head|Tail]) ->
  max_or_min(fun erlang:'>'/2, Tail, Head).


-spec(max_or_min(function(), list(), number()) -> number()).

max_or_min(_, [], Current) ->
  Current;
max_or_min(F, [Head|Tail], Current) ->
  case F(Head, Current) of
    true -> max_or_min(F, Tail, Head);
    false -> max_or_min(F, Tail, Current)
  end.


-spec(mean(list()) -> number()).

mean(List) ->
  lists:foldl(fun(X, Sum) -> X + Sum end, 0, List)/length(List).


-spec(stdv(list()) -> number()).

stdv(List) ->
  N = length(List),
  {Sum, SumOfSquares} = lists:foldl(fun(X, {S, SqS}) -> {X + S, (X * X) + SqS} end, {0, 0}, List),
  Difference = (SumOfSquares * N) - (Sum * Sum),
  Quotient = Difference/(N * (N - 1)),
  math:sqrt(Quotient).
