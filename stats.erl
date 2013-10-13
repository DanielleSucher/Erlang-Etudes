-module(stats).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([maximum/1, minimum/1, range/1]).

-spec(range(list()) -> list()).

range(List) ->
  [minimum(List), maximum(List)].

-spec(minimum(list()) -> number()).

minimum([Head|Tail]) ->
  max_or_min(fun(X,Y) -> X < Y end, Tail, Head).


-spec(maximum(list()) -> number()).

maximum([Head|Tail]) ->
  max_or_min(fun(X,Y) -> X > Y end, Tail, Head).


-spec(max_or_min(function(), list(), number()) -> number()).

max_or_min(_, [], Current) ->
  Current;
max_or_min(F, [Head|Tail], Current) ->
  case F(Head, Current) of
    true -> max_or_min(F, Tail, Head);
    false -> max_or_min(F, Tail, Current)
  end.
