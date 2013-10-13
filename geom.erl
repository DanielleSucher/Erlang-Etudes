%% @author Danielle Sucher <dsucher@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2013 Danielle Sucher
%% @version 0.1

-module(geom).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([area/1, area/3]).


-spec(area(atom(), number(), number()) -> number()).

area(_, A,B) when A =< 0; B =< 0 -> undefined;
area(rectangle, A, B) -> A * B;
area(triangle, A, B) -> A * B / 2.0;
area(ellipse, A, B) -> math:pi() * A * B;
area(_, _, _) -> 0.


-spec(area(tuple()) -> number()).

area({Atom, _, _}) when not is_atom(Atom) ->
  io:format("The first element of your tuple must be an atom.~n", []);
area({_, A, B}) when not is_number(A); not is_number(B) ->
  io:format("The second two elements of your tuple must be numbers.~n", []);
area({Atom, A, B}) ->
  area(Atom, A, B);
area(_) ->
  io:format("You must pass in a tuple in the form of {atom, number, number}.~n", []).

