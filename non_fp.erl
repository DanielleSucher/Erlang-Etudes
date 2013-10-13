-module(non_fp).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([generate_teeth/2]).

-spec(generate_teeth(string(), number()) -> list()).

generate_teeth(Present, ProbabilityGood) ->
  generate_teeth(Present, ProbabilityGood, []).


-spec(generate_teeth(string(), number(), list()) -> list()).

generate_teeth([], _, Acc) ->
  Acc;
generate_teeth([Head|Tail], ProbabilityGood, Acc) ->
  Tooth = case Head of
    $T ->
      generate_tooth(ProbabilityGood);
    $F ->
      [0]
  end,
  generate_teeth(Tail, ProbabilityGood, [Tooth|Acc]).


-spec(generate_tooth(number()) -> list()).

generate_tooth(ProbabilityGood) ->
  BaseDepth = case really_random() < ProbabilityGood of
    true -> 2;
    false -> 3
  end,
  generate_tooth(BaseDepth, 6, []).


-spec(generate_tooth(number(), number(), list()) -> list()).

generate_tooth(_, 0, Acc) ->
  Acc;
generate_tooth(BaseDepth, NumberRemaining, Acc) ->
  NewBaseDepth = BaseDepth + 2 - random:uniform(3),
  generate_tooth(BaseDepth, NumberRemaining - 1, [NewBaseDepth|Acc]).


-spec(really_random() -> number()).

really_random() ->
  random:seed(now()),
  random:uniform().
