%% @author Danielle Sucher <dsucher@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2013 Danielle Sucher
%% @version 0.1

-module(ask_area).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([area/0]).

-spec(area() -> number()).

area() ->
  {ok, [ShapeChar]} = io:fread("R)ectangle, T)riangle, or E)llipse > ", "~s"),
  {A, B} = get_dimensions("Enter width > ", "Enter height > "),
  Shape = char_to_shape(ShapeChar),
  calculate(Shape, A, B).


% private

-spec(calculate(atom(), number(), number()) -> number()).

calculate(unknown, _, _) -> "Unknown shape.";
calculate(_, A, _) when not is_number(A) -> "Error in first number.";
calculate(_, _, B) when not is_number(B) -> "Error in second number.";
calculate(_, A, B) when A < 0; B < 0 -> "Both numbers must be greater than or equal to zero.";
calculate(Shape, A, B) -> geom:area(Shape, A, B).


-spec(char_to_shape(string()) -> atom()).

char_to_shape(Char) ->
  case string:to_lower(Char) of
    "r" -> rectangle;
    "t" -> triangle;
    "e" -> ellipse;
    _   -> unknown
  end.


-spec(get_dimensions(string(), string()) -> {number(), number()}).

get_dimensions(A_Prompt, B_Prompt) -> {get_number(A_Prompt), get_number(B_Prompt)}.


-spec(get_number(string()) -> number()).

get_number(Prompt) ->
  {ok, [String]} = io:fread(Prompt, "~s"),
  {Number, _} = case string:to_float(String) of
    {error,no_float} -> string:to_integer(String);
    _                -> string:to_float(String)
  end,
  Number.
