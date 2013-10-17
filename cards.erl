-module(cards).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([make_deck/0, show_deck/1, shuffle/1]).

-define(SUITS, ["Clubs", "Diamonds", "Hearts", "Spades"]).
-define(RANKS, [2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K", "A"]).


-spec(make_deck() -> list()).

make_deck() ->
  [{Rank, Suit} || Rank <- ?RANKS, Suit <- ?SUITS].

show_deck(Deck) ->
    lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

%% @doc Shuffles the deck by splitting the deck into two lists of 
%% random length and moving the first element of the second list 
%% into the accumulator. It then recursively calls itself with the first 
%% list plus the remaining cards of the second list as the first argument,
%% and the accumulator as the second argument.
%% When no cards remain, the accumulator is returned.

shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).
