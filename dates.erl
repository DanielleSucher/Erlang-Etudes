-module(dates).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([date_parts/1, julian/1]).

-define(DAYS_PER_MONTH, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]).

-spec(date_parts(string()) -> [number(),...]).

date_parts(DateString) ->
  [YearString, MonthString, DayString] = re:split(DateString, "-", [{return, list}]),
  {Year, _} = string:to_integer(YearString),
  {Month, _} = string:to_integer(MonthString),
  {Day, _} = string:to_integer(DayString),
  [Year, Month, Day].

% Better way via the suggested solution:

% date_parts(DateStr) ->
%   [YStr, MStr, DStr] = re:split(DateStr, "-", [{return, list}]),
%   [element(1, string:to_integer(YStr)),
%     element(1, string:to_integer(MStr)),
%     element(1, string:to_integer(DStr))].

-spec(julian(string()) -> number()).

julian(DateString) ->
  [Year, Month, Day] = date_parts(DateString),
  julian(Year, Month, Day, ?DAYS_PER_MONTH, 0).

-spec(julian(integer(), integer(), integer(), list(), integer()) -> integer()).

julian(Year, Month, Day, DaysOfMonthList, Acc) when Month < (14 - length(DaysOfMonthList)) ->
  Leap = case Month > 2 andalso is_leap_year(Year) of
    true -> 1;
    false -> 0
  end,
  Acc + Day + Leap;
julian(Year, Month, Day, [Head|Tail], Acc) ->
  julian(Year, Month, Day, Tail, Acc + Head).

is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
    orelse (Year rem 400 == 0).
