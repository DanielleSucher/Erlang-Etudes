-module(teeth).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([alert/1]).

% PocketDepths = [[0], [2,2,1,2,2,1], [3,1,2,3,2,3],
  % [3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
  % [3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
  % [3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
  % [1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3], [0],
  % [3,2,3,1,1,2], [2,2,1,1,3,2], [2,1,1,1,1,2],
  % [3,3,2,1,1,3], [3,1,3,2,3,2], [3,3,1,2,3,3],
  % [1,2,2,3,3,3], [2,2,3,2,3,3], [2,2,2,4,3,4],
  % [3,4,3,3,3,4], [1,1,2,3,1,2], [2,2,3,2,1,3],
  % [3,4,2,4,4,3], [3,3,2,1,2,3], [2,2,2,2,3,3],
  % [3,2,3,2,3,2]].

alert(List) ->
  alert(List, 1, []).

alert([], _, Result) ->
  lists:reverse(Result);
alert([Head|Tail], Index, Result) ->
  case lists:any(fun(X) -> X >= 4 end, Head) of
    true -> alert(Tail, Index + 1, [Index|Result]);
    false -> alert(Tail, Index + 1, Result)
  end.

