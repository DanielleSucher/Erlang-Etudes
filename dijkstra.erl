-module(dijkstra).
-author("Danielle Sucher <dsucher@gmail.com>").
-export([gcd/2]).

-spec(gcd(integer(), integer()) -> integer()).

gcd(M, M) ->
  M;
gcd(M, N) when M > N ->
  gcd(M - N, N);
gcd(M, N) when M < N ->
  gcd(M, N - M).
