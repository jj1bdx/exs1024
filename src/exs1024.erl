%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @copyright 2014 Kenji Rikitake
%% @doc Xorshift1024star for Erlang
%% @end
%% (MIT License)
%%
%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy of
%% this software and associated documentation files (the "Software"), to deal in
%% the Software without restriction, including without limitation the rights to
%% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%

-module(exs1024).

-export([
     next/1,
     seed0/0,
     seed/0,
     seed/1,
     seed/3,
     uniform/0,
     uniform/1,
     uniform_s/1,
     uniform_s/2
 ]).

-export_type([
        seedval/0,
        state/0,
        uint64/0
    ]).

%% @type uint64(). 64bit unsigned integer type.

-type uint64() :: 0..16#ffffffffffffffff.

%% @type seedval(). Internal seed data type for exs1024.
%% Representing 16 64-bit numbers with a single list.

-opaque seedval() :: list(uint64()).

%% @type state(). Internal state data type for exs1024.
%% Representing 16 64-bit numbers with a pair of 
%% the list and a reverse list.

-opaque state() :: {list(uint64()), list(uint64())}.

-define(UINT22MASK, 16#3fffff).
-define(UINT64MASK, 16#ffffffffffffffff).

%% @doc Calculation of xorshift1024star.
%% calc(S0, S1) -> {X, NS1}.
%% X: random number output

-spec calc(uint64(), uint64()) -> {uint64(), uint64()}.

calc(S0, S1) ->
    S11 = S1 bxor ((S1 bsl 31) band ?UINT64MASK),
    S12 = S11 bxor (S11 bsr 11),
    S01 = S0 bxor (S0 bsr 30),
    NS1 = S01 bxor S12,
    {(NS1 * 1181783497276652981) band ?UINT64MASK, NS1}. 

%% @doc Advance xorshift1024star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift1024star internal state.

-spec next(state()) -> {uint64(), state()}.

next({[H], RL}) ->
    next({[H|lists:reverse(RL)], []});
next({L, RL}) ->
    [S0|L2] = L,
    [S1|L3] = L2,
    {X, NS1} = calc(S0, S1),
    {X, {[NS1|L3], [S0|RL]}}.

-spec seed0() -> state().

%% @doc Set the default seed value to xorshift1024star state
%% in the process directory (Compatible with random:seed0/0).

seed0() ->
    {
     [
      16#0123456789abcdef,
      16#123456789abcdef0,
      16#23456789abcdef01,
      16#3456789abcdef012,
      16#456789abcdef0123,
      16#56789abcdef01234,
      16#6789abcdef012345,
      16#789abcdef0123456,
      16#89abcdef01234567,
      16#9abcdef012345678,
      16#abcdef0123456789,
      16#bcdef0123456789a,
      16#cdef0123456789ab,
      16#def0123456789abc,
      16#ef0123456789abcd,
      16#f0123456789abcde
     ], []}.

%% @doc Set the default seed value to xorshift1024star state
%% in the process directory %% (Compatible with random:seed/1).

-spec seed() -> state().

seed() ->
    case seed_put(seed0()) of
        undefined -> seed0();
        {L, RL} -> {L, RL}
    end.

%% @doc Put the seed, or internal state, into the process dictionary.

-spec seed_put(state()) -> 'undefined' | state().

seed_put(R) ->
    put(exs1024_seed, R).

%% @doc Set the seed value to xorshift1024star state in the process directory.
%% with the given three-element tuple of unsigned 32-bit integers
%% (Compatible with random:seed/1).

-spec seed({integer(), integer(), integer()}) -> 'undefined' | state().

seed({A1, A2, A3}) ->
    seed(A1, A2, A3).

%% @doc Set the seed value to xorshift1024star state in the process directory
%% with the given three unsigned 22-bit integer arguments
%% (Compatible with random:seed/3).
%% Multiplicands here: three 22-bit primes.
%% TODO: this seeding isn't complete yet.

-spec seed(integer(), integer(), integer()) -> 'undefined' | state().

seed(A1, A2, A3) ->
    B1 = (((A1 band ?UINT22MASK) + 1) * 4194277) band ?UINT22MASK,
    B2 = (((A2 band ?UINT22MASK) + 1) * 4194287) band ?UINT22MASK,
    B3 = (((A3 band ?UINT22MASK) + 1) * 4194301) band ?UINT22MASK,
    seed_put({exs64l:gen1024(
                  (B1 bsl 45) bor (B2 bsl 23) bor (B3 bsl 1) bor 1),
              []}).

%% @doc Generate float from
%% given xorshift1024star internal state.
%% (Note: 0.0 =&lt; result &lt; 1.0)
%% (Compatible with random:uniform_s/1)

-spec uniform_s(state()) -> {float(), state()}.

uniform_s(R0) ->
    {V, R1} = next(R0),
    {V / 18446744073709551616.0, R1}.

-spec uniform() -> float().

%% @doc Generate float
%% given xorshift1024star internal state
%% in the process dictionary.
%% (Note: 0.0 =&lt; result &lt; 1.0)
%% (Compatible with random:uniform/1)

uniform() ->
    R = case get(exs1024_seed) of
        undefined -> seed0();
        _R -> _R
    end,
    {V, R2} = uniform_s(R),
    put(exs1024_seed, R2),
    V.

%% @doc Generate integer from given xorshift1024star internal state.
%% (Note: 0 =&lt; result &lt; MAX (given positive integer))
-spec uniform_s(pos_integer(), state()) -> {pos_integer(), state()}.

uniform_s(Max, R) when is_integer(Max), Max >= 1 ->
    {V, R1} = next(R),
    {(V rem Max) + 1, R1}.

%% @doc Generate integer from the given xorshift1024star internal state
%% in the process dictionary.
%% (Note: 1 =&lt; result =&lt; N (given positive integer))
%% (compatible with random:uniform/1)

-spec uniform(pos_integer()) -> pos_integer().

uniform(N) when is_integer(N), N >= 1 ->
    R = case get(exs1024_seed) of
        undefined -> seed0();
        _R -> _R
    end,
    {V, R1} = uniform_s(N, R),
    put(exs1024_seed, R1),
    V.

