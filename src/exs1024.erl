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
     seed_list/1,
     uniform/0,
     uniform/1,
     uniform_s/1,
     uniform_s/2,
     jump/1
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

-define(UINT21MASK, 16#1fffff).
-define(UINT33MASK, 16#1ffffffff).
-define(UINT64MASK, 16#ffffffffffffffff).

%% @doc Calculation of xorshift1024star.
%% calc(S0, S1) -> {X, NS1}.
%% X: random number output

-spec calc(uint64(), uint64()) -> {uint64(), uint64()}.

calc(S0, S1) ->
    S11 = S1 bxor ((S1 band ?UINT33MASK) bsl 31),
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

%% @doc Put the list of 16 64-bit integers as the seed into the
%% process dictionary.

-spec seed_list(seedval()) -> 'undefined' | state().

seed_list(L) when is_list(L), length(L) =:= 16 ->
    seed_put({L, []}).

%% @doc Set the seed value to xorshift1024star state in the process directory.
%% with the given three-element tuple of unsigned 32-bit integers
%% (Compatible with random:seed/1).

-spec seed({integer(), integer(), integer()}) -> 'undefined' | state().

seed({A1, A2, A3}) ->
    seed(A1, A2, A3).

%% @doc Set the seed value to xorshift1024star state in the process directory
%% with the given three unsigned 21-bit integer arguments
%% (Compatible with random:seed/3).
%% Multiplicands here: three 21-bit primes.
%% TODO: this seeding isn't complete yet.

-spec seed(integer(), integer(), integer()) -> 'undefined' | state().

seed(A1, A2, A3) ->
    B1 = (((A1 band ?UINT21MASK) + 1) * 2097131) band ?UINT21MASK,
    B2 = (((A2 band ?UINT21MASK) + 1) * 2097133) band ?UINT21MASK,
    B3 = (((A3 band ?UINT21MASK) + 1) * 2097143) band ?UINT21MASK,
    seed_put({exs64l:gen1024(
                  (B1 bsl 43) bor (B2 bsl 22) bor (B3 bsl 1) bor 1),
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

%% @doc This is the jump function for the generator. It is equivalent
%% to 2^512 calls to next(); it can be used to generate 2^512
%% non-overlapping subsequences for parallel computations.

%% Jump constant in 64-bit-split 1024-bit number:
%% [16#84242f96eca9c41d,
%%  16#a3c65b8776f96855,
%%  16#5b34a39f070b5837,
%%  16#4489affce4f31a1e,
%%  16#2ffeeb0a48316f40,
%%  16#dc2d9891fe68c022,
%%  16#3659132bb12fea70,
%%  16#aac17d8efa43cab8,
%%  16#c4cb815590989b13,
%%  16#5ee975283d71c93b,
%%  16#691548c86c1bd540,
%%  16#7910c41d10a1e6a5,
%%  16#0b5fc64563b3e2a8,
%%  16#047f7684e9fc949d,
%%  16#b99181f2d8f685ca,
%%  16#284600e3f30e38c3]).

%% Jump constant here split into 58 bits for speed
-define(JUMPCONSTHEAD, 16#00242f96eca9c41d).
-define(JUMPCONSTTAIL,
        [16#0196e1ddbe5a1561,
         16#0239f070b5837a3c,
         16#03f393cc68796cd2,
         16#0248316f404489af,
         16#039a30088bffbac2,
         16#02fea70dc2d9891f,
         16#032ae0d9644caec4,
         16#0313aac17d8efa43,
         16#02f132e055642626,
         16#01ee975283d71c93,
         16#00552321b06f5501,
         16#00c41d10a1e6a569,
         16#019158ecf8aa1e44,
         16#004e9fc949d0b5fc,
         16#0363da172811fdda,
         16#030e38c3b99181f2,
         16#0000000a118038fc]).
-define(JUMPTOTALLEN, 1024).
-define(JUMPELEMLEN, 58).
-define(RINGLEN, 16).

-spec jump(state()) -> state().

jump({L, RL}) ->
    P = length(RL),
    AS = jump({L, RL}, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
         ?JUMPCONSTTAIL, ?JUMPCONSTHEAD, ?JUMPELEMLEN, ?JUMPTOTALLEN),
    {ASL, ASR} = lists:split(?RINGLEN - P, AS),
    {ASL, lists:reverse(ASR)}.

-spec jump(state(), list(non_neg_integer()),
           list(non_neg_integer()), non_neg_integer(),
           non_neg_integer(), non_neg_integer()) -> list(non_neg_integer()).

jump(_, AS, _, _, _, 0) ->
    AS;
jump(S, AS, [H|T], _, 0, TN) ->
    jump(S, AS, T, H, ?JUMPELEMLEN, TN);
jump({L, RL}, AS, JL, J, N, TN) ->
    {_, NS} = next({L, RL}),
    case (J band 1) of
        1 ->
            AS2 = lists:zipwith(fun(X, Y) -> X bxor Y end,
                        AS, L ++ lists:reverse(RL)),
            jump(NS, AS2, JL, J bsr 1, N-1, TN-1);
        0 ->
            jump(NS, AS, JL, J bsr 1, N-1, TN-1)
    end.
