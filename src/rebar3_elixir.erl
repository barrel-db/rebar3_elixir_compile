-module(rebar3_elixir).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_elixir_prv_ex_compiler:init(State),
    {ok, State1}.
