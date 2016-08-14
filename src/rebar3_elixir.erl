-module(rebar3_elixir).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_elixir_prv_ex_compiler:init(State),
    State2 = rebar_state:add_resource(State1, {elixir, rebar3_elixir_resource}),
    {ok, State2}.
