-module(rebar3_elixir).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_elixir_prv_ex_compiler:init(State),
    State2 = rebar_state:add_resource(State1, {elixir, rebar3_elixir_resource}),
    {_BinDir, _Env, _Config, LibDir} = rebar3_elixir_util:get_details(State2),
    code:add_patha(filename:join(LibDir, "elixir/ebin")),
    code:add_patha(filename:join(LibDir, "mix/ebin")),
    code:add_patha(filename:join(LibDir, "logger/ebin")),
    State3 = rebar3_elixir_util:add_deps_to_path(State2),
    {ok, State3}.
