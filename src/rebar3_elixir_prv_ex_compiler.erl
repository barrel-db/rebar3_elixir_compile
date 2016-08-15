-module(rebar3_elixir_prv_ex_compiler).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).
-define(NAMESPACE, ex).

init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},     
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 rebar3_elixir"},
            {opts, []},
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    {ok, State2} = add_elixir_libs(State),
    {ok, State2}.

add_elixir_libs(State) ->
    rebar_api:console("===> Adding Elixir Libs", []),
    State1 = rebar3_elixir_util:add_elixir(State),
    State2 = rebar_state:set(State1, libs_target_dir, default),
    BaseDirState = rebar_state:set(State2, elixir_base_dir, filename:join(rebar_dir:root_dir(State2), "elixir_libs/")),
    State3 = rebar3_elixir_util:compile_libs(BaseDirState),
    rebar_api:console("~p", [rebar_state:deps_names(State)]),
    {ok, State3}.

format_error(Reason) ->
    rebar_api:console("~p", [Reason]).
