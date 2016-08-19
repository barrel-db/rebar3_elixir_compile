-module(rebar3_elixir_compile_prv_ex_compiler).

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
            {example, "rebar3 rebar3_elixir_compile"},
            {opts, []},
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    State2 = rebar3_elixir_compile_util:add_deps_to_path(State),
    {ok, State3} = rebar_prv_lock:do(State2),
    {ok, State4} = add_elixir_libs(State3),
    Dir = filename:join(rebar_dir:root_dir(State4), "elixir_libs/"),
    {ok, Apps} = rebar_utils:list_dir(Dir),
    State5 = rebar3_elixir_compile_util:add_deps_to_path(State4, Apps, false),
    {ok, State5}.

add_elixir_libs(State) ->
    rebar_api:console("===> Adding Elixir Libs", []),
    State1 = rebar3_elixir_compile_util:add_elixir(State),
    State2 = rebar_state:set(State1, libs_target_dir, default),
    Dir = filename:join(rebar_dir:root_dir(State2), "elixir_libs/"),
    BaseDirState = rebar_state:set(State2, elixir_base_dir, Dir),
    State3 = rebar3_elixir_compile_util:compile_libs(BaseDirState, true),
    {ok, State3}.

format_error(Reason) ->
    rebar_api:console("~p", [Reason]).
