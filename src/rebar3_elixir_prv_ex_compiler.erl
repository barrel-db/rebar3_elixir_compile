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
    add_elixir_deps(State),
    add_elixir_libs(State),
    {ok, State}.

add_elixir_deps(State) ->
    MixState = add_elixir(State),    
    Mix = rebar_state:get(MixState, mix),
    rebar_api:console("===> Adding Elixir Deps", []),
    DepsDir = filename:join(rebar_dir:root_dir(MixState), "elixir_libs"),
    maybe_create_deps_app(Mix, DepsDir),
    MixFile = create_mix_file(MixState),
    rebar_file_utils:write_file_if_contents_differ(filename:join([DepsDir, "elixir_deps", "mix.exs"]), MixFile),                 
    ok.

create_mix_file(State) ->
    MixDeps = case lists:keyfind(deps, 1, rebar_state:get(State, elixir_opts)) of
        false -> "";
        {deps, Deps} -> rebar_to_mix_deps(Deps, [])
    end,
    lists:concat([
        "defmodule ElixirDeps.Mixfile do
        use Mix.Project

        def project do
            [app: :elixir_deps,
            version: \"0.0.1\",
            elixir: \"~> 1.2\",
            deps: deps]
        end

        defp deps do
            [",
                MixDeps,
            "]
        end
        end
        "]).

maybe_create_deps_app(Mix, DepsDir) ->
    case filelib:is_dir(filename:join(DepsDir, "elixir_deps")) of
        true -> ok;
        false -> rebar_utils:sh(Mix ++ "new elixir_deps", [{cd, DepsDir}, {use_stdout, true}])
    end,
    maybe_delete_dir(filename:join([DepsDir, "elixir_deps", "lib"])),
    ok.    


rebar_to_mix_deps([], MixDeps) ->
    rebar_api:console("===>  MixDeps ~p", [MixDeps]),
    string:join(MixDeps, ", \n");

rebar_to_mix_deps([Dep | Deps], MixDeps) ->
    rebar_to_mix_deps(Deps, MixDeps ++ [convert_dep(Dep)]).

convert_dep({App, Details}) ->    
    case Details of
        {git, URL, {RefType, Ref}} -> lists:concat(["{:", App, ", [{:git, ", "\"", URL, "\"", "}, {:", RefType, ", ", "\"", Ref, "\"", "}]}"]);
        {git, URL} -> lists:concat(["{:", App, ", [{:git, ", "\"", URL, "\"", "}]}"]);
        _ -> lists:concat(["{:", App, ", ", "\"", Details, "\"", "}" ])
    end.

add_elixir_libs(State) ->
    rebar_api:console("===> Adding Elixir Libs", []),
    MixState = add_elixir(State),
    compile_libs(MixState),
    ok.

add_elixir(State) ->
    RebarConfig = rebar_file_utils:try_consult(rebar_dir:root_dir(State) ++ "/rebar.config"),
    {elixir_opts, Config} = lists:keyfind(elixir_opts, 1, RebarConfig),
    {lib_dir, LibDir} = lists:keyfind(lib_dir, 1, Config),
    {bin_dir, BinDir} = lists:keyfind(bin_dir, 1, Config),
    {env, Env} = lists:keyfind(env, 1, Config),
    MixState = add_states(State, BinDir, Env, Config),
    code:add_patha(LibDir ++ "/elixir/ebin"),
    code:add_patha(LibDir ++ "/mix/ebin"),
    MixState.

add_states(State, BinDir, Env, Config) ->
    EnvState = rebar_state:set(State, mix_env, Env),
    rebar_state:set(State, elixir_opts, Config),
    ElixirState = rebar_state:set(EnvState, elixir, BinDir ++ "/elixir "),
    rebar_state:set(ElixirState, mix, BinDir ++ "/mix ").    

compile_libs(State) ->
    {ok, Apps} = rebar_utils:list_dir(rebar_dir:root_dir(State) ++ "/elixir_libs"),
    compile_libs(State, Apps).

compile_libs(_State, []) ->
    ok;          

compile_libs(State, [App | Apps]) ->
    AppDir = rebar_dir:root_dir(State) ++ "/elixir_libs/" ++ App,
    Mix = rebar_state:get(State, mix),
    Env = rebar_state:get(State, mix_env),
    Profile = case Env of
        dev -> ""; 
        prod -> "MIX_ENV=prod "
    end,    
    rebar_utils:sh(Profile ++ Mix ++ "deps.get", [{cd, AppDir}, {use_stdout, true}]),
    rebar_utils:sh(Profile ++ Mix ++ "compile", [{cd, AppDir}, {use_stdout, true}]),
    LibsDir = filename:join([AppDir, "_build/", Env , "lib/"]),
    {ok, Libs} = file:list_dir_all(LibsDir),
    transfer_libs(State, Libs, LibsDir),
    compile_libs(State, Apps).

transfer_libs(_State, [], _LibsDir) ->
    ok;

transfer_libs(State, [Lib | Libs], LibsDir) ->
    DepsDir = rebar_dir:deps_dir(State),
    maybe_copy_dir(LibsDir ++ "/" ++ Lib, DepsDir),
    transfer_libs(State, Libs, LibsDir).

maybe_copy_dir(Source, Target) ->
    TargetApp = lists:last(filename:split(Source)),
    case filelib:is_dir(filename:join([Target, TargetApp])) of
        true -> ok;
        false -> rebar_file_utils:cp_r([Source], filename:join([Target, TargetApp]))
    end.    

maybe_delete_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> rebar_file_utils:rm_rf(Dir)
    end.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
