-module(rebar3_elixir_util).

-export([add_elixir/1, get_details/1, add_states/4, compile_libs/1, clean_app/2, transfer_libs/3]).

add_elixir(State) ->
    {BinDir, Env, Config, LibDir} = rebar3_elixir_util:get_details(State),
    MixState = rebar3_elixir_util:add_states(State, BinDir, Env, Config),
    code:add_patha(LibDir ++ "/elixir/ebin"),
    code:add_patha(LibDir ++ "/mix/ebin"),
    MixState.

get_details(State) ->
    RebarConfig = rebar_file_utils:try_consult(rebar_dir:root_dir(State) ++ "/rebar.config"),
    {elixir_opts, Config} = lists:keyfind(elixir_opts, 1, RebarConfig),
    {lib_dir, LibDir} = lists:keyfind(lib_dir, 1, Config),
    {bin_dir, BinDir} = lists:keyfind(bin_dir, 1, Config),
    {env, Env} = lists:keyfind(env, 1, Config),
    {BinDir, Env, Config, LibDir}.

add_states(State, BinDir, Env, Config) ->
    EnvState = rebar_state:set(State, mix_env, Env),
    RebarState = rebar_state:set(EnvState, elixir_opts, Config),
    BaseDirState = rebar_state:set(RebarState, elixir_base_dir, filename:join(rebar_dir:root_dir(RebarState), "elixir_libs/")),
    ElixirState = rebar_state:set(BaseDirState, elixir, BinDir ++ "/elixir "),
    rebar_state:set(ElixirState, mix, BinDir ++ "/mix ").    

compile_libs(State) ->
    filelib:ensure_dir("_build/default/lib/"),
    {ok, Apps} = rebar_utils:list_dir(rebar_state:get(State, elixir_base_dir)),
    compile_libs(State, Apps).

compile_libs(_State, []) ->
    ok;          

compile_libs(State, [App | Apps]) ->
    AppDir = filename:join(rebar_state:get(State, elixir_base_dir), App), 
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
    case rebar_state:get(State, libs_target_dir) of
                default -> 
                    maybe_copy_dir(filename:join(LibsDir, Lib), rebar_dir:deps_dir(State), true);
                Dir -> 
                    maybe_copy_dir(filename:join(LibsDir, Lib), Dir, false)
    end,
    transfer_libs(State, Libs, LibsDir).

clean_app(State, App) ->
    ec_file:remove(filename:join(rebar_dir:deps_dir(State), App), [recurisve]).

maybe_copy_dir(Source, Target, CreateNew) ->
    TargetApp = lists:last(filename:split(Source)),
    TargetDir = case CreateNew of
                    false ->  Target;
                    _ -> filename:join([Target, TargetApp])    
                end,
    case filelib:is_dir(filename:join([Target, TargetApp, "ebin"])) of
        true -> ok;
        false ->
            ec_file:remove(TargetDir, [recurisve]),
            ec_file:copy(Source, TargetDir, [recursive])
    end.    
