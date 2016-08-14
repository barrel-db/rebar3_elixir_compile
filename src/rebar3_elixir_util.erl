-module(rebar3_elixir_util).

-export([add_elixir/1, get_details/1, add_states/4, compile_libs/1, clean_app/2, transfer_libs/3, to_binary/1]).

-spec to_binary(binary()|list()|integer()|atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).

add_elixir(State) ->
    {BinDir, Env, Config, LibDir} = rebar3_elixir_util:get_details(State),
    MixState = rebar3_elixir_util:add_states(State, BinDir, Env, Config),
    code:add_patha(filename:join(LibDir, "elixir/ebin")),
    code:add_patha(filename:join(LibDir, "mix/ebin")),
    MixState.

get_details(State) ->
    Config = rebar_state:get(State, elixir_opts),
    BinDir = case lists:keyfind(bin_dir, 1, Config) of
                false -> 
                    {ok, ElixirBin_} = find_executable("elixir"),
                    filename:dirname(ElixirBin_);
                {bin_dir, Dir1} -> Dir1
             end, 

    LibDir = case lists:keyfind(lib_dir, 1, Config) of
                false -> 
                    {ok, ElixirLibs_} = rebar_utils:sh("elixir -e 'IO.puts :code.lib_dir(:elixir)'", []),
                    filename:join(re:replace(ElixirLibs_, "\\s+", "", [global,{return,list}]), "../");
                {lib_dir, Dir2} -> Dir2
             end,            
    {env, Env} = lists:keyfind(env, 1, Config),
    {BinDir, Env, Config, LibDir}.

find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path -> {ok, filename:nativename(Path)}
    end.

add_states(State, BinDir, Env, Config) ->
    EnvState = rebar_state:set(State, mix_env, Env),
    RebarState = rebar_state:set(EnvState, elixir_opts, Config),
    BaseDirState = rebar_state:set(RebarState, elixir_base_dir, filename:join(rebar_dir:root_dir(RebarState), "elixir_libs/")),
    ElixirState = rebar_state:set(BaseDirState, elixir, filename:join(BinDir, "elixir ")),
    rebar_state:set(ElixirState, mix, filename:join(BinDir, "mix ")).    

compile_libs(State) ->
    Dir = rebar_state:get(State, elixir_base_dir),
    file:make_dir(Dir),
    {ok, Apps} = rebar_utils:list_dir(Dir),
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
    case ec_file:exists(filename:join(AppDir, "mix.exs")) of
        true -> rebar_utils:sh(Profile ++ Mix ++ "deps.get", [{cd, AppDir}, {use_stdout, true}]),
                rebar_utils:sh(Profile ++ Mix ++ "compile", [{cd, AppDir}, {use_stdout, true}]),
                LibsDir = filename:join([AppDir, "_build/", Env , "lib/"]),
                {ok, Libs} = file:list_dir_all(LibsDir),
                transfer_libs(State, Libs, LibsDir);
        false -> ok                 
    end,
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
