-module(rebar3_elixir_util).

-export([add_elixir/1, get_details/1, add_states/4, compile_libs/1, clean_app/2, transfer_libs/3, to_binary/1, to_string/1, convert_lock/3, add_mix_locks/1, add_deps_to_path/1, is_app_in_dir/2, maybe_copy_dir/3,fetch_mix_app_from_dep/2]).

-spec to_binary(binary()|list()|integer()|atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).


to_string(Term) when is_binary(Term) ->
    binary_to_list(Term);

to_string(Term) ->
    [String] = io_lib:format("~p",[Term]),
    String.  

add_deps_to_path(State) ->
    add_deps_to_path(State, deps_from_mix_lock(State)).
    
add_deps_to_path(State, []) ->
    State;

add_deps_to_path(State, [App | Apps]) ->
    TargetDir = filename:join([rebar_dir:deps_dir(State), "../lib", to_string(App), "ebin"]),
    State2 = rebar_state:update_code_paths(State, all_deps, TargetDir),
    State3 = case add_mix_locks(State2) of
                {State2_, NewLock} -> 
                    code:add_patha(filename:join([rebar_dir:deps_dir(State), "../lib", to_string(App), "ebin"])),
                    rebar_state:set(State2_, mixlock, NewLock);
                _ -> State2
            end,
    add_deps_to_path(State3, Apps).

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

add_mix_locks(State) ->
    Dir = filename:absname("_elixir_build"),
    file:make_dir(Dir),
    {ok, Apps} = rebar_utils:list_dir(Dir),
    [Profile | _] = rebar_state:current_profiles(State),
    CurrentLock = rebar_state:get(State, {locks, Profile}, []),
    {State2, ExtraLock} = case mix_to_rebar_lock(State, Dir, Apps) of
        {State2_, ExtraLock_} -> 
            {State2_, ExtraLock_};
        _ -> {State, []}
    end, 
    {State2, lists:ukeymerge(1, CurrentLock, ExtraLock)}.

deps_from_mix_lock(State) ->
    {_State2, Lock} = add_mix_locks(State),
    lists:map(fun({D, _, _}) -> D end, Lock).

fetch_mix_app_from_dep(State, Dep) ->
    Dir = filename:absname("_elixir_build"),
    file:make_dir(Dir),
    {ok, Apps} = rebar_utils:list_dir(Dir),
    fetch_mix_app_from_dep(State, Dep, Apps, Dir).

fetch_mix_app_from_dep(_State, _Dep, [], _Dir) ->
    false;

fetch_mix_app_from_dep(State, Dep, [App | Apps], Dir) ->
    Env = rebar_state:get(State, mix_env, ["dev"]),
    LibsDir = filename:join([Dir, App, "_build/", Env , "lib"]),
    DepDir = filename:join(LibsDir, Dep),
    case filelib:is_dir(DepDir) of
        false -> fetch_mix_app_from_dep(State, Dep, Apps, Dir);
        _ -> 
            DepDir
    end.


mix_to_rebar_lock(_State, _Dir, []) ->
    [];

mix_to_rebar_lock(State, Dir, [App | Apps]) ->
    AppDir = filename:join(Dir, App),
    application:ensure_all_started(elixir),
    AppLock = case 'Elixir.File':read(lockfile(AppDir)) of
            {ok,Info} ->
                case 'Elixir.Code':eval_string(Info, [], [file, lockfile(AppDir)]) of
                    {Lock_, _binding}  -> 'Elixir.Enum':to_list(Lock_)
                end;
            {error, _} ->
                []
    end,
    RebarLock = convert_lock(AppLock, AppLock, 1),
    DepLocks = mix_to_rebar_lock(State, Dir, Apps),
    Lock = lists:ukeymerge(1, DepLocks, RebarLock),
    Env = rebar_state:get(State, mix_env, ["dev"]),
    LibsDir = filename:join([AppDir, "_build/", Env , "lib/"]),
    Deps = lists:filter(fun({D, _, _}) -> is_app_in_dir(rebar_dir:deps_dir(State), to_string(D)) or is_app_in_dir(LibsDir, to_string(D)) end, Lock),
    State2 = add_deps_to_state(State, App, Deps, LibsDir),
    {State2, Deps}.

add_deps_to_state(State, _Parent, [], _Dir) ->
    State;

add_deps_to_state(State, Parent, [Dep | Deps], Dir) ->
    {Name, {elixir, Pkg, Vsn}, Level} = Dep,
    {ok, AppInfo} = rebar_app_info:new(to_binary(Name), Vsn, filename:join([Dir, Name])),
    AppInfo2 = rebar_app_info:dep_level(AppInfo, Level),
    AppInfo3 = rebar_app_info:source(AppInfo2, {elixir, Pkg, Vsn}),
    AppInfo4 = rebar_app_info:parent(AppInfo3, to_binary(Parent)),
    {true, AppInfo5} = rebar_app_discover:find_app(AppInfo4, filename:join([Dir, to_string(Name)]), all),
    State2 = case lists:member(AppInfo5, rebar_state:lock(State)) of
                false -> rebar_state:lock(State, AppInfo5);
                true -> State
             end,
    add_deps_to_state(State2, Parent, Deps, Dir).      

convert_lock(_Lock, [], _Level) ->
    [];

convert_lock(Lock, [Dep | Deps], Level) ->
    case Dep of
        {Name, {hex, Pkg, Vsn, _Hash, _Manager, SubDeps}} ->
            RebarDep = {rebar3_elixir_util:to_binary(Name), {elixir, rebar3_elixir_util:to_string(Pkg), rebar3_elixir_util:to_string(Vsn)}, Level},
            case {SubDeps, is_app_in_code_path(Name)} of
              {[], true} ->
                convert_lock(Lock, Deps, Level);
              {[], false} ->  
                lists:ukeymerge(1, convert_lock(Lock, Deps, Level), [RebarDep]);
              {SubDeps_, true} ->
                  lists:ukeymerge(1, convert_lock(Lock, Deps, Level), convert_lock(Lock, SubDeps_, Level+1));
              {SubDeps_, false} ->    
                  lists:ukeymerge(1, lists:ukeymerge(1, convert_lock(Lock, Deps, Level), convert_lock(Lock, SubDeps_, Level+1)), [RebarDep])
            end;
        {Name, _VSN, _Opts} ->  
            SubDep = lists:keyfind(Name, 1, Lock),
            convert_lock(Lock, [SubDep], Level);
        _ -> 
            convert_lock(Lock, Deps, Level)
    end.

is_app_in_code_path(Name) ->
    CodePath = lists:filter(fun (Path) -> lists:member(to_string(Name), filename:split(Path)) and lists:member("rebar3", filename:split(Path)) end, code:get_path()),
    case CodePath of
        [] -> false;
        _ -> 
            true
    end.

is_app_in_dir(Dir, App) ->
    filelib:is_dir(filename:join([Dir, App])).

lockfile(AppDir) ->
    filename:join(AppDir, "mix.lock").


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
    {ok, State1} = compile_libs(State, Apps),
    Deps = deps_from_mix_lock(State),
    rebar_state:set(State1, deps, Deps).

compile_libs(State, []) ->
    {ok, State};          

compile_libs(State, [App | Apps]) ->
    AppDir = filename:join(rebar_state:get(State, elixir_base_dir), App), 
    Mix = rebar_state:get(State, mix),
    Env = rebar_state:get(State, mix_env),
    Profile = case Env of
        dev -> ""; 
        prod -> "MIX_ENV=prod "
    end,    
    case {ec_file:exists(filename:join(AppDir, "mix.exs")), ec_file:exists(filename:join(AppDir, "rebar.config"))} of
        {true, false} -> 
            rebar_utils:sh(Profile ++ Mix ++ "deps.get", [{cd, AppDir}, {use_stdout, true}]),
            rebar_utils:sh(Profile ++ Mix ++ "compile", [{cd, AppDir}, {use_stdout, true}]),
            LibsDir = filename:join([AppDir, "_build/", Env , "lib/"]),
            {ok, Libs} = file:list_dir_all(LibsDir),
            transfer_libs(State, Libs, LibsDir);
        {_, true} ->
            transfer_libs(State, [App], filename:join(AppDir, "../"));
        {false, _} -> State                 
    end,        
    compile_libs(State, Apps).

transfer_libs(State, [], _LibsDir) ->
    State;

transfer_libs(State, [Lib | Libs], LibsDir) ->
    case {rebar_state:get(State, libs_target_dir), is_app_in_code_path(Lib)} of
        {default, true} ->
            State;
        {default, _} ->     
            maybe_copy_dir(filename:join(LibsDir, Lib), rebar_dir:deps_dir(State), true),
            State;
        {Dir, _} -> 
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
