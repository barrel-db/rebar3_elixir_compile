-module(rebar3_elixir_compile_util).

-export([add_elixir/1, get_details/1, add_states/4, add_deps_to_path/3, compile_libs/1, compile_libs/2, clean_app/2, transfer_libs/3, to_binary/1, to_string/1, convert_lock/3, add_mix_locks/1, add_deps_to_path/1, is_app_in_dir/2, maybe_copy_dir/3,fetch_mix_app_from_dep/2, libs_dir/2]).

-spec to_binary(binary()|list()|integer()|atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).


to_string(Term) when is_binary(Term) ->
    binary_to_list(Term);

to_string(Term) when is_binary(Term) ->
    [String] = io_lib:format("~p",[Term]),
    String;
    
to_string(Term) ->
    to_string(to_binary(Term)).
  
add_deps_to_path(State) ->
    add_deps_to_path(State, deps_from_mix_lock(State), true).
    
add_deps_to_path(State, [], _Check) ->
    State;

add_deps_to_path(State, [App | Apps], Check) ->
    TargetDir = filename:join([rebar_dir:deps_dir(State), "../lib", to_string(App), "ebin"]),
    State2 = rebar_state:update_code_paths(State, all_deps, [TargetDir]),
    Dir = filename:join([rebar_dir:deps_dir(State), "../lib", to_string(App), "ebin"]),
    State3 = case Check of
               true -> 
                    case add_mix_locks(State2) of
                        {State2_, NewLock} -> 
                            code:add_patha(Dir),
                            rebar_state:set(State2_, mixlock, NewLock);
                        _ -> State2
                    end;
                false ->
                    code:add_patha(Dir),
                    State2
             end, 
    add_deps_to_path(State3, Apps, Check).

add_elixir(State) ->
    {BinDir, Env, Config, LibDir} = rebar3_elixir_compile_util:get_details(State),
    MixState = rebar3_elixir_compile_util:add_states(State, BinDir, Env, Config),
    code:add_patha(filename:join(LibDir, "elixir/ebin")),
    code:add_patha(filename:join(LibDir, "mix/ebin")),
    code:add_patha(filename:join(LibDir, "logger/ebin")),
    MixState.

get_details(State) ->
    Config = rebar_state:get(State, elixir_opts, []),
    BinDir = case lists:keyfind(bin_dir, 1, Config) of
                false -> 
                    {ok, ElixirBin_} = find_executable("elixir"),
                    filename:dirname(ElixirBin_);
                {bin_dir, Dir1} -> Dir1
             end, 

    LibDir = case lists:keyfind(lib_dir, 1, Config) of
                false -> 
                    {ok, ElixirLibs_} = sh("elixir -e \"IO.puts :code.lib_dir(:elixir)\"", []),
                    filename:join(re:replace(ElixirLibs_, "\\s+", "", [global,{return,list}]), "../");
                {lib_dir, Dir2} -> Dir2
             end,            
    Env =
        case os:getenv("MIX_ENV") of
            false ->
                {env, E} = lists:keyfind(env, 1, Config),
                E;
            E ->
                list_to_atom(E)
        end,
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
    LibsDir = libs_dir(filename:join([Dir, App]), Env),
    DepDir = filename:join(LibsDir, Dep),
    case filelib:is_dir(DepDir) of
        false -> fetch_mix_app_from_dep(State, Dep, Apps, Dir);
        _ -> 
            DepDir
    end.


mix_to_rebar_lock(State, _Dir, []) ->
    {State, []};

mix_to_rebar_lock(State, Dir, [App | Apps]) ->
    AppDir = filename:join(Dir, App),
    application:ensure_all_started(elixir),

    Lockfile = lockfile(AppDir),
    AppLock =
        %% This code i basically an Erlang transcribed version of Mix.Dep.Lock.read()
        case 'Elixir.File':read(Lockfile) of
            {ok,Info} ->
                Opts = [{file, Lockfile}, {warn_on_unnecessary_quotes, false}],
                {ok, Quoted} = 'Elixir.Code':string_to_quoted(Info, Opts),
                {EvalRes, _Binding} = 'Elixir.Code':eval_quoted(Quoted, Opts),
                'Elixir.Enum':to_list(EvalRes);
            {error, _} ->
                []
        end,

    RebarLock = convert_lock(AppLock, AppLock, 1),
    {State2, DepLocks} = mix_to_rebar_lock(State, Dir, Apps),
    Lock = lists:ukeymerge(1, DepLocks, RebarLock),
    Env = rebar_state:get(State2, mix_env, [dev]),
    LibsDir = libs_dir(AppDir, Env), 
    Deps = lists:filter(fun({D, _, _}) -> is_app_in_dir(rebar_dir:deps_dir(State), to_string(D)) or is_app_in_dir(LibsDir, to_string(D)) end, Lock),
    State3 = add_deps_to_state(State2, App, Deps, LibsDir),
    {State3, Deps}.

add_deps_to_state(State, _Parent, [], _Dir) ->
    State;

add_deps_to_state(State, Parent, [Dep | Deps], Dir) ->
    {Name, {elixir, Pkg, Vsn}, Level} = Dep,
    {ok, AppInfo} = rebar_app_info:new(to_binary(Name), Vsn, filename:join([Dir, Name])),
    AppInfo2 = rebar_app_info:dep_level(AppInfo, Level),
    AppInfo3 = rebar_app_info:source(AppInfo2, {elixir, Pkg, Vsn}),
    AppInfo4 = rebar_app_info:parent(AppInfo3, to_binary(Parent)),
    State2 = case rebar_app_discover:find_app(AppInfo4, filename:join([Dir, to_string(Name)]), all) of
                {true, AppInfo_} -> case is_member(Name, State) of
                                        false -> rebar_state:lock(State, AppInfo_);
                                        true -> State
                                    end;
                _ ->  State
               end,
    add_deps_to_state(State2, Parent, Deps, Dir).      

is_member(Name, State) ->
    is_member(Name, State, rebar_state:lock(State)).

is_member(_Name, _State, []) ->
    false;

is_member(Name, State, [App | Apps]) ->
    case Name == rebar_app_info:name(App) of
        true -> true;
        false -> is_member(Name, State, Apps)
    end.

convert_lock(_Lock, [], _Level) ->
    [];

convert_lock(Lock, [Dep | Deps], Level) ->
    case Dep of
        {Name, {hex, Pkg, Vsn, _Hash, _Manager, SubDeps, _}} ->
            RebarDep = {rebar3_elixir_compile_util:to_binary(Name), {elixir, rebar3_elixir_compile_util:to_string(Pkg), rebar3_elixir_compile_util:to_string(Vsn)}, Level},
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

is_app_in_code_path(_Name) ->
    % Ignore cache
    false.

is_app_in_dir(Dir, App) ->
    filelib:is_dir(filename:join([Dir, App])).

lockfile(AppDir) ->
    iolist_to_binary(filename:join(AppDir, "mix.lock")).


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
    compile_libs(State, false).

compile_libs(State, AddToPath) ->
    Dir = rebar_state:get(State, elixir_base_dir),
    file:make_dir(Dir),
    {ok, Apps} = rebar_utils:list_dir(Dir),
    {ok, State1} = compile_libs(State, Apps, AddToPath),
    Deps = deps_from_mix_lock(State),
    rebar_state:set(State1, deps, Deps).    

compile_libs(State, [], _AddToPath) ->
    {ok, State};          

compile_libs(State, [App | Apps], AddToPath) ->
    AppDir = filename:join(rebar_state:get(State, elixir_base_dir), App), 
    Mix = rebar_state:get(State, mix),
    Env = rebar_state:get(State, mix_env),
    Profile = profile(Env),
    case {ec_file:exists(filename:join(AppDir, "mix.exs")), ec_file:exists(filename:join(AppDir, "rebar.config"))} of
        {true, false} -> 
            sh(Profile ++ Mix ++ "deps.get", [{cd, AppDir}, {use_stdout, true}]),
            sh(Profile ++ Mix ++ "compile", [{cd, AppDir}, {use_stdout, true}]),
            LibsDir = libs_dir(AppDir, Env),
            {ok, Libs} = file:list_dir_all(LibsDir),
            transfer_libs(State, Libs, LibsDir);
        {_, true} ->
            transfer_libs(State, [App], filename:join(AppDir, "../"));
        {false, _} -> State                 
    end,
    compile_libs(State, Apps, AddToPath).

profile(Env) ->
  case Env of
    dev -> ""; 
    prod -> "env MIX_ENV=" ++ atom_to_list(Env) ++ " "
  end.   

libs_dir(AppDir, Env) ->
    case filelib:is_dir(filename:join([AppDir, "_build", "shared" , "lib"])) of
        true -> filename:join([AppDir, "_build/", "shared" , "lib/"]);
        false -> filename:join([AppDir, "_build/", Env , "lib/"])
    end.

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

sh(Command, Options) ->
    rebar_utils:sh(Command, [{env, [{"ERL_FLAGS", ""}]} | Options]).
