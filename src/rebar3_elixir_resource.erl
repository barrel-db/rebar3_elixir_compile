 -module(rebar3_elixir_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

lock(_Dir, Source) ->
    rebar_api:console("===> Locking ~p", [Source]),
    Source.

download(Dir, {elixir, Name, Vsn}, State) ->
    Pkg = {pkg, Name, Vsn},
    {ok, Config} = file:consult(filename:join([rebar_dir:root_dir(State), "rebar.config"])),
    {deps, Deps} = lists:keyfind(deps, 1 , Config),
    rebar3_elixir_util:add_deps_to_path(State),
    case isDepThere(Deps, Name, rebar_dir:deps_dir(State)) of 
        false -> 
            fetch_and_compile(State, Dir, Pkg);
        true ->
            ok
    end,
    {ok, true}.

isDepThere(Deps, Name, Dir) ->
    InConfig = lists:filter(fun ({D, _}) -> rebar3_elixir_util:to_binary(D) == rebar3_elixir_util:to_binary(Name) end, Deps),
    InDir = filelib:is_dir(filename:join(Dir, Name)),
    V = case {InConfig, InDir} of
        {[], true} ->
            false;
         {_, true} ->
             false;
         {_, false} ->
             true
    end,
    V.

needs_update(Dir, {pkg, _Name, Vsn}) ->
    [AppInfo] = rebar_app_discover:find_apps([Dir], all),
    case rebar_app_info:original_vsn(AppInfo) =:= ec_cnv:to_list(Vsn) of
        true ->
            false;
        false ->
            true
    end.

make_vsn(_) ->
    {error, "Replacing version of type elixir not supported."}.

fetch_and_compile(State, Dir, Pkg = {pkg, Name, _Vsn}) ->
    fetch(Pkg),
    State1 = rebar3_elixir_util:add_elixir(State),
    State2 = rebar_state:set(State1, libs_target_dir, default),
    BaseDir = filename:join(rebar_dir:root_dir(State2), "_elixir_build/"),
    BaseDirState = rebar_state:set(State2, elixir_base_dir, BaseDir),
    Env = rebar_state:get(BaseDirState, mix_env),
    AppDir = filename:join(BaseDir, Name),
    LibDir = filename:join([AppDir, "_build/", Env , "lib/"]),
    rebar3_elixir_util:compile_libs(BaseDirState),
    rebar3_elixir_util:transfer_libs(rebar_state:set(BaseDirState, libs_target_dir, Dir), [Name], LibDir).

fetch({pkg, Name_, Vsn_}) ->
    Dir = filename:join([filename:absname("_elixir_build"), Name_]),
    Name = rebar3_elixir_util:to_binary(Name_), 
    Vsn  = rebar3_elixir_util:to_binary(Vsn_),
    case filelib:is_dir(Dir) of
        false ->
            CDN = "https://repo.hex.pm/tarballs",
            Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
            Url = string:join([CDN, Package], "/"),
            case request(Url) of
                {ok, Binary} ->
                    {ok, Contents} = extract(Binary),
                    ok = erl_tar:extract({binary, Contents}, [{cwd, Dir}, compressed]);
                _ ->
                    rebar_api:console("Error: Unable to fetch package ~p ~p~n", [Name, Vsn])
            end;
        true ->
            rebar_api:console("Dependency ~s already exists~n", [Name])
    end.

extract(Binary) ->
    {ok, Files} = erl_tar:extract({binary, Binary}, [memory]),
    {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
    {ok, Contents}.

request(Url) ->
    case httpc:request(get, {Url, []},
                       [{relaxed, true}],
                       [{body_format, binary}],
                       rebar) of
        {ok, {{_Version, 200, _Reason}, _Headers, Body}} ->
            {ok, Body};
        Error ->
            Error
    end.
