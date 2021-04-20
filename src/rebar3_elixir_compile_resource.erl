 -module(rebar3_elixir_compile_resource).

-behaviour(rebar_resource).

-define(DEFAULT_CDN_SITE, "https://repo.hex.pm").
-define(CDN_TARBALL_LOCATION, "/tarballs").

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

lock(_Dir, {elixir, Name, Vsn}) ->
    {elixir, rebar3_elixir_compile_util:to_binary(Name), rebar3_elixir_compile_util:to_binary(Vsn)}.

download(Dir, {elixir, Name, _Vsn} = Pkg, State) ->
    {ok, Config} = file:consult(filename:join([rebar_dir:root_dir(State), "rebar.config"])),
    {deps, Deps} = lists:keyfind(deps, 1 , Config),
    case is_dep_there(Deps, Name, rebar_dir:deps_dir(State)) of 
        false -> 
            fetch_and_compile(State, Dir, Pkg);
        true ->
            rebar3_elixir_compile_util:maybe_copy_dir(rebar3_elixir_compile_util:fetch_mix_app_from_dep(State, Name), Dir, false),
            rebar3_elixir_compile_util:maybe_copy_dir(filename:join([rebar_dir:deps_dir(State), Name]), Dir, false)
    end,
    {ok, true}.

is_dep_there(Deps, Name, Dir) ->
    InConfig = lists:filter(fun 
                            ({D, _}) -> rebar3_elixir_compile_util:to_binary(D) == rebar3_elixir_compile_util:to_binary(Name); 
                            (_) -> false
                            end, Deps),
    InDir = filelib:is_dir(filename:join([Dir, Name, "ebin"])),
    case {InConfig, InDir} of
        {[], true} ->
            true;
         {_, true} ->
             false;
        {[], false} ->
             true;          
         {_, false} ->
             false
    end.

needs_update(Dir, {elixir, _Name, Vsn}) ->
    rebar_api:console("Checking for update, ~p", _Name),
    [AppInfo] = rebar_app_discover:find_apps([Dir], all),
    case rebar_app_info:original_vsn(AppInfo) =:= ec_cnv:to_list(Vsn) of
        true ->
            false;
        false ->
            true
    end.

make_vsn(_) ->
    {error, "Replacing version of type elixir not supported."}.

fetch_and_compile(State, Dir, {elixir, Name, _Vsn} = Pkg) ->
    CDN = cdn(State),
    fetch(Pkg, CDN),
    State1 = rebar3_elixir_compile_util:add_elixir(State),
    State2 = rebar_state:set(State1, libs_target_dir, default),
    BaseDir = filename:join(rebar_dir:root_dir(State2), "_elixir_build/"),
    BaseDirState = rebar_state:set(State2, elixir_base_dir, BaseDir),
    Env = rebar_state:get(BaseDirState, mix_env),
    AppDir = filename:join(BaseDir, Name),
    rebar3_elixir_compile_util:compile_libs(BaseDirState),
    LibsDir = rebar3_elixir_compile_util:libs_dir(AppDir, Env),
    rebar3_elixir_compile_util:transfer_libs(rebar_state:set(BaseDirState, libs_target_dir, Dir), [Name], LibsDir).
  
cdn(State) ->
  Opts = rebar_state:get(State, elixir_opts, []),
  CDNSite = proplists:get_value(cdn, Opts, ?DEFAULT_CDN_SITE),
  CDNSite ++ ?CDN_TARBALL_LOCATION.

fetch({elixir, Name_, Vsn_}, CDN) ->
    Dir = filename:join([filename:absname("_elixir_build"), Name_]),
    Name = rebar3_elixir_compile_util:to_binary(Name_), 
    Vsn  = rebar3_elixir_compile_util:to_binary(Vsn_),
    case filelib:is_dir(Dir) of
        false ->
            Config = #{
              http_adapter => hex_http_httpc,
              http_adapter_config => #{profile => default},
              http_user_agent_fragment => user_agent(),
              repo_url => list_to_binary(CDN)
            },
            case hex_repo:get_tarball(Config, Name, Vsn) of
                {ok, Binary, _} ->
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

user_agent() ->
    AppName = rebar3_elixir_compile,
    AppVsn = case lists:keyfind(AppName, 1, application:loaded_applications()) of
                 {_, _, Ver} ->
                     Ver;
                 false ->
                     "unknown"
             end,
    list_to_binary([atom_to_list(AppName), "/", AppVsn, " (httpc)"]).