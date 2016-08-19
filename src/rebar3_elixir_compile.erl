-module(rebar3_elixir_compile).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_elixir_compile_prv_ex_compiler:init(State),
    State2 = rebar_state:add_resource(State1, {elixir, rebar3_elixir_compile_resource}),
    {_BinDir, _Env, _Config, LibDir} = rebar3_elixir_compile_util:get_details(State2),
    code:add_patha(filename:join(LibDir, "elixir/ebin")),
    code:add_patha(filename:join(LibDir, "mix/ebin")),
    code:add_patha(filename:join(LibDir, "logger/ebin")),
    State3 = rebar3_elixir_compile_util:add_deps_to_path(State2),
    Dir = filename:join(rebar_dir:root_dir(State3), "elixir_libs/"),
    State4 = case rebar_utils:list_dir(Dir) of
        {ok, Apps} -> rebar3_elixir_compile_util:add_deps_to_path(State3, Apps, false);
        _ -> State3
    end,
    RelxConfig = rebar_state:get(State4, relx, []),
    NewRelxConfig = case lists:keyfind(lib_dirs, 1, RelxConfig) of
        {lib_dirs, OldLibDir} -> 
            NewLibDir = OldLibDir ++ [LibDir],
            lists:keyreplace(lib_dirs, 1, RelxConfig, {lib_dirs, NewLibDir});
        false -> 
            [{lib_dirs, [LibDir]}] ++ RelxConfig  
    end,
    State6 = rebar_state:set(State4, relx, NewRelxConfig),
    {ok, State6}.
    
