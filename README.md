rebar3_elixir
=====
A rebar3 plugin to use elixir in your applications.

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_elixir, ".*", {git, "https://github.com/barrel-db/rebar3_elixir.git", {branch, "master"}}}
    ]}.
    
    {deps, [
     {plug, {elixir, "plug" ,"1.1.0"}}
    ]}.
    
    {provider_hooks, [{post, [{compile, {ex, compile}}]}]}.
    {elixir_opts, 
      [
        {env, dev},
      ]
    }.

If you want to modify elixir bin and lib directories, add to elixir opts the following: 

    {lib_dir, "/usr/local/lib/elixir/lib/"},
    {bin_dir, "/usr/local/bin/"}

Place your elixir mix applications in ./elixir_libs.


