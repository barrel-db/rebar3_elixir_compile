rebar3_elixir_compile
=====
A rebar3 elixir compiler plugin

![rebar3_elixir_compile in action](doc/screenshot-release.png)

Usage
-----

Add the plugin to your rebar config:

```erlang
{erl_opts, [debug_info]}.

{plugins, [
    { rebar3_elixir_compile, ".*", {git, "https://github.com/barrel-db/rebar3_elixir_compile.git", {branch, "master"}}}
]}.

{deps, [
   {faker, {elixir, "faker" ,"0.6.0"}}
]}.

{provider_hooks, [
  {pre, [{compile, {ex, compile}}]}
  %% {pre, [{release, {ex, compile}}]}  
]}.

{elixir_opts, 
  [
    {env, dev}
  ]
}.
```

Full example in https://github.com/barrel-db/rebar3_elixir_compile/tree/master/examples/demo

If you want to modify elixir bin and lib directories, add to elixir opts the following: 

    {lib_dir, "/usr/local/lib/elixir/lib/"},
    {bin_dir, "/usr/local/bin/"}

Place your elixir mix applications in ./elixir_libs.
The plugin also works with a rebar release.

