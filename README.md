rebar3_elixir
=====

A rebar3 plugin to use elixir in your applications.


Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_elixir, ".*", {git, "https://github.com/sivsushruth/rebar3_elixir.git", {branch, "master"}}}
    ]}.
    {provider_hooks, [{post, [{compile, {ex, compile}}]}]}.
    {elixir_opts, 
      [{lib_dir, "/usr/local/lib/elixir/lib/"},
      {bin_dir, "/usr/local/bin/"},
      {env, dev}]
    }.

Place your elixir mix applications in ./elixir_libs.

If you want to use it with relx add the following line :

    {lib_dirs, ["/usr/local/lib/elixir/lib/"]},
