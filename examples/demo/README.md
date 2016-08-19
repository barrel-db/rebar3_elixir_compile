demo
=====

Sample rebar3 app that uses rebar3_elixir.

Build
-----

    $ rebar3 compile
    $ rebar3 shell
    Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

    Eshell V7.3  (abort with ^G)
    1> 'Elixir.MacroApp':run().
    
Release
-----

You have to add a release provider hook to your rebar config.
    
    {provider_hooks, [
      {pre, [{release, {ex, compile}}]}
    ]}.
    
The demo has sample code for macros, releases, usage of mix dependencies from Hex and also ability to add own mix apps to `elixir_libs`

    $ rebar3 compile
    $ rebar3 release
    Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

    Eshell V7.3  (abort with ^G)
    1> demo_app:fake_city().
    <<"Lake Barton">>
    
    

