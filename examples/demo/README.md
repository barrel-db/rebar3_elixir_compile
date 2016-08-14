demo
=====

Sample rebar3 app that uses rebar3_elixir.

Build
-----

    $ rebar3 compile
    $ rebar3 shell
    Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

    Eshell V7.3  (abort with ^G)
    1> demo_app:decimal_demo_run().
    Result is : #{'__struct__' => 'Elixir.Decimal',coef => 11,exp => 0,sign => 1}ok


Demo app has the following function

    A = 'Elixir.Decimal':new(7),
    B = 'Elixir.Decimal':new(4),
    C = 'Elixir.Decimal':add(A, B),
    io:format("Result is : ~p", [C]).

