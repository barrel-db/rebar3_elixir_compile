%%%-------------------------------------------------------------------
%% @doc demo public API
%% @end
%%%-------------------------------------------------------------------

-module(demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, decimal_demo_run/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    demo_sup:start_link().

decimal_demo_run() ->
    A = 'Elixir.Decimal':new(7),
    B = 'Elixir.Decimal':new(4),
    C = 'Elixir.Decimal':add(A, B),
    io:format("Result is : ~p", [C]).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
