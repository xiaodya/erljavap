%%%-------------------------------------------------------------------
%% @doc erljavap public API
%% @end
%%%-------------------------------------------------------------------

-module(erljavap_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erljavap_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
