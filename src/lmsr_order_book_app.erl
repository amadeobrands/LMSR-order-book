-module(lmsr_order_book_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    KeyLength = constants:key_length(),%constants key_length
    BalanceBytes = constants:balance_bytes(),
    PriceBytes = constants:price_bytes(),
    AccountBytes = constants:account_bytes(),
    lmsr_order_book_sup:start_link(KeyLength, BalanceBytes, PriceBytes, AccountBytes).

stop(_State) ->
    ok.
