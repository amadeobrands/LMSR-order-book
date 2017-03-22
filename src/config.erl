-module(config).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	key_length/0,balance_bytes/0,price_bytes/0,account_bytes/0]).
init(X) -> {ok, X}.
start_link(K, B, P, A) -> 
    %Sum = (K*2) + B + P + A,
    %X = 8 - (Sum rem 8),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [K, B, P, A], []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(key_length, _From, X) -> 
    [K, _, _, _] = X,
    {reply, K, X};
handle_call(balance_bytes, _From, X) -> 
    [_, B, _, _] = X,
    {reply, B, X};
handle_call(price_bytes, _From, X) -> 
    [_, _, P, _] = X,
    {reply, P, X};
handle_call(account_bytes, _From, X) -> 
    [_, _, _, A] = X,
    {reply, A, X};
handle_call(_, _From, X) -> {reply, X, X}.

key_length() -> gen_server:call(?MODULE, key_length).
balance_bytes() -> gen_server:call(?MODULE, balance_bytes).
price_bytes() -> gen_server:call(?MODULE, price_bytes).
account_bytes() -> gen_server:call(?MODULE, account_bytes).
