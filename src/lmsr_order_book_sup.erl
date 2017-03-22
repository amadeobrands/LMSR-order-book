-module(lmsr_order_book_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(K, B, P, A) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [K, B, P, A]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([KeyLength, BalanceBytes, PriceBytes, AccountBytes]) ->
    LeafSize = (2 * KeyLength) + BalanceBytes + PriceBytes + AccountBytes,%constants account_size %check orders.erl
    TrieSize = 5000,%constants trie_size %how many orders can be stored in memory at once, not including garbage collected orders.
    Children = [
		{open_orders_sup, {trie_sup, start_link, [KeyLength, LeafSize, open_orders, TrieSize, hd]}, permanent, 5000, supervisor, [trie_sup]},
		{config, {config, start_link, [KeyLength, BalanceBytes, PriceBytes, AccountBytes]}, permanent, 5000, worker, [config]}
	       ],
    {ok, { {one_for_one, 5, 10}, Children} }.



