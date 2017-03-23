-module(order_book).
-export([add_orders/3, match_orders/2, test/0]).

%if storing N bytes is more expensive than downloading 2N bytes, then this is a good design.
%Looks like storage is far cheaper https://what-if.xkcd.com/31/
%so it is probably a bad design.

%so why does ethereum use merkle trees then? They could take a single hash of a huge binary database, it would be much more efficient than a merkle tree.

%maybe it is so that we can be sure we are downloading the correct data?
%maybe it is so there is no long waiting period when you first boot up? (But light nodes wouldn't download the data anyway?)
%Maybe we want light nodes that act as full nodes for the most recent 20 blocks, which would protect us from attacks where the miner makes illegal state changes.

%We want to deterministically define the entire state of the blockchain in a merkle trie, because this makes it easy to prove facts about the state, without having to send the entire state over the wire.
%For example, if we are only adding a single open order to the order book, we don't need to know the entire list of open orders to add one more. We just need to know about the next lower in the list.

%We will build a linked list on top of the merkle trie. We store by integer. Each leaf points to the next higher leaf in the sorted list.
add_orders(Root, BuyOrders, SellOrders) ->
%to add an order, we need to know the next lower order.
%We need a function that takes a merkle root, and a list of new orders. It produces merkle proofs of all the data that got changed, and the new merkle root.
%full nodes only need to download orders who's pointer changed.
%This function is run by miners when they are computing the next header. Only miners that know about the relevant orders can include transactions that change them.
%When the miner shares the block, they send proofs with it, that way more nodes will accept the new block sooner.
    
    %First, sort the orders.
    %SortedOrders = orders:sort(Orders),
    {_, HeadsLeaf, Proof} = trie:get(1, Root, open_orders),
    Heads = leaf:value(HeadsLeaf),
    {BuyHead, SellHead} = deserialize_heads(Heads),
    {BuyHead2, Root2, Proofs2} = 
	add_orders3(BuyHead, orders:sort(BuyOrders), Root, Root),
    %BuyHead is the integer that is a key to look up the first element of the list of buys.
    %If the new orders are all bigger than the loset old order, then BuyHead2 should remain unchanged, it is still the head of the list.
    {SellHead2, Root3, Proofs3} = 
	add_orders3(SellHead, orders:sort(SellOrders), Root2, Root),
    P = Proof ++ Proofs2 ++ Proofs3,
    NewHeads = serialize_heads(BuyHead2, SellHead2),
    Root4 = trie:put(1, NewHeads, Root3, open_orders),
    {Root4, P}.
add_orders3(Head, [], Root, _) ->
    {Head, Root, []};
add_orders3(Head, [R|Orders], Root, FirstRoot) ->
    RID = orders:id(R),
    {NewHead, Proofs1, Root2} = 
	if
	    Head == 0 -> 
		
		{_, empty, ProofsInner} = orders:get(orders:id(R), FirstRoot),
		Root3 = orders:write(R, Root),
		{RID, ProofsInner, Root3};
	    true ->
		{_, HeadOrder, _Proofs} = orders:get(Head, Root),
		HeadPrice = orders:price(HeadOrder),
		RPrice = orders:price(R),
		{NH, Root4, ProofsInner} = 
		    if
			RPrice > HeadPrice -> 
			    {_, empty, Proofs5} = orders:get(orders:id(R), FirstRoot),
			    Root3 = orders:write(R, Root),
			    {RID, Root3, Proofs5};
			true -> {Head, Root, []}
		    end,
		{NH, ProofsInner, Root4}
	end,
    {NewRoot, Proofs2} = add_orders2(NewHead,[R|Orders], Root2, []),
    {NewHead, NewRoot, Proofs1++Proofs2}.
add_orders2(List, [], Root, Proofs) ->
    %Root2 = orders:write(List, Root),%The deepest element of the linked list may be in a new location.
    {Root, Proofs};
add_orders2(List, [S|SortedOrders], Root, Proofs) ->
    N = orders:id(S),%Users select an empty location in the tree to store their order. If multiple users select the same location, then only one of the trades can be included.

    %N needs to be in the range of values that the trie can store. We need to check.
    case List of
	0 ->
	    Root2 = orders:write(S, Root),
	    add_orders2(N, SortedOrders, Root2, Proofs);
	_ -> add_orders22(List, [S|SortedOrders], Root, Proofs)
    end.
add_orders22(List, [], Root, Proofs) ->
    %Root2 = orders:write(List, Root),%The deepest element of the linked list may be in a new location.
    {Root, Proofs};

add_orders22(List, [S|SortedOrders], Root, Proofs) ->
    {_, Order, _Proofs3} = orders:get(List, Root),
    P = orders:pointer(Order),
    N = orders:id(S),
    case P of
	0 -> 
	    Order2 = orders:update_pointer(Order, N),
	    Root2 = orders:write(Order2, Root),
	    %{_, empty, Proofs2} = orders:get(orders:id(Order2), Root),
	    Root3 = orders:write(S, Root2),
	    add_orders22(N,
			SortedOrders,
			Root3, 
			Proofs);
	X -> 
	    OldP = orders:price(Order),
	    NewP = orders:price(S),
	    if
		NewP > OldP ->
		    add_orders22(orders:pointer(Order),
				[S|SortedOrders],
				Root, 
				Proofs);
		true -> 
		    S2 = orders:update_id(S, List),
		    S3 = orders:update_pointer(S, N),
		    Root2 = orders:write(S3, Root),
		    List2 = orders:update_id(List, N),
		    add_orders22(List2, 
				 SortedOrders, 
				 Root2, 
				 Proofs)
	    end
    end.
 
remove_orders(_Root, _BuyOrders, _SellOrders) ->
    %reverse of add_orders
    %we need to zero out the empty spots, and fix the pointers.
    NewRoot = ok,
    Proofs = ok,
    {NewRoot, Proofs}.
match_orders(_BuyRoot, _SellRoot) ->
%We need a function that takes a merkle root, and closes as many orders as possible. It should produce the new merkle root.
%full nodes only need to download copies of the orders that get matched, and orders that used to point to orders that got matched. 
    %We need to prove that all the orders that can be matched were matched.
    %We need to prove that the price is the price that matches the most trades possible.
    %we need to zero out the empty stuff in the trie.
    NewRoot = ok,
    Proofs = ok,
    {NewRoot, Proofs}.
serialize_heads(BuyHead, SellHead) ->
    Rest = (constants:balance_bytes() + constants:price_bytes() + constants:account_bytes())*8,
    KeyLength = constants:key_length()*8,
    <<BuyHead:KeyLength,
      SellHead:KeyLength,
      0:Rest>>.
    
deserialize_heads(B) ->
    Rest = (constants:balance_bytes() + constants:price_bytes() + constants:account_bytes())*8,
    KeyLength = constants:key_length()*8,
    <<BuyHead:KeyLength,
      SellHead:KeyLength,
      _:Rest>> = B,
    {BuyHead, SellHead}.
empty_list() ->
    %lets store a pointer to the root in 1.
    %pointing to 0 is null, it signifies the end of the list.
    X = serialize_heads(0, 0),
    trie:put(1, X, 0, open_orders).

test() ->
    %start with an empty order book, add some orders, remove some orders, match some, check the final result is good.
    OrdersA = [orders:make_order(5, 0, 1, 256*8, 0, 100),
	       orders:make_order(2, 0, 1, 256*10, 0, 100)],
    OrdersB = [orders:make_order(3, 0, 1, 256*9, 0, 100),
	       orders:make_order(4, 0, 1, 256*7, 0, 100)],
    L4 = [],
    L = empty_list(),
    L3 = add_orders(L, OrdersB, []),
    L2 = add_orders(L, OrdersA++OrdersB, []).
test2(L, OrdersA, OrdersB) ->
    L2 = add_orders(L, OrdersA++OrdersB, []),
    L3 = remove_orders(L2, OrdersA, []),
    OrdersC = [orders:make_order(5, 0, 2, 256*8, 0, 80)],
    Sell = add_orders(L, [], OrdersC),
    L5 = match_orders(L3, Sell),
    L4 = match_orders(Sell, Sell),
    success.
