-module(order_book).
-export([add_orders/3, match_orders/2, test/0]).

add_orders(Root, BuyOrders, SellOrders) ->
%to add an order, we need to know the next lower order.
%full nodes only need to download orders who's pointer changed.
%This function is run by miners when they are computing the next header. Only miners that know about the relevant orders can include transactions that change them.
%When the miner shares the block, they send proofs with it, that way more nodes will accept the new block sooner.
    
    {_, HeadsLeaf, Proof} = trie:get(1, Root, open_orders),
    Heads = leaf:value(HeadsLeaf),
    {BuyHead, SellHead} = deserialize_heads(Heads),
    GT = fun(X, Y) -> X>Y end,
    {BuyHead2, Root2, Proofs2} = 
	add_orders2(BuyHead, orders:sort(BuyOrders, GT), Root, Root, GT),
    %BuyHead is the integer that is a pointer to the first element of the list of buys.
    %If the new orders are all bigger than the lowest old order, then BuyHead2 should remain unchanged, it is still the head of the list.
    LT = fun(X, Y) -> X<Y end,
    {SellHead2, Root3, Proofs3} = 
	add_orders2(SellHead, orders:sort(SellOrders, LT), Root2, Root, LT),
    P = Proof ++ Proofs2 ++ Proofs3,
    NewHeads = serialize_heads(BuyHead2, SellHead2),
    Root4 = trie:put(1, NewHeads, Root3, open_orders),
    {Root4, P}.
add_orders2(Head, [], Root, _FirstRoot, _) ->
    {Head, Root, []};
add_orders2(0, [R|Orders], Root, FirstRoot, F) ->
    RID = orders:id(R),
    {_, empty, Proof1} = orders:get(RID, FirstRoot),
    {Head2, NewRoot, Proof2} = add_orders2(0, Orders, Root, FirstRoot, F),
    R2 = orders:update_pointer(R, Head2), 
    Root2 = orders:write(R2, NewRoot),
    {RID, Root2, Proof1++Proof2};
add_orders2(Head, [R|Orders], Root, FirstRoot, F) ->
    {_, OldHead, _Proof1} = orders:get(Head, Root),
    OldPrice = orders:price(OldHead),
    NewPrice = orders:price(R),
    Bool = F(NewPrice, OldPrice),
    if
	Bool ->
	    RID = orders:id(R),
	    {Head2, Root3, Proofs} = add_orders2(Head, Orders, Root, FirstRoot, F),
	    R2 = orders:update_pointer(R, Head2),
	    Root4 = orders:write(R2, Root3),
	    {RID, Root4, Proofs};
	true ->
	    io:fwrite("new price bigger\n"),
	    {Head2, Root3, Proofs} = add_orders2(orders:pointer(OldHead), [R|Orders], Root, FirstRoot, F),
	    NewHead = orders:update_pointer(OldHead, Head2),
	    Root4 = orders:write(NewHead, Root3),
	    {Head, Root4, Proofs}
    end.
	    
remove_orders(Root, BuyOrders, SellOrders) ->
    %reverse of add_orders
    %we need to zero out the empty spots, and fix the pointers.
    {_, HeadsLeaf, Proof} = trie:get(1, Root, open_orders),
    Heads = leaf:value(HeadsLeaf),
    {BuyHead, SellHead} = deserialize_heads(Heads),
    GT = fun(X, Y) -> X>Y end,
    {BuyHead2, Root2, Proofs2} = 
	remove_orders2(BuyHead, orders:sort(BuyOrders, GT), Root, Root),
    LT = fun(X, Y) -> X<Y end,
    {SellHead2, Root3, Proofs3} = 
	remove_orders2(SellHead, orders:sort(SellOrders, LT), Root2, Root),
    NewHeads = serialize_heads(BuyHead2, SellHead2),
    NewRoot = trie:put(1, NewHeads, Root3, open_orders),
    {NewRoot, Proof++Proofs2++Proofs3}.
remove_orders2(0, [], Root, _) ->
    {0, Root, []};
remove_orders2(Head, [], Root, FirstRoot) ->
    %removing none of the orders
    {_, HeadOrder, _Proofs} = orders:get(Head, Root),
    {Head2, Root2, Proofs} = remove_orders2(orders:pointer(HeadOrder), [], Root, FirstRoot),
    H2 = orders:update_pointer(HeadOrder, Head2),
    Root3 = orders:write(H2, Root2),
    {Head, Root3, Proofs};
remove_orders2(Head, [R|Orders], Root, FirstRoot) ->
    RID = orders:id(R),
    {_, HeadOrder, _Proofs} = orders:get(Head, Root),
    HID = orders:id(HeadOrder),
    Next = orders:pointer(HeadOrder),
    if 
	HID == RID -> 
	    %remove the order. 
	    %write the pointer from the removed order onto whatever used to point to the order we removed 
	    Root2 = orders:delete(HID, Root),
	    remove_orders2(Next, Orders, Root2, FirstRoot);
	true ->
	    {Head2, Root2, Proofs} = remove_orders2(Next, [R|Orders], Root, FirstRoot),
	    R2 = orders:update_pointer(HeadOrder, Head2),
	    Root3 = orders:write(R2, Root2),
	    {Head, Root3, Proofs}
    end.

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
    OrdersA = [orders:make_order(5, 0, 1, 8000, 0, 100),
	       orders:make_order(2, 0, 1, 9000, 0, 100)],
    OrdersB = [orders:make_order(3, 0, 1, 10000, 0, 100),
	       orders:make_order(4, 0, 1, 7000, 0, 100)],
    L = empty_list(),
    {L3, _Proofs} = add_orders(L, OrdersA, []),
    {L4, _} = add_orders(L3, OrdersB, []),
    {L2, _} = add_orders(L, OrdersA++OrdersB, []),
    {RH, _} = orders:root_hash(L4),
    {RH, _} = orders:root_hash(L2),
    {_, HeadsLeaf, _} = trie:get(1, L4, open_orders),
    {BuyHead, _} = deserialize_heads(leaf:value(HeadsLeaf)),
    {_, Order1, _} = orders:get(BuyHead, L4),
    P2 = orders:pointer(Order1),
    {_, Order2, _} = orders:get(P2, L4),
    P3 = orders:pointer(Order2),
    {_, Order3, _} = orders:get(P3, L4),
    P4 = orders:pointer(Order3),
    {_, Order4, _} = orders:get(P4, L4),
    {Order1, Order2, Order3, Order4},
    {L5, _} = remove_orders(L2, OrdersB, []),
    %{_, HeadsLeaf2, _} = trie:get(1, L5, open_orders),
    %{BuyHead2, _} = deserialize_heads(leaf:value(HeadsLeaf2)),
    %{_, Order1a, _} = orders:get(BuyHead2, L5),
    %P1a = orders:pointer(Order1a),
    %{_, Order2a, _} = orders:get(P1a, L5),
    %{Order1a, Order2a},
    %io:fwrite("order book l3 "),
    %{_, HeadsLeaf3, _} = trie:get(1, L3, open_orders),
    %{BuyHead3, _} = deserialize_heads(leaf:value(HeadsLeaf3)),
    %{_, Order1b, _} = orders:get(BuyHead3, L3),
    %P1b = orders:pointer(Order1b),
    %{_, Order2b, _} = orders:get(P1b, L3),
    %{Order1a, Order2a} = {Order1b, Order2b},
    {RH2, _} = orders:root_hash(L5),
    {RH2, _} = orders:root_hash(L3),
    success.


    %0 = orders:pointer(Order4),
    %4 = orders:id(Order1),
    %5 = orders:id(Order2),
    %2 = orders:id(Order3),
    %3 = orders:id(Order4),
    %success.


    %{L4, _} = remove_orders(L2, OrdersA, []),
    %{RootHash, _, _} = orders:get(1, L3),
    %{RootHash, _, _} = orders:get(1, L4).
    
    %calculate an equivalent tree in a different way, and make sure that the root hashes are equal.
test2(L, OrdersA, OrdersB) ->
    L2 = add_orders(L, OrdersA++OrdersB, []),
    L3 = remove_orders(L2, OrdersA, []),
    OrdersC = [orders:make_order(5, 0, 2, 256*8, 0, 80)],
    Sell = add_orders(L, [], OrdersC),
    L5 = match_orders(L3, Sell),
    L4 = match_orders(Sell, Sell),
    success.
