-module(orders).
-export([write/2, delete/2, get/2, make_order/6, id/1, 
	 pointer/1, price/1, update_pointer/2, sort/2,
	 update_id/2, update_amount/2, root_hash/1, 
	 amount/1,
	 test/0]).

%each oracle has it's own trie of orders. This single tree contains 2 linked lists, one of buys, and the other of sells.
%Besides the tree, we need to remember the current head of each list. Maybe we can store the heads in location 1 of the tree.

%How is an order stored in the trie?
%needs:
-record(order, {id = 0, %This is the location of this leaf in the merkle tree.
		oracle_id = 0, %This is the unique id for the oracle that this trade is participating in.
		account_id = 0, 
		price = 0, 
		pointer = 0, %either points to the next element of the list, or if it is the last element, it points to 0.
		amount = 0}).%amount of coins they want to spend.
id(X) -> X#order.id.
pointer(X) -> X#order.pointer.
price(X) -> X#order.price.
amount(X) -> X#order.amount.
update_pointer(X, P) ->
    X#order{pointer = P}.
update_id(X, I) ->
    X#order{id = I}.
update_amount(X, A) ->
    B = X#order.amount - A,
    true = B > 0,
    X#order{amount = B}.
root_hash(X) ->
    {A, _, P} = trie:get(1, X, open_orders), 
    {A, P}.
make_order(ID, OID, AID, Price, Pointer, Amount) ->
    #order{id = ID, account_id = AID, 
	   oracle_id = OID,
	   price = Price, pointer = Pointer, 
	   amount = Amount}.
serialize(A) ->
    KeyLength = config:key_length() * 8,
    BalanceBytes = config:balance_bytes() * 8,
    PriceBytes = config:price_bytes() * 8,
    AccountBytes = config:account_bytes() * 8,
    <<(A#order.pointer):KeyLength,
      (A#order.amount):BalanceBytes,
      (A#order.price):PriceBytes,
      (A#order.account_id):AccountBytes,
      (A#order.id):KeyLength>>.%we don't need to store the id, because this is the location where it lives. If we look it up, then we must already know the id.
deserialize(A) ->
    KeyLength = config:key_length() * 8,
    BalanceBytes = config:balance_bytes() * 8,
    PriceBytes = config:price_bytes() * 8,
    AccountBytes = config:account_bytes() * 8,
    <<Pointer:KeyLength,
      Amount:BalanceBytes,
      Price:PriceBytes,
      AccountID:AccountBytes, 
      ID:KeyLength>> = A,
    #order{account_id = AccountID, price = Price, 
	   pointer = Pointer, amount = Amount, id = ID}.
      


write(Leaf, Root) ->
    ID = Leaf#order.id,
    M = serialize(Leaf),
    trie:put(ID, M, Root, open_orders).
delete(ID, Root) ->
    trie:delete(ID, Root, open_orders).
get(ID, Root) ->
    {RH, Leaf, Proof} = trie:get(ID, Root, open_orders),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, [Proof]}.
sort(L, F) ->
    L2 = to_lists(L),
    hd(merge_sort(L2, F)).
to_lists([]) -> [];
to_lists([H|T]) -> 
    [[H]|to_lists(T)].
merge_sort([], _) -> [[]];
merge_sort([X], _) ->
    [X];
merge_sort(X, F) ->
    merge_sort(merge(X, F), F).
merge([], _) -> [];
merge([A], _) -> [A];
merge([A|[B|T]], F) -> 
    [merge2(A, B, F)|merge(T, F)].
merge2([], A, _) -> A;
merge2(A, [], _) -> A;
merge2([A|AT], [B|BT], F) -> 
    AP = A#order.price,
    BP = B#order.price,
    C = F(BP, AP),
    if
	C -> [B|merge2([A|AT], BT, F)];
	true -> [A|merge2(AT, [B|BT], F)]
    end.
	    
		
    
test() ->
    X = make_order(1,0,2,3,4,5),
    X = deserialize(serialize(X)),
    sort([make_order(1,1,1,1,1,1),
	  make_order(1,1,1,3,1,1),
	  make_order(1,1,1,2,1,1),
	  make_order(1,1,1,4,1,1)],
	 fun(Z, Y) -> Z>Y end).
    %success.
