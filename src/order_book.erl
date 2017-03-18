-module(order_book).
-export([add_orders/2, match_orders/1]).

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
add_orders(Root, Orders) ->
%We need a function that takes a merkle root, and a list of new orders. It produces merkle proofs of all the data that got changed, and the new merkle root.
%full nodes only need to download orders who's pointer changed.
    ok.
remove_orders(Root, Orders) ->
    %reverse of add_orders
    ok.
match_orders(Root) ->
%We need a function that takes a merkle root, and closes as many orders as possible. It should produce proofs of all the data that got changed, and the new merkle root.
%full nodes only need to download copies of the orders that get matched, and orders that used to point to orders that got matched. 
    ok.


