It seems like the best strategy is to buy and sell really close to the limits.
So why not make all the trades at 50/50 prices?
We should only sell shares, and not buy them back. We should sell at least 2 kinds at once in the same amount, so we only have to store one kind of share in the order book at a time.

The on-chain order book can have a minimum trade size, and a maximum volume. So we only have to remember like 10 or 100 trades at a time.
orders that are never stored in the order book can be smaller. So defenders don't have to be as rich as attackers.

If there isn't enough trading within a time-limit, then we should mark it as a bad question and give everyone who participated in the oracle their money back, except for the initial money to start the oracle.

The person who starts the oracle has to pay some money. This money should be given as a reward to whoever's left money sitting in the order book until the end. It should be given only to the users who placed the order that will be matched next.



Letting users select the location in the list orders:id is a bad idea. Instead it should be stored by the hash of a value. Otherwise an attacker could bribe the miners to censor participation in the oracle.

A problem with storing by hash is that sometimes a trade is only partially matched, so it's new amount is less than the old. So the location where the data is stored would change. Maybe this isn't a problem?

We should add checks to make sure any value fed into the oracle is within the range of values that the merkle tree can handle.


There will be many oracles at the same time.
Should they all use the same merkle tree, or should we make a merkle tree of oracle roots?

If we use a single oracle tree, then we need a oracle-id number on each leaf, that way we can prove a particular leaf exists in a particular oracle without proving the entire linked list.


Once orders are matched, we need to keep a record of how many shares in the oracle each account owns.
Since this is only an "order book", maybe keeping a record of how many shares each account owns shouldn't be done in this repository.

Instead of having a fixed sized account tree, we should probably have variable sized. That way we can make each account data bigger to fit all this oracle information.
We should have some maximum size for each account.