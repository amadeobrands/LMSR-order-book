Letting users select the location in the list orders:id is a bad idea. Instead it should be stored by the hash of a value. Otherwise an attacker could bribe the miners to censor participation in the oracle.

We should add checks to make sure any value fed into the oracle is within the range of values that the merkle tree can handle.


There will be many oracles at the same time.
Should they all use the same merkle tree, or should we make a merkle tree of oracle roots?

If we use a single oracle tree, then we need a oracle-id number on each leaf, that way we can prove a particular leaf exists in a particular oracle without proving the entire linked list.


Once orders are matched, we need to keep a record of how many shares in the oracle each account owns.
Since this is only an "order book", maybe keeping a record of how many shares each account owns shouldn't be done in this repository.

Instead of having a fixed sized account tree, we should probably have variable sized. That way we can make each account data bigger to fit all this oracle information.
We should have some maximum size for each account.