-module(lmsr).
-export([test/0]).
-define(E, 2.71828182845).	

prices(_SharesSold, _Trades) ->
    ok.
lmsr_helper(Shares, B) ->
    math:log(lmsr_helper2(Shares, B)).
lmsr_helper2([], B) -> 0;
lmsr_helper2([H|T], B) ->
    (math:pow(?E, H/B)) + lmsr_helper2(T, B).
    
lmsr_price(SharesSold, B, Trades) ->
    C0 = lmsr_helper(SharesSold, B),
    C1 = lmsr_helper(mapadd(SharesSold, Trades), B),
    (C1 - C0)*B.
mapadd([], []) -> [];
mapadd(_, _) -> ok.
dlmsr(N, SharesSold, B) ->
    X = element(N, list_to_tuple(SharesSold)),
    lmsr_helper([X], B)/lmsr_helper(SharesSold, B).
    
%derivative with respect to q1
%price1 = e^(q1/b) / (sum over qs e^(q/b))
			
test() ->
%easy example: start at 10 20, go to 30 15
    B = 30,
    Shares1 = [0, 0],
    Shares2 = [100, 0],
    [W, X] = Shares1,
    [Y, Z] = Shares2,
    Shares2 = [Y, Z],
    {B*(lmsr_helper(Shares2, B)-
	    lmsr_helper(Shares1, B)),
     dlmsr(1, Shares1, B),
     dlmsr(2, Shares1, B),
     dlmsr(1, Shares2, B),
     dlmsr(2, Shares2, B),
     (dlmsr(1, Shares1, B)+ 
     dlmsr(1, Shares2, B))*(Y-W)/2,
     (dlmsr(2, Shares1, B) + 
     dlmsr(2, Shares2, B))*(Z-X)/2
    }.

%d/dx e^x = x*e^x
