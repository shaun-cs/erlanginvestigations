-module(recursion).
-export([tail_func/1,zip/2]).

tail_func(N) -> tail_func(N,1).

tail_func(0,Acc) -> Acc;
tail_func(N,Acc) when N > 0 -> tail_func(N-1,Acc*N).

zip(L,R) -> zip(L,R,[]).

zip([],[],A) -> lists:reverse(A);
zip([],_,A) -> zip([],[],A);
zip(_,[],A) -> zip([],[],A);
zip([L|Ls],[R|Rs],A) -> zip(Ls,Rs,[{L,R}|A]).
 
