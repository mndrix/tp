:- module(diff, [diff/3,is_patch/1]).

:- use_module(library(lcs),[lcs/3]).

is_patch(add(_)).
is_patch(context(_)).
is_patch(delete(_)).

:- multifile user:portray/1.
user:portray([Patch|Patches]) :-
    maplist(is_patch,[Patch|Patches]),
    !,
    maplist(user:portray,[Patch|Patches]).
user:portray(add(X)) :-
    format("+~s~n", [X]).
user:portray(context(X)) :-
    format(" ~s~n", [X]).
user:portray(delete(X)) :-
    format("-~s~n", [X]).


%% diff(?Old:list,?New:list,?Diff:list(patch))
%
%  True if patches in Diff applied to the content in Old produces the content in
%  New.  This can be used to calculate the diff between two lists.  It can also
%  be used to apply (or unapply) a diff to some original (or subsequent)
%  content.
diff(Old,New,Diff) :-
    nonvar(Old),
    nonvar(New),
    !,
    lcs(Old,New,Lcs),
    diff_(Lcs,Old,New,Diff).
diff(Old,New,Diff) :-
    nonvar(Diff),
    ( nonvar(Old); nonvar(New) ),
    !,
    diff_(_Lcs,Old,New,Diff).

diff_([X|Lcs],[X|Old],[X|New],[context(X)|Diff]) :-
    diff_(Lcs,Old,New,Diff).
diff_(Lcs,[O|Old],New,[delete(O)|Diff]) :-
    dif_head(Lcs,[O|Old]),
    diff_(Lcs,Old,New,Diff).
diff_(Lcs,Old,[N|New],[add(N)|Diff]) :-
    dif_head(Lcs,[N|New]),
    diff_(Lcs,Old,New,Diff).
diff_([],[],[],[]).


%% dif_head(?A:list, ?B:list) is semidet.
%
%  True if A and B have different head elements.  An empty list is considered
%  to have a head which is differrent from all other elements.
dif_head([],[_|_]).
dif_head([X|_],[Y|_]) :-
    dif(X,Y).
%dif_head([_|_],[]).  % never called with empty 2nd argument
