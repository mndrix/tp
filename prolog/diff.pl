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


dif_head([],[_|_]).
dif_head([X|_],[Y|_]) :-
    dif(X,Y).
%dif_head([_|_],[]).  % never called with empty 2nd argument
