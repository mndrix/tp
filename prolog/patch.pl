:- module(patch, [commute/4,float/3,inverse/2,is_patch/1]).

:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(lcs),[]).

% uncomment when tracing through clpfd-heavy code
%:- initialization(set_prolog_flag(clpfd_goal_expansion, false)).

:- multifile error:has_type/2.
error:has_type(patch, Patch) :-
    is_patch(Patch).


:- multifile user:portray/1.
user:portray([Patch|Patches]) :-
    GitOrder = [Patch|Patches],
    maplist(is_patch,GitOrder),
    !,
    canonical_order(GitOrder,CanonicalOrder),
    reverse(CanonicalOrder,ExecutionOrder),
    maplist(user:portray,ExecutionOrder).
user:portray(add_line(N,X)) :-
    ground(N),
    ground(X),
    format("+~@ ~s~n", [portray_index(3,N),X]).
user:portray(rm_line(N,X)) :-
    ground(N),
    ground(X),
    format("-~@ ~s~n", [portray_index(3,N),X]).


portray_index(Width,N) :-
    format("~|~t~d~*+", [N,Width]).


%% is_patch(+Patch) is semidet.
%
%  True if Patch is a valid patch term.
%
%  When adding clauses to this predicate, please keep them sorted
%  alphabetically by functor name.
is_patch(add_line(N,Text)) :-
    error:positive_integer(N),
    error:is_of_type(codes,Text).
is_patch(rm_line(N,Text)) :-
    error:positive_integer(N),
    error:is_of_type(codes,Text).


%% line(?Patch:patch,?Line:positive_integer) is det.
%
%  True if Line is the line number whose content is changed by Patch.
line(add_line(N,_),N).
line(rm_line(N,_),N).


%% removes_content(+Patch:patch) is semidet.
%
%  True if Patch only removes content from the document to which it applies.
removes_content(rm_line(_,_)).


%% inverse(+A:patch, -Ainv:patch) is det.
%% inverse(-A:patch, +Ainv:patch) is det.
%
%  True if Ainv is the inverse of patch A.
inverse(A,Ainv) :-
    inverse_(A,Ainv),
    !.
inverse(A,Ainv) :-
    inverse_(Ainv,A).

% When adding clauses to this predicate, please keep them sorted
% alphabetically by functor name.  Likewise the functor of the first
% argument should lexicographically precede the functor of the second
% argument. This assures a consistent pattern and reduces the chances
% that we'll accidentally omit a case.
inverse_(add_line(N,Text),rm_line(N,Text)).


%% commute(+A0:patch, +B0:patch, -B1:patch, -A1:patch) is det.
%% commute(?A0:patch, ?B0:patch, +B1:patch, +A1:patch) is det.
%
%  True if the change induced by the patch series A0 then B0
%  is the same as the change induced by the patch series B1 then A1.
%
%  In the clauses below, we use the following naming conventions:
%
%    * M0, M, N0, N are indices (of a line, token, etc.)
%    * M0 is the initial index of the first patch
%    * N0 is the initial index of the second patch
%    * M is the final index of what started as the first patch
%    * N is the final index of what started as the second patch
%
%  When adding clauses to this predicate, please observe the same clause
%  sorting rules as inverse_/2.  That gives some confidence that we've covered
%  all cases systematically.
%
%  Use clpfd for arithmetic and numeric relations anywhere that's possible.
commute(add_line(M0,TextA),add_line(N0,TextB),add_line(N,TextB),add_line(M,TextA)) :-
    ( M0#<N0, M#=M0,   N#=N0-1
    ; M0#=N0, M#=M0+1, N#=N0
    ; M0#>N0, M#=M0+1, N#=N0
    ).
commute(add_line(M0,TextA),rm_line(N0,TextB),rm_line(N,TextB),add_line(M,TextA)) :-
    ( M0#<N0, M#=M0,   N#=N0-1
    % M0=N0 conflicts
    ; M0#>N0, M#=M0-1, N#=N0
    ).
commute(rm_line(M0,TextA),add_line(N0,TextB),add_line(N,TextB),rm_line(M,TextA)) :-
    ( M0#<N0, M#=M0,   N#=N0+1
    ; M0#=N0, M#=M0+1, N#=N0
    ; M0#>N0, M#=M0+1, N#=N0
    ).
commute(rm_line(M0,TextA),rm_line(N0,TextB),rm_line(N,TextB),rm_line(M,TextA)) :-
    ( M0#<N0, M#=M0,   N#=N0+1
    ; M0#=N0, M#=M0,   N#=N0+1
    ; M0#>N0, M#=M0-1, N#=N0
    ).

/*
Note_1:

With add_line/2 and rm_line/2 patches, we have 4 clauses and 11 disjunctions.
I'd like to generalize the rules so that they can embrace other, similar
patches. That would save us the trouble of writing quadratically many clauses as
the number of patches increases.

In the above clauses, it seems like it would be simpler to describe the one
patch combination that results in a conflict like:

    commute_conflict(add_line(N,_),rm_line(N,_)).

Followed by some rules that cover all other index manipulations. These
candidate rules seem to fit the bill:

    * if the first patch is deeper than the second, apply the "impact"
      of the second patch to the first (leave second patch alone)
    * if the first patch is not deeper than the first, undo the "impact"
      of the first patch from the second (leave first patch alone)

In the preceding rules, "impact" means the change a patch exerts on the line
indices of patches "deeper" in the file than it is.  For example an add_line/2
patch increases indices by 1; an rm_line/2 patch decreases indices by 1.

In the preceding paragraph, "deeper" means either `>` or `>=` depending on the
patch type.  For example:

    deeper(add_line(M,_),FollowingPatch) :-
        line(FollowingPatch,N),
        M #>= N.
    deeper(rm_line(M,_),FollowingPatch) :-
        line(FollowingPatch,N),
        M #> N.

I'm hesitant to refactor commute/4 as described until I have a couple more patch
types and know whether the rules carry over.  It could be that a quadratic
listing  of all combinations is the most succinct way to go (or just the easiest
to verify).

*/


%% float(+N:integer,+Patches0:list(patch),-Patches:list(patch))
%
%  True if the patch in position N of Patches0 can be commuted to the head to
%  form Patches.
float(1,Patches,Patches).
float(N0,[PatchA0|Patches0],[PatchB1,PatchA1|Patches]) :-
    N0 #> 1,
    N #= N0 - 1,
    float(N,Patches0,[PatchB0|Patches]),
    commute(PatchB0,PatchA0,PatchA1,PatchB1).


%% less(+A:patch,+B:patch) is semidet.
%
%   True if patch A should appear before patch B in a standard diff output.
%   Namely, smaller line numbers appear before larger ones.  On the same line,
%   content removal comes first.
less(A,B) :-
    line(A,LineA),
    line(B,LineB),
    LineA #< LineB.
less(A,B) :-
    line(A,Line),
    line(B,Line),
    removes_content(A).


%% canonical_order(+Patches0:list(patch), -Patches:list(patch)) is det.
%
%  True if Patches0 sorts into the canonical order given by Patches. This
%  predicate only sorts as much as possible.  Sometimes two patches want to
%  commute but can't.  In that case, they only migrate as close to their ideal
%  position as possible.
%
%  Patches0 and Patches are both in git order.  Any patches which cancel out one
%  another will be dropped.
canonical_order(Patches0,Patches) :-  % See Note_sort
    bubble(Patches0,Patches1),
    ( Patches0 = Patches1 ->
        % bubbling patches made no progress. we're done
        Patches = Patches1
    ; otherwise ->
        canonical_order(Patches1,Patches)
    ).

% bubble(A,B) is det.
bubble([],[]).
bubble([X],[X]) :-
    !.  % optimization. nothing below matches a single element list
bubble([X0,Y0|Rest0],Rest) :-
    inverse(Y0,X0),  % patches cancel out
    !,               % force "\+ inverse(Y0,X0)" below
    bubble(Rest0,Rest).
bubble([X0,Y0|Rest0],[X0|Rest]) :-
    less(Y0,X0),     % already proper order, leave it alone
    !,
    bubble([Y0|Rest0],Rest).
bubble([X0,Y0|Rest0],[Y1|Rest]) :-
    commute(Y0,X0,X1,Y1), % always succeeds since "\+ inverse(Y0,X0)"
    less(X1,Y1),          % commute made things more sorted
    !,                    % force "\+ less(X1,Y1)" below
    bubble([X1|Rest0],Rest).
bubble([X0,Y0|Rest0],[X0|Rest]) :-
    bubble([Y0|Rest0],Rest).

/*
Note_sort:

The sort algorithm used in canonical_order/2 must only swap adjacent elements in
the list. Patch commute is the only operation we have for changing position of
two patches and it only operates on adjacent patches.

This requirement forces us to use something like bubble sort.  Fortunately,
patches are usually close to sorted when we start so we rarely encounter the
full O(N^2) run time.
*/


%% diff(?Old:list,?New:list,?Diff:list(patch))
%
%  True if patches in Diff applied to the content in Old produces the content in
%  New.  This can be used to calculate the diff between two lists.  It can also
%  be used to apply (or unapply) a diff to some original (or subsequent)
%  content.
%
%  This is similar to diff:diff/3 but omits only generates patches for which
%  patch:is_patch/1 is true.
diff(Old,New,Diff) :-
    nonvar(Old),
    nonvar(New),
    !,
    lcs:lcs(Old,New,Lcs),
    diff_(Lcs,Old,New,1,Diff).
diff(Old,New,Diff) :-
    nonvar(Diff),
    ( nonvar(Old); nonvar(New) ),
    !,
    diff_(_Lcs,Old,New,1,Diff).

diff_([X|Lcs],[X|Old],[X|New],I0,Diff) :-
    I #= I0 + 1,
    diff_(Lcs,Old,New,I,Diff).
diff_(Lcs,[O|Old],New,I,[rm_line(I,O)|Diff]) :-
    dif_head(Lcs,[O|Old]),
    diff_(Lcs,Old,New,I,Diff).
diff_(Lcs,Old,[N|New],I0,[add_line(I0,N)|Diff]) :-
    I #= I0 + 1,
    dif_head(Lcs,[N|New]),
    diff_(Lcs,Old,New,I,Diff).
diff_([],[],[],_I,[]).

%% dif_head(?A:list, ?B:list) is semidet.
%
%  True if A and B have different head elements.  An empty list is considered
%  to have a head which is differrent from all other elements.
dif_head([],[_|_]).
dif_head([X|_],[Y|_]) :-
    dif(X,Y).
%dif_head([_|_],[]).  % never called with empty 2nd argument
