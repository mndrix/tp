:- module(merge3, [merge/4]).
:- use_module(library(lcs),[lcs/3]).


/*
%% ok(+Left,+Base,+Right,-Output) is semidet.
%
% True if a Left and Right element, originating from Base should produce
% Output during a 3-way merge.  Fails for a conflict.  It can be helpful
% to use library(maybe) to handle missing values.
ok(A, A, A, A).     % no changes in any branch
ok(A, B, A, A) :-   % left and right branches made the same change
    dif(A, B).
ok(B, A, A, B) :-   % only left branch made a change
    dif(B, A).
ok(A, A, B, A) :-   % only right branch made a change
    dif(A, B).
*/

% merge/4 doesn't work yet
%% merge(+Left,+Base,+Right,-Merged) is det.
%
% Perform a 3-way merge between Left and Right with Base
% as the common ancestor.  Merged is unified with the
% results of the merge.
%
% Within Merge, elements of Left and Right which merged successfully
% are wrapped in ok/1 while conflicts are wrapped in conflict/3.
% For example:
%
%   ?- merge([left,x],[base,x],[right,x],Merged).
%   Merged = [conflict(left,base,right),ok(x)].
merge(Left,Base,Right,Merged) :-
    lcs(Left,Base,LeftLcs),
    lcs(Right,Base,RightLcs),
    merge_(LeftLcs,Left,Base,RightLcs,Right,Merged).

/*
merge_(LeftLcs,Left,Base,RightLcs,Right,[Merge|Merged]) :-
    maplist(maybe_list,[LL,L,B,RL,R],[LeftLcs,Left,Base,RightLcs,Right]),
    ( L=LL, L=B, B=R, R=RL -> % no changes in any branch
        Merge=
    ),
*/
merge_(                        % no changes in any branch
    [X|LeftLcs], [X|Left],
    [X|Base],
    [X|RightLcs], [X|Right],
    [ok(X)|Merged]
) :-
    merge_(LeftLcs,Left,Base,RightLcs,Right,Merged).
merge_(                        % only left branch made a change
    [L|LeftLcs], [Y|Left],
    [X|Base],
    [X|RightLcs], [X|Right],
    [ok(Y)|Merged]
) :-
    \+ (X=L,L=Y),
    merge_([L|LeftLcs],Left,[X|Base],[X|RightLcs],[X|Right],Merged).
merge_(                        % only right branch made a change
    [X|LeftLcs], [X|Left],
    [X|Base],
    [R|RightLcs], [Y|Right],
    [ok(Y)|Merged]
) :-
    \+ (X=R,R=Y),
    merge_([X|LeftLcs],[X|Left],[X|Base],[R|RightLcs],Right,Merged).
merge_(                        % left and right branches made the same change
    [L|LeftLcs], [X|Left],
    [B|Base],
    [R|RightLcs], [X|Right],
    [ok(X)|Merged]
) :-
    \+ (L=B,B=R,B=X),
    merge_([L|LeftLcs],Left,[B|Base],[R|RightLcs],Right,Merged).
merge_(                        % branches disagree
    [L|LeftLcs], [X|Left],
    [B|Base],
    [R|RightLcs], [Y|Right],
    [conflict(X,B,Y)|Merged]
) :-
    \+ (L=B,B=R,B=X,B=Y),
    merge_([L|LeftLcs],Left,Base,[R|RightLcs],Right,Merged).
