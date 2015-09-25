:- use_module(library(clpfd)).
:- use_module(library(list_util), [split/3]).
:- use_module(library(readutil),[read_file_to_codes/3]).

:- use_module(patch,[]).

main([File]) :-
    read_file_to_codes(File,Text,[]),
    phrase(file(Conflicted),Text),
    maplist(resolve,Conflicted,Resolved),
    maplist(portray,Resolved),
    ( has_a_conflict(Resolved) -> halt(1); halt(0) ).

file([Chunk|Chunks]) -->
    chunk(Chunk),
    file(Chunks).
file([]) -->
    [].

:- multifile portray/1.
portray(conflict(LeftLabel,LeftCodes,OriginLabel,OriginCodes,RightLabel,RightCodes)) :-
    format("<<<<<<< ~s~s", [LeftLabel,LeftCodes]),
    format("||||||| ~s~s", [OriginLabel,OriginCodes]),
    format("=======~n~s",[RightCodes]),
    format(">>>>>>> ~s", [RightLabel]).
portray(agree(Text)) :-
    write(Text).


has_a_conflict(Hunks) :-
    memberchk(conflict(_,_,_,_,_,_),Hunks).


chunk(conflict(LeftLabel,LeftCodes,OriginLabel,OriginCodes,RightLabel,RightCodes)) -->
    start_conflict_left(LeftLabel),
    agreement_lines(LeftCodes),
    start_conflict_origin(OriginLabel),
    agreement_lines(OriginCodes),
    start_conflict_right,
    agreement_lines(RightCodes),
    end_conflict(RightLabel).
chunk(agree(Text)) -->
    { dif(Codes,[]) },
    agreement_lines(Codes),
    { string_codes(Text,Codes) }.


agreement_lines(Codes) -->
    agreement_lines(Codes,[]).

agreement_lines([],[]) -->
    \+ [_].
agreement_lines([],[]) -->
    \+ \+ conflict_marker.
agreement_lines([C|Cs],Tail) -->
    \+ conflict_marker,
    [C],
    ( { C = 0'\n } ->
        agreement_lines(Cs,Tail)
    ; [] ->
        rest_of_line(Cs,Tail0),
        agreement_lines(Tail0,Tail)
    ).


conflict_marker -->
    start_conflict_left(_).
conflict_marker -->
    start_conflict_origin(_).
conflict_marker -->
    start_conflict_right.
conflict_marker -->
    end_conflict(_).


nl -->
    nl(_,_).


nl([LineFeed|Tail],Tail) -->
    { LineFeed = 0'\n },
    [LineFeed].
nl([CarriageReturn,LineFeed|Tail],Tail) -->
    { CarriageReturn = 0'\r },
    { LineFeed = 0'\n },
    [CarriageReturn,LineFeed].


end_of_line -->
    nl.
end_of_line -->
    end_of_stream.


end_of_stream([],[]).


start_conflict_left(Label) -->
    "<<<<<<<",
    conflict_label(Label).


start_conflict_origin(Label) -->
    "|||||||",
    conflict_label(Label).


start_conflict_right -->
    "=======",
    nl.


end_conflict(Label) -->
    ">>>>>>>",
    conflict_label(Label).


conflict_label("") -->
    end_of_line.
conflict_label(Label) -->
    " ",
    rest_of_line(Codes, []),
    { string_codes(Label,Codes) }.


%% rest_of_line(?Line:codes, ?Tail:codes)
%
% Captures the rest of a line, including the end of line marker.
rest_of_line(Tail,Tail) -->
    end_of_stream.
rest_of_line(Tail0,Tail) -->
    nl(Tail0,Tail).
rest_of_line([C|Cs],Tail) -->
    \+ end_of_line,
    [C],
    rest_of_line(Cs,Tail).


resolve(agree(Text),agree(Text)).
resolve(
    conflict(_,LeftCodes,_,OriginCodes,_,RightCodes),
    agree(MergedText)
) :-
    % split conflicted content into lines
    split(OriginCodes,0'\n,OriginLines),
    split(LeftCodes,0'\n,LeftLines),
    split(RightCodes,0'\n,RightLines),

    % calculate patches which caused each side to diverge from origin
    once(diff(OriginLines,LeftLines,LeftPatches)),
    once(diff(OriginLines,RightLines,RightPatches)),

    % do patch algebra merge between the two diff lists
    ( merge(LeftPatches,RightPatches,MergedPatches) ->
        patch:canonical_order(MergedPatches,CanonicalPatches),
        reverse(CanonicalPatches,ExecutionOrder),
        patch:diff(OriginLines,MergedLines,ExecutionOrder),
        split(MergedCodes,0'\n,MergedLines),
        string_codes(MergedText,MergedCodes)
    ; otherwise ->
        throw(could_not_merge_patches(LeftPatches,RightPatches))
    ).


diff(Base,Head,GitOrder) :-
    patch:diff(Base,Head,ExecutionOrder),
    reverse(ExecutionOrder,GitOrder).

% do patch algebra to merge patches together
merge(Left,Right,Merged) :-
    maplist(patch:inverse,Right,RightInv),
    reverse(RightInv,RightInvReversed),
    append(RightInvReversed,Right,RightNoop),
    append(Left,RightNoop,Merged0),

    % remove the inverse patches by commuting them off the tip
    length(Right,RightSize),
    length(Left,LeftSize),
    remove_inverse_patches(RightSize,LeftSize,Merged0,Merged).

% commute inverse patches to the tip then drop them
remove_inverse_patches(0,_,Merged,Merged).
remove_inverse_patches(N0,LeftSize,Merged0,Merged) :-
    InversePatchIndex is LeftSize + 1,
    patch:float(InversePatchIndex,Merged0,[_|Merged1]),
    N is N0 - 1,
    remove_inverse_patches(N,LeftSize,Merged1,Merged).
