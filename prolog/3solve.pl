:- use_module(library(readutil),[read_file_to_codes/3]).

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
    conflict(LeftLabel,LeftCodes,OriginLabel,OriginCodes,RightLabel,RightCodes),
    conflict(LeftLabel,LeftCodes,OriginLabel,OriginCodes,RightLabel,RightCodes)
) :-
    true.
