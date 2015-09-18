main([File]) :-
    read_file_to_codes(File,Text,[]),
    phrase(file(Hunks),Text),
    writeq(Hunks),
    nl.

file([Chunk|Chunks]) -->
    chunk(Chunk),
    file(Chunks).
file([]) -->
    [].


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


eol -->
    nl.
eol -->
    eos.


eos([],[]).


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
    eol.
conflict_label(Label) -->
    " ",
    rest_of_line(Codes, []),
    { string_codes(Label,Codes) }.


%% rest_of_line(?Line:codes, ?Tail:codes)
%
% Captures the rest of a line, including the end of line marker.
rest_of_line(Tail,Tail) -->
    eos.
rest_of_line(Tail0,Tail) -->
    nl(Tail0,Tail).
rest_of_line([C|Cs],Tail) -->
    \+ eol,
    [C],
    rest_of_line(Cs,Tail).
