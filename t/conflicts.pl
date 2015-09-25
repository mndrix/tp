#!/usr/bin/env swipl -q -t main -f
:- use_module('prolog/3solve.pl').

:- use_module(library(tap)).

'adjacent-lines.go' :-
    resolve_file('conflicts/adjacent-lines.go/conflicted', 'conflicts/adjacent-lines.go/resolved.got', _),
    shell("diff -q conflicts/adjacent-lines.go/resolved conflicts/adjacent-lines.go/resolved.got >/dev/null",Status),
    Status == 0.

'multiple-conflicts.go' :-
    resolve_file('conflicts/multiple-conflicts.go/conflicted', 'conflicts/multiple-conflicts.go/resolved.got', _),
    shell("diff -q conflicts/multiple-conflicts.go/resolved conflicts/multiple-conflicts.go/resolved.got >/dev/null",Status),
    Status == 0.
