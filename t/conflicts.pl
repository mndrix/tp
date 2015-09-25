#!/usr/bin/env swipl -q -t main -f
:- use_module('prolog/3solve.pl').

term_expansion(generate_tests, Tests) :-
    expand_file_name('conflicts/*',Dirs),
    maplist(generate_test,Dirs,Tests).

generate_test(Dir,Test) :-
    atom_concat('conflicts/',Name,Dir),
    format(atom(Conflicted),'~s/conflicted',[Dir]),
    format(atom(Resolved),'~s/resolved',[Dir]),
    format(atom(ResolvedGot),'~s/resolved.got',[Dir]),
    format(atom(Diff),"diff -q ~s ~s >/dev/null",[Resolved,ResolvedGot]),
    Test = (
        Name :-
            resolve_file(Conflicted,ResolvedGot,_),
            shell(Diff,Status),
            Status == 0
    ),
    tap:register_test(Name).

:- use_module(library(tap)).

generate_tests.
