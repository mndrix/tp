:- module(tp,[main/1]).

:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(process), [process_create/3]).
:- use_module(library(random),[random_between/3]).
:- use_module(library(unix),[exec/1]).
:- use_module(library(readutil), [read_line_to_codes/2]).

:- use_module('3solve.pl', []).

% syntactic sugar for describing rebase lists
:- op(990,yf,(...)).

main([]) :-
    main(help,[]).
main([Command|Args]) :-
    once( main(Command,Args)
        ; main(help,[])
        ).

main(help,_) :-
    writeln("Commands:"),
    nl,
    writeln(" di       show unsaved changes"),
    writeln(" discard  throw away unsaved changes"),
    writeln(" lg       view local patches"),
    writeln(" float    move a patch to the stack's top"),
    writeln(" push     publish patches"),
    writeln(" rebase   interactive rebase"),
    writeln(" reword   edit a patch message"),
    writeln(" rm       throw away a patch"),
    writeln(" save     store current changes in a patch"),
    writeln(" sink     move a patch to the stack's bottom"),
    writeln(" st       summarize unsaved changes"),
    writeln(" unsave   convert a patch into unsaved worktree changes"),
    writeln(" up       update patches based on upstream changes").
main(di,[]) :-
    inside_git_repository,
    exec(git(diff,'--diff-algorithm=histogram','--color=auto','--find-renames','--find-copies','--no-prefix')).
main(discard,[]) :-
    inside_git_repository,
    exec(git(checkout,'-p')).
main(lg,[]) :-
    inside_git_repository,
    exec(git(log,'--abbrev-commit','--oneline','origin/master..')).
main(float,[TargetName]) :-
    inside_git_repository,
    patch_name_id(TargetName, Target),
    rebase(
        Target,
        [pick-Target, Rest..., pick-Index, pick-Worktree],
        [Rest..., pick-Target, pick-Index, pick-Worktree]
    ).
main(push,[]) :-
    inside_git_repository,
    git_current_branch(Branch),
    exec(git(push,'--tags',origin,Branch)).
main(rebase,[]) :-
    exec(git(rebase,'--interactive','--autostash','origin/master')).
main(reword,[]) :-
    main(reword,['HEAD']).
main(reword,[PatchName]) :-
    inside_git_repository,
    patch_name_id(PatchName,Target),
    Edit = shell("git commit --amend"),
    rebase(
        Target,
        [pick-Target, Rest...],
        [edit(Edit)-Target, Rest...]
    ).
main(rm,[]) :-
    !,
    main(rm,['HEAD']).
main(rm,[TargetName]) :-
    inside_git_repository,
    patch_name_id(TargetName,Target),
    rebase(
        Target,
        [ pick-Target, Others... ],
        [ Others... ]
    ).
main(save,[to]) :-
    !,
    inside_git_repository,
    prompt_for_each_hunk,
    exec(git(commit,'--amend','--no-edit')).
main(save,['to', PatchName]) :-
    !,
    inside_git_repository,
    patch_name_id(PatchName, Target),
    prompt_for_each_hunk,
    temp_commit("save to ~s", [PatchName]),
    rebase(
        Target,
        [ pick-Target, Others..., pick-Changes, pick-Index, pick-Worktree ],
        [ pick-Target, fixup-Changes, Others..., pick-Index, pick-Worktree ]
    ).
main(save,[]) :-
    inside_git_repository,
    prompt_for_each_hunk,
    exec(git(commit,'-v')).
main(sink,[TargetName]) :-
    inside_git_repository,
    patch_name_id(TargetName, Target),
    rebase(
        child(origin/master),
        [A..., pick-Target, Z...],
        [pick-Target, A..., Z...]
    ).
main(st,[]) :-
    inside_git_repository,
    exec(git(status,'--short','--untracked-files')).
main(unsave,[]) :-
    !,
    % can't just "git reset HEAD^" because that changes the index
    main(unsave,['HEAD']).
main(unsave,[TargetName]) :-
    inside_git_repository,
    patch_name_id(TargetName,Target),
    rebase(
        Target,
        [ pick-Target, Others..., pick-Index, pick-Worktree ],
        [ Others..., pick-Index, pick-Worktree, fixup-Target ]
    ).
main(up,['-r']) :-  % recursively fetch all repositories
    !,
    shell("find . -name .git -type d | xargs -n1 -P3 -I% git --git-dir=% --work-tree=%/.. remote update -p").
main(up,[]) :-
    inside_git_repository,
    shell("git remote update -p origin"),
    Upstream = 'origin/master',
    patch_name_id(Upstream,Origin),
    shell_line(git,['merge-base',Upstream,'HEAD'], MergeBase),
    ( Origin=MergeBase ->
        true % no rebase necessary
    ; otherwise ->
        rebase(
            child(MergeBase),
            Upstream,
            [A...],
            [A...]
        )
    ).



% present each hunk to the user, asking whether it
prompt_for_each_hunk :-
    prompt_for_untracked_files,
    shell("git add -p").


% create a temporary commit based on the current index
temp_commit(Format, Args) :-
    format(string(Msg), Format, Args),
    shellf("git commit -m 'temp commit: ~s'", [Msg]).


%% stash
%
%  Stash away changes to the index and work tree so that they
%  can be restored later.  This is similar in spirit to "git stash"
%  but uses commits directly on the current branch.
%
%  This approach has the following benefits:
%
%    * stashed changes behave identically to user's other changes
%    * easier for users to recover from errors (data in reflogs)
%    * facilitates merge conflict resolution
%
%  See also unstash/0
stash :-
    shell("git commit --allow-empty -m 'tp stash: unsaved index'"),
    shell("git commit --all --allow-empty -m 'tp stash: unsaved work tree'").

%% unstash
%
%  Restores changes saved with stash/0.
unstash :-
    shell("git reset HEAD^"),
    shell("git reset --soft HEAD^").


%% with_clean_tree(+Goal)
%
%  Runs goal with a clean Git working tree.  Any changes in
%  the tree are stashed and restored, regardless what happens
%  in Goal.
:- meta_predicate with_clean_tree(0).
with_clean_tree(Goal) :-
    setup_call_cleanup(stash, Goal, unstash).


%% on_temp_branch(+Basis,+Goal)
%
%  Perform Goal on a temporary branch based on Basis.
%  When finished, swap the temporary branch into place
:- meta_predicate on_temp_branch(+,0).
on_temp_branch(Basis,Goal) :-
    setup_call_catcher_cleanup(
        on_temp_branch_setup(Basis,Original,Tmp),
        Goal,
        exit,
        on_temp_branch_cleanup(Original,Tmp)
    ).

on_temp_branch_setup(Basis,Original,Tmp) :-
    git_current_branch(Original),
    random_between(1,99999999,X),
    format(string(Tmp),"tp-tmp-~d", [X]),
    shellf("git checkout -b ~s ~p", [Tmp,Basis]).

on_temp_branch_cleanup(Original,Tmp) :-
    shellf("git checkout ~s", [Original]),
    shellf("git reset --hard ~s", [Tmp]),
    shellf("git branch -D ~s", [Tmp]).


inside_git_repository :-
    once(git_dir(_); throw(not_in_git_repo)).

git_dir(".git") :-
    exists_directory(".git"),
    !.
git_dir(Dir) :-
    shell_line(git, ['rev-parse', '--git-dir'], Dir).


patch_name_id(Name,Id) :-
    shell_line(git, ['rev-parse',Name], Id).


git_changed_files(Files) :-
    shell_lines(git,[diff, '--name-only'], Files0),
    maplist(string_codes,Files1,Files0),
    sort(Files1,Files).  % remove duplicates


git_current_branch(Branch) :-
    shell_line(git, ['rev-parse','--abbrev-ref','HEAD'], Branch).


% like shell/1 but supports format/1 patterns
shellf(Pattern,Args) :-
    shellf(Pattern,Args,0).


% like shell/2 but supports format/1 patterns
shellf(Pattern,Args,Status) :-
    format(string(Command),Pattern,Args),
    shell(Command,Status).


% prompt the user for each untracked file, asking whether he wants
% to start tracking it.
prompt_for_untracked_files :-
    untracked_files(Files),
    prompt_for_untracked_files(Files).

prompt_for_untracked_files([]).
prompt_for_untracked_files([File|Files]) :-
    format("~s~n", [File]),
    ansi_format([bold,fg(blue)],"Track this file [y,n,i,d]? ",[]),
    get_single_char(Answer),
    ( Answer=0'y ->
        shellf("git add --intent-to-add -- '~s'", [File])
    ; Answer=0'i ->
        shellf("echo ~s >> .gitignore", [File]),
        shell("git add .gitignore")
    ; Answer=0'd ->
        shellf("rm -rf ~s", [File])
    ; otherwise ->
        true  % ignore invalid selection
    ),
    format("~n~n",[]),
    prompt_for_untracked_files(Files).

untracked_files(Files) :-
    shell_lines(git,['ls-files','--others','--exclude-standard'], Files).


shell_line(Command,Args,Line) :-
    shell_lines(Command,Args,[LineCodes]),
    string_codes(Line,LineCodes).


shell_lines(Command,Args,Lines) :-
    process_create(
        path(Command),
        Args,
        [stdin(null),stdout(pipe(Out))]
    ),
    read_lines(Out,Lines).

read_lines(Out,Lines) :-
    read_line_to_codes(Out,Line),
    ( Line = end_of_file ->
        Lines = []
    ; otherwise ->
        Lines = [Line|Lines1],
        read_lines(Out,Lines1)
    ).


% rules for displaying Git commit references
:- multifile user:portray/1.
user:portray(parent(child(Id))) :-
    !,
    once( portray(Id); write(Id) ).
user:portray(parent(Id)) :-
    once( portray(Id); write(Id) ),
    write("^").
user:portray(child(Parent)) :-
    format(string(Range),"~w..", [Parent]),
    shell_lines(git,[log,'--pretty=format:%H','--reverse',Range],Lines),
    ( Lines = [Child|_] ->
        format('~s',[Child])
    ; otherwise ->
        throw(commit_has_no_child(Parent))
    ).


%% rebase(+First,+Pattern0,+Pattern)
%
%  This is like 'git rebase -i' but editing the todo list with
%  pattern matching rather than a text editor. First is the first
%  local commit to rebase.  Pattern0 describes actions that would appear in a
%  text editor.  Pattern describes a modified list of actions that is
%  executed.
rebase(First,Pattern0,Pattern) :-
    rebase(First,parent(First),Pattern0,Pattern).

rebase(First,Onto,Pattern0,Pattern) :-
    with_clean_tree(rebase_(First,Onto,Pattern0,Pattern)).

rebase_(First,Onto,Pattern0,Pattern) :-
    rebase_todo(First,Actions0),
    once(phrase(rebase_pattern(Pattern0),Actions0)),
    once(phrase(rebase_pattern(Pattern),Actions)),
    on_temp_branch(Onto,rebase_execute(Actions)).


%% rebase_todo(+Target, -Actions:list(pair))
%
% create a rebase action list with "pick Target" as the first action
rebase_todo(Target,Actions) :-
    format(string(Range),"~p..", [parent(Target)]),
    shell_lines(git,[log,'--pretty=format:%H','--reverse',Range],CommitsCodes),
    maplist(rebase_todo_,CommitsCodes,Actions).

rebase_todo_(IdCodes,pick-Id) :-
    string_codes(Id,IdCodes).


% match a rebase pattern against a rebase todo list
rebase_pattern([]) -->
    [].
rebase_pattern([X...|Actions]) -->
    any(X),
    rebase_pattern(Actions).
rebase_pattern([Action|Actions]) -->
    [Action],
    rebase_pattern(Actions).

any([]) -->
    [].
any([X|Xs]) -->
    [X],
    any(Xs).

% execute a series of rebase operations against the current worktree
rebase_execute([]).
rebase_execute([Action-PatchId|Todo]) :-
    rebase_action(Action,PatchId),
    rebase_execute(Todo).

% perform a single step of a rebase
rebase_action(edit(Goal),PatchId) :-
    rebase_action(pick,PatchId),
    call(Goal).
rebase_action(fixup,PatchId) :-
    shellf("git cherry-pick --no-commit ~s", [PatchId]),
    shell("git commit --amend --no-edit").
rebase_action(pick,PatchId) :-
    cherry_pick(PatchId).


cherry_pick(PatchId) :-
    shellf("git cherry-pick --allow-empty ~s", [PatchId], Status),
    cherry_pick_check_git_status(Status).

cherry_pick_check_git_status(0).
cherry_pick_check_git_status(1) :-
    resolve_conflicts(Status),
    cherry_pick_check_3solve_status(Status).

cherry_pick_check_3solve_status(resolved) :-
    Name = 'GIT_EDITOR',

    % which editor does the user prefer?
    ( getenv(Name,Val) ->
        OldValue=just(Val)
    ; otherwise ->
        OldValue=nothing
    ),

    % commit with a noop editor, then restore user's real preference
    setup_call_cleanup(
        setenv(Name,'/usr/bin/true'),  % noop editor
        shell("git cherry-pick --continue"),
        (OldValue=just(Val) -> setenv(Name,Val); unsetenv(Name) )
    ).
cherry_pick_check_3solve_status(conflicted) :-
    % TODO suspend our process
    % TODO let user resolve conflict
    % TODO user does 'fg' to resume
    % TODO verify all files are resolved
    % TODO commit as with ResolveStatus=resolved
    fail.


resolve_conflicts(Status) :-
    git_changed_files(Files),
    resolve_conflicts(Files,Status).

resolve_conflicts([],resolved).
resolve_conflicts([File|Files],FinalStatus) :-
    tmp_file('tp-',Tmp),
    threesolve:resolve_file(File, Tmp, ResolveStatus),
    rename_file(Tmp,File),
    ( ResolveStatus=resolved ->
        format(user_error,"good - 3solve resolved ~s~n",[File]),
        shellf("git add ~s", [File]),
        resolve_conflicts(Files,FinalStatus)
    ; ResolveStatus=conflicted ->
        format(user_error,"bad - 3solve failed on ~s~n",[File]),
        FinalStatus=conflicted,
        resolve_conflicts(Files,_)
    ).
