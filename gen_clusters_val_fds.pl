% Purpose: Utilities required for generation of sequences of Conditional Functional Dependencies and Prefix Trees of CFDs
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_clusters_val_fds,[gen_clusters_val_fds/5,
                                collect_cluster_zero/2,
                                build_tree_wrt_common_prefix/2]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(utility).
:- use_module(bool_cond_utility).

gen_clusters_val_fds(Table, Arity, DistinctVal, TwoLevelsGrouping, ValFds) :-
    gen_model1(Table, Arity, DistinctVal, TwoLevelsGrouping, NoFDSet, Sols),
    (NoFDSet = 1 ->
         group_fd_with_val1(Sols, SolsFdVal1),
         sort(SolsFdVal1, ValFds)
    ;
         ValFds = []
    ).

build_tree_wrt_common_prefix([], []) :- !.
build_tree_wrt_common_prefix(ValFds, [Node|RNodes]) :-
    ValFds = [[Fd-Val1|_]|_],
    split_wrt_common_prefix(ValFds, Fd-Val1, SuffixOfCommonPrefix, Rest),
    Node = t(Fd,Val1,_,Childs),
    build_tree_wrt_common_prefix(SuffixOfCommonPrefix, Childs),
    build_tree_wrt_common_prefix(Rest, RNodes).

split_wrt_common_prefix([[[]-Val1]|Rest], []-Val1, [], Rest) :- !.
split_wrt_common_prefix([[Fd-Val1|R]|S], Fd-Val1, [R|T], Rest) :-
    !,
    split_wrt_common_prefix(S, Fd-Val1, T, Rest).
split_wrt_common_prefix(Rest, _, [], Rest).

group_fd_with_val1([], []) :- !.
group_fd_with_val1([Sol|R], [SolFdVal1|S]) :-
    arg(1, Sol, Fds),
    arg(2, Sol, Vals1),
    group_fd_with_val(Fds, Vals1, SolFdVal1),
    group_fd_with_val1(R, S).

group_fd_with_val([], [Val1], [[]-Val1]) :- !.
group_fd_with_val([Fd|R], [Val1|S], [Fd-Val1|T]) :-
    group_fd_with_val(R, S, T).

collect_cluster_zero([],       []       ) :- !.
collect_cluster_zero([t(_,  Layer, _, Branch)|_], [Layer|R]) :-
    collect_cluster_zero(Branch, R).

gen_model1(Table, Arity, DistinctVal, TwoLevelsGrouping, NoFDSet, Sols) :-

    % declare the value variables associated to each line of the case statement (1) and set up the domain of the set of values of all clusters
    %.........................................................................................................................................
    length(DistinctVal, NVal),
    length(ValueVars1, NVal),
    list_to_fdset(DistinctVal, FDSet),
    declare_domains1(ValueVars1, FDSet),
    all_distinct(ValueVars1),    % all value variables should be distinct

    % declare the value variables associated to each line of the case statement (0) and set up the domain of the set of values of all clusters
    %.........................................................................................................................................
    NVal1 is NVal-1,
    declare_domains0(NVal1, ListValueVars0, FDSet),

    % declare the functional dependency variables associated to each line of the case statement and set up their initial domain
    %..........................................................................................................................
    length(FdVars, NVal1),
    declare_domains_fdvars(FdVars, NVal1, TwoLevelsGrouping, NoFDSet),
    (NoFDSet = 1 ->
    % for each layer from 1 to NVal-1 link the variables ValueVars1, ValueVars0 and FdVar wrt the conditional functional dependencies
    %................................................................................................................................
         gen_table_ctr(NVal1, ValueVars1, ListValueVars0, FdVars, TwoLevelsGrouping),

    % for all i in [1,NVal-1], for all j in [1,i-1]: ValueVar1[i] neq ValueVar0[i+1,j]
    % (the value ValueVar1[i] at level i should be completely discarded from the values considered at all next levels)
    %.................................................................................................................
         gen_incompatible_value_ctrs(1, NVal1, ValueVars1, ListValueVars0),

    % for each attribute create a 0-1 variable that will be set to 1 if this attribute is used by at least one functional dependency
    %...............................................................................................................................
         length(BoolAttrs, Arity),
         domain(BoolAttrs, 0, 1),
         reverse(FdVars, RevFdVars),
         link_used_attrs_to_used_fds(BoolAttrs, 1, RevFdVars, TwoLevelsGrouping),

    % compute solutions
    %..................
         append(ValueVars1, FdVars, AllVars),   % list of decision variables passed to the labeling
         gen_sum_term(BoolAttrs, var, SumTerm), % sum of the Boolean variables telling whether a attribute is used or not
         Sum in 1..Arity,                       % Sum will be the total number of attributes used among the different levels
         call(Sum #= SumTerm),                  % get the minimum number of attributes that has to be used
         get_min_nb_of_attributes_for_a_solution(Sum, AllVars, MinNbAttr),
         Sum #= MinNbAttr,                      % fix Sum to the minimum number of attributes that has to be used
         findall(t(SelectedFds, ValueVars1),
                 (labeling([], AllVars),        % search all solutions that use the minimum number of attributes
                  get_selected_fd(FdVars, NVal1, Table, TwoLevelsGrouping, SelectedFds)),
                 Sols)
    ;
         true
    ).

get_min_nb_of_attributes_for_a_solution(MinNbAttr, AllVars, _) :-
    once(labeling([], [MinNbAttr|AllVars])),
    assert(min_nb_of_attributes_for_a_solution(MinNbAttr)),
    false.
get_min_nb_of_attributes_for_a_solution(_, _, MinNbAttr) :-
    min_nb_of_attributes_for_a_solution(MinNbAttr),
    retractall(min_nb_of_attributes_for_a_solution(_)).

declare_domains1([], _) :- !.
declare_domains1([V|R], FDSet) :-
    V in_set FDSet,
    declare_domains1(R, FDSet).

declare_domains0(0, [], _) :- !.
declare_domains0(I, [ValueVars0|R], FDSet) :-
    I > 0,
    !,
    length(ValueVars0, I),
    declare_domains1(ValueVars0, FDSet),
    I1 is I-1,
    declare_domains0(I1, R, FDSet).

% Generates the initial domains of the functional dependency variables:
%  . [FdVar|R]   : list of functional dependency variables telling for each level (except the last one) which functional dependency will be selected;
%                  FdVar correspond to the first level, i.e. the level of the first condition of the case statement.
%  . CardSet0    : number of complementary values (i.e. corresponding to a 0) of the current cluster (ValueVar corresponds to a 1).
%  . FDSet       : meta-data describing the conditional functional dependencies we can use for the different clusters.
%  . NoFDSet     : flag denoting whether or not there are FDs on each level.
%                  NoFDSet = 0 means there are no FDs on one of the levels
%                  NoFDSet = 1 means there are at least one FD on each level. 
declare_domains_fdvars([], _, _, 1) :- !.
declare_domains_fdvars([FdVar|R], CardSet0, FDSet, NoFDSet) :-
    arg(CardSet0, FDSet, CfdVal-_),  % get the functional dependencies corresponding to the current level 
    functor(CfdVal, cfd_val, Arity), % get nb.of ways to partition current level in a value corresponding to 1 and in the other values
    findall(Len, (I in 1..Arity,     % extract lists of functional dependencies of current level
                  indomain(I),
                  arg(I,CfdVal,ArgI),
                  member(t(_,_,Fd), ArgI),
                  length(Fd,Len)),
            FdLengths),
    sumlist(FdLengths, Max),         % get the total number of functional dependencies of current level
    (Max = 0 ->
         NoFDSet = 0
    ;
         FdVar in 1..Max,                 % declare the domain of the functional dependency variable of current level
         NextCardSet0 is CardSet0-1,
         declare_domains_fdvars(R, NextCardSet0, FDSet, NoFDSet)
    ).

gen_table_ctr(0, [_], [], [], _) :- !.
gen_table_ctr(I, [ValueVar1|R], [ValueVars0|S], [FdVar|T], TwoLevelsGrouping) :-
    I > 0,
    Vars = [FdVar,ValueVar1|ValueVars0],
    arg(I, TwoLevelsGrouping, TwoLevelsGroupingI-_),
    collect_functor_args(TwoLevelsGroupingI, List),
    gen_table_tuples_cur_level(List, 1, Tuples, []),
    table([Vars], Tuples),
    I1 is I-1,
    gen_table_ctr(I1, R, S, T, TwoLevelsGrouping).

collect_functor_args(TwoLevelsGroupingI, List) :-
    functor(TwoLevelsGroupingI, _, Arity),
    collect_functor_args(1, Arity, TwoLevelsGroupingI, List).

collect_functor_args(I, N, _, []) :-
    I > N,
    !.
collect_functor_args(I, N, Term, [List|R]) :-
    I =< N,
    arg(I, Term, List),
    I1 is I+1,
    collect_functor_args(I1, N, Term, R).

gen_table_tuples_cur_level([], _, Anchor, Anchor) :- !.
gen_table_tuples_cur_level([List|R], IndFd, OldAnchor, Anchor) :-
    gen_table_tuples_cur_level1(List, IndFd, NextIndFd, OldAnchor, CurAnchor),
    gen_table_tuples_cur_level(R, NextIndFd, CurAnchor, Anchor).

gen_table_tuples_cur_level1([], IndFd, IndFd, Anchor, Anchor) :- !.
gen_table_tuples_cur_level1([t([Val1],Vals0,ListFd)|R], IndFd, NextIndFd, OldAnchor, Anchor) :-
    length(ListFd, LenListFd),
    IndFdLast is IndFd+LenListFd,
    collect_new_tuples(IndFd, IndFdLast, Val1, Vals0, OldAnchor, CurAnchor),
    gen_table_tuples_cur_level1(R, IndFdLast, NextIndFd, CurAnchor, Anchor).

collect_new_tuples(IndFd, IndFdLast, _, _, Anchor, Anchor) :-
    IndFd = IndFdLast, !.
collect_new_tuples(IndFd, IndFdLast, Val1, Vals0, OldAnchor, Anchor) :-
    IndFd < IndFdLast,
    OldAnchor = [[IndFd,Val1|Vals0]|CurAnchor],
    IndFd1 is IndFd + 1,
    collect_new_tuples(IndFd1, IndFdLast, Val1, Vals0, CurAnchor, Anchor).

gen_incompatible_value_ctrs(I, N, _, _) :-
    I > N,
    !.
gen_incompatible_value_ctrs(I, N, [Value1|R], [_|Values0]) :-
    I =< N,
    gen_incompatible_value_ctrs1(Values0, Value1),
    I1 is I+1,
    gen_incompatible_value_ctrs(I1, N, R, Values0).

gen_incompatible_value_ctrs1([], _) :- !.
gen_incompatible_value_ctrs1([Values0|R], Value1) :-
    gen_incompatible_value_ctrs2(Values0, Value1),
    gen_incompatible_value_ctrs1(R, Value1).

gen_incompatible_value_ctrs2([], _) :- !.
gen_incompatible_value_ctrs2([Value0|R], Value1) :-
    Value1 #\= Value0,
    gen_incompatible_value_ctrs2(R, Value1).

get_selected_fd([], _, _, _, []) :- !.
get_selected_fd([Fd|R], I, Table, TwoLevelsGrouping, [FdUsedAttributes|S]) :-
    arg(I, TwoLevelsGrouping, _-ListAttrFds),
    findall(col(Table,Attribute), (member(Attribute-AttrListFd, ListAttrFds),
                                   memberchk(Fd, AttrListFd)),                FdUsedAttributes),
    I1 is I-1,
    get_selected_fd(R, I1, Table, TwoLevelsGrouping, S).

link_used_attrs_to_used_fds([], _, _, _) :- !.
link_used_attrs_to_used_fds([BoolAttr|R], I, FdVars, TwoLevelsGrouping) :-
    link_used_attrs_to_used_fds1(FdVars, 1, TwoLevelsGrouping, BoolAttr, I, Disj),
    call(BoolAttr #<=> Disj),
    I1 is I+1,
    link_used_attrs_to_used_fds(R, I1, FdVars, TwoLevelsGrouping).

link_used_attrs_to_used_fds1([], _, _, _, _, 0) :-
    !.
link_used_attrs_to_used_fds1([Fd|R], J, TwoLevelsGrouping, BoolAttr, I, Fd in_set FDSet #\/ S) :-
    arg(J, TwoLevelsGrouping, _-AttrFdLabels),
    memberchk(I-Labels, AttrFdLabels),
    !,
    list_to_fdset(Labels, FDSet),
    J1 is J+1,
    link_used_attrs_to_used_fds1(R, J1, TwoLevelsGrouping, BoolAttr, I, S).
link_used_attrs_to_used_fds1([_|R], J, TwoLevelsGrouping, BoolAttr, I, S) :-
    J1 is J+1,
    link_used_attrs_to_used_fds1(R, J1, TwoLevelsGrouping, BoolAttr, I, S).
