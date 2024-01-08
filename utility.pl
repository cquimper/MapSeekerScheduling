% Purpose: List utilities and constraint term creation utilities
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(utility,[init_cpt/2,
                   increment_cpt/2,
                   sign/2,
                   gen_pairs/3,
                   lists_intersect/2,
                   insert_elem/3,
                   convert_list_to_term/3,
                   combine_two_lists_in_a_list_of_terms/4,
                   formula_pattern/2,
                   forbid_cols_which_can_be_leq0/4,
                   var_diff_from_values/2,
                   gen_table_ctr_wrt_authorised_cmp/6,
                   gen_table_ctr_wrt_forbidden_cmp/5,
                   get_all_col_pairs_in_cmps/4,
                   shift_pairs/2,
                   gen_combinations_of_values/2,
                   scalar_prod/3,
                   gen_integer_list_from_low_to_up/3,
                   gen_count_ctrs/3,
                   gen_or_ctrs/3,
                   gen_or_term/3,
                   gen_sum_term/3,
                   gen_linear_sum/3,
                   gen_lists_dvars/5,
                   gen_list_dvars_min_maxs/3,
                   get_pos_value/5,
                   prefixes_length/3,
                   suffixes_length/3,
                   length_same_prefix/2,
                   flatten/2,
                   members/2,
                   memberchks/2,
                   eq_memberchks/2,
                   lists_member/2,
                   member_split/4,
                   nths1/3,
                   sublist/2,
                   sublist_max_card/3,
                   sublist_max_card/5,
                   sum_prod_after/2,
                   remove_values/3,
                   args/3,
                   same_args/4,
                   get_common_intersection_and_complement/3,
                   gcd/2,                                       % assumes non-negative integers
                   gcd/3,                                       % assumes non-negative integers
                   get_max_abs_dvars/2,
                   gen_signature/2,
                   gen_cost_sum_abs/2,
                   gen_cost_sum_abs/3,
                   gen_cost_sum_abs_max1/2,
                   gen_cost_diff0/2,
                   gen_cost_max_abs/2,
                   atoms_concat/2,
                   atom_proper_suffix_add_underscore/2,
                   atoms_concat_with_underscore/2,
                   add_key/2,
                   prodlist/2,
                   gen_occ_vars/4,
                   gen_occ_vars/5,
                   compute_max_abs/3,
                   split_list_wrt_smallest_length/3,
                   cartesian_product_without_sort/3,
                   cartesian_product_with_sort/3,
                   sort_wrt_list_length/2,
                   listify/2,
                   build_continuation_list/3,
                   create_list_of_empty_lists/2,
                   remove_first_last_elements/4,
                   remove_first_last_elements/2,
                   complement_elements/3,
                   intersect_elements/3,
                   more_than_one_unique_value/1,
                   write_atoms/1,
                   write_list/1,
                   write_list/2,
                   write_list_without_brackets/1,
                   write_spaces/1,
                   call_with_time_out/1,
                   binary_term_calculation/5,
                   list_included/2,
                   intersect_lists/3,
                   remove_elements_from_unsorted_list/3,
                   remove_elements_from_list/3,
                   remove_ith_elem/3,
                   del/3,
                   del/5,
                   gen_all_element_subset_pairs/2,
                   group_same_keys/2,
                   combine_lists/3,
                   combine_lists_exclusive/3,
                   merge_lists/3,
                   length_prefix/4,
                   remove_first_elem/3,
                   remove_consecutive_identical_entries/2,
                   sum_squares/2,
                   get_non_zero_exponents/3,
                   count_distinct_vals/3,
                   unify_variables/4,
                   replace_recursively_atoms_by_vars/3,
                   extract_nb_distinct_atoms/2,
                   atom_not_in_term/2,
                   collect_temporal_attrs/10, % used to acquire precedence and resource constraints
                   complement_intervals/2,    % used to acquire calendar constraints
                   call_imply_cond_ctr/3,
                   pk_project/4,
                   restrict_pks_candidates/3,
                   fd_same_key/1,
                   build_source_target_extraction_terms/3,
                   get_table_entries/4,
                   remove_last_elem/3,
                   match_list_indices_sublist/3,
                   simplify_term/2,
                   to_atom/2]). 

:- use_module(library(timeout)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(table_access).                 % as gen_table_ctr_wrt_authorised_cmp/6 and gen_table_ctr_wrt_forbidden_cmp/5 use tab_get_cmp/2
:- use_module(test_if_scheduling_attribute).

init_cpt(CptName, CptVal) :-
    functor(Term, CptName, 1),        % build term corresponding to cpt
    retractall(Term),
    arg(1, Term, CptVal),             % set argument of Term to cpt value
    assert(Term).                     % assert value

increment_cpt(CptName, CptNewVal) :-
    functor(Term, CptName, 1),        % build term corresponding to cpt
    call(Term),
    arg(1, Term, CptCurVal),          % get value of current cpt
    CptNewVal is CptCurVal+1,         % increment value of current cpt
    functor(NewTerm, CptName, 1),     % build a new term that will correspond to incremented value
    retractall(NewTerm),              % retract previous cpt (note that argument of NewTerm is still a variable)
    arg(1, NewTerm, CptNewVal),       % set argument of NewTerm to incremented value
    assert(NewTerm).                  % assert incremented value

sign(X,  1) :- X > 0, !. % do not use sign/1 to avoid comparaisons with floats
sign(X, -1) :- X < 0, !.
sign(_,  0).

gen_pairs([], [], []) :- !.
gen_pairs([X|R], [Y|S], [X-Y|T]) :-
    gen_pairs(R, S, T).

lists_intersect(L1, L2) :-
    member(E1, L1),
    memberchk(E1, L2),
    !.

insert_elem(List, Elem, [Elem|List]).
insert_elem([First|Rest], Elem, [First|List]) :-
    insert_elem(Rest, Elem, List).

convert_list_to_term(List, Functor, Term) :-
    length(List, L),
    functor(Term, Functor, L),
    convert_list_to_term1(List, 1, Term).

convert_list_to_term1([], _, _) :- !.
convert_list_to_term1([X|Y], I, Term) :-
    arg(I, Term, X),
    I1 is I+1,
    convert_list_to_term1(Y, I1, Term).

combine_two_lists_in_a_list_of_terms([], [], _, []) :- !.
combine_two_lists_in_a_list_of_terms([Name|R], [Var|S], Functor, [Term|T]) :-
    functor(Term, Functor, 2),
    arg(1, Term, Name),
    arg(2, Term, Var),
    combine_two_lists_in_a_list_of_terms(R, S, Functor, T).

formula_pattern(Field, FormulaPattern) :- % set/get a field describing a formula
    memberchk(Field, FormulaPattern).

forbid_cols_which_can_be_leq0(LenA, AttrNames, Shift, Var) :-
    findall(I1,
            (I in 1..LenA,
             indomain(I),                   % go through the attributes we can have in the formula
             nth1(I, AttrNames, AttrNameI), % get the different attributes names
             tab_get_min(AttrNameI, MinI),  % get minimum value of attribute I
             (MinI  =< 0  -> true ;         % catch case when MinI =< 0
                             false),
             I1 is I + Shift),
            ListI),
    var_diff_from_values(ListI, Var).       % remove all values of ListI from variable Var

var_diff_from_values([], _) :- !.
var_diff_from_values([Val|R], Var) :-
    Var #\= Val,
    var_diff_from_values(R, Var).

gen_table_ctr_wrt_authorised_cmp(LenA, 2, AttrNames, AuthorisedCmps, Shift, PairOfVars) :- !,
    get_all_col_pairs_in_cmps(LenA, AttrNames, AuthorisedCmps, AuthorisedPairs),
    (AuthorisedPairs = [] ->
        false
    ;
        (Shift = 0 ->
            Authorised = AuthorisedPairs
        ;                                              % if call from gen_bool_formula (i.e. Shift is 1)
            shift_pairs(AuthorisedPairs, Authorised0), % then allow also the pair [1,1] corresponding to
            Authorised = [[1,1]|Authorised0]           % the fact that the Boolean condition will be not used
        ),                                             % (as without [1,1] it would force to use the Boolean condition)
        call(table([PairOfVars], Authorised))
    ).
gen_table_ctr_wrt_authorised_cmp(LenA, 3, AttrNames, AuthorisedCmps, Shift, TriplesOfVars) :-
    get_all_col_triples_in_cmps(LenA, AttrNames, AuthorisedCmps, AuthorisedTriples),
    (AuthorisedTriples = [] ->
        false
    ;
        (Shift = 0 ->
            Authorised = AuthorisedTriples
        ;                                                  % if call from gen_bool_formula (i.e. Shift is 1)
            shift_triples(AuthorisedTriples, Authorised0), % then allow also the triple [1,1,1] corresponding to
            Authorised = [[1,1,1]|Authorised0]             % the fact that the Boolean condition will be not used
        ),                                                 % (as without [1,1,1] it would force to use the Boolean condition)
        call(table([TriplesOfVars], Authorised))
    ).

gen_table_ctr_wrt_forbidden_cmp(LenA, AttrNames, ForbiddenCmps, Shift, PairOfVars) :-
    get_all_col_pairs_in_cmps(LenA, AttrNames, ForbiddenCmps, ForbiddenPairs),
    (Shift = 0 ->
        Forbidden = ForbiddenPairs
    ;
        shift_pairs(ForbiddenPairs, Forbidden)%,
  %write(shift_pairs(ForbiddenPairs, Forbidden)),nl
    ),
    call(#\table([PairOfVars], Forbidden)).

shift_pairs([], []) :- !.
shift_pairs([[X,Y]|R], [[X1,Y1]|S]) :-
    X1 is X+1, Y1 is Y+1,
    shift_pairs(R, S).

shift_triples([], []) :- !.
shift_triples([[X,Y,Z]|R], [[X1,Y1,Z1]|S]) :-
    X1 is X+1, Y1 is Y+1, Z1 is Z+1,
    shift_triples(R, S).

get_all_col_pairs_in_cmps(LenA, AttrNames, Cmps, Table) :-
    findall([I,J],
            (I in 1..LenA,
             indomain(I),                      % go through the attributes we can have in the formula
             nth1(I, AttrNames, AttrNameI),    % get the different attributes names
             tab_get_cmp(AttrNameI, ListCmpI), % extract all comparaison relations found for AttrNameI
             member(Cmp, Cmps),                % go through the different comparaison operators
             functor(Term, Cmp, 2),            % contruct the term Cmp(AttrNameI,AttrNameJ)
             arg(1, Term, AttrNameI),          % extract all attributes AttrNameJ for which
             arg(2, Term, AttrNameJ),          % the comparaison Cmp with AttrNameI holds
             findall(AttrNameJ, member(Term,ListCmpI), AttrNamesJ),
             member(AttrNameJ, AttrNamesJ),    % extract elements of AttrNamesJ
             nth1(J, AttrNames, AttrNameJ)),   % get index J associated with AttrNameJ
            Pairs),
    sort(Pairs, Table).                        % remove duplicates

get_all_col_triples_in_cmps(LenA, AttrNames, Cmps, Table) :-
    findall([I,J,K],
            (I in 1..LenA,
             indomain(I),
             nth1(I, AttrNames, AttrNameI),
             tab_get_cmp(AttrNameI, ListCmpI), % list of comparisons terms of Ith attribute
             member(TCmpI, ListCmpI),          % get next comparison term of Ith attribute
             functor(TCmpI, CmpI, 2),		   % get comparison operator itself
             memberchk(CmpI, Cmps),            % check that inside required set of comparison operators
             arg(2, TCmpI, AttrNameJ),         % get corresponding Jth attribute of the form col(table,attr)
             AttrNameJ \== AttrNameI,          % all all selected attributes should be distinct
             tab_get_cmp(AttrNameJ, ListCmpJ), % list of comparisons terms of Jth attribute
             member(TCmpJ, ListCmpJ),          % get next comparison term of Jth attribute
             functor(TCmpJ, CmpJ, 2),		   % get comparison operator itself
             memberchk(CmpJ, Cmps),            % check that inside required set of comparison operators
             arg(2, TCmpJ, AttrNameK),         % get corresponding Kth attribute of the form col(table,attr)
             AttrNameK \== AttrNameI,          % all all selected attributes should be distinct
             AttrNameK \== AttrNameJ,          % all all selected attributes should be distinct
             nth1(J, AttrNames, AttrNameJ),    % get index of AttrNameJ in list AttrNames
             nth1(K, AttrNames, AttrNameK)),   % get index of AttrNameK in list AttrNames
            Triples),
	sort(Triples, Table).                      % remove duplicate

gen_combinations_of_values([], _) :- !.
gen_combinations_of_values([V|R], Values) :- member(V, Values), gen_combinations_of_values(R, Values).

scalar_prod(List1, List2, Res) :-
    scalar_prod(List1, List2, 0, Res).

scalar_prod([], [], Res, Res) :- !.
scalar_prod([Val1|R1], [Val2|R2], Cur, Res) :-
    Next is Cur + Val1*Val2,
    scalar_prod(R1, R2, Next, Res).

gen_integer_list_from_low_to_up(I, Limit, []) :- I > Limit, !.
gen_integer_list_from_low_to_up(I, Limit, [I|R]) :-
    I =< Limit,
    I1 is I+1,
    gen_integer_list_from_low_to_up(I1, Limit, R).

gen_count_ctrs([], _, _) :- !.
gen_count_ctrs([List|R], Val, Ctr) :-
    (Ctr = eq([Cst|S])  ->                  count(Val, List, #=,  Cst), gen_count_ctrs(R, Val, eq(S) ) ;
     Ctr = eq(Cst)      ->                  count(Val, List, #=,  Cst), gen_count_ctrs(R, Val, Ctr   ) ;
     Ctr = geq([Cst|S]) ->                  count(Val, List, #>=, Cst), gen_count_ctrs(R, Val, geq(S)) ;
     Ctr = geq(Cst)     ->                  count(Val, List, #>=, Cst), gen_count_ctrs(R, Val, Ctr   ) ;
     Ctr = in(Min,Max)  -> Cst in Min..Max, count(Val, List, #=,  Cst), gen_count_ctrs(R, Val, Ctr   ) ;
                                            write(gen_count_ctrs), nl, halt                            ).

gen_or_ctrs([], _, []) :- !.
gen_or_ctrs([L|R], Type, [B|S]) :-
    gen_or_term(L, Type, Term),
    call(B #= Term),
    gen_or_ctrs(R, Type, S).

gen_or_term([], _, 0) :- !.
gen_or_term([V|R], var, V #\/ S) :- !,
    gen_or_term(R, var, S).
gen_or_term([V|R], var_neq(Val), (V #\= Val) #\/ S) :-
    gen_or_term(R, var_neq(Val), S).

gen_sum_term([], _, 0) :- !.
gen_sum_term([V|R], var, V+S) :- !,
    gen_sum_term(R, var, S).
gen_sum_term([V|R], var_neq(Val), (V #\= Val) + S) :-
    gen_sum_term(R, var_neq(Val), S).

gen_linear_sum([], [], 0) :- !.
gen_linear_sum([C], [V], C*V) :- !.
gen_linear_sum([C|R], [V|S], C*V+T) :-
    gen_linear_sum(R, S, T).

gen_lists_dvars(0, _, _, _, []) :- !.
gen_lists_dvars(I, MaxJ, MinV, MaxV, [ListDvars|R]) :-
    I > 0,
    length(ListDvars, MaxJ),
    domain(ListDvars, MinV, MaxV),
    I1 is I-1,
    gen_lists_dvars(I1, MaxJ, MinV, MaxV, R).

gen_list_dvars_min_maxs([], _, []) :- !.
gen_list_dvars_min_maxs([V|R], MinV, [MaxV|S]) :-
    V in MinV..MaxV,
    gen_list_dvars_min_maxs(R, MinV, S).

% get_pos_value(List, Value, I, K, Result):
% Result will be the positions in List of the first K occurrences of value V (I is the position of the first element of List) 
get_pos_value([], _, _, _, []) :- !.
get_pos_value([V|R], V, I, K, [I|S]) :- !,
    I1 is I+1,
    K1 is K-1,
    (K1 = 0 -> S = [] ; get_pos_value(R, V, I1, K1, S)).
get_pos_value([_|R], V, I, K, S) :- !,
    I1 is I+1,
    get_pos_value(R, V, I1, K, S).

prefixes_length([], [], _) :- !.
prefixes_length([List|R], [Prefix|S], Len) :-
    prefix_length(List, Prefix, Len),
    prefixes_length(R, S, Len).

suffixes_length([], [], _) :- !.
suffixes_length([List|R], [Suffix|S], Len) :-
    suffix_length(List, Suffix, Len),
    suffixes_length(R, S, Len).

length_same_prefix([], 0) :- !.
length_same_prefix([V|R], Res) :-
    length_same_prefix(R, V, 1, Res).

length_same_prefix([V|R], V, Cur, Res) :- !,
    Next is Cur+1,
    length_same_prefix(R, V, Next, Res).
length_same_prefix(_, _, Res, Res).

flatten(List, FlatList) :-
    flatten(List, FlatList, []).

flatten([], List, List):- !.
flatten([List|R], X, Z) :-
    is_list(List), !,
    flatten(List, X, Y),
    flatten(R, Y, Z).
flatten([E|R], [E|X], Z) :-
    flatten(R, X, Z).

members([], _) :- !.
members([V|R], L) :-
    member(V, L),
    members(R, L).

memberchks([], _) :- !.
memberchks([V|R], L) :-
    memberchk(V, L),
    memberchks(R, L).

eq_memberchks(L1, L2) :-
    length(L1, N),
    length(L2, N),
    memberchks(L1, L2).

lists_member([List|_], Elem) :-
    member(Elem, List).
lists_member([_|R], Elem) :-
    lists_member(R, Elem).

% select a subset of the elements of the list List, and on backtracking get the next element
% the selection process is parametrised by SplitDiv which give the number of parts used to split the list, and SplitMod which give the part we focus on (index starts at 1)
%  for instance spliting a list of ten elements with SplitDiv=4 and SplitMod=1 would successively return elements 1, 2, 3
%  for instance spliting a list of ten elements with SplitDiv=4 and SplitMod=2 would successively return elements 4, 5, 6
%  for instance spliting a list of ten elements with SplitDiv=4 and SplitMod=3 would successively return elements 7, 8
%  for instance spliting a list of ten elements with SplitDiv=4 and SplitMod=4 would successively return elements 9, 10
member_split(List, SplitDiv, SplitMod, Elem) :-
    length(List, Len),
    Split is min(SplitDiv, Len),
    (SplitMod =< Split -> true ; false),
    Mod   is Len mod Split,
    Floor is Len // Split,
    Ceil  is (Len+Split-1) // Split,
    (SplitMod =< Mod ->
        Delta = 1,
        H = Ceil,
        I = SplitMod
    ;
        Delta is 1 + Mod * Ceil,
        H = Floor,
        I is SplitMod-Mod
    ),
    Low is Delta + (I-1)*H,
    Up  is Low + H - 1,
    member_split(List, 1, Low, Up, Elem).

member_split([_|R], I, Low, Up, Elem) :-
    I < Low,
    I1 is I+1,
    member_split(R, I1, Low, Up, Elem).
member_split([Elem|_], I, Low, Up, Elem) :-
    Low =< I,
    I =< Up.
member_split([_|R], I, Low, Up, Elem) :-
    Low =< I,
    I < Up,
    I1 is I+1,
    member_split(R, I1, Low, Up, Elem).

nths1([], _, _) :- !.
nths1([I|R], List, Val) :-
    nth1(I, List, Val),
    nths1(R, List, Val).

sublist(Source, Target) :-
    length(Source, N),
    M in 1..N,
    indomain(M),
    length(Target, M),
    sublist1(Target, Source).

sublist_max_card(Source, Max, Target) :-
    length(Source, N),
    Limit is min(N, Max),
    M in 1..Limit,
    indomain(M),
    length(Target, M),
    sublist1(Target, Source).

sublist_max_card(Source1, Source2, Max2, Max12, Target) :-
    length(Source1, N1),
    length(Source2, N2),
    N12 is N1+N2,
    Limit12 is min(N12, Max12), M12 in 1..Limit12,
    Limit1  is min(N1,  Max12), M1  in 0..Limit1,
    Limit2  is min(N2,  Max2),  M2  in 0..Limit2,
    M12 #= M1 + M2, labeling([], [M12,M2,M1]),
    length_anchor(M1, Target, Target2),
    length(Target2, M2),
    sublist1(Target2, Source2),
    sublist1(Target, Source1).

length_anchor(0, L, L) :- !.
length_anchor(I, [_|R], L) :-
    I > 0,
    I1 is I-1,
    length_anchor(I1, R, L).

sublist1([], _) :- !.
sublist1([V|_], _) :-
    ground(V), !.
sublist1([V|R], [V|S]) :-
    sublist1(R, S).
sublist1([V|R], [_|S]) :-
    sublist1([V|R], S).

sum_prod_after(L, Res) :-
    sliding_sum(L, 0, S),
    sum_prod_after(L, S, 0, Res).

sliding_sum([], _, []) :- !.
sliding_sum([X|R], Prev, [Y|S]) :-
    Y is Prev + X,
    sliding_sum(R, Y, S).

sum_prod_after([], _, Res, Res) :- !.
sum_prod_after([X|R], [Y|S], Prev, Res) :-
    Cur is Prev + X*Y,
    sum_prod_after(R, S, Cur, Res).

remove_values([Value|R], Value, S) :- !,
    remove_values(R, Value, S).
remove_values(L, _, L).

args([], _, []) :- !.
args([Ind|R], Term, [Val|S]) :-
    arg(Ind, Term, Val),
    args(R, Term, S).

same_args(I, N, _, _) :-
    I > N, !.
same_args(I, N, Term1, Term2) :-
    arg(I, Term1, V),
    arg(I, Term2, V),
    I1 is I+1,
    same_args(I1, N, Term1, Term2).

get_common_intersection_and_complement(Lists, CommonIntersection, Complement) :-
    Lists = [List1|R],
    common_intersection(R, List1, CommonIntersection),
    not_in_intersection(Lists, CommonIntersection, Complement).

common_intersection(_, [], []) :- !.
common_intersection([], Result, Result) :- !.
common_intersection([SubList|R], Intersection, Result) :-
    intersection(Intersection, SubList, NewIntersection),
    common_intersection(R, NewIntersection, Result).

intersection([], _, []) :- !.
intersection([Elem|R], L, [Elem|S]) :-
        memberchk(Elem, L),
        !,
        intersection(R, L, S).
intersection([_|R], L, S) :-
        intersection(R, L, S).

not_in_intersection(Lists, Intersection, NotInIntersection) :-
    flatten(Lists, FlatList),
    sort(FlatList, SortedFlatList),
    findall(Elem, (member(Elem,SortedFlatList), \+ memberchk(Elem,Intersection)), NotInIntersection).

gcd([R], R) :- !.
gcd([X1,X2|R], S) :-
    gcd(X1, X2, G),
    gcd([G|R], S).

gcd(0, Z, Z) :- !.
gcd(Z, 0, Z) :- !.
gcd(X, X, X) :- !.
gcd(X, Y, Z) :-
    X > Y, !,
    X1 is X mod Y,
    gcd(X1, Y, Z).
gcd(X, Y, Z) :-
    X < Y,
    gcd(Y, X, Z).

get_max_abs_dvars(DVars, MaxAbs) :-
    get_max_abs_dvars(DVars, 0, MaxAbs).

get_max_abs_dvars([], MaxAbs, MaxAbs) :- !.
get_max_abs_dvars([V|R], Cur, MaxAbs) :-
    fd_min(V, Min),
    fd_max(V, Max),
    Next is max(Cur, max(abs(Min),abs(Max))),
    get_max_abs_dvars(R, Next, MaxAbs).

gen_signature([], []) :- !.
gen_signature([Var|R], [Sig|S]) :-
    Sig in 0..2,
    Var #< 0 #<=> Sig #= 0,
    Var #= 0 #<=> Sig #= 1,
    Var #> 0 #<=> Sig #= 2,
    gen_signature(R, S).

gen_cost_sum_abs(L, Cost) :-
    gen_sum_abs(L, 0, SumAbs),
    call(SumAbs #= Cost).

gen_cost_sum_abs(L, List, Cost) :-
    length(List, Len),
    Delta is (Len+1) // 2,
    gen_sum_abs(L, Delta, SumAbs),
    call(SumAbs #= Cost).

gen_sum_abs([], Delta, Delta) :- !.
gen_sum_abs([V|R], Delta, abs(V)+S) :-
    gen_sum_abs(R, Delta, S).

gen_cost_sum_abs_max1(L, Cost) :-
    gen_sum_abs_max1(L, SumAbsMax1),
    call(SumAbsMax1 #= Cost).

gen_sum_abs_max1([], 0) :- !.
gen_sum_abs_max1([V|R], max(1,abs(V))+S) :-
    gen_sum_abs_max1(R, S).

gen_cost_diff0(L, Cost) :-
    gen_diff0(L, Diff0),
    call(Diff0 #= Cost).

gen_diff0([], 0) :- !.
gen_diff0([V|R], B+S) :-
    B #<=> V #\= 0,
    gen_diff0(R, S).

gen_cost_max_abs(L, Cost) :-
    gen_abs(L, Abs),
    call(maximum(Cost, Abs)).

gen_abs([], []) :- !.
gen_abs([V|R], [A|S]) :-
    A #= abs(V),
    gen_abs(R, S).

atoms_concat([Atom|R], Final) :-
    to_atom(Atom, AtomC),
    atoms_concat(R, AtomC, Final).

atoms_concat([], Final, Final) :- !.
atoms_concat([Atom|R], Prev, Final) :-
    to_atom(Atom, AtomC),
    atom_concat(Prev, AtomC, Cur),
    atoms_concat(R, Cur, Final).

atom_proper_suffix_add_underscore(Atom, Suffix) :-
    atom_codes('_', Code),
    atom_codes(Atom, AtomCodes),
    atom_codes(Suffix, SuffixCodes),
    append(Code, SuffixCodes, SuffixCodes1),
    proper_suffix(AtomCodes, SuffixCodes1).

atoms_concat_with_underscore([F|R], Final) :-
    ((atom(F), R = []) ->
        Final = F
    ;
     (atom(F), R = [_|_]) ->
        atom_concat(F, '_', First),
        atoms_concat_with_underscore(R, Remainder),
        atom_concat(First, Remainder, Final)
    ;
     (is_list(F), R = []) ->
        atoms_concat_with_underscore(F, Final)
    ;
     (is_list(F), R = [_|_]) ->
        atoms_concat_with_underscore(F, First),
        atom_concat(First, '_', First1),
        atoms_concat_with_underscore(R, Remainder),
        atom_concat(First1, Remainder, Final)
    ;
        write(atoms_concat_with_underscore([F|R])), nl, halt
    ).

% convert to atoms
to_atom(X,X):-
        atom(X),
        !.
to_atom(X,Y):-
        number(X),
        number_codes(X,L),
        atom_codes(Y,L).

add_key(List, ListWithKey) :-
    add_key(List, 1, ListWithKey).

add_key([], _, []) :- !.
add_key([X|R], K, [K-X|S]) :-
    K1 is K+1,
    add_key(R, K1, S).

prodlist(Numbers, Product) :-
(   foreach(X,Numbers),
    fromto(1,P0,P,Product)
do  P is P0*X
).

gen_occ_vars(I, N, _, []) :-
    I > N, !.
gen_occ_vars(I, N, Max, [I-Occ|R]) :-
    I =< N,
    Occ in 1..Max,
    I1 is I+1,
    gen_occ_vars(I1, N, Max, R).

gen_occ_vars(I, N, _, [], []) :-
    I > N, !.
gen_occ_vars(I, N, Max, [I-Occ|R], [Occ|S]) :-
    I =< N,
    Occ in 1..Max,
    I1 is I+1,
    gen_occ_vars(I1, N, Max, R, S).

compute_max_abs([], [], []) :- !.
compute_max_abs([Min|R], [Max|S], [MaxAbs|T]) :-
    MaxAbs is max(abs(Min),abs(Max)),
    compute_max_abs(R, S, T).

% split a list L of terms of the form functor(List) in two parts:
%  . the first part LSmallestLen contains the sublists of smallest length,
%  . the second part LRest contains the rest of the sublist
% assumes that the input list is sorted by increasing sublist length
split_list_wrt_smallest_length([], [], []) :- !.
split_list_wrt_smallest_length([Term|R], [Term|S], T) :-
    arg(1, Term, L),
    length(L, SmallestLen),
    split_list_wrt_smallest_length(R, SmallestLen, S, T).

split_list_wrt_smallest_length([], _, [], []) :- !.
split_list_wrt_smallest_length([Term|R], SmallestLen, [Term|S], T) :-
    arg(1, Term, L),
    length(L, SmallestLen),
    !,
    split_list_wrt_smallest_length(R, SmallestLen, S, T).
split_list_wrt_smallest_length(Terms, _, [], Terms).

cartesian_product_without_sort(L1, L2, L12) :-
    findall(E12, (member(E1, L1), member(E2, L2), append([E1], E2, E12)), L12).

cartesian_product_with_sort(L1, L2, L12) :-
    findall(S12, (member(E1, L1),
                  member(E2, L2),
                  ((is_list(E1), is_list(E2)) -> append(E1, E2,   E12) ;
                    is_list(E1)               -> append(E1, [E2], E12) ;
                                                 append([E1], E2, E12) ),
                  sort(E12, S12)),                                        LS12),
    sort(LS12, L12).

sort_wrt_list_length(L, SL) :-
    add_len_key(L, KL),
    keysort(KL, SKL),
    add_len_key(SL, SKL).

add_len_key([], []) :- !.
add_len_key([L|R], [K-L|S]) :-
    length(L, K),
    add_len_key(R, S).

listify([], []) :- !.
listify([L|R], [[L]|S]) :-
    listify(R, S).

% | ?- build_continuation_list([5,1,3], Cont, List).
% Cont = [_A,_B,_C],
% List = [[5|_A],[1|_B],[3|_C]]
build_continuation_list([], [], []) :- !.
build_continuation_list([Element|RE], [Continuation|RC], [[Element|Continuation]|R]) :-
    build_continuation_list(RE, RC, R).

create_list_of_empty_lists(0, []) :- !.
create_list_of_empty_lists(N, [[]|R]) :-
    N > 0,
    N1 is N-1,
    create_list_of_empty_lists(N1, R).

% remove_first_last_elements(L1, LMin, LMax, L2):
%  L1: list of lists of distinct integer values sorted in increasing order
%  LMin: list of minimum values we try to remove from the sublist of L1
%  LMax: list of maximum values we try to remove from the sublist of L1
%  L2: list L1 from which we remove the first and the last elements of each sublist in L1
%      if they correspond to the minimum/maximum values of LMin/LMax
remove_first_last_elements([], _, _, []) :- !.
remove_first_last_elements([Li|R], [Min|S], [Max|T], [Lj|U]) :-
    remove_first_last_elements1(Li, Min, Max, Lj),
    remove_first_last_elements(R, S, T, U).

remove_first_last_elements1([], _, _, []) :- !.
remove_first_last_elements1([Min|R], Min, Max, S) :-
    !,
    remove_last_element(R, Max, S).
remove_first_last_elements1([V|R], _, Max, [V|S]) :-
    remove_last_element(R, Max, S).

remove_last_element([], _, []) :- !.
remove_last_element([Max], Max, []) :- !.
remove_last_element([V], _, [V]) :- !.
remove_last_element([V|R], Max, [V|S]) :-
    remove_last_element(R, Max, S).


% remove the first and last element of a list
remove_first_last_elements([],  []) :- !.
remove_first_last_elements([_], []) :- !.
remove_first_last_elements([_|R], L) :-
    length(R, N),
    N1 is N-1,
    prefix_length(R, L, N1).

% complement_elements(L1, L2, L):
%  L1: a list of lists of distinct integer values sorted in increasing order
%  L2: a list of lists of distinct integer values sorted in increasing order (L1 and L2 have the same length)
%  L : compute the pairwise differences between the sublists in L1 and L2
complement_elements([], [], []) :- !.
complement_elements([Li|R], [Lj|S], [L|T]) :-
    complement_elements1(Li, Lj, L),
    complement_elements(R, S, T).

complement_elements1(L, [], L) :- !.
complement_elements1([], _, []) :- !.
complement_elements1([U|R], [V|S], [U|T]) :-
    U < V,
    !,
    complement_elements1(R, [V|S], T).
complement_elements1([U|R], [U|S], T) :-
    !,
    complement_elements1(R, S, T).
complement_elements1([U|R], [_|S], T) :-
    complement_elements1([U|R], S, T).

% intersect_elements(L1, L2, L):
%  L1: a list of lists of distinct integer values sorted in increasing order
%  L2: a list of lists of distinct integer values sorted in increasing order (L1 and L2 have the same length)
%  L : compute the pairwise intersections between the sublists in L1 and L2
intersect_elements([], [], []) :- !.
intersect_elements([Li|R], [Lj|S], [L|T]) :-
    intersect_elements1(Li, Lj, L),
    intersect_elements(R, S, T).

intersect_elements1(_, [], []) :- !.
intersect_elements1([], _, []) :- !.
intersect_elements1([U|R], [V|S], T) :-
    U < V,
    !,
    intersect_elements1(R, [V|S], T).
intersect_elements1([U|R], [U|S], [U|T]) :-
    !,
    intersect_elements1(R, S, T).
intersect_elements1([U|R], [_|S], T) :-
    intersect_elements1([U|R], S, T).

more_than_one_unique_value([])  :- !, false.
more_than_one_unique_value([_]) :- !, false.
more_than_one_unique_value([H|R]) :-
   more_than_one_unique_value1(R, H).

more_than_one_unique_value1([X|_], Y) :-
   X =\= Y, !.
more_than_one_unique_value1([X|R], _) :-
   more_than_one_unique_value1(R, X).

write_atoms(L) :- length(L, N), write_atoms(L, N).

write_atoms([A], N) :- !,
    (N > 1 -> write(' and ') ; true), write(A).
write_atoms([A|R], N) :-
    write(A), (R = [_] -> true ; write(', ')), write_atoms(R, N).

write_list([]) :- !.
write_list([P|R]) :-
    write(P), nl,
    write_list(R).

write_list([], _) :- !.
write_list([P|R], Sout) :-
    format(Sout,'~w.~n',[P]),
    write_list(R, Sout).

write_list_without_brackets([]) :- !.
write_list_without_brackets([X|Y]) :-
    write(X),
    (Y = [] -> true ; write(',')),
    write_list_without_brackets(Y).

write_spaces(0) :- !.
write_spaces(N) :-
    N > 0,
    write(' '),
    N1 is N-1,
    write_spaces(N1).

% Val1 and Val2 are fixed
binary_term_calculation(BinaryTerm, Val1, Val2, Col, Res) :-
    (BinaryTerm = mfloor -> tab_get_min(Col,MinAttr), Res is (Val1 // max(Val2 - MinAttr, 1)) ;
     BinaryTerm = plus   -> Res is Val1 + Val2                                                ;
     BinaryTerm = minus  -> Res is Val1 - Val2                                                ;
     BinaryTerm = prod   -> Res is Val1 * Val2                                                ;
     BinaryTerm = abs    -> Res is abs(Val1 - Val2)                                           ;
     BinaryTerm = min    -> Res is min(Val1, Val2)                                            ;
     BinaryTerm = max    -> Res is max(Val1, Val2)                                            ;
     BinaryTerm = floor  -> Val2 =\= 0, Res is Val1 // Val2                                   ;
     BinaryTerm = ceil   -> Val2 =\= 0, Res is (Val1 + Val2 - 1) // Val2                      ;
     BinaryTerm = mod    -> Val2 =\= 0, Res is Val1 mod Val2                                  ;
     BinaryTerm = cmod   -> Val1 =\= 0, Res is Val1 - (Val2 mod Val1)                         ;
     BinaryTerm = dmod   -> Val2 =\= 0, Res is Val1 - (Val1 mod Val2)                         ;
     BinaryTerm = fmod   -> Val2 =\= 0, M is Val1 mod Val2, (M = 0 -> Res = Val2 ; Res = M)   ;
                            false                                                             ).

% succeed if all elements of the first list are also elements of the second list
list_included([], _) :- !.
list_included([E|R], L) :-
    memberchk(E, L),
    list_included(R, L).

% intersect_lists(L1, L2, L3):
% L1 and L2 are two sorted lists; L3 contains only elements that are in both lists.
% example: intersect_lists([2,3,5],[1,2,3,4,6,7],Res).
intersect_lists([], _, []) :- !.
intersect_lists(_, [], []) :- !.
intersect_lists([X|R], [X|S], [X|T]) :- !,intersect_lists(R,S,T).
intersect_lists([X|R], [Y|S], T) :-
    ((X @< Y) -> intersect_lists(R, [Y|S], T) ; intersect_lists([X|R], S, T)).

% remove_elements_from_unsorted_list(L1, L2, L3):
%   L1 and L2 are two unsorted lists; L3 contains only elements that are in L1, but not in L2.
remove_elements_from_unsorted_list(L1, L2, L3) :-
    sort(L1, S1),
    sort(L2, S2),
    remove_elements_from_list(S1, S2, L3).

% remove the i-th element of a list
remove_ith_elem(I, Lin, Lout) :-
    remove_ith_elem(1, I, Lin, Lout).

remove_ith_elem(J, I, [E|R], [E|S]) :-
    J < I,
    !,
    J1 is J+1,
    remove_ith_elem(J1, I, R, S).
remove_ith_elem(I, I, [_|R], S) :-
    !,
    J1 is I+1,
    remove_ith_elem(J1, I, R, S).
remove_ith_elem(J, I, L, L) :-
    J > I.

% remove_elements_from_list(L1, L2, L3):
% L1 and L2 are two sorted lists; L3 contains only elements that are in L1, but not in L2.
%
% ?- remove_elements_from_list([2,3,5], [1,2,3,4,6,7], Res).
% Res = [5]
remove_elements_from_list([], _, []) :- !.
remove_elements_from_list(L, [], L)  :- !.
remove_elements_from_list([X|R], [X|S], T) :- !,
    remove_elements_from_list(R, S, T).
remove_elements_from_list([X|R], [Y|S], [X|T]) :-
    X @< Y, !,
    remove_elements_from_list(R, [Y|S], T).
remove_elements_from_list(L, [_|R], S) :-
    remove_elements_from_list(L, R, S).

del(Elem, [Elem|Rest], Rest).
del(Elem, [Skip|List], [Skip|Rest]) :-
    del(Elem, List, Rest).

% delete the different elements of a list and provide a) the deleted element, b) the list without the deleted element, c) the index of the deleted element
del(Elem, [Elem|Rest], Rest, I, I).
del(Elem, [Skip|List], [Skip|Rest], I, J) :-
    I1 is I+1,
    del(Elem, List, Rest, I1, J).

% ?- gen_all_element_subset_pairs([0,1,2],L).
% L = [[0]-[1], [0]-[2], [0]-[1,2], [1]-[2], [1]-[0,2], [2]-[0,1]]
% between pairs [i]-[j] and [j]-[i] select only one of these that have increasing order of numbers of entries within the data table
gen_all_element_subset_pairs(ValueCounts, ElementSubsetPairs) :-
    length(ValueCounts, N),
    keys_and_values(ValueCounts,Values,_),
    N1 is N-1,
    findall([Element]-Subset,
            (del(Element, Values, Rest),
             sublist_max_card(Rest,N1,Subset),
             (Subset = [E] ->
                %Element < E % do the check here
                memberchk(Element-ElementCounts, ValueCounts),
                memberchk(      E-ECounts      , ValueCounts),
                (ElementCounts < ECounts ->
                    true
                ;
                 (ElementCounts = ECounts, Element < E) ->
                    true
                ;
                    false)
             ;
                true
             )), ElementSubsetPairs).

% intersect_lists(L1, L2, L3):
% L1 and L2 are two sorted lists; L3 contains elements that are in one of the lists or both lists, but only one time.
% example: combine_lists([2,3,5],[1,2,3],Res).
% ?- Res = [1,2,3,5]
combine_lists([], [], []) :- !.
combine_lists([], S, S) :- !.
combine_lists(R, [], R) :- !.
combine_lists([X|R], [X|S], [X|T]) :-
    !,
    combine_lists(R,S,T).
combine_lists([X|R], [Y|S], [X|T]) :-
    X @< Y,
    !,
    combine_lists(R,[Y|S],T).
combine_lists(R, [Y|S], [Y|T]) :-
    combine_lists(R,S,T).

% intersect_lists(L1, L2, L3):
% L1 and L2 are two sorted lists; L3 contains elements that are in only one of the lists.
% example: combine_lists_exclusive([2,3,5],[1,2,3],Res).
% ?- Res = [1,5]
combine_lists_exclusive([], [], []) :- !.
combine_lists_exclusive([], S, S) :- !.
combine_lists_exclusive(R, [], R) :- !.
combine_lists_exclusive([X|R], [Y|S], [X|T]) :-
    X @< Y,
    !,
    combine_lists_exclusive(R,[Y|S],T).
combine_lists_exclusive([X|R], [Y|S], [Y|T]) :-
    X @> Y,
    !,
    combine_lists_exclusive([X|R],S,T).
combine_lists_exclusive([_|R], [_|S], T) :-
    combine_lists_exclusive(R,S,T).

call_with_time_out(Goal) :- % used as posting some arithmetic constraints sometimes uses too much time
%   write(goal(Goal)), nl,
%   check_goal(Goal, Goal),
    time_out(Goal, 3000, Res),
%   time_out(call(Goal), 3000, Res),
    (Res = success -> true ; write(time_out(Goal)), nl, false). % TO REMOVE

check_goal(Goal, _) :-
    integer(Goal), !.

check_goal(Goal, InitialGoal) :-
    var(Goal), !,
    fd_min(Goal, Min),
    fd_max(Goal, Max),
    (memberchk(Min, [inf,sup]) -> write('1: '), write(InitialGoal), nl, write(Goal), nl, halt ;
     memberchk(Max, [inf,sup]) -> write('2: '), write(InitialGoal), nl, write(Goal), nl, halt ;
                                  true                                                        ),
    Range is Max-Min,
    (Range > 200 -> write('3: '), write(InitialGoal), nl, write(Goal), nl, halt ; true).

check_goal(Goal, InitialGoal) :-
    functor(Goal, Functor, Arity),
    (Functor = element -> A is Arity - 1 ; A = Arity),
    check_goal(A, Goal, InitialGoal).

check_goal(0, _, _) :- !.
check_goal(I, Goal, InitialGoal) :-
    I > 0,
    arg(I, Goal, ArgI),
    check_goal(ArgI, InitialGoal),
    I1 is I-1,
    check_goal(I1, Goal, InitialGoal).

group_same_keys([], []) :- !.
group_same_keys([K-V|R], [K-[V|G]|S]) :-
    group_same_keys1(R, K, G, Rest),
    group_same_keys(Rest, S).

group_same_keys1([K-V|R], K, [V|S], T) :- !,
    group_same_keys1(R, K, S, T).
group_same_keys1(L, _, [], L).

% Merges two lists but unlike append the order doesn't matter, so List1 elements would be present in reverse order within List3.
% Has better performance for cases where the exact order of elements between two lists doesn't matter
% Example of a call:
% :- merge_lists(List1, List2, List3). 
merge_lists([], List2, List2) :- !.
merge_lists([E|R], List2, List3) :-
    merge_lists(R, [E|List2], List3).

length_prefix([V|R], V, L, PrefixLength) :- !,
    NextL is L+1,
    length_prefix(R, V, NextL, PrefixLength).
length_prefix(_, _, PrefixLength, PrefixLength). 

remove_first_elem([Elem|R], Elem, S) :- !,
    remove_first_elem(R, Elem, S).
remove_first_elem(L, _, L).

remove_consecutive_identical_entries([], []) :- !.
remove_consecutive_identical_entries([E], [E]) :- !.
remove_consecutive_identical_entries([E,E|R], S) :- !,
    remove_consecutive_identical_entries([E|R], S).
remove_consecutive_identical_entries([E,F|R], [E|S]) :-
    remove_consecutive_identical_entries([F|R], S).

sum_squares(List, SumSquares) :-
    sum_squares(List, 0, SumSquares).

sum_squares([], SumSquares, SumSquares) :- !.
sum_squares([V|R], SumSquaresPrev, SumSquares) :-
    SumSquaresCur is SumSquaresPrev + V*V,
    sum_squares(R, SumSquaresCur, SumSquares).

get_non_zero_exponents([], _, []) :- !.
get_non_zero_exponents([D|R], I, Res) :-
    I1 is I+1,
    (D = 0 -> get_non_zero_exponents(R, I1, Res)             ;
              get_non_zero_exponents(R, I1, S), Res = [I-D|S]).

% given a list of entries and a list of distinct values returns a list of pairs Value-Count,
% where the Value is one of the provided distinct values and Count is the number of occurences of
% of this Value in the list of entries
count_distinct_vals([], DistinctVals, CountsVals) :-
    !,
    (foreach(Val, DistinctVals), foreach(Val-0,CountsVals)
    do
     true
    ).
count_distinct_vals([ValEntry|R], DistinctVals, CountsValsNew) :-
    count_distinct_vals(R, DistinctVals, CountsValsOld),
    (foreach(ValI-CountOld, CountsValsOld), foreach(ValI-CountNew, CountsValsNew), param(ValEntry)
    do
     (ValI = ValEntry ->
         CountNew is CountOld + 1
     ;
         CountNew is CountOld
     )
    ).

unify_variables([], _, _, _) :- !.
unify_variables([J|R], I, TermSource, TermSet) :-
    arg(J, TermSource, Var),
    arg(I, TermSet,    Var),
    I1 is I+1,
    unify_variables(R, I1, TermSource, TermSet).

replace_recursively_atoms_by_vars(Term, _, Term) :-
    integer(Term), !.
replace_recursively_atoms_by_vars(Term, AtomsVars, Var) :-
    memberchk(Term-Var, AtomsVars), !.
replace_recursively_atoms_by_vars(Term, AtomsVars, NewTerm) :-
    functor(Term, Functor, Arity),
    functor(NewTerm, Functor, Arity),
    replace_recursively_atoms_by_vars(1, Arity, Term, AtomsVars, NewTerm).

replace_recursively_atoms_by_vars(I, Arity, _, _, _) :-
    I > Arity, !.
replace_recursively_atoms_by_vars(I, Arity, Term, AtomsVars, NewTerm) :-
    I =< Arity,
    arg(I, Term, ArgI),
    replace_recursively_atoms_by_vars(ArgI, AtomsVars, NewArgI),
    arg(I, NewTerm, NewArgI),
    NextI is I+1,
    replace_recursively_atoms_by_vars(NextI, Arity, Term, AtomsVars, NewTerm).

extract_nb_distinct_atoms(Term, NbDistinctAtoms) :-
    extract_atoms(Term, List),
    sort(List, SortedList),
    length(SortedList, NbDistinctAtoms).

extract_atoms([], []) :- !.
extract_atoms(Term, []) :- var(Term), !.
extract_atoms(Term, []) :- integer(Term), !.
extract_atoms(Term, [Term]) :- atom(Term), !.
extract_atoms(Term, L) :- functor(Term, _, Arity), extract_atoms(1, Arity, Term, L).

extract_atoms(I, N, _, []) :-
    I > N, !.
extract_atoms(I, N, Term, Result) :-
    I =< N,
    arg(I, Term, ArgI),
    extract_atoms(ArgI, Atoms),
    I1 is I+1,
    extract_atoms(I1, N, Term, RestAtoms),
    append(Atoms, RestAtoms, Result).

atom_not_in_term(Atom, Term) :-
    extract_atoms(Term, List),
    \+ memberchk(Atom, List).

collect_temporal_attrs(K, N, _, _, _, _, _, _, _, []) :-
    K > N, !.
collect_temporal_attrs(K, N, I, J, TableName, Names, Types, Equals, Cmps, TempAttrs) :-
    K =< N,                                                       % scan the different columns of the current table and check
    arg(K, Names,  Name),                                         % whether each column K could potential be part of a generalised
    arg(K, Types,  Type),                                         % precedence constraint or not
    arg(K, Equals, Equal),
    arg(K, Cmps,   Cmp),
    K1 is K+1,
    (K = I ->                                                     % a column corresponding to a supposed primary key is discarded
        collect_temporal_attrs(K1,N,I,J,TableName,Names,Types,Equals,Cmps,TempAttrs)
    ;
     K = J ->                                                     % a column corresponding to a potential successor arc is discarded
        collect_temporal_attrs(K1,N,I,J,TableName,Names,Types,Equals,Cmps,TempAttrs)
    ;
     Type = set ->                                                % a column corresponding to sets of values is discarded
        collect_temporal_attrs(K1,N,I,J,TableName,Names,Types,Equals,Cmps,TempAttrs)
    ;
     (Type = cst,                                                 % a column corresponding to cst values and not a start, duration, end attribute is discarded
      \+ check_if_name_corresponds_to_scheduling_attr(Name)) ->                                                
        collect_temporal_attrs(K1,N,I,J,TableName,Names,Types,Equals,Cmps,TempAttrs)
    ;
     Equal > 0 ->                                                 % a column that is equal to a previous column is discarded
        collect_temporal_attrs(K1,N,I,J,TableName,Names,Types,Equals,Cmps,TempAttrs)
    ;
     memberchk(within(col(TableName,K),col(TableName,_)), Cmp) -> % a column whose values are in some other columns is discarded
        collect_temporal_attrs(K1,N,I,J,TableName,Names,Types,Equals,Cmps,TempAttrs)
    ;
        collect_temporal_attrs(K1,N,I,J,TableName,Names,Types,Equals,Cmps,TAttrs),
        TempAttrs = [K|TAttrs]                                    % otherwise K is considered as a potential temporal attribute, i.e.
    ).                                                            % an attribute that may occur in a generalised precedence constraint

% complement a sorted list of intervals, e.g. "?- complement_intervals([1-1,2-2,10-20], L)." returns "L = [-1152921504606846976-0,3-9,21-1152921504606846975]"
complement_intervals([], [MinInt,MaxInt]) :- !,
    prolog_flag(min_tagged_integer, MinInt),
    prolog_flag(max_tagged_integer, MaxInt).
complement_intervals([Low-Up|R], [MinInt-Low1|S]) :-
    prolog_flag(min_tagged_integer, MinInt),
    Low1 is Low-1,
    complement(R, Up, S).

complement([], Up, [Up1-MaxInt]) :- !,
    prolog_flag(max_tagged_integer, MaxInt),
    Up1 is Up+1.
complement([Low-Up|R], PrevUp, Result) :-
    PrevUp1 is PrevUp+1,
    Low1 is Low-1,
    (PrevUp1 =< Low1 -> Result = [PrevUp1-Low1|S] ; Result = S),
    complement(R, Up, S).

call_imply_cond_ctr(Var, Val, Ctr) :-
    integer(Var), !,
    (Var = Val -> call(Ctr) ; true).
call_imply_cond_ctr(Var, Val, Ctr) :- % catch reified constraint posted from gen_bool_formula (should generate a prefix tree if not lazy)
    functor(Ctr, table, 2),           % to rather generate an mdd constraint
    arg(1, Ctr, [Tuple]),
    arg(2, Ctr, [[-1,0],[-1,1],[0,0],[1,1]]), !,
    Val_1 is Val-1,
    Val1  is Val+1,
    MinusOne = -1,
    MDD = [node(0, X1, [(inf..Val_1)-1,(Val..Val)-3,(Val1..sup)-1]),
           node(1, X2, [(inf..sup)-2]),
           node(2, X3, [inf..sup]),
           node(3, X2, [(MinusOne..MinusOne)-4,(0..0)-5,(1..1)-6]),
           node(4, X3, [0..1]),
           node(5, X3, [0..0]),
           node(6, X3, [1..1])],
    case([X1,X2,X3], [[Var|Tuple]], MDD).
call_imply_cond_ctr(Var, Val, Ctr) :- % catch reified constraint posted from gen_bool_formula (should generate a prefix tree if not lazy)
    functor(Ctr, table, 2),           % to rather generate an mdd constraint
    arg(1, Ctr, [Tuple]),
    arg(2, Ctr, [[-1,0],[-1,1],[0,1],[1,0]]), !,
    Val_1 is Val-1,
    Val1  is Val+1,
    MinusOne = -1,
    MDD = [node(0, X1, [(inf..Val_1)-1,(Val..Val)-3,(Val1..sup)-1]),
           node(1, X2, [(inf..sup)-2]),
           node(2, X3, [inf..sup]),
           node(3, X2, [(MinusOne..MinusOne)-4,(0..0)-5,(1..1)-6]),
           node(4, X3, [0..1]),
           node(5, X3, [1..1]),
           node(6, X3, [0..0])],
    case([X1,X2,X3], [[Var|Tuple]], MDD).
call_imply_cond_ctr(Var, Val, Ctr) :-
    call((Var #= Val) #=> Ctr).

pk_project(Set, N, TableName, Projected) :-
    length(Set, M),
    findall(TermSet,
            (functor(TermSource, TableName, N),
             functor(TermSet, t, M),
             unify_variables(Set, 1, TermSource, TermSet),
             call(user:TermSource)),
            Projected).

restrict_pks_candidates([], _, []) :- !.
restrict_pks_candidates([Candidate|R], FoundPk, S) :-
    subseq0(Candidate, FoundPk),
    !,
    restrict_pks_candidates(R, FoundPk, S).
restrict_pks_candidates([Candidate|R], FoundPk, [Candidate|S]) :-
    restrict_pks_candidates(R, FoundPk, S).

fd_same_key([Key-_,Key-_|_]) :- !.
fd_same_key([_|R]) :-
    fd_same_key(R).

build_source_target_extraction_terms([], _, []) :- !.
build_source_target_extraction_terms([col(_,J)|R], SourceTerm, [Var|S]) :-
    arg(J, SourceTerm, Var),
    build_source_target_extraction_terms(R, SourceTerm, S).

get_table_entries(InputParams, col(Table,OutputCol), ParamsOutput, TableEntries) :-
    tab_get_arity(col(Table,_), Arity),
    append(InputParams, [col(Table,OutputCol)], ParamsOutput), % add just after the parameters of the formula the formula output
    length(ParamsOutput, LenTarget),                                        % number of parameters + 1
    length(TargetTerm, LenTarget),                                          % create a target term and a source term in order to extract
    functor(SourceTerm, Table, Arity),                                      % the projection of the table wrt the input parameters and the output
    build_source_target_extraction_terms(ParamsOutput, SourceTerm, TargetTerm),
    findall(TargetTerm, call(user:SourceTerm),                              % WARNING: data/KindCombi/metadata files are read from main program (use user:)
                        TableEntries).                                      % get projection of the table (will search a formula valid for all entries)


remove_last_elem([Last], [], Last) :- !.
remove_last_elem([X,Y|R], [X|S], Last) :-
    remove_last_elem([Y|R], S, Last).


% Two use cases: 
% - given the list of indices and the list obtain the corresponding subset:
%   :- match_list_indices_sublist([a1, a2, a3, a4, a5], [1, 3, 4], Subset).
%   Subset = [a1, a3, a4]
% - given the subset and the list obtain the corresponding indices:
%   :- match_list_indices_sublist([a1, a2, a3, a4, a5], [1, 3, 4], [a1, a3, a4]).
%   Indices = [1, 3, 4]
% IMPORTANT: The list List must not contain duplicates
match_list_indices_sublist(List, Indices, Subset):-
    (foreach(I, Indices), foreach(Val, Subset), param(List)
    do
     nth1(I, List, Val)
    ).

simplify_term(Term, TermNew) :-
    var(Term), !,
    TermNew = Term.
simplify_term(Term, TermNew) :-
    atom(Term), !,
    TermNew = Term.
simplify_term(Term, TermNew) :-
    Term = abs(Term1), !,
    simplify_term(Term1, Term1New),
    (integer(Term1New) ->
         (Term1New = 0 -> TermNew is 0         ;
          Term1New < 0 -> TermNew is -Term1New ;
                          TermNew is  Term1New )
    ;
         TermNew = Term1New
    ).
simplify_term(Term, TermNew) :-
    functor(Term, Name, 2), !,
    arg(1, Term, Term1),
    arg(2, Term, Term2),
    simplify_term(Term1, Term1New),
    simplify_term(Term2, Term2New),
    (integer(Term1New) ->
         ((Term1New = 0, Name = plus) ->
              CheckTerm2 is 0,
              TermNew = Term2New
         ;
          (Term1New = 0, Name = prod)  ->
              CheckTerm2 is 0,
              TermNew is 0
         ;
          (Term1New = 1, Name = prod)  ->
              CheckTerm2 is 0,
              TermNew = Term2New
         ;
          (Term1New = 0, Name = power) ->
              CheckTerm2 is 0,
              TermNew is 0
         ;
          (Term1New = 1, Name = power) ->
              CheckTerm2 is 0,
              TermNew is 1
         ;
          (Term1New = 0, Name = mod)   ->
              CheckTerm2 is 0,
              TermNew is 0
         ;
              CheckTerm2 is 1
         )
    ;
         CheckTerm2 is 1
    ),
    (CheckTerm2 = 0 ->
         true
    ;
     integer(Term2New) ->
         ((Term2New = 0, Name = plus) ->
              TermNew = Term1New
         ;
          (Term2New < 0, Name = plus) ->
              Term2Neg is -Term2New,
              TermNew = minus(Term1New,Term2Neg)
         ;
          (Term2New = 0, Name = prod)  ->
              TermNew is 0
         ;
          (Term2New = 1, Name = prod)  ->
              TermNew = Term1New
         ;
          (Term2New = 0, Name = power) ->
              TermNew is 1
         ;
          (Term2New = 1, Name = power) ->
              TermNew = Term1New
         ;
          (Term2New = 1, Name = floor) ->
              TermNew = Term1New
         ;
          (Term2New = 1, Name = ceil)  ->
              TermNew = Term1New
         ;
          (Term2New = 1, Name = mod)   ->
              TermNew is 0
         ;
              functor(TermNew, Name, 2),
              arg(1, TermNew, Term1New),
              arg(2, TermNew, Term2New)
         )
    ;
         functor(TermNew, Name, 2),
         arg(1, TermNew, Term1New),
         arg(2, TermNew, Term2New)
    ).

% rewrite in a new way: first simplify subterms, then check the type of the term itself ...
simplify_term(Term, TermNew):-
    functor(Term,    TermName, N),
    functor(TermNew, TermName, N),
    (for(I, 1, N), param(Term, TermNew)
    do
     arg(I, Term, TermI),
     simplify_term(TermI, TermI1),
     arg(I, TermNew, TermI1)
    ).
