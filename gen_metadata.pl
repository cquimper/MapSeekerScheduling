% Purpose: GENERATE THE METADATA ASSOCIATED WITH THE DIFFERENT TABLES (use the file tables.pl to get the names of the tables)
% Authors : Nicolas Beldiceanu, IMT Atlantique
%           Ramiz Gindullin,    IMT Atlantique (primary & foreign keys)

% Phase 1 (valid both for the bound seeker and for the model seeker):
% call top(KindCombi,MaxN) for the bound seeker where
%  . KindCombi is the type of combinatorial object we considre,
%  . MaxN      is the maximum number of elements we consider in the combinatorial objects
% call top(model_seeker,0) for the model seeker
%
% Phase 2 (only valid for the model seeker, used to complete the metadata computed in Phase 1 with the foreign keys):
% call top(model_seeker,-1)

:- use_module(library(timeout)).
:- use_module(library(lists)).
:- use_module(library(samsort)).
:- use_module(library(clpfd)).
:- use_module(utility).
:- use_module(tables).
:- use_module(table_access).
:- use_module(gen_candidate_tables).
:- use_module(rank_fd).
:- use_module(gen_clusters_val_fds).
:- use_module(data_monotonicity).
:- use_module(test_if_scheduling_attribute). % NEW

top(0) :- % for the model seeker only, i.e. not for combinatorial objects
    top(0, 1, 1).

top(1) :- % for the model seeker only, i.e. not for combinatorial objects, when we don't search for foreign keys,
          % i,e, we assume that all tables has 'no_fk' option assigned to them
    top(1, 1, 1).

top(KindCombi, MaxNbVertices) :-
    top(KindCombi, MaxNbVertices, 1, 1).

top(0, SplitDiv, SplitMod) :- % for the model seeker only, i.e. not for combinatorial objects
    top(model_seeker, 0, SplitDiv, SplitMod), top(model_seeker, -1, SplitDiv, SplitMod).

top(1, SplitDiv, SplitMod) :- % for the model seeker only, i.e. not for combinatorial objects
    top(model_seeker, 0, SplitDiv, SplitMod), top(model_seeker, -2, SplitDiv, SplitMod).

% Each table has the following five arguments:
%  . the name of the table
%  . the value of MaxN (i.e. the maximum number of elements) used to generate the table
%  . the number of columns of the table
%  . the number of parameters of the lower/upper bound
%  . the lower/upper bound considered in case of combinatorial objects, or
%    a list of following options in case of the model seeker (used to control gen_metadata.pl):
%     (a) no_pk:                   do not calculate the primary key for that table (use essentially for debugging purpose)
%     (b) no_fk:                   if no relation between tables (e.g. when using to individual tables)
%     (c) pk(ListOfColumnIndices): used if know the preselected primary key (then do not compute it)
%     TODO: think of a good format to pass a precomputed foreign key

% MaxNbVertices > 0 if generates metadata for the a family of combinatorial objects (generate metadata for size 2 up to MaxNbVertices)
% MaxNbVertices = 0 if generates metadata for a standalone file (generate metadata for one single file)
% KindCombi in {graph, forest, forest0, tree, partition, partition0, group, cgroup}
top(KindCombi, MaxNbVertices, SplitDiv, SplitMod) :-
    MaxNbVertices >= 0,
    get_tables(KindCombi, _, _, _, Tables),                % enumerate first on tables, then of sizes to benefit from the incremental computation of fd
    sort(Tables, SortedTables),                            % remove duplicates !!!
    %member(Functor, SortedTables),
    member_split(SortedTables, SplitDiv, SplitMod, Functor),
    retractall(prev_table_meta_data(_)),                   % as start computing metadata for a new table destroy previous recorded metadata
    (MaxNbVertices > 0 ->                                  % used in the context of combinatorial object
        MaxN in 2..MaxNbVertices,
        indomain(MaxN)
    ;                                                      % used in the context of one single file
        MaxN = 0
    ),
    number_codes(MaxN, CodesMaxN),                         % convert MaxN to an atom in order to create
    atom_codes(AtomMaxN, CodesMaxN),                       % the name of the subfolder that will contain
    atoms_concat(['data/',KindCombi,'/data',AtomMaxN,'/'], % all metadata files
                 PrefixName),
    get_metadata_attributes(IndexNames),
    gen_metadata(KindCombi, Functor, MaxN, IndexNames, PrefixName, MaxNbVertices),
    false.

% MaxNbVertices = -1: used only to compute the foreign keys information and the ranked functional dependencies
top(KindCombi, -1, SplitDiv, SplitMod) :- !,
    write('Start the second step'),nl,
    get_tables(KindCombi, _, _, _, Tables),                     % get all table names
    sort(Tables, SortedTables),                                 % remove duplicates !!!
    findall(Table,
            member_split(SortedTables, SplitDiv, SplitMod, Table),
            TablesSplit),
    write('get all tables names'),nl,
    read_all_data_and_meta_data_files(TablesSplit, KindCombi),  % read data/metadata files for all tables (as need all files for computing fks)
    write('read all files'),nl,
    gen_all_fks(TablesSplit, KindCombi, TablesSplit).           % generate foreign keys: replace metadata facts and rewrite metadata files
% In case when we have a large amount of tables but they don't have foreign keys to search for (they all have 'no_fk' option),
% i.e., we can just load tables one by one instead of loading them all at once
top(KindCombi, -2, SplitDiv, SplitMod) :- !,
    write('Start the second step'),nl,
    get_tables(KindCombi, _, _, _, Tables),                     % get all table names
    sort(Tables, SortedTables),                                 % remove duplicates !!!
    findall(Table,
            member_split(SortedTables, SplitDiv, SplitMod, Table),
            TablesSplit),
    write('get all tables names'),nl,
    (foreach(TableC, TablesSplit), param(KindCombi)
    do
     read_all_data_and_meta_data_files([TableC], KindCombi),
     gen_all_fks([TableC], KindCombi, [TableC])
    ).
top(_, _, _, _).

gen_metadata(KindCombi, Functor, MaxN, IndexNames, PrefixName, MaxNbVertices) :-
    atom_concat(PrefixName, Functor, PrefixNameFunctor),
    atom_concat(PrefixNameFunctor, '.pl', ConsultFile),
    max_size_combi(KindCombi, MaxMaxN),
   (MaxN = MaxMaxN ->
%       write(ConsultFile), nl,
        true
    ;
        true
    ),
    consult(ConsultFile),
%   statistics(runtime, [T0|_]),
    build_meta_data(KindCombi, MaxN, MaxNbVertices, IndexNames, Functor, TableMetaData),
    (MaxN = MaxMaxN ->
%       statistics(runtime, [T1|_]), Time is T1-T0, write(time(Time)), nl,
        true
    ;
        true
    ),
    atom_concat(PrefixNameFunctor, '_metadata.pl', PrologFile),
    get_metadata_arity(Arity),
    write_metadata_file(PrologFile, Arity, TableMetaData),
    signature(Functor, _, Args),
    functor(Args, t, NbColumns),
    functor(Term, Functor, NbColumns),
    retractall(signature(_,_,_)),
    retractall(Term).

write_metadata_file(PrologFile, Arity, TableMetaData) :- % create a metadata file with the metadata fact we just compute
    open(PrologFile, write, Sout),
    number_codes(Arity, CodesArity),                     % convert Arity to an atom in order to create
    atom_codes(AtomArity, CodesArity),                   % the name of the metadata predicate
    format(Sout,':- multifile table_metadata/~a.~n',[AtomArity]),
    format(Sout,':- dynamic table_metadata/~a.~n~n',[AtomArity]),
    format(Sout,'~w.~n',[TableMetaData]),
    close(Sout).

build_meta_data(KindCombi, MaxN, MaxNbVertices, IndexNames, TableName, TableMetaData) :-
    signature(TableName, MaxN, Args),
    functor(Args,     t,      NbColumns),
    get_table_size(TableName, NbColumns, NbRows),
    functor(ColNames,       name,           NbColumns),
    functor(Kinds,          kind,           NbColumns),
    functor(InOuts,         inout,          NbColumns),
    functor(Mins,           min,            NbColumns),
    functor(Maxs,           max,            NbColumns),
    functor(Types,          types,          NbColumns),
    functor(NVals,          nval,           NbColumns),
    functor(Medians,        median,         NbColumns),
    functor(Gcds,           gcd,            NbColumns),
    functor(Sums,           sum,            NbColumns),
    functor(Means,          mean,           NbColumns),
    functor(Equals,         equal,          NbColumns),
    functor(Fds,            fd,             NbColumns),
    functor(NbFds,          nb_fd,          NbColumns),
    functor(Cmps,           cmp,            NbColumns),
    functor(NbCmps,         nb_cmp,         NbColumns),
    functor(Ctrs,           ctr,            NbColumns),
    functor(MaxOccs,        maxocc,         NbColumns),
    functor(MaxOccVals,     maxoccval,      NbColumns),
    functor(Affinities,     affinity,       NbColumns),
    functor(DistinctVals,   distinct_vals,  NbColumns),
    functor(ValsFds,        vals_fds,       NbColumns),
    functor(Monotonicities, monotonicities, NbColumns),
    MaxDistinctVals = 4,                                % when =< MaxDistinctVals distinct values will compute the list of distinct values
    get_nb_primary_and_input(1, NbColumns, Args, 0, NbPrimaryInput), % no set in primary (only integers)
    (KindCombi \== model_seeker ->
        get_bound_type_and_name(1, NbColumns, Args, BoundTypeName),
        write_table_fact_for_tables_put_pairs_together(TableName, MaxN, NbColumns, NbPrimaryInput, BoundTypeName)
    ;
        true
    ),
    gen_col_names(1, NbColumns, Args, ColNames),
    gen_kinds(1, NbColumns, Args, Kinds),
    gen_inouts(1, NbColumns, Args, InOuts),
    gen_mins(1, NbColumns, TableName, Mins),
    gen_maxs(1, NbColumns, TableName, Maxs),
    gen_types(1, NbColumns, TableName, Mins, Maxs, Types),
    gen_nvals(1, NbColumns, TableName, Types, MaxDistinctVals, NVals, DistinctVals),
    gen_medians_maxoccs_maxoccvals(1, NbColumns, TableName, Types, Medians, MaxOccs, MaxOccVals),
    gen_gcds(1, NbColumns, TableName, Types, Gcds),
    gen_sums(1, NbColumns, TableName, Types, Sums),
    gen_means(1, NbColumns, NbRows, Types, Sums, Means),
    gen_equals(1, NbColumns, TableName, Types, Sums, Equals),
    gen_fds(KindCombi, MaxN, IndexNames, NbColumns, TableName, Kinds, InOuts, Types, Equals, NbPrimaryInput, DistinctVals, Fds, ValsFds),
    gen_nb_fds(1, NbColumns, TableName, Fds, NbFds),
    gen_cmps(NbColumns, TableName, Types, Equals, Cmps),
    gen_nb_cmps(1, NbColumns, TableName, Cmps, NbCmps),
    gen_ctrs(1, NbColumns, MaxN, TableName, ColNames, Kinds, Types, Equals, Cmps, Ctrs), % NEW
% write(Ctrs), nl,
    (MaxN = 0 -> % generate primary keys only for the model seeker (but not for the Bound seeker as not used and can take a lot of time)
        get_table(KindCombi, 0, _, Options, TableName),
        ((memberchk(no_pk, Options) ; memberchk(pk([]), Options)) -> Pks = []                            ;
          memberchk(pk(Pk), Options)                              -> Pks = [1-Pk]                        ;
                                                                     gen_pks(TableName,
                                                                             NbRows, NbColumns, Types,
                                                                             Mins, Maxs,
                                                                             NVals, Equals, MaxOccs, Pks))
    ;
        true
    ),
    (MaxN = 0 ->  % MaxN = 0 (Assistant): 
        true      %  . foreign key computed in a second phase
    ;             % MaxN > 0 (Bound seeker):
        Pks = [], %  . no primary key
        Fks = []  %  . no foreign key
    ),
    gen_affinities(1, NbColumns, NbRows, Kinds, Types, Equals, TableName, Affinities),
    gen_monotonicities(1, NbColumns, TableName, KindCombi, MaxN, MaxNbVertices, Kinds, Monotonicities),
    gen_inf_sup(Mins, Maxs, InfSup),
    (MaxN = 0 -> true ; RankedFds = []), % MaxN = 0 (Assistant): RankedFds computed in a second phase. MaxN > 0 (Bound seeker): no ranked fd
    get_metadata_arity(Arity),
    functor(TableMetaData, table_metadata, Arity),
    memberchk(IndTableName-table_name,          IndexNames), arg(IndTableName,      TableMetaData, TableName),
    memberchk(IndMaxN-max_n,                    IndexNames), arg(IndMaxN,           TableMetaData, MaxN),
    memberchk(IndNbColumns-nb_columns,          IndexNames), arg(IndNbColumns,      TableMetaData, NbColumns),
    memberchk(IndNbRows-nb_rows,                IndexNames), arg(IndNbRows,         TableMetaData, NbRows),
    memberchk(IndColNames-col_names,            IndexNames), arg(IndColNames,       TableMetaData, ColNames),
    memberchk(IndKinds-kinds,                   IndexNames), arg(IndKinds,          TableMetaData, Kinds),
    memberchk(IndInOuts-in_outs,                IndexNames), arg(IndInOuts,         TableMetaData, InOuts),
    memberchk(IndMins-mins,                     IndexNames), arg(IndMins,           TableMetaData, Mins),
    memberchk(IndMaxs-maxs,                     IndexNames), arg(IndMaxs,           TableMetaData, Maxs),
    memberchk(IndTypes-types,                   IndexNames), arg(IndTypes,          TableMetaData, Types),
    memberchk(IndNVals-nvals,                   IndexNames), arg(IndNVals,          TableMetaData, NVals),
    memberchk(IndMedians-medians,               IndexNames), arg(IndMedians,        TableMetaData, Medians),
    memberchk(IndGcds-gcds,                     IndexNames), arg(IndGcds,           TableMetaData, Gcds),
    memberchk(IndSums-sums,                     IndexNames), arg(IndSums,           TableMetaData, Sums),
    memberchk(IndMeans-means,                   IndexNames), arg(IndMeans,          TableMetaData, Means),
    memberchk(IndEquals-equals,                 IndexNames), arg(IndEquals,         TableMetaData, Equals),
    memberchk(IndFds-fds,                       IndexNames), arg(IndFds,            TableMetaData, Fds),
    memberchk(IndNbFds-nb_fds,                  IndexNames), arg(IndNbFds,          TableMetaData, NbFds),
    memberchk(IndCmps-cmps,                     IndexNames), arg(IndCmps,           TableMetaData, Cmps),
    memberchk(IndNbCmps-nb_cmps,                IndexNames), arg(IndNbCmps,         TableMetaData, NbCmps),
    memberchk(IndCtrs-ctrs,                     IndexNames), arg(IndCtrs,           TableMetaData, Ctrs),
    memberchk(IndMaxOccs-maxoccs,               IndexNames), arg(IndMaxOccs,        TableMetaData, MaxOccs),
    memberchk(IndMaxOccVals-maxoccvals,         IndexNames), arg(IndMaxOccVals,     TableMetaData, MaxOccVals),
    memberchk(IndPks-pks,                       IndexNames), arg(IndPks,            TableMetaData, Pks),
    memberchk(IndFks-fks,                       IndexNames), arg(IndFks,            TableMetaData, Fks),
    memberchk(IndAffinities-affinities,         IndexNames), arg(IndAffinities,     TableMetaData, Affinities),
    memberchk(IndInfSup-inf_sup,                IndexNames), arg(IndInfSup,         TableMetaData, InfSup),
    memberchk(IndRankedFds-ranked_fds,          IndexNames), arg(IndRankedFds,      TableMetaData, RankedFds),
    memberchk(IndDistinctVals-distinct_vals,    IndexNames), arg(IndDistinctVals,   TableMetaData, DistinctVals),
    memberchk(IndValsFds-vals_fds,              IndexNames), arg(IndValsFds,        TableMetaData, ValsFds),
    memberchk(IndMonotonicities-monotonicities, IndexNames), arg(IndMonotonicities, TableMetaData, Monotonicities),
    retractall(prev_table_meta_data(_)),
    assert(prev_table_meta_data(TableMetaData)),
    !.
build_meta_data(_, MaxN, _, _, TableName, _) :-
    write('could not build metadata for table '), write(TableName), write(' for MaxN='), write(MaxN), halt.

get_nb_primary_and_input(I, N, _, Res, Res) :-
    I > N, !.
get_nb_primary_and_input(I, N, Args, Cpt, Res) :-
    I =< N,
    (arg(I, Args, t(_,primary,input)) -> Cpt1 is Cpt+1 ; Cpt1 = Cpt),
    I1 is I+1,
    get_nb_primary_and_input(I1, N, Args, Cpt1, Res).

% assume called on a combinatorial object (as bounds only exist for combinatorial objects)
get_bound_type_and_name(I, N, Args, BoundTypeName) :-
    I =< N,
    I1 is I+1,
    (arg(I, Args, t(Name,low,output)) -> BoundTypeName = low(Name)                          ;
     arg(I, Args, t(Name, up,output)) -> BoundTypeName =  up(Name)                          ;
                                         get_bound_type_and_name(I1, N, Args, BoundTypeName)).

% write in the console a fact that one need to cup and paste in tables_put_pairs_together to generate the factorises tables part to update tables.pl
% when new characteristics or when bigger maximum size (only used in case of combinatorial object)
write_table_fact_for_tables_put_pairs_together(TableName, MaxN, NbColumns, NbPrimaryInput, BoundTypeName) :-
    write('table('),
    write(TableName),
    write(', '),
    write(MaxN),
    write(', '),
    write(NbColumns),
    write(', '),
    write(NbPrimaryInput),
    write(', '),
    write(BoundTypeName),
    write(').'),
    nl.

get_table_size(TableName, N, NbRows) :-
    findall(Val, (functor(Term,TableName,N), call(Term), arg(1, Term, Val)), Vals),
    length(Vals, NbRows).

gen_col_names(I, N, _, _) :-
    I > N, !.
gen_col_names(I, N, Args, ColNames) :-
    I =< N,
    arg(I, Args, t(Name,_,_)),
    arg(I, ColNames, Name),
    I1 is I+1,
    gen_col_names(I1, N, Args, ColNames).

gen_kinds(I, N, _, _) :-
    I > N, !.
gen_kinds(I, N, Args, Kinds) :-
    I =< N,
    arg(I, Args, t(_,Kind,_)),
    arg(I, Kinds, Kind),
    I1 is I+1,
    gen_kinds(I1, N, Args, Kinds).

gen_inouts(I, N, _, _) :-
    I > N, !.
gen_inouts(I, N, Args, InOuts) :-
    I =< N,
    arg(I, Args, t(_,_,InOut)),
    arg(I, InOuts, InOut),
    I1 is I+1,
    gen_inouts(I1, N, Args, InOuts).

gen_mins(I, N, _, _) :-
    I > N, !.
gen_mins(I, N, TableName, Mins) :-
    I =< N,                         % if on a set of sorted intervals then get smallest value of the set
    findall(Val, (functor(Term,TableName,N), call(Term), arg(I, Term, ValI), get_mins_value(ValI, Val)), Vals),
    min_member(Min, Vals),
    arg(I, Mins, Min),
    I1 is I+1,
    gen_mins(I1, N, TableName, Mins).

% if integer returns the integer, if set of sorted intervals return first value
get_mins_value(Val, Val) :-    % return integer
    integer(Val), !.
get_mins_value(Val-_, Val) :-  % return first value of the interval
    integer(Val), !.
get_mins_value(List, Val) :-   % return first value of first interval
    is_list(List), !,
    List = [First|_],
    (integer(First) -> Val = First                      ;
     First = Low-_  -> Val = Low                        ;
                       write(get_mins_value1), nl, halt).
get_mins_value(_, _) :-
    write(get_mins_value2), nl, halt.

gen_maxs(I, N, _, _) :-
    I > N, !.
gen_maxs(I, N, TableName, Maxs) :-
    I =< N,                         % if on a set of sorted intervals then get largest value of the set
    findall(Val, (functor(Term,TableName,N), call(Term), arg(I, Term, ValI), get_maxs_value(ValI, Val)), Vals),
    max_member(Max, Vals),
    arg(I, Maxs, Max),
    I1 is I+1,
    gen_maxs(I1, N, TableName, Maxs).

% if integer returns the integer, if set of sorted intervals return last value
get_maxs_value(Val, Val) :-    % return integer
    integer(Val), !.
get_maxs_value(_-Val, Val) :-  % return last value of the interval
    integer(Val), !.
get_maxs_value(List, Val) :-   % return last value of last interval
    is_list(List), !,
    last(List, Last),
    (integer(Last) -> Val = Last                      ;
     Last = _-Up   -> Val = Up                        ;
                      write(get_maxs_value1), nl, halt).
get_maxs_value(_, _) :-
    write(get_maxs_value2), nl, halt.

gen_types(I, N, _, _, _, _) :-
    I > N, !.
gen_types(I, N, TableName, Mins, Maxs, Types) :-
    I =< N,
    (check_if_set(TableName, N, I) ->
        Type = set
    ;
        arg(I, Mins, Min),
        arg(I, Maxs, Max),
        D is Max-Min,
        (D = 0              -> Type = cst  ;
         (Min = 0, Max = 1) -> Type = bool ;
                               Type = int  )
    ),
    arg(I, Types, Type),
    I1 is I+1,
    gen_types(I1, N, TableName, Mins, Maxs, Types).

% succeed if Ith column of the table TableName contains at least one entry corresponding to a list
check_if_set(TableName, N, I) :-
    functor(Term, TableName, N),    % create a term with the appropriate table name and arity
    call(Term),                     % get an entry of the table
    arg(I, Term, Col),              % get ith column of current table
    (is_list(Col) -> true ;         % if we are on a list then we identify a set
     Col = _-_    -> true ;         % if we are on an interval Low-Up then we identify a set
                     false),        % otherwise we will check next entries
    !.                              % as identify a set we stop scanning the next entries of the table

gen_nvals(I, N, _, _, _, _, _) :-
    I > N, !.
gen_nvals(I, N, TableName, Types, MaxDistinctVals, NVals, DistinctVals) :-
    I =< N,
    arg(I, Types, Type),            % get type of current column
    (Type = set ->                  % if on a column whose type is a set of values then
        arg(I, NVals, none),        %  . do not generate NVal
        arg(I, DistinctVals, [])    %  . do not generate DistinctVals
    ;
        findall(Val, (functor(Term,TableName,N), call(Term), arg(I, Term, Val)), Vals),
        sort(Vals, SortedVals),
        length(SortedVals, NVal),
        arg(I, NVals, NVal),
        (NVal =< MaxDistinctVals ->
            count_distinct_vals(Vals, SortedVals, SortedValsCounts),
            arg(I, DistinctVals, SortedValsCounts)
        ;
            arg(I, DistinctVals, [])
        )
    ),
    I1 is I+1,
    gen_nvals(I1, N, TableName, Types, MaxDistinctVals, NVals, DistinctVals).

gen_medians_maxoccs_maxoccvals(I, N, _, _, _, _, _) :-
    I > N, !.
gen_medians_maxoccs_maxoccvals(I, N, TableName, Types, Medians, MaxOccs, MaxOccVals) :-
    I =< N,
    arg(I, Types, Type),                                 % get type of current column
    (Type = set ->                                       % if on a column whose type is a set of values
        Median = none, MaxOcc = none, MaxOccVal = none   % then do not generate Median, MaxOcc and MaxOccVals
    ;
        findall(Val, (functor(Term,TableName,N), call(Term), arg(I, Term, Val)), Vals),
        samsort(Vals, SortedVals),
        length(SortedVals, M),
        S is M // 2 + 1,
        suffix_length(SortedVals, Suffix, S),
        Odd is M mod 2,
        (Odd = 1 ->
            Suffix = [Median|_]
        ;
            Suffix = [V1,V2|_], Median is (V1+V2) / 2
        ),
        gen_maxoccs_maxoccvals(SortedVals, MaxOcc, MaxOccVal)
    ),
    arg(I, Medians,    Median),
    arg(I, MaxOccs,    MaxOcc),
    arg(I, MaxOccVals, MaxOccVal),
    I1 is I+1,
    gen_medians_maxoccs_maxoccvals(I1, N, TableName, Types, Medians, MaxOccs, MaxOccVals).

gen_maxoccs_maxoccvals([], 0, _) :- !.
gen_maxoccs_maxoccvals([V|R], MaxOcc, MaxOccVal) :-
    gen_maxoccs_maxoccvals1(R, 1, V, 1, V, MaxOcc, MaxOccVal).

gen_maxoccs_maxoccvals1([], _, _, MaxOcc, MaxOccVal, MaxOcc, MaxOccVal) :- !.
gen_maxoccs_maxoccvals1([V|R], CurOcc, CurVal, CurMaxOcc, CurMaxOccVal, MaxOcc, MaxOccVal) :-
    (V = CurVal ->
        NextOcc is CurOcc+1,
        (NextOcc > CurMaxOcc ->
            NextCurMaxOcc    = NextOcc,
            NextCurMaxOccVal = V
        ;
            NextCurMaxOcc    = CurMaxOcc,
            NextCurMaxOccVal = CurMaxOccVal
        ),
        gen_maxoccs_maxoccvals1(R, NextOcc, CurVal, NextCurMaxOcc, NextCurMaxOccVal, MaxOcc, MaxOccVal)
    ;
        gen_maxoccs_maxoccvals1(R, 1, V, CurMaxOcc, CurMaxOccVal, MaxOcc, MaxOccVal)
    ).

gen_gcds(I, N, _, _, _) :-
    I > N, !.
gen_gcds(I, N, TableName, Types, Gcds) :-
    I =< N,
    arg(I, Types, Type),                                 % get type of current column
    (Type = set ->                                       % if on a column whose type is a set of values
        Gcd = none                                       % then do not generate gcd
    ;
        findall(AbsVal, (functor(Term,TableName,N), call(Term), arg(I, Term, Val), AbsVal is abs(Val)), AbsVals),
        gcd(AbsVals, Gcd)
    ),
    arg(I, Gcds, Gcd),
    I1 is I+1,
    gen_gcds(I1, N, TableName, Types, Gcds).

gen_sums(I, N, _, _, _) :-
    I > N, !.
gen_sums(I, N, TableName, Types, Sums) :-
    I =< N,
    arg(I, Types, Type),                                 % get type of current column
    (Type = set ->                                       % if on a column whose type is a set of values
        Sum = none                                       % then do not generate sum
    ;
        findall(Val, (functor(Term,TableName,N), call(Term), arg(I, Term, Val)), Vals),
        sumlist(Vals, Sum)
    ),
    arg(I, Sums, Sum),
    I1 is I+1,
    gen_sums(I1, N, TableName, Types, Sums).

gen_means(I, N, _, _, _, _) :-
    I > N, !.
gen_means(I, N, NbRows, Types, Sums, Means) :-
    I =< N,
    arg(I, Types, Type),                                 % get type of current column
    (Type = set ->                                       % if on a column whose type is a set of values
        Mean = none                                      % then do not generate mean
    ;
        arg(I, Sums, Sum),
        Mean is Sum / NbRows
    ),
    arg(I, Means, Mean),
    I1 is I+1,
    gen_means(I1, N, NbRows, Types, Sums, Means).

gen_equals(I, N, _, _, _, _) :-
    I > N, !.
gen_equals(I, N, TableName, Types, Sums, Equals) :-
    (I = 1 ->
        arg(I, Equals, 0)
    ;
     arg(I, Types, set) ->  % if on a column whose type is a set of values
        arg(I, Equals, 0)   % then do not compare with other columns
    ;
        I =< N,
        J is I-1,
        arg(I, Sums, SumI),
        gen_equals1(J, I, N, TableName, Sums, SumI, 0, Equals)
    ),
    I1 is I+1,
    gen_equals(I1, N, TableName, Types, Sums, Equals).

gen_equals1(0, I, _, _, _, _, _, Equals) :- !,
    arg(I, Equals, 0).
gen_equals1(J, I, N, TableName, Sums, SumI, ValsI, Equals) :-
    J >= 1,
    J1 is J-1,
    arg(J, Sums, SumJ),
    (SumI = SumJ ->
        (ValsI = 0 ->
            findall(ValI, (functor(TermI,TableName,N), call(TermI), arg(I,TermI,ValI)), NewValsI)
        ;
            NewValsI = ValsI
        ),
        findall(ValJ, (functor(TermJ,TableName,N), call(TermJ), arg(J,TermJ,ValJ)), ValsJ),
        (NewValsI = ValsJ -> arg(I, Equals, J) ; gen_equals1(J1, I, N, TableName, Sums, SumI, NewValsI, Equals))
    ;
        gen_equals1(J1, I, N, TableName, Sums, SumI, NewValsI, Equals)
    ).

% search for all functional dependencies whose size is not strictly greater than the size of the smallest functional dependency + 1
gen_fds(KindCombi, MaxN, IndexNames, NbColumns, TableName, Kinds, InOuts, Types, Equals, NbPrimaryInput, DistinctVals, Fds, ValsFds) :-
    ((MaxN > 2, prev_table_meta_data(_)) ->
        prev_table_meta_data(PrevTableMetaData),
        memberchk(IndNbColumns-nb_columns, IndexNames), arg(IndNbColumns, PrevTableMetaData, PrevNbColumns),
        memberchk(IndEquals-equals,        IndexNames), arg(IndEquals,    PrevTableMetaData, PrevEquals),
        memberchk(IndFds-fds,              IndexNames), arg(IndFds,       PrevTableMetaData, PrevFds),
        ((NbColumns = PrevNbColumns, Equals = PrevEquals) -> IncrementalFd = 1 ; IncrementalFd = 0)
    ;
        IncrementalFd = 0
    ),
    gen_fds1(1, NbColumns, KindCombi, TableName, Kinds, InOuts, Types, Equals, NbPrimaryInput, MaxN, IncrementalFd, PrevFds, DistinctVals, Fds, ValsFds).

gen_fds1(I, N, _, _, _, _, _, _, _, _, _, _, _, _, _) :-
    I > N, !.
gen_fds1(I, N, KindCombi, TableName, Kinds, InOuts, Types, Equals, NbPrimaryInput, MaxN, IncrementalFd, PrevFds, DistinctVals, Fds, ValsFds) :-
    I =< N,
    arg(I, Kinds,  Kind),
    arg(I, InOuts, InOut),
    arg(I, Types,  Type),
    arg(I, Equals, Equal),
    ( Type  = set                     -> arg(I, Fds, []), arg(I, ValsFds, []) ;
     (Kind  = primary, InOut = input) -> arg(I, Fds, []), arg(I, ValsFds, []) ;
     (Kind  = variable)               -> arg(I, Fds, []), arg(I, ValsFds, []) ;
      Type  = cst                     -> arg(I, Fds, []), arg(I, ValsFds, []) ;
      Equal > 0                       -> arg(I, Fds, []), arg(I, ValsFds, []) ;
        (KindCombi \== model_seeker ->
            findall(J, (J in 1..N, J #\= I, indomain(J), arg(J,Kinds,primary),   arg(J,Equals,0)), SourcesP),
            findall(J, (J in 1..N, J #\= I, indomain(J), arg(J,Kinds,secondary), arg(J,Equals,0)), SourcesS),
            CardMax is min(NbPrimaryInput+1,10),
            (NbPrimaryInput >= 5 -> CardMax2 = 3 ; CardMax2 = 4)
        ;
            findall(J, (J in 1..N, J #\= I, indomain(J), arg(J,InOuts,input), arg(J,Equals,0)), SourcesP),
            SourcesS = [],
            CardMax is 5,
            CardMax2 is 5
        ),
        (IncrementalFd = 1 ->                                 % if try to find same functional dependencies as those found in previous step
            arg(I, PrevFds, PrevFounds),
            select_previous_fd(PrevFounds, PrevSetsSources),  % construct the sets of columns indices corresponding to previous functional dependencies
            (search_functional_dependencies(PrevSetsSources, I, N, MaxN, TableName, []-[], PrevFounds, _) ->
%                write('incremental succeed'), nl,
                arg(I, Fds, PrevFounds)                       % record the functional dependencies found at previous step
            ;                                                 % if could not retrieve previous functional dependencies then compute them from scratch
%                write('incremental failed'), nl,
                findall(Set, sublist_max_card(SourcesP,SourcesS,CardMax2,CardMax,Set), SetsSources),
                search_functional_dependencies(SetsSources, I, N, MaxN, TableName, []-[], Founds, _),
                arg(I, Fds, Founds)
%                write('after incremental failed, succeed from scratch'), nl
            )
        ;                                                     % if in non incremental mode then compute functional dependencies from scratch
%            write('non incremental'), nl,
            findall(Set, sublist_max_card(SourcesP,SourcesS,CardMax2,CardMax,Set), SetsSources),
            search_functional_dependencies(SetsSources, I, N, MaxN, TableName, []-[], Founds, _),
            arg(I, Fds, Founds)
%            write('succeed from scratch'), nl
        ),
        arg(I, DistinctVals, DistinctValCount),
        (DistinctValCount = [_,_,_|_] ->                      % used only if at least 3 distinct output values
            keys_and_values(DistinctValCount,DistinctVal,_),
            gen_all_element_subset_pairs(DistinctValCount, ElementSubsetPairs),
            gen_fds2(ElementSubsetPairs, SourcesP, SourcesS, CardMax2, CardMax, I, N, MaxN, TableName, AllFounds),
            two_levels_grouping_fds2(AllFounds, DistinctVal, TwoLevelsGrouping),
            (gen_clusters_val_fds(TableName, N, DistinctVal, TwoLevelsGrouping, ValFds) ->
                true
            ;
                write(gen_clusters_val_fds(TableName)), nl, halt
            ),
            arg(I, ValsFds, ValFds),
            max_size_combi(KindCombi, MaxMaxN),
            (MaxN = MaxMaxN ->
%               write('generate cluster facts for column '), write(I), write(' of table '), write(TableName), nl,
                true
            ;
                true
            )
        ;
            arg(I, ValsFds, [])
        )
    ),
    I1 is I+1,
    gen_fds1(I1, N, KindCombi, TableName, Kinds, InOuts, Types, Equals, NbPrimaryInput, MaxN, IncrementalFd, PrevFds, DistinctVals, Fds, ValsFds).

% We group conditional functional dependencies (which are in AllFounds)
%  . first on the cardinality of the set S0 (i.e. the number of values of DistinctVal which are considered as a 0),
%  . second on the value of set S1 (all sets S1 consist of a unique element corresponding to a value of DistinctVal which is considered as a 1).
%
% This allow one to retrieve all functional dependencies associated with a given level, and
% within a same level to retrieve all functional dependencies associated with a given value of S1
%
% cfd(S01,S02,...,S0m), where
%  . n   is length(DistinctVal)
%  . m   is n-1
%  . SOi is the set of conditional functional dependencies where value 0 aggregates i values of DistinctVal
%    SOi is a term of the form cfd_val(LT1,LT2,...,LTn)-AttrLabelFds, where
%    LTj is a list of element of the form t(S1,S0,Founds,AttrFds)
%      AttrLabelFds gives for every attributes occurring in the functional dependencies of LT1,LT2,...,LTn
%       the label of the functional dependencies where it occur.
%        
two_levels_grouping_fds2(AllFounds, DistinctVal, TwoLevelsGrouping) :-
    gen_keys_for_grouping(AllFounds, KeyAllFounds),
    length(DistinctVal, N),
    M is N-1,
    functor(TwoLevelsGrouping, cfd, M),
    two_levels_grouping_fds2a(1, M, N, DistinctVal, TwoLevelsGrouping, KeyAllFounds).

two_levels_grouping_fds2a(I, M, _, _, _, _) :-
    I > M,
    !.
two_levels_grouping_fds2a(I, M, N, DistinctVal, TwoLevelsGrouping, KeyAllFounds) :-
    I =< M,
    findall(key(I,K1)-t(S1,S0,Founds), member(key(I,K1)-t(S1,S0,Founds), KeyAllFounds), IKeyAllFounds),
    functor(CfdVal, cfd_val, N),
    arg(I, TwoLevelsGrouping, CfdVal-AttrLabelFds),
    two_levels_grouping_fds2b(DistinctVal, 1, CfdVal, IKeyAllFounds),
    two_levels_grouping_fds2c(N, CfdVal, AttrLabelFds),
    I1 is I+1,
    two_levels_grouping_fds2a(I1, M, N, DistinctVal, TwoLevelsGrouping, KeyAllFounds).

two_levels_grouping_fds2b([], _, _, _) :-
    !.
two_levels_grouping_fds2b([Val|R], J, CfdVal, IKeyAllFounds) :-
    findall(t(S1,S0,Founds), member(key(_,Val)-t(S1,S0,Founds), IKeyAllFounds), JFounds),
    arg(J, CfdVal, JFounds),
    J1 is J+1,
    two_levels_grouping_fds2b(R, J1, CfdVal, IKeyAllFounds).

two_levels_grouping_fds2c(N, CfdVal, GroupAttrLabelFd) :-
    two_levels_grouping_fds2c(1, N, 1, CfdVal, AttrLabelFdPairs),
    sort(AttrLabelFdPairs, SAttrLabelFdPairs),
    group_same_keys(SAttrLabelFdPairs, GroupAttrLabelFd).

two_levels_grouping_fds2c(I, N, _, _, []) :-
    I > N,
    !.
two_levels_grouping_fds2c(I, N, CurLabel, CfdVal, AttrLabelFds) :-
    I =< N,
    arg(I, CfdVal, ArgI),
    two_levels_grouping_fds2d(ArgI, CurLabel, NextLabel, AttrLabelFdsI),
    I1 is I+1,
    two_levels_grouping_fds2c(I1, N, NextLabel, CfdVal, AttrLabelFdsRest),
    append(AttrLabelFdsI, AttrLabelFdsRest, AttrLabelFds).
    
two_levels_grouping_fds2d([], CurLabel, CurLabel, []) :- !.
two_levels_grouping_fds2d([t(_,_,ListFds)|R], CurLabel, NextLabel, AttrLabelFds) :-
    two_levels_grouping_fds2e(ListFds, CurLabel, NewLabel, AttrListFds1),
    two_levels_grouping_fds2d(R, NewLabel, NextLabel, AttrListFds2),
    append(AttrListFds1, AttrListFds2, AttrLabelFds).

two_levels_grouping_fds2e([], CurLabel, CurLabel, []) :- !.
two_levels_grouping_fds2e([Fd|R], CurLabel, NextLabel, AttrLabelFds) :-
    two_levels_grouping_fds2f(Fd, CurLabel, AttrListFds1),
    NewLabel is CurLabel+1,
    two_levels_grouping_fds2e(R, NewLabel, NextLabel, AttrListFds2),
    append(AttrListFds1, AttrListFds2, AttrLabelFds).

two_levels_grouping_fds2f([], _, []) :- !.
two_levels_grouping_fds2f([col(_,Attr)|R], CurLabel, [Attr-CurLabel|S]) :-
    two_levels_grouping_fds2f(R, CurLabel, S).

gen_keys_for_grouping([], []) :- !.
gen_keys_for_grouping([t(S1,S0,Founds)|R], [key(K0,K1)-t(S1,S0,Founds)|S]) :-
    length(S0,K0),               % first  group on the cardinality of the set S0
    S1 = [K1],                   % second group on the value of the set S1
    gen_keys_for_grouping(R, S).

gen_fds2([], _, _, _, _, _, _, _, _, []) :- !.
gen_fds2([S1-S0|R], SourcesP, SourcesS, CardMax2, CardMax, I, N, MaxN, TableName, [t(S1,S0,Founds)|S]) :-
    findall(Set, sublist_max_card(SourcesP,SourcesS,CardMax2,CardMax,Set), SetsSources),
    search_functional_dependencies(SetsSources, I, N, MaxN, TableName, S1-S0, Founds, _),
    gen_fds2(R, SourcesP, SourcesS, CardMax2, CardMax, I, N, MaxN, TableName, S).

select_previous_fd([], []) :- !.
select_previous_fd([Fd|R], [FdCols|S]) :-
    select_previous_fd1(Fd, FdCols),
    select_previous_fd(R, S).

select_previous_fd1([], []) :- !.
select_previous_fd1([col(_,PrevCol)|R], [PrevCol|S]) :-
    select_previous_fd1(R, S).

% Limit is a limit on the number of parameters in a functional dependency;
% initially limit is not yet known as did not find the first fd
search_functional_dependencies([], _, _, _, _, _, [], _) :- !.
search_functional_dependencies([Set|R], Target, N, MaxN, TableName, Vals1-Vals0, Founds, Limit) :-
    (is_not_a_functional_dependency(Set, Target, N, TableName, Vals1-Vals0) ->
        NewR = R,
        Founds = RFounds
    ;
        findall(NewElem, (member(Elem,Set),functor(NewElem,col,2),arg(1,NewElem,TableName),arg(2,NewElem,Elem)), NewSet),
        Founds = [NewSet|RFounds],
        (integer(Limit) -> RR = R                                              ;  % if did already found a functional dependency do nothing
                           length(NewSet, LenNewSet),                             % if first time we found a functional dependency
                           (MaxN = 0 ->                                           % then remove from the candidates sets all sets whole cardinality
                               Limit is LenNewSet+3                               % try to catch more functional dependencies when one single table
                           ;                                                      % by setting Limit to a higher value
                               Limit is LenNewSet+1                               
                           ),
                           remove_list_whose_length_exceeds_limit(R, Limit, RR)), % exceed Limit
        remove_dominated_sets(RR, Set, NewR)
    ),
    search_functional_dependencies(NewR, Target, N, MaxN, TableName, Vals1-Vals0, RFounds, Limit).

is_not_a_functional_dependency(Set, Target, N, TableName, Vals1-Vals0) :- 
    fd_project(Set, Target, N, TableName, Vals1-Vals0, Projected),
    sort(Projected, SortedProjected),
    fd_same_key(SortedProjected).

% Compute the projection of table TableName on column Target.
%  Set        : set of variables corresponding to the different column we want to extract from table TableName
%  Target     : target column on which we project
%  N          : number of entries of table TableName,
%  TableName  : name of the table on which we do the projection
%  Vals1-Vals0: two empty list if standard projection, otherwise:
%                . Vals1 is the set of values of the target column that will be considered as value 1
%                . Vals0 is the set of values of the target column that will be considered as value 0
%  Projected  : the result of the projection of table TableName on column Target
fd_project(Set, Target, N, TableName, Vals1-Vals0, Projected) :-
    length(Set, M),
    findall(TermSet-VarTarget,
            (functor(TermSource, TableName, N),
             functor(TermSet, t, M),
             unify_variables(Set, 1, TermSource, TermSet),
             call(TermSource),
             arg(Target,TermSource,ValTarget),
             ((Vals1 = [], Vals0 = [])    -> VarTarget = ValTarget ;
              memberchk(ValTarget, Vals1) -> VarTarget = 1         ;
              memberchk(ValTarget, Vals0) -> VarTarget = 0         ;
                                             false                 )
             ),
            Projected).

remove_list_whose_length_exceeds_limit([], _, []) :- !.
remove_list_whose_length_exceeds_limit([List|R], Limit, [List|S]) :-
    length(List, Len),
    Len =< Limit,
    !,
    remove_list_whose_length_exceeds_limit(R, Limit, S).
remove_list_whose_length_exceeds_limit([_|R], Limit, S) :-
    remove_list_whose_length_exceeds_limit(R, Limit, S).

remove_dominated_sets([], _, []) :- !.
remove_dominated_sets([SetJ|R], SetI, S) :-
    set_is_include(SetI, SetJ),
    !,
    remove_dominated_sets(R, SetI, S).
remove_dominated_sets([SetJ|R], SetI, [SetJ|S]) :-
    remove_dominated_sets(R, SetI, S).

set_is_include([], _) :- !.
set_is_include([Elem|R], Set) :-
    memberchk(Elem, Set),
    set_is_include(R, Set).

gen_nb_fds(I, N, _, _, _) :-
    I > N, !.
gen_nb_fds(I, N, TableName, Fds, NbFds) :-
    I =< N,
    arg(I, Fds, Fd),
    length(Fd, NbFd),
    arg(I, NbFds, NbFd),
    I1 is I+1,
    gen_nb_fds(I1, N, TableName, Fds, NbFds).

gen_cmps(N, TableName, Types, Equals, Cmps) :-
    functor(Term, TableName, N),
    findall(Term, call(Term), Terms),
    findall(t(II,Ctr,JJ), (I in 1..N, J in 1..N, I #< J,
                           indomain(I), arg(I, Equals, 0), arg(I, Types, TypeI),
                           indomain(J), arg(J, Equals, 0), arg(J, Types, TypeJ),
                           ((TypeI \= set, TypeJ \= set) -> II = I, JJ = J, Init = init   ;  % standart: compare two columns corresponding to integers
                            (TypeI \= set, TypeJ  = set) -> II = I, JJ = J, Init = within ;  % compare integers with sets
                            (TypeI =  set, TypeJ \= set) -> II = J, JJ = I, Init = within ;  % compare set with integers (permute)
                                                            false                         ), % do not compare two sets
                           extract_comp_ctr(Init, Terms, II, JJ, Ctr)),           Ctrs),
    gen_cmps1(1, N, TableName, Ctrs, Cmps).

gen_cmps1(I, N, _, _, _) :-
    I > N, !.
gen_cmps1(I, N, TableName, Ctrs, Cmps) :-
    I =< N,
    findall(Term1,
            (member(t(I,Cmp,J), Ctrs),
             functor(Term1, Cmp, 2),
             arg(1, Term1, col(TableName, I)),
             arg(2, Term1, col(TableName, J))),
            Terms1),
    findall(Term2,
            (member(t(J,Cmp,I), Ctrs),
             invert_cmp(Cmp, InvCmp),
             functor(Term2, InvCmp, 2),
             arg(1, Term2, col(TableName, I)),
             arg(2, Term2, col(TableName, J))),
            Terms2),
    append(Terms1, Terms2, Terms),
    arg(I, Cmps, Terms),
    I1 is I+1,
    gen_cmps1(I1, N, TableName, Ctrs, Cmps).

invert_cmp(geq,    leq).
invert_cmp(gt,     lt).
invert_cmp(leq,    geq).
invert_cmp(lt,     gt).
invert_cmp(eq,     eq).
invert_cmp(neq,    neq).
invert_cmp(within, contain).

extract_comp_ctr(init,   [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi < Xj, !, extract_comp_ctr(lt,  R, I, J, Res).
extract_comp_ctr(init,   [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi = Xj, !, extract_comp_ctr(eq,  R, I, J, Res).
extract_comp_ctr(init,   [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi > Xj, !, extract_comp_ctr(gt,  R, I, J, Res).
extract_comp_ctr(lt,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi < Xj, !, extract_comp_ctr(lt,  R, I, J, Res).
extract_comp_ctr(lt,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi = Xj, !, extract_comp_ctr(leq, R, I, J, Res).
extract_comp_ctr(lt,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi > Xj, !, extract_comp_ctr(neq, R, I, J, Res).
extract_comp_ctr(eq,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi < Xj, !, extract_comp_ctr(leq, R, I, J, Res).
extract_comp_ctr(eq,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi = Xj, !, extract_comp_ctr(eq,  R, I, J, Res).
extract_comp_ctr(eq,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi > Xj, !, extract_comp_ctr(geq, R, I, J, Res).
extract_comp_ctr(gt,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi < Xj, !, extract_comp_ctr(neq, R, I, J, Res).
extract_comp_ctr(gt,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi = Xj, !, extract_comp_ctr(geq, R, I, J, Res).
extract_comp_ctr(gt,     [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi > Xj, !, extract_comp_ctr(gt,  R, I, J, Res).
extract_comp_ctr(leq,    [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi < Xj, !, extract_comp_ctr(leq, R, I, J, Res).
extract_comp_ctr(leq,    [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi = Xj, !, extract_comp_ctr(leq, R, I, J, Res).
extract_comp_ctr(neq,    [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi < Xj, !, extract_comp_ctr(neq, R, I, J, Res).
extract_comp_ctr(neq,    [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi > Xj, !, extract_comp_ctr(neq, R, I, J, Res).
extract_comp_ctr(geq,    [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi = Xj, !, extract_comp_ctr(geq, R, I, J, Res).
extract_comp_ctr(geq,    [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi > Xj, !, extract_comp_ctr(geq, R, I, J, Res).
extract_comp_ctr(within, [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), is_list(Xj), !, memberchk(Xi,Xj),
                                                 extract_comp_ctr(within, R, I, J, Res).
extract_comp_ctr(within, [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xj = Low-Up, !, Low =< Xi, Xi =< Up,
                                                 extract_comp_ctr(within, R, I, J, Res).
extract_comp_ctr(within, [Term|R], I, J, Res) :- arg(I, Term, Xi), arg(J, Term, Xj), Xi = Xj, !,
                                                 extract_comp_ctr(within, R, I, J, Res).
extract_comp_ctr(Res,    [],       _, _, Res) :- Res \= init. % unify last argument Res with first argument (containing the comparaison)

gen_nb_cmps(I, N, _, _, _) :-
    I > N, !.
gen_nb_cmps(I, N, TableName, Cmps, NbCmps) :-
    I =< N,
    arg(I, Cmps, Cmp),
    length(Cmp, NbCmp),
    arg(I, NbCmps, NbCmp),
    I1 is I+1,
    gen_nb_cmps(I1, N, TableName, Cmps, NbCmps).

gen_ctrs(I, N, _, _, _, _, _, _, _, _) :- % NEW
    I > N, !.
gen_ctrs(I, N, MaxN, TableName, ColNames, Kinds, Types, Equals, Cmps, Ctrs) :- % NEW
    I =< N,
    arg(I, Types,  Type),
    arg(I, Equals, Equal),
    (Type  = set -> arg(I, Ctrs, []) ;
     Type  = cst -> arg(I, Ctrs, []) ;
     Equal > 0   -> arg(I, Ctrs, []) ;
        findall(Val, (functor(Term,TableName,N), call(Term), arg(I, Term, Val)), Vals),
        length(Vals, M),
        sort(Vals, SortedVals),
        length(SortedVals, NewM),
        (M = NewM ->
            Vals = [Min|_],
            last(Vals, Max),
            Range is Max-Min+1,
            ((Min = 1, Max = NewM) -> Ctrs1 = [permutation]                     ;
             Range = M             -> Ctrs1 = [alldifferent_consecutive_values] ;
                                      Ctrs1 = [alldifferent]                    ;
                                      Ctrs1 = []                                ),
            gen_inclusion_except_default_value_ctrs(1, N, MaxN, I, SortedVals, TableName, ColNames, Kinds, Types, Equals, Cmps, Ctrs2) % NEW
        ;
            count_zeros(Vals, 0, Z),
            ZNewM is Z+NewM-1,        % -1 as one 0 remains in SortedVals !
            ((Z>0, ZNewM = M) -> Ctrs1 = [alldifferent_except_0] ;
                                 Ctrs1 = []                      ),
            Ctrs2 = []
        ),
        (increasing_values(Vals) ->
            (strictly_increasing_values(Vals) -> Ctrs3 = [strictly_increasing] ; Ctrs3 = [increasing])
        ;
            (decreasing_values(Vals) ->
                (strictly_decreasing_values(Vals) -> Ctrs3 = [strictly_decreasing] ; Ctrs3 = [decreasing])
            ;
                Ctrs3 = []
            )
        ),
        ((ground(Ctrs1), ground(Ctrs2), ground(Ctrs3)) -> true ; write(gen_ctrs(Ctrs1,Ctrs2,Ctrs3)), nl, halt),
        append([Ctrs1,Ctrs2,Ctrs3], Ctrs123),
        arg(I, Ctrs, Ctrs123)
    ),
    I1 is I+1,
    gen_ctrs(I1, N, MaxN, TableName, ColNames, Kinds, Types, Equals, Cmps, Ctrs). % NEW

% given the I-th column of TableName where all values are distinct,
% identify other columns J (of type int or set of int)
% for which all values, except eventually one default value,
% belong to the set of values of the I-th column
gen_inclusion_except_default_value_ctrs(J, N, _, _, _, _, _, _, _, _, _, []) :- % NEW
    J > N, !.
gen_inclusion_except_default_value_ctrs(J, N, MaxN, I, ValsI, TableName, ColNames, Kinds, Types, Equals, Cmps, AllCtrs) :- % NEW
    J =< N,
    (J = I ->
        AllCtrs = Ctrs
    ;
        arg(J, Types,  TypeJ),
        arg(J, Equals, EqualJ),
        (TypeJ = cst -> AllCtrs = Ctrs ;
         EqualJ > 0  -> AllCtrs = Ctrs ;
                        findall(ValJ, (functor(Term,TableName,N), call(Term), arg(J, Term, ValJ)), ValsJ),
                        (TypeJ = set -> flatten(ValsJ, FlatValsJ) ; FlatValsJ = ValsJ),
                        sort(FlatValsJ, SortedValsJ),
                        (include_except_default_value(SortedValsJ, ValsI, Default) -> % check for temporal constraints only when 
                            ((MaxN=0,                                                 %  . we are on the model seeker
                              integer(Default),                                       %  . there is a default value in column J
                              check_if_no_cycle(N, I, J, TableName, Default)) ->      %  . there is no cycle wrt columns I and J
                                (check_if_temporal_ctrs(N, I, J, TableName, ColNames, Kinds, Types, Equals, Cmps, Default, TemporalCtrs) -> % NEW
                                    AllCtrs = [include_except_default_value_no_cycle(I, J, Default, TemporalCtrs)|Ctrs],
                                    write(include_except_default_value_no_cycle(I, J, Default, TemporalCtrs)),nl
                                ;
                                    AllCtrs = [include_except_default_value_no_cycle(I, J, Default, [])|Ctrs],
                                    write(include_except_default_value_no_cycle(I, J, Default, [])),nl
                                )
                            ;
                                AllCtrs = [include_value(I, J)|Ctrs]
                            )
                        ;
                            AllCtrs = Ctrs
                        )
        )
    ),
    J1 is J+1,
    gen_inclusion_except_default_value_ctrs(J1, N, MaxN, I, ValsI, TableName, ColNames, Kinds, Types, Equals, Cmps, Ctrs). % NEW

% SortedValsJ contains a list composed from two parts:
%  . a prefix, possibly empty, of integers sorted by increasing value,
%  . a suffix, possibly empty, of intervals Low-Up sorted in increasing start.
% ValsI is a list of integers sorted by increasing value.
%
% Succed if all values of SortedValsJ (except at most one possible default value) belong to the sorted list of values ValsI
include_except_default_value(SortedValsJ, ValsI, Default) :-
    include_except_default_value(SortedValsJ, ValsI, ValsI, Default).

include_except_default_value([], _, _, _) :- !.
include_except_default_value([ValJ], [], _, Default) :-
    integer(ValJ),  % we are on an integer value (and not on an interval of values)
    !,
    ValJ = Default. % if first value not in ValsI then set Default to this value
include_except_default_value([ValJ|R], [ValI|S], ValsI, Default) :-
    integer(ValJ),  % we are on an integer value (and not on an interval of values)
    ValJ < ValI,    % ValJ not in ValsI
    !,
    ValJ = Default, % if first value not in ValsI then set Default to this value
    include_except_default_value(R, [ValI|S], ValsI, Default).
include_except_default_value([ValJ|R], [ValI|S], ValsI, Default) :-
    integer(ValJ),  % we are on an integer value (and not on an interval of values)
    ValJ = ValI,    % ValJ in ValsI,
    !,
    include_except_default_value(R, S, ValsI, Default).
include_except_default_value([ValJ|R], [ValI|S], ValsI, Default) :-
    integer(ValJ),  % we are on an integer value (and not on an interval of values)
    ValJ > ValI,    % not yet sure that ValJ is in ValsI
    !,              % continue to search ValJ in ValsI
    include_except_default_value([ValJ|R], S, ValsI, Default).
include_except_default_value([LowJ-UpJ|R], _, ValsI, Default) :-
    include_except_default_value1([LowJ-UpJ|R], ValsI, Default).

include_except_default_value1([], _, _) :- !.
include_except_default_value1([LowJ-UpJ], [], Default) :- !,
    LowJ = UpJ,
    LowJ = Default.
include_except_default_value1([LowJ-UpJ|R], [ValI|S], Default) :-
    LowJ < ValI,    % LowJ not in ValsI
    !,
    LowJ = Default, % set Default to this value
    (LowJ = UpJ ->
        include_except_default_value1(R, [ValI|S], Default)
    ;
        LowJ1 is LowJ + 1,
        include_except_default_value1([LowJ1-UpJ|R], [ValI|S], Default)
    ).
include_except_default_value1([LowJ-UpJ|R], [ValI|S], Default) :-
    LowJ = ValI,    % LowJ in ValsI
    !,
    (LowJ = UpJ ->
        include_except_default_value1(R, S, Default)
    ;
        LowJ1 is LowJ + 1,
        include_except_default_value1([LowJ1-UpJ|R], S, Default)
    ).
include_except_default_value1([LowJ-UpJ|R], [ValI|S], Default) :-
    LowJ > ValI,    % not yet sure that LowJ is in ValsI,
    !,
    include_except_default_value1([LowJ-UpJ|R], S, Default).

% succed if no cycle in a column that for a row K is intrepreted as an arc from
% row col(K,I) to row col(K,J) (rows for which col(K,J)=Default are ignored);
% used only when all values in the I-th column are distincts and when the set
% of values of the J-th column are included in the set of values of the I-th column.
check_if_no_cycle(N, I, J, TableName, Default) :-
    findall(Ind, (functor(Term,TableName,N),
                  call(Term),
                  arg(I, Term, Ind)), LInd),              % get all Ith indices
    length(LInd, M),
    length(LVar, M),                                      % create a list of rank variables (one for each row)
    domain(LVar, 1, M),                                   % will create a < constraint between rank variables for each arc
    keys_and_values(LIndVar, LInd, LVar),                 % put together index and rank variable
    findall(Pred-CleanedSucc, (functor(Term,TableName,N), % create a list of arcs (do not consider Default value) of the form
                               call(Term),                % Pred-CleanSucc meaning that we have an arc between Pred and all its
                               arg(I, Term, Pred),        % successors (if we have only one successor
                               arg(J, Term, Succ),        % CleanSucc is a list of integers (even if Succ was an integer)
                               clean_succ(Succ, Default, CleanedSucc)), Arcs),
    check_if_no_cycle(Arcs, LIndVar).

% succeed if at least one successor, i.e. a value different from Default;
% if Succ is a list remove default value Default from that list and fail if resulting list is empty
clean_succ(Succ, Default, CleanedSucc) :-
    (integer(Succ) ->
        Succ \= Default,
        CleanedSucc = [Succ]                 % normalise integer: put it in a list
    ;
     is_list(Succ) ->
        delete(Succ, Default, CleanedSucc),  % list of successors from which remove default value
        CleanedSucc \= []
    ;
        CleanedSucc = false
    ).

check_if_no_cycle([], _) :- !.
check_if_no_cycle([false|_], _) :- !,                   % an interval of the form Low-Up was used
    false.                                              % do not allow intervals for describing successors
check_if_no_cycle([Pred-Succs|R], LIndVar) :-
    memberchk(Pred-VPred, LIndVar),                     % get rank variable of the predecessor (will fail when Pred is an interval)
    check_if_no_cycle(Succs, VPred, LIndVar),           % add < constraints between rank of pred. and all rank of its succ.
    check_if_no_cycle(R, LIndVar).

check_if_no_cycle([], _, _) :- !.
check_if_no_cycle([Succ|R], VPred, LIndVar) :- !,       % scan successors of predecessor
    memberchk(Succ-VSucc, LIndVar),                     % get rank variable of the successor
    VPred #< VSucc,                                     % post a < constraint between rank of pred and rank of succ
    check_if_no_cycle(R, VPred, LIndVar).

% Succeed if could find some precedence constraints from the arcs associated to columns I and J.
%  . N        : number of columns of the table we currently consider.
%  . I        : column with distinct values interpreted as a primary key
%  . J        : column whose values are included in Ith column (except an extra value Default) interpreted as a successor
%  . TableName: name of the table we currently consider
%  . ColNames : name of each column % NEW
%  . Kinds    : kind information of each column, in the model seeker 'secondary' means an ouput parameter of a model
%  . Types    : type information of each column: constant, boolean, integer, list of integers
%  . Equals   : if >0 indicate an other column with exactly the same values (duplicated column somehow)
%  . Cmps     : comparison constraints of each column wrt the other columns (used to check that we are within binary ctr)
%  . Default  : default value used in column J
%  . Ctrs     : list of temporal constraints we found
check_if_temporal_ctrs(N, I, J, TableName, ColNames, Kinds, Types, Equals, Cmps, Default, Ctrs) :- % NEW
%   write(temporal(N, I, J, Types)), nl,
    collect_temporal_attrs(1, N, I, J, TableName, ColNames, Types, Equals, Cmps, TempAttrs1), % extract all potential temporal attributes without using column names             % NEW
    get_temporal_attrs_from_column_names(ColNames, N, TempAttrs2),                            % extract potential temporal attribute from the column names                       % NEW
    ((length(TempAttrs2, LenTempAttrs2), LenTempAttrs2 =< 1) ->                               % if, from the column names could not figure out at least 2 temporal attributes then use TempAttrs1 % NEW
        TempAttrs = TempAttrs1                                                                % then consider that column names are no significant and take TempAttrs1           % NEW
    ;                                                                                         % if intersection contains at least two temporal attributes                        % NEW
        intersect_lists(TempAttrs1, TempAttrs2, TempAttrs)                                    % then take intersection to focus more the search taking advantage of column names % NEW
    ),                                                                                        % (unlike in gen_resource_ctrs we want to focus on temporal attributes as did not
    length(TempAttrs, M),                                                                     %  yet compute the precedence constraints)
    M > 0,
%   write(TempAttrs), nl,                                                                     % for each potential temporal attribute, create
    create_bools(TempAttrs, Kinds, LeftBools,  SecondaryLeftBools,  SecondaryTempAttrs),      % 0/1 vars for each attribute for leftmost  term
    create_bools(TempAttrs, Kinds, RightBools, SecondaryRightBools, SecondaryTempAttrs),      % 0/1 vars for each attribute for rigthmost term
    append([LeftBools, RightBools], Bools),                                                   % get also 0/1 vars corresponding to output attr.
%   write(Kinds), nl,
%   write('left: '), write(LeftBools), write('  right: '), write(RightBools), nl,
%   write('secondary left bools: '), write(SecondaryLeftBools),
%   write('secondary right bools: '), write(SecondaryRightBools), nl,
    gen_sum_term(SecondaryLeftBools,  var, SumSecondaryLeftBools),
    gen_sum_term(SecondaryRightBools, var, SumSecondaryRightBools),
    call(SumSecondaryLeftBools  #> 0),                                                        % leftmost  term contain at least 1 output attr.
    call(SumSecondaryRightBools #> 0),                                                        % rightmost term contain at least 1 output attr.
    common_output_if_single(SecondaryLeftBools,                                               % for all i:
                            SecondaryRightBools,                                              %  SumSecondaryLeftBools  = 1 /\ 
                            SumSecondaryLeftBools,                                            %  SumSecondaryRightBools > 1 /\
                            SumSecondaryRightBools),                                          %  SecondaryLeftBools[i]  = 1       => 
                                                                                              %        SecondaryRightBools[i] = 1

    common_output_if_single(SecondaryRightBools,                                              % for all i:
                            SecondaryLeftBools,                                               %  SumSecondaryRightBools = 1 /\ 
                            SumSecondaryRightBools,                                           %  SumSecondaryLeftBools  > 1 /\
                            SumSecondaryLeftBools),                                           %  SecondaryRightBools[i] = 1       => 
                                                                                              %        SecondaryLeftBools[i] = 1

    gen_sum_term(LeftBools,  var, SumLeftBools),
    gen_sum_term(RightBools, var, SumRightBools),
    call(SumLeftBools  #=< 2),                                                                % leftmost  term contain at most 2 attr.
    call(SumRightBools #=< 2),                                                                % rightmost term contain at most 2 attr.
    findall(Term, (functor(Term,TableName,N), call(Term)), AllTerms),                         % get all rows, i.e. potential successors
    findall(Term, (functor(Term,TableName,N),                                                 % get all rows corresponding to predecessors
                   call(Term),
                   arg(J, Term, IndJ),
                   clean_succ(IndJ, Default, _)), LTerms),
%   write(all_terms(AllTerms)), nl,
%   write(lterms(LTerms)), nl,
%   write(temp_attrs(TempAttrs)), nl,
    length(SecondaryTempAttrs, LS),                                                           % create for each secondary temporal attribute:
    length(SecondaryLeq, LS),                                                                 %  . a 0/1 var=1 iff val_i always leq val_j
    length(SecondaryGeq, LS),                                                                 %  . a 0/1 var=1 iff val_i always geq val_j
    post_temporal_ctrs(LTerms, AllTerms,
                       N, I, J, TableName,                                               
                       TempAttrs, LeftBools, RightBools,                                      % try to post all temporal constraints, and
                       SecondaryTempAttrs, SecondaryLeftBools, SecondaryRightBools,           % return the list of distance variables LDist
                       SecondaryLeq, SecondaryGeq,                                            % (the constant used in each temporal constraint)
                       Default, LDist, []),
    at_least_one_common_output_ordered(SecondaryLeftBools,                                    % within the common output attributes that are
                                       SecondaryRightBools,                                   % mentioned both on the left hand side and on
                                       SecondaryLeq,                                          % the right hand side, there is at least one of them
                                       SecondaryGeq,                                          % for which the right hand side values are always =<
                                       OrTerm),                                               % or always >= than the left hand side values
    call((SumSecondaryRightBools #> 1 #/\ SumSecondaryLeftBools #> 1) #=> OrTerm),            % post the disjunction
    MinDist in 0..5000000000000,
    MaxDist in 0..5000000000000,
    MinDist #=< MaxDist,
    minimum(MinDist, LDist),                                                                  % minimum value of the constants
    maximum(MaxDist, LDist),                                                                  % maximum value of the constants
    Cost #= MinDist+MaxDist,                                                                  % cost that will be minimized
    Cost #>= 2*MinDist,

 %   write('----------------------------------------------------------------------'), nl,
 %   write('TempAttrs                : '), write(TempAttrs),              nl,
 %   write('kinds                    : '), write(Kinds),                  nl,
 %   write('left                     : '), write(LeftBools),              nl,
 %   write('right                    : '), write(RightBools),             nl,
 %   write('secondary left bools     : '), write(SecondaryLeftBools),     nl,
 %   write('secondary right bools    : '), write(SecondaryRightBools),    nl,
 %   write('secondary_temp_attrs     : '), write(SecondaryTempAttrs),     nl, 
 %   write('sum_secondary_left_bools : '), write(SumSecondaryLeftBools),  nl,
 %   write('sum_secondary_right_bools: '), write(SumSecondaryRightBools), nl,
 %   write('sum_left_bools           : '), write(SumLeftBools),           nl,
 %   write('sum_right_bools          : '), write(SumRightBools),          nl,
 %   write('all_terms                : '), write(AllTerms),               nl,
 %   write('lterms                   : '), write(LTerms),                 nl,
 %   write('ldist                    : '), write(LDist),                  nl,
 %   write('min_dist                 : '), write(MinDist),                nl,
 %   write('max_dist                 : '), write(MaxDist),                nl,
 %   write('cost                     : '), write(Cost),                   nl,

    TimeOut = 60000,                                                                          % set up timeout for computing precedences
    time_out(label_prec(Cost, Bools, LDist), TimeOut, Result),                                % label on Booleans and fix distances to minimum
    (Result \== time_out ->

%        write('----------------------------------------------------------------------'), nl,
%        write('TempAttrs                : '), write(TempAttrs),              nl,
%        write('kinds                    : '), write(Kinds),                  nl,
%        write('left                     : '), write(LeftBools),              nl,
%        write('right                    : '), write(RightBools),             nl,
%        write('secondary left bools     : '), write(SecondaryLeftBools),     nl,
%        write('secondary right bools    : '), write(SecondaryRightBools),    nl,
%        write('secondary_temp_attrs     : '), write(SecondaryTempAttrs),     nl, 
%        write('sum_secondary_left_bools : '), write(SumSecondaryLeftBools),  nl,
%        write('sum_secondary_right_bools: '), write(SumSecondaryRightBools), nl,
%        write('sum_left_bools           : '), write(SumLeftBools),           nl,
%        write('sum_right_bools          : '), write(SumRightBools),          nl,
%        write('all_terms                : '), write(AllTerms),               nl,
%        write('lterms                   : '), write(LTerms),                 nl,
%        write('ldist                    : '), write(LDist),                  nl,
%        write('min_dist                 : '), write(MinDist),                nl,
%        write('max_dist                 : '), write(MaxDist),                nl,
%        write('cost                     : '), write(Cost),                   nl,


        extract_selected_attribute_names(LeftBools,  TempAttrs, TableName, LeftAttrs ),
        extract_selected_attribute_names(RightBools, TempAttrs, TableName, RightAttrs),
        (MinDist = MaxDist ->                                                                 % if same Min and Max we have an equality ctr:
            Ctrs = [precedence(LeftAttrs,MinDist,=,RightAttrs)]                               % sum of LeftAttrs+MinDist = sum of RightAttrs
        ;
            count_distances(LDist, MinDist, MaxDist, 0, 0, CloserToMinDist, CloserToOccMaxDist),
            (CloserToMinDist = CloserToOccMaxDist ->
                Ctrs = [precedence(LeftAttrs, MinDist, =<, RightAttrs),                      
                        precedence(LeftAttrs, MaxDist, >=, RightAttrs)]
            ;
            CloserToMinDist >= CloserToOccMaxDist ->                                          % if closer to MinDist than MaxDist:
                Ctrs = [precedence(LeftAttrs, MinDist, =<, RightAttrs)]                       %  sum of LeftAttrs+MinDist =< sum of RightAttrs
            ;                                                                                 % if closer to MaxDist than MinDist:
                Ctrs = [precedence(LeftAttrs, MaxDist, >=, RightAttrs)]                       %  sum of LeftAttrs+MinDist >= sum of RightAttrs
            )
        )
    ;
        false
    ).

common_output_if_single([], [], _, _) :- !.
common_output_if_single([Bool1|R], [Bool2|S], SumBool1, SumBool2) :-
    call((SumBool1 #= 1 #/\ SumBool2 #> 1 #/\ Bool1 #= 1) #=> Bool2 #= 1),
    common_output_if_single(R, S, SumBool1, SumBool2).

% extract the name of the selected attributes (within the list corresponding to the first argument a "1" indicates a selected attribute)
extract_selected_attribute_names([], [], _, []) :- !.
extract_selected_attribute_names([1|R], [ColNumber|S], TableName, [col(TableName,ColNumber)|T]) :-
    extract_selected_attribute_names(R, S, TableName, T).
extract_selected_attribute_names([0|R], [_|S], TableName, T) :-
    extract_selected_attribute_names(R, S, TableName, T).

% closer from max: #(Max-V < V-Min);  closer from min: #(Max-V > V-Min).
count_distances([], _, _, MinClose, MaxClose, MinClose, MaxClose) :- !.
count_distances([Dist|R], MinDist, MaxDist, CloserToMinDist, CloserToMaxDist, MinClose, MaxClose) :-
    Max is MaxDist - Dist,          % distance between Dist and maximum value MaxDist
    Min is Dist - MinDist,          % distance between Dist and minimum value MinDist
    (Max < Min -> CloserToMinDist1 is CloserToMinDist  , CloserToMaxDist1 is CloserToMaxDist+1 ;
     Max > Min -> CloserToMinDist1 is CloserToMinDist+1, CloserToMaxDist1 is CloserToMaxDist   ;
                  CloserToMinDist1 is CloserToMinDist  , CloserToMaxDist1 is CloserToMaxDist   ),
    count_distances(R, MinDist, MaxDist, CloserToMinDist1, CloserToMaxDist1, MinClose, MaxClose).

post_temporal_ctrs([], _, _, _, _, _, _, _, _, _, _, _, SecondaryLeq, SecondaryGeq, _, LastAnchor, LastAnchor) :- !,
    force_non_ground_to_1(SecondaryLeq),
    force_non_ground_to_1(SecondaryGeq).
post_temporal_ctrs([Term|R], LTerms, N, I, J, TableName,
                   TempAttrs, LeftBools, RightBools,
                   SecondaryTempAttrs, SecondaryLeftBools, SecondaryRightBools,
                   SecondaryLeq, SecondaryGeq,
                   Default, Anchor, LastAnchor) :-
    extract_attrs(TempAttrs, Term, LeftAttrs),                                      % get attr.values of left-hand side of prec.
    get_jth_terms(N, I, J, TableName, Default, Term, LTerms, LTermJ),               % get list of successors of current term
    post_temporal_ctr(LTermJ, TempAttrs, LeftBools, RightBools,                     % iterate through succ. to post temporal ctrs.
                      LeftAttrs, Anchor, NextAnchor),
    extract_attrs(SecondaryTempAttrs, Term, LeftSecondaryAttrs),
    update_secondary_leq_geq(LTermJ, SecondaryTempAttrs,                            % iterate through succ. to update 
                             LeftSecondaryAttrs, SecondaryLeq, SecondaryGeq),       % SecondaryLeq and SecondaryGeq
    post_temporal_ctrs(R, LTerms, N, I, J, TableName,
                       TempAttrs, LeftBools, RightBools,
                       SecondaryTempAttrs, SecondaryLeftBools, SecondaryRightBools,
                       SecondaryLeq, SecondaryGeq,
                       Default, NextAnchor, LastAnchor).

post_temporal_ctr([], _, _, _, _, LastAnchor, LastAnchor) :- !.
post_temporal_ctr([TermJ|R], TempAttrs, LeftBools, RightBools, LeftAttrs, Anchor, LastAnchor) :-
    extract_attrs(TempAttrs, TermJ, RightAttrs),
    gen_linear_sum(LeftAttrs,  LeftBools,  LeftSum),
    gen_linear_sum(RightAttrs, RightBools, RightSum),
    Dist in 0..5000000000000,
    call(LeftSum + Dist #= RightSum),
    Anchor = [Dist|NextAnchor],
    post_temporal_ctr(R, TempAttrs, LeftBools, RightBools, LeftAttrs, NextAnchor, LastAnchor).

update_secondary_leq_geq([], _, _, _, _) :- !.    
update_secondary_leq_geq([TermJ|R], SecondaryTempAttrs, LeftSecondaryAttrs, SecondaryLeq, SecondaryGeq) :-
    extract_attrs(SecondaryTempAttrs, TermJ, RightSecondaryAttrs),
    update_secondary_leq_geq(LeftSecondaryAttrs, RightSecondaryAttrs, SecondaryLeq, SecondaryGeq),
    update_secondary_leq_geq(R, SecondaryTempAttrs, LeftSecondaryAttrs, SecondaryLeq, SecondaryGeq).

update_secondary_leq_geq([], [], [], []) :- !.
update_secondary_leq_geq([LeftSecondaryAttr|R], [RightSecondaryAttr|S], [Leq|T], [Geq|U]) :-
    (LeftSecondaryAttr > RightSecondaryAttr -> Leq = 0 ;
     LeftSecondaryAttr < RightSecondaryAttr -> Geq = 0 ;
                                               true    ),
    update_secondary_leq_geq(R, S, T, U).

at_least_one_common_output_ordered([], [], [], [], 0) :- !.
at_least_one_common_output_ordered([SecondaryLeftBool|R],
                                   [SecondaryRightBool|S],
                                   [SecondaryLeq|T],
                                   [SecondaryGeq|U],
                                   (SecondaryLeftBool  #= 1 #/\
                                    SecondaryRightBool #= 1 #/\
                                    (SecondaryLeq #= 1 #\/ SecondaryGeq #= 1)) #\/ V) :-
    at_least_one_common_output_ordered(R, S, T, U, V).

force_non_ground_to_1([]) :- !.
force_non_ground_to_1([V|R]) :-
    integer(V),
    !,
    force_non_ground_to_1(R).
force_non_ground_to_1([1|R]) :-
    force_non_ground_to_1(R).

label_prec(Cost, Bools, LCst) :-
    minimize((labeling([],Bools),
              once(labeling([], LCst)),
              labeling([up], [Cost])), Cost),
    !.

create_bools([], _, [], [], []) :- !.
create_bools([I|R], Kinds, [B|S], [B|T], [I|U]) :-
    arg(I, Kinds, Kind),
    Kind \== primary,                                           % Kind in {variable,secondary}, where:
    !,                                                          %  . variable : do not search for a formula and the column will be found by the model
    B in 0..1,                                                  %  . secondary: search for a formula
    create_bools(R, Kinds, S, T, U).
create_bools([_|R], Kinds, [B|S], T, U) :-                      % Kind in {primary}, where:
    B in 0..1,                                                  %  . primary: do not search for a formula and the column will be a parameter of the model
    create_bools(R, Kinds, S, T, U).

get_jth_terms(N, I, J, TableName, Default, Term, LTerms, LTermJ) :-
    arg(J, Term, IndJ),
    clean_succ(IndJ, Default, CleanedIndJ),                      % sure that not fail as applied a preliminary filter
    get_jth_terms(CleanedIndJ, N, I, TableName, LTerms, LTermJ).

get_jth_terms([], _, _, _, _, []) :- !.
get_jth_terms([IndJ|R], N, I, TableName, LTerms, [TermJ|S]) :-
    functor(TermJ, TableName, N), % create a term for the entry
    arg(I, TermJ, IndJ),          % set i-th argument of the created term to the index of the row
    memberchk(TermJ, LTerms),     % search for that term in the list of terms (one term for each row)
    get_jth_terms(R, N, I, TableName, LTerms, S).

extract_attrs([], _, []) :- !.
extract_attrs([IndAttr|R], Term, [ValAttr|S]) :-
    arg(IndAttr, Term, ValAttr),
    extract_attrs(R, Term, S).

count_zeros([], Z, Z) :- !.
count_zeros([V|R], Z, S) :-
    (V = 0 -> Z1 is Z+1 ; Z1 = Z),
    count_zeros(R, Z1, S).

increasing_values([_]) :- !.
increasing_values([U,V|R]) :-
    U =< V,
    increasing_values([V|R]).

strictly_increasing_values([_]) :- !.
strictly_increasing_values([U,V|R]) :-
    U < V,
    strictly_increasing_values([V|R]).

decreasing_values([_]) :- !.
decreasing_values([U,V|R]) :-
    U >= V,
    decreasing_values([V|R]).

strictly_decreasing_values([_]) :- !.
strictly_decreasing_values([U,V|R]) :-
    U > V,
    strictly_decreasing_values([V|R]).

% search for all primary keys given a maximum number of columns
gen_pks(TableName, NbRows, NbColumns, Types, MinS, MaxS, NVals, Equals, MaxOccs, Pks) :-
    findall(I,                               % extract first all columns which are candidate of a primary key, that is
            (I in 1..NbColumns, indomain(I), % columns for which we have at least two distinct values, and
             arg(I, Types, Type),            % columns which are not completely equal to some previous column
             Type \= set,
             arg(I, NVals, NVal), NVal > 1,
             arg(I, Equals, 0)),
            ColCandidates),
    PksMaxSize is min(3, NbColumns),         % maximum number of columns in a primary key
    findall(Candidate, sublist_max_card(ColCandidates,PksMaxSize,Candidate), Candidates),
    check_if_pks(Candidates, TableName, NbRows, NbColumns, NVals, MaxOccs, FoundPks),
    gen_cost_pks(FoundPks, MinS, MaxS, CostPks),
    keysort(CostPks, Pks).

gen_cost_pks([], _, _, []) :- !.
gen_cost_pks([Pk|R], MinS, MaxS, [Cost-Pk|S]) :-
    length(Pk, LenPk),
    cost_pks(Pk, LenPk, MinS, MaxS, 1, Cost),
    gen_cost_pks(R, MinS, MaxS, S).

% cost is the product of the range of the columns of the primary keys + the number of columns of the primary keys
% the number of columns of the primary keys is added to favor primary keys with a smaller number of columns
% smaller cost are better
cost_pks([], LenPk, _, _, Cur, Cost) :- !, Cost is Cur + LenPk.
cost_pks([Col|R], LenPk, MinS, MaxS, Cur, Cost) :-
    arg(Col, MinS, Min),
    arg(Col, MaxS, Max),
    Range is Max-Min+1,
    Next is Cur*Range,
    cost_pks(R, LenPk, MinS, MaxS, Next, Cost).

check_if_pks([], _, _, _, _, _, []) :- !.
check_if_pks([Candidate|R], TableName, NbRows, NbColumns, NVals, MaxOccs, [Candidate|S]) :-
    checked_that_is_pks(Candidate, TableName, NbRows, NbColumns, NVals, MaxOccs), !,
    restrict_pks_candidates(R, Candidate, NewR),                         % as Candidate is a primary key
    check_if_pks(NewR, TableName, NbRows, NbColumns, NVals, MaxOccs, S). % we remove all subsequent candidates which contain Candidate
check_if_pks([_|R], TableName, NbRows, NbColumns, NVals, MaxOccs, S) :-  % continue the search for primary keys
    check_if_pks(R, TableName, NbRows, NbColumns, NVals, MaxOccs, S) .

checked_that_is_pks(Candidate, TableName, NbRows, NbColumns, NVals, MaxOccs) :- % check first that the product of the number of distinct values
    check_wrt_nvals(Candidate, 1, NVals, NbRows),                               % if the columns of Candidate is not too small wrt NbRows
    (length(Candidate, 1) ->                                                    % if only one column then values on this column are distinct
        true                                                                    % so we found a primary key consisting of one single columns
    ;                                                                           % using NVals and MaxOccs check that is not a pks
        (check_that_is_pks1(Candidate, TableName, NbRows, NbColumns, NVals, MaxOccs) ->
            true
        ;
            false
        )
    ).

check_wrt_nvals([I|R], CurProd, NVals, NbRows) :-
    arg(I, NVals, NVal),
    NextProd is CurProd*NVal,
    (NextProd >= NbRows -> true ; check_wrt_nvals(R, NextProd, NVals, NbRows)).

check_that_is_pks1(Candidate, TableName, NbRows, NbColumns, NVals, MaxOccs) :-
    findall(MaxOcc-Col, (nth1(_I,Candidate,Col),arg(Col,MaxOccs,MaxOcc)), MaxOccCol),
    keysort(MaxOccCol, SortedMaxOccCol),
    reverse(SortedMaxOccCol, RSortedMaxOccCol),
    keys_and_values(RSortedMaxOccCol, _, SortedCandidate),
    findall([Set|RSet]-Set2, partition_list(SortedCandidate,[Set|RSet],Set2), SetPairs),
                                                             % necessary condition for being a primary key that is checked
                                                             % for all partitions of the columns in SortedCandidate in two
    check_pairs_condition(SetPairs, NbRows, NVals, MaxOccs), % sets Set1 and Set2 (where Set1 is not empty)
    pk_project(Candidate, NbColumns, TableName, Projected),  % compute the projection wrt candidate primary key
    sort(Projected, SortedProjected),                        % sort tuples of candidate primary key (and remove duplicates)
    same_length(Projected, SortedProjected).                 % check that all tuples were all distinct

check_pairs_condition([], _, _, _) :- !.
check_pairs_condition([Set1-Set2|R], NbRows, NVals, MaxOccs) :-
    findall(MaxOcc, (nth1(_I,Set1,Col),arg(Col,MaxOccs,MaxOcc)), ListMaxOcc),
    sumlist(ListMaxOcc, SumMaxOcc),              % sum of the maximum number of occurrences of value in the columns of Set1
    length(Set1, Len1),
    LeftHandSide is SumMaxOcc - (Len1-1)*NbRows,
    (LeftHandSide =< 0 ->
        true
    ;
        findall(NVal, (nth1(_I,Set2,Col),arg(Col,NVals,NVal)), ListNVal),
        prodlist(ListNVal, RightHandSide),       % product of the number of distinct values in the columns of Set2
        LeftHandSide =< RightHandSide
    ),
    check_pairs_condition(R, NbRows, NVals, MaxOccs).

partition_list([], [], []) :- !.   % compute all possible partitions of a list in two sublists
partition_list([X|Y], W, [X|Z]) :- % put this rule first as want to ensure that all sets before a given set s do not contain s
    partition_list(Y, W, Z).       % even if we do currently use this property in the code
partition_list([X|Y], [X|Z], W) :-
    partition_list(Y, Z, W).

% compute foreign keys (used only for model seeker)
gen_all_fks([], _, _) :- !.                       % scan the different tables in order to generate
gen_all_fks([TableChild|R], KindCombi, Tables) :- % for each table its potential foreign keys wrt
    write('process table: '), write(TableChild),
    get_table(KindCombi, 0, _, Options, TableChild),
    (memberchk(no_fk, Options) ->
        FKTableChild = []
    ;                                             % all other tables, except itself
        gen_fks(Tables, TableChild, FKTableChild)
    ),
    tab_get_arity(col(TableChild,_),   NbColumns),
    tab_get_maxn(col(TableChild,_),    MaxN),
    tab_get_nb_rows(col(TableChild,_), NbRows),
    functor(RankedFds, ranked_fd, NbColumns),
    gen_ranked_fds(1, NbColumns, MaxN, NbRows, TableChild, FKTableChild, RankedFds),
    record_fks_ranked_fds_in_metadata_fact_and_metadata_file(KindCombi, TableChild, FKTableChild, RankedFds),
    write('...DONE'),nl,
    gen_all_fks(R, KindCombi, Tables).

gen_fks([], _, []) :- !.                       % for the selected child table, scan all other tables
gen_fks([TableChild|R], TableChild, FKs) :-    % except itself
    !,
    gen_fks(R, TableChild, FKs).               
gen_fks([TableParent|R], TableChild, FKs) :-   
    gen_fk(TableParent, TableChild, FKParent),
    gen_fks(R, TableChild, RFK),
    (FKParent = [] ->                          % do not add the information
        FKs = RFK                              %  when the list of foreign keys is empty
    ;
        FKs = [FKParent|RFK]
    ).

gen_fk(TableParent, TableChild, FKsChildSorted) :-               % for the selected pair of parent and child tables
   tab_get_pks(col(TableParent,_), PKsParent),                   % get the list of parent primary keys 
   gen_fk1(PKsParent, TableParent, TableChild, FKsChild),        % iterate over the primary keys of the parent table to find 
   findall(Cost-FK, (member(FK, FKsChild),                       % a list of corresponding foreign keys in the child table
                     FK = ind(Cost,_,_)),
           FKsChildCost),
   keysort(FKsChildCost, FKsChildCostSorted),                    % sort all foreign keys (in case if there are more than one) 
   findall(FK, member(_-FK,FKsChildCostSorted), FKsChildSorted). % from the parent table in the ascending order (by increasing cost)

gen_fk1([], _, _, []) :- !.
gen_fk1([_-PkParent|R], TableParent, TableChild, FKsChild) :- % select first candidate primary key 
    gen_fk2(TableParent, TableChild, PkParent,                % search through the columns of the child table if
            FKsChild, NewFKsChild),                           % it has corresponding foreign keys
    gen_fk1(R, TableParent, TableChild, NewFKsChild).

% four step algorithm to find foreign keys for the selected primary key of the parent table
gen_fk2(_, _, [], FKsChild, FKsChild) :- !. % the end of the result list remains unchanged as nothing to add
gen_fk2(TableParent, TableChild, PKParent, FKsChild, NewFKsChild) :-
    % STEP 1: for every column of the current primary key of parent
    %         find column candidates (i.e., that can be included in primary key column) in the child table 
    get_fk_column_candidates(PKParent, TableParent, TableChild, ColumnCandidates),
    %
    % STEP 2: generate list of foreign key candidates from lists of column candidates
    findall(X, (list_cartesian_product(ColumnCandidates,X), % ensure that all columns in a foreign key candidate X are <>:
                sort(X,X1),                                 % this is done by sorting X (and remove duplicate) and 
                same_length(X,X1)                           % check that the resulting list has the same length
               ), FKCandidatesAll),
    %
    % STEP 3: check if the candidate is a true foreign key, and, if yes,
    % STEP 4: calculate the cost of the found foreign key
    tab_get_arity(col(TableParent,_), NbColumnsParent),
    pk_project(PKParent, NbColumnsParent, TableParent, RowsParent),
    sort(RowsParent, SortedRowsParent),
    check_all_fk_candidates(FKCandidatesAll, TableParent, TableChild, SortedRowsParent, PKParent, FKsChild, NewFKsChild). 

get_fk_column_candidates([], _, _, []) :- !.
get_fk_column_candidates([ColumnPKParent|R], TableParent, TableChild, [ColumnCandidates|S]) :-
    tab_get_arity(col(TableChild,_), NbColumns),
    findall(I, (I in 1..NbColumns, indomain(I),
                tab_get_type(col(TableChild,I), ColType),
                ColType \= set,
                tab_get_min(col(TableParent, ColumnPKParent), MinParent),
                tab_get_max(col(TableParent, ColumnPKParent), MaxParent),
                tab_get_nval(col(TableParent,ColumnPKParent), NValParent),
                tab_get_min(col(TableChild,I), MinChild), MinChild >= MinParent, (MinChild > MinParent -> S1 = 1 ; S1 = 0),
                tab_get_max(col(TableChild,I), MaxChild), MaxChild =< MaxParent, (MaxChild < MaxParent -> S2 = 1 ; S2 = 0),
                NValParent1 is (NValParent - S1 - S2),
                tab_get_nval(col(TableChild,I), NValChild), NValChild > 1, NValChild =< NValParent1
               ), ColumnCandidates),
    get_fk_column_candidates(R, TableParent, TableChild, S).

check_all_fk_candidates([], _, _, _, _, FKsChild, FKsChild) :- !.
check_all_fk_candidates([FKCandidate|R], TableParent, TableChild, SortedRowsParent, PKParent, FKsChild, FinalFKsChild) :-
    FKsChild = [ind(Cost, pk(TableParent,PKParent), fk(FKCandidate))|NewFKsChild],
    tab_get_arity(col(TableChild,_), NbColumnsChild),
    pk_project(FKCandidate, NbColumnsChild, TableChild,  RowsChild), % retrieve rows from both parent and child tables
    sort(RowsChild, SortedRowsChild),                                % check that RowsChild is included in RowsParent
    sort_included(SortedRowsChild, SortedRowsParent),
    !,
    length(SortedRowsChild, N),                                      % the cost of the foreign key we just found is the
    tab_get_nb_rows(col(TableParent,_), NbRowsParent),               % division of the number of rows of the parent table to
    Cost is NbRowsParent / N,                                        % the number of rows, that is covered by the foreign key
    check_all_fk_candidates(R, TableParent, TableChild, SortedRowsParent, PKParent, NewFKsChild, FinalFKsChild).
check_all_fk_candidates([_|R], TableParent, TableChild, SortedRowsParent, PKParent, FKsChild, FinalFKsChild) :-
                                                                     % skip current foreign key candidate
    check_all_fk_candidates(R, TableParent, TableChild, SortedRowsParent, PKParent, FKsChild, FinalFKsChild).

% make the cartesian product of a list of lists:
% e.g., from [[1,2,3],[6],[2,5]] create the list of triples [[1,6,2],[1,6,5],[2,6,2],[2,6,5],[3,6,2],[3,6,5]]
list_cartesian_product([], []).
list_cartesian_product([ListOfColumns|R], [Column|S]) :-
    nth1(_, ListOfColumns, Column),
    list_cartesian_product(R, S).

sort_included([], _) :- !.
sort_included([X|R], [X|S]) :- !,
    sort_included(R, S).
sort_included([X|R], [Y|S]) :-
    X @> Y,                    % here X and Y are terms of the same arity and same functor, e.g. t(2,3), this is why we use the comparison X @> Y
    sort_included([X|R], S).

read_all_data_and_meta_data_files([], _) :- !.
read_all_data_and_meta_data_files([Table|R], KindCombi) :-
    read_data_and_meta_data_files(KindCombi, Table),
    read_all_data_and_meta_data_files(R, KindCombi).

read_data_and_meta_data_files(KindCombi, Table) :-
    atoms_concat(['data/',KindCombi,'/data'], DirData),    
    gen_table_and_metadata_file_names(DirData, 0, Table, TableFile, MetadataFile),
    consult(TableFile),    % read the main table
    consult(MetadataFile). % read the metadata of the main table

record_fks_ranked_fds_in_metadata_fact_and_metadata_file(KindCombi, Table, FKTable, RankedFds) :-
    get_metadata_arity(Arity),                                % arity of metadata facts
    get_metadata_attributes(IndexNames),                      % attributes names in the metadata facts
    memberchk(IndTableName-table_name, IndexNames),           % position of the table name in a metadata fact
    memberchk(IndFks-fks,              IndexNames),           % position of the foreign keys in a metadata fact
    memberchk(IndRankedFds-ranked_fds, IndexNames),           % position of the ranked functional dependencies in a metadata fact
    functor(TableMetaData, table_metadata, Arity),            % create a metadata fact
    arg(IndTableName, TableMetaData, Table),                  % with the appropriate table name
    call(TableMetaData),                                      % in order to extract the corresponding metadata
    arg(IndFks, TableMetaData, FKTable),                      % record the founded foreign keys
    arg(IndRankedFds, TableMetaData, RankedFds),              % record the founded ranked functional dependencies
    functor(TableMetaDataRetract, table_metadata, Arity),     % create a metadata fact
    arg(IndTableName, TableMetaDataRetract, Table),           % with the appropriate table name in order
    retractall(TableMetaDataRetract),                         % to remove previous metadata where foreign keys were not stored
    assert(TableMetaData),                                    % record updated metadata fact containing the founded foreign keys
    atoms_concat(['data/',KindCombi,'/data0/',                % directory where data,metadata are in the model seeker
                  Table,                                      % add the table name
                  '_metadata.pl'], PrologFile),               % add suffix '_metadata.pl'
    write_metadata_file(PrologFile, Arity, TableMetaData).    % rewrite new metadata file to store computed foreign keys

% compute affinities of the different columns
gen_affinities(I, N, _, _, _, _, _, _) :-
    I > N, !.
gen_affinities(I, N, NbRows, Kinds, Types, Equals, TableName, Affinities) :-
    I =< N,                                                   % iterate over columns of the table to compute affinity of each column
    arg(I, Kinds,  Kind),
    arg(I, Types,  Type),
    arg(I, Equals, Equal),                                    % compute affinity of i-th column only if
    (Type  = set     -> arg(I, Affinities, none) ;            %  . i-th column is not a set
     Kind  = primary -> arg(I, Affinities, none) ;            %  . i-th column is not a primary characteristics
     Type  = cst     -> arg(I, Affinities, none) ;            %  . i-th column is not a constant
     Equal > 0       -> arg(I, Affinities, none) ;            %  . i-th column is not equal to some previous column
        findall(J, (J in 1..N, indomain(J),                   % get all primary columns for which will compute affinity with column i
                    arg(J,Kinds,primary),
                    arg(J,Types,TypeJ),
                    TypeJ \= cst,
                    TypeJ \= set,
                    arg(J,Equals,0)), IndPrimaries),          % compute affinity between column i and all columns in IndPrimaries
        gen_affinity(IndPrimaries, I, N, NbRows, TableName, t(0,0,0,0,0,_), Affinity),
        (Affinity = t(0,0,0,0,0,_) ->                         % if did not find any affinity (because of the timeout)
            arg(I, Affinities, none)                          % then record nothing
        ;                                                     % otherwise check if can complete affinity by a constant and record best affinity found
            check_if_can_complete_affinity_with_a_single_cst(I, N, TableName, Affinity),
            Affinity = t(_,_,_,_,_,Cst),                      % if we have an affinity with all entries of the table then force Cst to none
            (var(Cst) -> Affinity = t(_,_,_,_,_,none) ; true),
            arg(I, Affinities, Affinity)                      % if cannot complete affinity succeed without fixing the constant (put none)
        )
    ),
    I1 is I+1,                                                % compute affinity of next columns
    gen_affinities(I1, N, NbRows, Kinds, Types, Equals, TableName, Affinities).

gen_affinity([], _, _, _, _, Affinity, Affinity) :- !.
gen_affinity([J|R], I, N, NbRows, TableName, BestAffinity, ResAffinity) :-
    BestAffinity = t(BestCost,_,_,_,_,_),                     % get Cost of best affinity found so far
    BestCost1 is BestCost+1,
    NbRows2 is NbRows//2,
    MinCost is max(BestCost1,NbRows2),                        % new lower bound of cost as want to maximise the cost
    gen_affinity1(I, J, N, NbRows, TableName,                 % compute affinity of columns I and J, and
                  MinCost, CurAffinity),                      % get cost of the affinity that was found
    CurAffinity  = t(CurCost,_,_,_,_,_),                      % record current affinity if better than best affinity found so far
    (integer(CurCost), CurCost > BestCost -> NewAffinity = CurAffinity ; NewAffinity = BestAffinity),
    gen_affinity(R, I, N, NbRows, TableName, NewAffinity, ResAffinity).

gen_affinity1(I, J, N, NbRows, TableName, MinCost, Affinity) :-
    Affinity = t(Cost, CoefA, CoefB, CoefC, J, _Cst),           % affinity term: its cost (nber of rows verifying linear term)
    CoefA in  1..3,                                             % the coefficients of the linear term, and the column index J
    CoefB in -3..3,
    CoefC in -3..3,
    CoefB #\= 0,
    CoefA #= abs(CoefB) #/\ abs(CoefB) #> 1 #=> CoefC #\= 0,
    CoefA #= abs(CoefB) #/\ CoefA #= abs(CoefC) #=> CoefA #= 1,
    Cost in MinCost..NbRows,                                    % set up domain of Cost of current affinity
    findall(Xi-Xj, (functor(Term,TableName,N), call(Term),      % for each row get pair of values of i-th and j-th columns
                    arg(I, Term, Xi), arg(J, Term, Xj)), Pairs),
    gen_affinity2(Pairs, CoefA, CoefB, CoefC, SumBools),        % post all affinity constraints: one for each row of the table
    call(Cost #= SumBools),                                     % enumerate on coef.variables, and not on reified variables as fixing coef.variables
    Vars = [CoefA,CoefB,CoefC],                                 % fixes reified variables and as much more reified variables than coef. vars.
    labeling([maximize(Cost),time_out(1000,_)], Vars),          % maximise the number of linear constraints that are satisfied
    !.
gen_affinity1(_, _, _, _, _, _, _).                             % succeed without fixing Affinity if could not find anything

gen_affinity2([], _, _, _, 0) :- !.
gen_affinity2([Xi-Xj|R], CoefA, CoefB, CoefC, Bool+S) :-
    Bool in 0..1,
    Xi*CoefA + Xj*CoefB + CoefC #= 0 #<=> Bool,                 % post a reified linear constraint on each row of the table
    gen_affinity2(R, CoefA, CoefB, CoefC, S).

% check if all entries that are not covered by the computed affinity constraint
% "Yi=Xi*CoefA + Xj*CoefB + CoefC" correspond or not to a same constant Cst
check_if_can_complete_affinity_with_a_single_cst(I, N, TableName, Affinity) :-
    Affinity = t(_Cost, CoefA, CoefB, CoefC, J, Cst),      % get coefficients of the best affinity constraint computed for column I
    findall(Xi-Xj, (functor(Term,TableName,N), call(Term), % get value pairs corresponding to all entries of columns I and J
                    arg(I, Term, Xi),                      % check if all entries that are not covered by the affinity constraint
                    arg(J, Term, Xj)), Pairs),             % correspond to a same constant Cst (initially Cst is a free variable)
    check_if_can_complete_affinity_with_a_single_cst1(Pairs, CoefA, CoefB, CoefC, Cst),
    !.                                                     % set constant to none
check_if_can_complete_affinity_with_a_single_cst(_, _, _, t(_, _, _, _, _, none)).

check_if_can_complete_affinity_with_a_single_cst1([], _, _, _, _) :- !.
check_if_can_complete_affinity_with_a_single_cst1([Xi-Xj|R], CoefA, CoefB, CoefC, Cst) :-
    Y is Xi*CoefA + Xj*CoefB + CoefC,
    (Y = 0 -> true ; Xi = Cst),
    check_if_can_complete_affinity_with_a_single_cst1(R, CoefA, CoefB, CoefC, Cst).

gen_monotonicities(I, N, _, _, _, _, _, _) :-
    I > N, !.
gen_monotonicities(I, N, TableName, KindCombi, MaxN, MaxNbVertices, Kinds, Monotonicities) :-
    I =< N,                                                     % iterate over columns of the table to compute monotonicity of each column
    (MaxN < MaxNbVertices ->                                    % if not on largest size then do not compute monotonicity as monotonicity information
        Monotonicity = []                                       % is not currently used when searching for conjectures
    ;
     KindCombi = model_seeker ->                                % no input parameters of a bound function if on model seeker, therefore no computation
        Monotonicity = []                                       % of monotonicity (use [] rather than none, as none may be a monotonicity result)
    ;                                                           % if on combinatorial object then compute monotonicity if on an input parameter
     arg(I, Kinds, primary) ->
        findall(J, (J in 1..N, indomain(J), J \== I, arg(J, Kinds, primary)),                          IndFixedParams),
        findall(J, (J in 1..N, indomain(J), J \== I, arg(J, Kinds, Bound), memberchk(Bound,[low,up])), [IndBound]    ),
        findall(ValFixedParams-t(ValSelectedParam,ValBound),    % build table of pairs from which check monotonicity wrt current parameter
                (functor(Term, TableName, N),                   %  . first  part of a pair consists of all other parameters
                 call(Term),                                    %  . second part of a pair consists of current parameter and of the bound
                 args(IndFixedParams, Term, ValFixedParams),
                 arg(I,               Term, ValSelectedParam),
                 arg(IndBound,        Term, ValBound)),
                Pairs),
    sort(Pairs, SortedPairs),                                   % sort the pairs to put together the groups, i.e. identical values
    check_monotonicity(SortedPairs, undetermined, Monotonicity) % iterate through sorted pairs to check monotonicity
    ;                                                           % if on combinatorial object but not on input parameter
        Monotonicity = []                                       % then do not compute monotonicity
    ),
    arg(I, Monotonicities, Monotonicity),                       % record computed monotonicity for current column I
    I1 is I+1,                                                  % compute monotonicity of next columns
    gen_monotonicities(I1, N, TableName, KindCombi, MaxN, MaxNbVertices, Kinds, Monotonicities).

% compute the smallest and the largest values occuring within the table;
% these values are used in the bound seeker (rather than the declaration inf..sup) to create tight domains,
% as huge domains can create some performance issue, e.g. posting a constraint like 2 #= X - X mod Y.
gen_inf_sup(Mins, Maxs, Inf-Sup) :-
    functor(Mins,_,N), 
    findall(Min, (I in 1..N, indomain(I), arg(I, Mins, Min)), LMins),
    findall(Max, (I in 1..N, indomain(I), arg(I, Maxs, Max)), LMaxs),
    min_member(Inf, LMins),
    max_member(Sup, LMaxs).

% compute a ranked list of functional dependencies for each output column
gen_ranked_fds(I, N, _, _, _, _, _) :-
    I > N, !.
gen_ranked_fds(I, N, MaxN, NbRows, TableName, FKs, RankedFds) :-
    I =< N,
    (MaxN = 0 ->                                           % if in the context of the model seeker
        tab_get_fd(col(TableName,I), Fd),
        (Fd = [] ->                                        % if no functional dependency
            arg(I, RankedFds, [])                          % then we have nothing to rank
        ;
            tab_get_inputs(TableName, Inputs),             % get the list of all inputs
            tab_get_pks(col(TableName, _), PKs),           % get the list of columns from the primary key with the lowest cost
            (PKs = [] ->                                   % if no primary key
                PK = []                                    % then the list of primary col. is empty
             ;                                             % otherwise take the primary key with the
                PKs = [_-PK|_]                             % lowest cost (the first one)
            ),
            findall(ColJ,
                    (member(ColI, Inputs),
                     tab_get_ctr(ColI, Ctrs), 
                     member(Ctr,Ctrs),
                     Ctr = include_except_default_value_no_cycle(_, ColJ, _Default, _TemporalCtrs),
                     memberchk(ColJ, PK)),
                    PKSequenceColumns), % should not use a successor column in FD
            findall(ColJ,
                    (member(ColI, Inputs),
                     tab_get_cmp(ColI, Cmps), 
                     member(within(col(_,ColJ),_),Cmps)),
                    WithinColumns),     % should not use a column that is part of WITHIN constraint
            get_fk_columns(FKs, FKColumns),
            append([PK, PKSequenceColumns, WithinColumns, FKColumns, [I]],
                   ColsBanned),      % combine both lists into one (ban also the output column I of the fd)
            findall(col(TableName,Col), (member(col(TableName,Col), Inputs),
                                         nonmember(Col, ColsBanned),
                                         tab_get_type(col(TableName,Col), Type),
                                         nonmember(Type, [set,cst])),
                    FilteredInputs),
            filter_fds(Fd, FilteredInputs, FilteredFd),
            NbColToAdd = 2,
            add_extra_input_cols_to_fd(NbColToAdd, FilteredInputs, FilteredFd, AugmentedFd),
            rank_fds(TableName, N, NbRows, I, AugmentedFd, RankedFd),
            arg(I, RankedFds, RankedFd)
        )
    ;                                                      % if in the context of combinatorial objects
        arg(I, RankedFds, [])
    ),
    I1 is I+1,
    gen_ranked_fds(I1, N, MaxN, NbRows, TableName, FKs, RankedFds).

add_extra_input_cols_to_fd(NbColToAdd, FilteredInputs, FilteredFd, AugmentedFds) :-
    findall(NewFd, (member(Fd, FilteredFd),
                    findall(TabCol, (member(TabCol, FilteredInputs), nonmember(TabCol, Fd)), NewFilteredInputs),
                    sublist_max_card(NewFilteredInputs, NbColToAdd, TabCols),
                    append(Fd, TabCols, UnsortedNewFd),
                    sort(UnsortedNewFd, NewFd)),       NewFds),
    append(FilteredFd, NewFds, FilteredAndNewFds),
    sort(FilteredAndNewFds, AugmentedFds).

filter_fds(Fd, FilteredInputs, FilteredFd) :-
    (Fd = [] ->
        FilteredFd = []
    ;
        findall(InputSubsetSorted, (member(InputSubset,Fd),
                                    sort(InputSubset, InputSubsetSorted),                   % need to sort so that subseq0 can be used
                                    subseq0(FilteredInputs,InputSubsetSorted)), FilteredFd)
    ).
