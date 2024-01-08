% Purpose: Primitives for accessing a table
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(table_access,[get_metadata_arity/1,
                        get_metadata_attributes/1,
                        tab_get_arity/2,
                        tab_get_nb_rows/2,
                        tab_get_maxn/2,
                        tab_get_names/2,
                        tab_get_name/2,
                        tab_get_col_name/3,
                        tab_get_kind/2,
                        tab_get_inout/2,
                        tab_get_min/2,
                        tab_get_max/2,
                        tab_get_types/2,
                        tab_get_type/2,
                        tab_get_nval/2,
                        tab_get_median/2,
                        tab_get_gcd/2,
                        tab_get_sum/2,
                        tab_get_mean/2,
                        tab_get_equals/2,
                        tab_get_equal/2,
                        tab_get_fd/2,
                        tab_get_fd_eq/2,
                        tab_get_nb_fd/2,
                        tab_get_cmps/2,
                        tab_get_cmp/2,
                        tab_get_nb_cmp/2,
                        tab_get_ctrs/2,
                        tab_get_ctr/2,
                        tab_get_maxocc/2,
                        tab_get_maxoccval/2,
                        tab_get_pks/2,
                        tab_get_fks/2,
                        tab_get_affinity/2,
                        tab_get_inf_sup/3,
                        tab_get_ranked_fd/2,
                        tab_get_distinct_val/2,
                        tab_get_distinct_val_count/2,
                        tab_get_vals_fd/2,
                        tab_get_monotonicity/2,
                        tab_get_min_max/3,
                        tab_get_mins_attr_names/2,
                        tab_get_maxs_attr_names/2,
                        tab_get_vals_attr_names/4,
                        tab_get_vals_attr_names/6,
                        tab_get_indices_in_range_attr_names/5,
                        tab_get_indices_neg_min_attr_names/3,
                        tab_get_primaries/2,
                        tab_get_secondaries/2,
                        tab_get_primaries_secondaries/2,
                        tab_get_inputs/2,
                        tab_get_small_fd/2,
                        tab_get_bound_col/2,
                        tab_get_index_identical_column/2,
                        tab_get_fk_columns/2,
                        get_fk_columns/2,
                        tab_get_max_name_length/3,
                        tab_get_bound_type/3]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(utility).

:- multifile table_metadata/31.
:- dynamic table_metadata/31.

get_metadata_arity(31).

get_metadata_attributes([1-table_name,       % name of the table Tab
                         2-max_n,            % 0 if one single size, > 1 if more than one size
                         3-nb_columns,       % number of columns of table Tab
                         4-nb_rows,          % number of rows of table Tab
                         5-col_names,        % names of the different columns of table Tab
                         6-kinds,            % kind of the different columns of table Tab (primary, secondary, low, up)
                         7-in_outs,          % tell whether a column if table Tab is an input or an output
                         8-mins,             % minimum value of the different columns of table Tab
                         9-maxs,             % maximum value of the different columns of table Tab
                         10-types,           % type of the different columns of table Tab (int, bool, cst)
                         11-nvals,           % number of distinct values of the different columns of table Tab
                         12-medians,         % median value of the different columns of table Tab
                         13-gcds,            % cumulated gcd of the different columns of table Tab
                         14-sums,            % sum of the different columns of table Tab
                         15-means,           % mean value of the different columns of table Tab
                         16-equals,          % index of previous equal column of the different columns of table Tab (0 if none)
                         17-fds,             % minimal functional dependencies of the different columns of table Tab
                         18-nb_fds,          % number of minimal functional dependencies of the different columns of table Tab
                         19-cmps,            % valid comparisons between a col. and the other cols. of the <> cols. of table Tab
                         20-nb_cmps,         % nber.of valid comparaisons between a col. and the other cols. of the <> cols of table Tab
                         21-ctrs,            % valid constraints of the different columns of table Tab
                         22-maxoccs,         % maximum number of occurrences over all values in a column
                         23-maxoccvals,      % smallest value corresponding to maxocc (smallest is used in case of tie)
                         24-pks,             % list of primary keys of the table, where each primary key is a term of the form
                                             % t(Cost,KeyCols), where KeyCols is a list of column references to the same table
                         25-fks,             % list of foreign keys of the table, where each foreign key is a term of the form
                                             % t(Cost,SourceCols,TargetTable,TargetCols), where:
                                             %  . SourceCols  is a list of column references to the same table,
                                             %  . TargetTable is a reference to an other table where the primary key is,
                                             %  . TargetCols  is a list of column references ot the TargetTable
                         26-affinities,      % provide for a column for which kind is in {low,up,secondary} the primary column
                                             % that maximise the affinity wrt a linear term
                         27-inf_sup,         % smallest and largest integer values in the table, i.e. min of mins and max of maxs
                         28-ranked_fds,      % ranked functional dependencies (not necessarly minimal) of the different columns of table Tab
                         29-distinct_vals,   % if not too many distinct value then list of distinct values else empty list
                         30-vals_fds,        % gives for each cluster its conditional functional dependencies: for each value (except the last one)
                                             % of a cluster, provides the list of columns which determine the entries corresponding to this value
                         31-monotonicities]).% for each input parameter give its monotonicity property (not used on the model seeker)

tab_get_metadata_field(Tab, Arg, MetadataField):-
    get_metadata_arity(MetadataArity),
    functor(Metadata,table_metadata,MetadataArity),
    arg(1,   Metadata, Tab),
    call(user:Metadata), !,
    arg(Arg, Metadata, MetadataField).

tab_get_metadata_fields(Tab, Args, MetadataFields):-
    get_metadata_arity(MetadataArity),
    functor(Metadata,table_metadata,MetadataArity),
    arg(1,   Metadata, Tab),
    call(user:Metadata), !,
    (foreach(Arg,Args), foreach(MetadataField,MetadataFields), param(Metadata)
    do
     arg(Arg, Metadata, MetadataField)
    ).

tab_get_maxn(col(Tab,_), MaxN)       :- tab_get_metadata_field(Tab,  2, MaxN).

tab_get_arity(col(Tab,_), Arity)     :- tab_get_metadata_field(Tab,  3, Arity).

tab_get_nb_rows(col(Tab,_), NbRows)  :- tab_get_metadata_field(Tab,  4, NbRows).

tab_get_names(col(Tab,_), Names)     :- tab_get_metadata_field(Tab,  5, Names).

tab_get_name(col(Tab,Col), Name)     :- tab_get_metadata_field(Tab,  5, Names), arg(Col,Names,Name).

tab_get_col_name(Name, Tab, Col)     :- tab_get_metadata_field(Tab,  5, Names),
                                        tab_get_metadata_field(Tab,  3, Arity),
                                        Col in 1..Arity, indomain(Col), arg(Col,Names,Name), !.

tab_get_kind(col(Tab,Col), Kind)     :- tab_get_metadata_field(Tab,  6, Kinds), arg(Col,Kinds,Kind).

tab_get_inout(col(Tab,Col), Inout)   :- tab_get_metadata_field(Tab,  7, Inouts), arg(Col,Inouts,Inout).

tab_get_min(col(Tab,Col), Min)       :- tab_get_metadata_field(Tab,  8, Mins), arg(Col,Mins,Min).

tab_get_max(col(Tab,Col), Max)       :- tab_get_metadata_field(Tab,  9, Maxs), arg(Col,Maxs,Max).

tab_get_types(col(Tab,_), Types)     :- tab_get_metadata_field(Tab, 10, Types).

tab_get_type(col(Tab,Col), Type)     :- tab_get_metadata_field(Tab, 10, Types), arg(Col,Types,Type).

tab_get_nval(col(Tab,Col), Nval)     :- tab_get_metadata_field(Tab, 11, Nvals), arg(Col,Nvals,Nval).

tab_get_median(col(Tab,Col), Median) :- tab_get_metadata_field(Tab, 12, Medians), arg(Col,Medians,Median).

tab_get_gcd(col(Tab,Col), Gcd)       :- tab_get_metadata_field(Tab, 13, Gcds), arg(Col,Gcds,Gcd).

tab_get_sum(col(Tab,Col), Sum)       :- tab_get_metadata_field(Tab, 14, Sums), arg(Col,Sums,Sum).

tab_get_mean(col(Tab,Col), Mean)     :- tab_get_metadata_field(Tab, 15, Means), arg(Col,Means,Mean).

tab_get_equals(col(Tab,_), Equals)   :- tab_get_metadata_field(Tab, 16, Equals).

tab_get_equal(col(Tab,Col), Equal)   :- tab_get_metadata_field(Tab, 16, Equals), arg(Col,Equals,Equal).

tab_get_fd(col(Tab,Col), Fd)         :- tab_get_metadata_field(Tab, 17, Fds), arg(Col,Fds,Fd).

% handle the case where a secondary characteristics has no fd and is just equal to a lower/upper bound:
% in this case return the functional dependencies attached to the bound
tab_get_fd_eq(col(Tab,Col), Fd) :-
    tab_get_metadata_fields(Tab, [6,     16,     17 ],
                                 [Kinds, Equals, Fds]),
    arg(Col, Fds, F),
    (F = [] ->
        arg(Col, Equals, Equal),
        (Equal > 0 ->
            arg(Equal, Kinds, Kind),
            (memberchk(Kind,[low,up]) ->
                arg(Equal, Fds, Fd)
            ;
                Fd = []
            )
        ;
            Fd = []
        )
    ;
        Fd = F
    ).

tab_get_nb_fd(col(Tab,Col), NbFd)          :- tab_get_metadata_field(Tab, 18, NbFds), arg(Col,NbFds,NbFd).

tab_get_cmps(col(Tab,_), Cmps)             :- tab_get_metadata_field(Tab, 19, Cmps).

tab_get_cmp(col(Tab,Col), Cmp)             :- tab_get_metadata_field(Tab, 19, Cmps), arg(Col,Cmps,Cmp).

tab_get_nb_cmp(col(Tab,Col), NbCmp)        :- tab_get_metadata_field(Tab, 20, NbCmps), arg(Col,NbCmps,NbCmp).

tab_get_ctrs(col(Tab,_), Ctrs)             :- tab_get_metadata_field(Tab, 21, Ctrs).

tab_get_ctr(col(Tab,Col), Ctr)             :- tab_get_metadata_field(Tab, 21, Ctrs), arg(Col,Ctrs,Ctr).

tab_get_maxocc(col(Tab,Col), MaxOcc)       :- tab_get_metadata_field(Tab, 22, MaxOccs), arg(Col,MaxOccs,MaxOcc).

tab_get_maxoccval(col(Tab,Col), MaxOccVal) :- tab_get_metadata_field(Tab, 23, MaxOccVals), arg(Col,MaxOccVals,MaxOccVal).

tab_get_pks(col(Tab,_), Pks)               :- tab_get_metadata_field(Tab, 24, Pks).

tab_get_fks(col(Tab,_), Fks)               :- tab_get_metadata_field(Tab, 25, Fks).

tab_get_affinity(col(Tab,Col), Affinity)   :- tab_get_metadata_field(Tab, 26, Affinities), arg(Col,Affinities,Affinity).

tab_get_inf_sup(col(Tab,_), Inf, Sup)      :- tab_get_metadata_field(Tab, 27, Inf-Sup).

tab_get_ranked_fd(col(Tab,Col), RankedFd)  :- tab_get_metadata_field(Tab, 28, RankedFds), arg(Col,RankedFds,RankedFd).

tab_get_distinct_val(col(Tab,Col), DistinctVal):-
    tab_get_metadata_field(Tab, 29, DistinctVals),
    arg(Col, DistinctVals, DistinctValCount),
    (foreach(I-_,DistinctValCount), foreach(I,DistinctVal) do true).

tab_get_distinct_val_count(col(Tab,Col), DistinctValCount) :- tab_get_metadata_field(Tab, 29, DistinctVals), arg(Col, DistinctVals, DistinctValCount).

tab_get_vals_fd(col(Tab,Col), ValsFd)                      :- tab_get_metadata_field(Tab, 30, ValsFds), arg(Col,ValsFds,ValsFd).

tab_get_monotonicity(col(Tab,Col), Monotonicity)           :- tab_get_metadata_field(Tab, 31, Monotonicities), arg(Col,Monotonicities,Monotonicity).

tab_get_min_max(col(Tab,Col), Min, Max) :-
    tab_get_metadata_fields(Tab, [8,    9   ],
                                 [Mins, Maxs]),
    arg(Col,Mins,Min), arg(Col,Maxs,Max).

tab_get_mins_attr_names([], []) :- !.
tab_get_mins_attr_names([ColRef|R], [Min|S]) :-
    tab_get_min(ColRef, Min),
    tab_get_mins_attr_names(R, S).

tab_get_maxs_attr_names([], []) :- !.
tab_get_maxs_attr_names([ColRef|R], [Max|S]) :-
    tab_get_max(ColRef, Max),
    tab_get_maxs_attr_names(R, S).

% for a given set of columns [ColRef|R], computes for each column the set of values Values
% that are in [MinVal,MaxVal]
% (do not compute anything if the result is already ground)
tab_get_vals_attr_names(_, _, _, LValues) :- ground(LValues), !.
tab_get_vals_attr_names([], _, _, []) :- !.
tab_get_vals_attr_names([ColRef|R], MinVal, MaxVal, [Values|S]) :-
    tab_get_vals_attr_name(ColRef, MinVal, MaxVal, Values),
    tab_get_vals_attr_names(R, MinVal, MaxVal, S).

tab_get_vals_attr_name(col(Table,Col), MinVal, MaxVal, Values) :-
    tab_get_arity(col(Table,_), N),
    findall(Val, (functor(Row, Table, N),
                  call(user:Row),
                  arg(Col, Row, Val),
                  Val >= MinVal,
                  Val =< MaxVal), RowValues),
    sort(RowValues, Values).

% for a given set of columns [ColRef|R], computes for each column the set of values Values
% that are in [MinVal,MaxVal] and for which the column ColTarget takes value ValTarget
% (do not compute anything if the result is already ground)
tab_get_vals_attr_names(_, _, _, _, _, LValues) :- ground(LValues), !.
tab_get_vals_attr_names([], _, _, _, _, []) :- !.
tab_get_vals_attr_names([ColRef|R], MinVal, MaxVal, ColTarget, ValTarget, [Values|S]) :-
    tab_get_vals_attr_name(ColRef, MinVal, MaxVal, ColTarget, ValTarget, Values),
    tab_get_vals_attr_names(R, MinVal, MaxVal, ColTarget, ValTarget, S).

tab_get_vals_attr_name(col(Table,Col), MinVal, MaxVal, col(Table,ColTarget), ValTarget, Values) :-
    tab_get_arity(col(Table,_), N),
    findall(Val, (functor(Row, Table, N),
                  call(user:Row),
                  arg(ColTarget, Row, ValTarget),
                  arg(Col,       Row, Val),
                  Val >= MinVal,
                  Val =< MaxVal), RowValues),
    sort(RowValues, Values).

tab_get_indices_in_range_attr_names([], _, _, _, []) :- !.
tab_get_indices_in_range_attr_names([ColRef|R], Low, Up, Index, Res) :-
    tab_get_min(ColRef, Min),
    (Min >= Low ->
        tab_get_max(ColRef, Max),
        (Max =< Up -> Res = [Index|S] ; Res = S)
    ;
        Res = S
    ),
    Index1 is Index+1,
    tab_get_indices_in_range_attr_names(R, Low, Up, Index1, S).

tab_get_indices_neg_min_attr_names([], _, []) :- !.
tab_get_indices_neg_min_attr_names([ColRef|R], Index, Res) :-
    tab_get_min(ColRef, Min),
    (Min < 1 -> Res = [Index|S] ; Res = S),
    Index1 is Index+1,
    tab_get_indices_neg_min_attr_names(R, Index1, S).

tab_get_primaries(Tab, Primaries) :-
    tab_get_metadata_fields(Tab, [3, 6],
                                 [A, Kinds]),
    findall(col(Tab,I), (I in 1..A, indomain(I), arg(I, Kinds, primary)), Primaries).

tab_get_secondaries(Tab, Secondaries) :-
    tab_get_metadata_fields(Tab, [3, 6],
                                 [A, Kinds]),
    findall(col(Tab,I), (I in 1..A, indomain(I), arg(I, Kinds, secondary)), Secondaries).

tab_get_primaries_secondaries(Tab, PrimariesSecondaries) :-
    tab_get_metadata_fields(Tab, [3, 6],
                                 [A, Kinds]),
    findall(col(Tab,I), (I in 1..A, indomain(I), arg(I, Kinds, Kind), memberchk(Kind, [primary,secondary])), PrimariesSecondaries).

tab_get_inputs(Tab, Inputs) :-
    tab_get_metadata_fields(Tab, [3, 7],
                                 [A, Inouts]),
    findall(col(Tab,I), (I in 1..A, indomain(I), arg(I, Inouts, input)), Inputs).

tab_get_small_fd(Tab, SmallFd) :-
    tab_get_metadata_fields(Tab, [3, 6,    17],
                                 [A, Kinds,Fds]),
    findall(I, (I in 1..A, indomain(I), arg(I, Kinds, Kind), memberchk(Kind,[low,up])), [IBound]),
    arg(IBound, Fds, BoundFds),
    BoundFds = [DF1|_],
    length(DF1, Len),
    Limit is Len+1,
    findall(DF, (member(DF, BoundFds), length(DF, LenDF), LenDF =< Limit), SmallFd).

tab_get_bound_col(Tab, IBound) :-
    tab_get_metadata_fields(Tab, [3, 6],
                                 [A, Kinds]),
    findall(I, (I in 1..A, indomain(I), arg(I, Kinds, Kind), memberchk(Kind,[low,up])), [IBound]).

tab_get_index_identical_column(col(Tab,Col), Equal) :-
    tab_get_equal(col(Tab,Col), Equal),
    Equal > 0,
    !.
tab_get_index_identical_column(col(Tab,Col), IndexIdenticalColumn) :-
    tab_get_metadata_fields(Tab, [3, 16],
                                 [A, Equals]),
    NextCol is Col+1,
    findall(I, (I in NextCol..A, indomain(I), arg(I, Equals, Col)), IndexIdenticalColumns),
    IndexIdenticalColumns = [IndexIdenticalColumn|_].

% get the list of columns from all foreign keys which have the lowest cost for each parent table of the child table Table
% used as we want to prevent using such columns as an input parameter in a formula
tab_get_fk_columns(Table, FkColsRes) :-
   tab_get_fks(col(Table,_), Fks),
   get_fk_columns(Fks, FkColsRes).

get_fk_columns(Fks, FkColsRes) :-
   (var(Fks) -> write(tab_get_fk_columns), nl, halt ; true),      % catch case where foreign keys were not initialised
   findall(Col, member([ind(_,_, fk(Col))|_], Fks), FkColsLists), % for each table take the foreign key with the lowest cost
   append(FkColsLists, FkCols),
   sort(FkCols, FkColsRes).                                       % remove duplicates, sort in ascending order

tab_get_max_name_length(Table, Kind, MaxNameLength) :-            % return maximum number of characters of column names
    tab_get_arity(col(Table,_), Arity),                           % for which kind is equal to Kind (or 100000 if no column
    findall(Len, (I in 1..Arity,                                  % for which Kind=secondary)
                  indomain(I),
                  tab_get_kind(col(Table,I), Kind),
                  tab_get_name(col(Table,I), Name),
                  atom_codes(Name, Codes),
                  length(Codes, Len)),              ListLen),
    max_member(MaxNameLength, ListLen),
    !.
tab_get_max_name_length(_, _, 100000).

% extract the fact that the bound table Tab is about a lower or an upper bound (i.e. BoundType=low or BoundType=up), fail if this is not the case
% (used to determine if has to use ceil or floor functions as ceil is natural for a lower bound and floor is natural for a upper bound)
tab_get_bound_type(KindCombi, Tab, BoundType) :-
    KindCombi \== model_seeker,
    tab_get_arity(col(Tab,_), Arity),
    findall(Kind, (Col in 1..Arity, indomain(Col), tab_get_kind(col(Tab,Col), Kind), memberchk(Kind, [low,up])), [BoundType]). % one single occurrence of low or up in all bound tables
