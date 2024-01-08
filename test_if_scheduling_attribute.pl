% Purpose:
% Functionality 1: Returns the column indices (indices start from 1) of the columns whose name cannot correspond to a specific scheduling attribute (start, duration, end, resource).
% Functionality 2: Returns the column index (indices start from 1) in a list of a single element of the columns whose name correspond to the duration attribute.
% Functionality 3: Returns the column indices (indices start from 1) of the columns whose name correspond to the start, duration or end attributes.
% Functionality 4: Check that a given name can potentially correspond or not to a given scheduling attribute (start, duration, end, resource)
%                  Used when acquiring scheduling constraints to further focus the search of columns of a table which can corresponding to specific scheduling attributes.
%                  Assert check_if_scheduling_attribute(true) if want to use this functionality (assert check_if_scheduling_attribute(false) otherwise).
% Functionality 5: Check if a given name can correspond to start, duration, or end.
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(test_if_scheduling_attribute, [get_not_specific_scheduling_attr_from_column_names/4,
                                         get_duration_attr_from_column_names/2,
                                         get_temporal_attrs_from_column_names/3,
                                         check_if_name_corresponds_to_scheduling_attr/2,
                                         check_if_name_corresponds_to_scheduling_attr/1]).

:- use_module(library(lists)).
:- use_module(table_access).

% set to true if want to check that a name can correspond or not to a given scheduling attribute, set to false otherwise
check_if_scheduling_attribute(true).

% Given a table Table and the total number of columns NbColumns of that table
% returns the column indices (indices start from 1) of the columns whose name cannot correspond to a specific scheduling attribute SchedAttr.
% to do so we proceed as follows:
%  . if at least one of the columns corresponds to a duration attribute then the other columns do not correspond to a duration attribute,
%  . if no column corresponds to a duration attribute then return an empty list (as do not want to prevent identying columns that cannot correspond to a duration using an other method)
get_not_specific_scheduling_attr_from_column_names(Table, NbColumns, SchedAttr, NoSchedulingAttr) :-
    get_not_specific_scheduling_attr_from_column_names(1, NbColumns, Table, SchedAttr, 0, NbSchedAttr, NoSchedAttr),
    (NbSchedAttr = 0 ->
        NoSchedulingAttr = []
    ;
        NoSchedulingAttr = NoSchedAttr
    ).

get_not_specific_scheduling_attr_from_column_names(I, NbColumns, _Table, _SchedAttr, NbSchedAttr, NbSchedAttr, []) :-
    I > NbColumns,
    !.
get_not_specific_scheduling_attr_from_column_names(I, NbColumns, Table, SchedAttr, NbSchedAttr, ResNbSchedAttr, R) :-
    I =< NbColumns,
    tab_get_name(col(Table,I), ColName),
    check_if_name_corresponds_to_scheduling_attr(SchedAttr, ColName),
    !,
    NbSchedAttr1 is NbSchedAttr+1,
    I1 is I+1,
    get_not_specific_scheduling_attr_from_column_names(I1, NbColumns, Table, SchedAttr, NbSchedAttr1, ResNbSchedAttr, R).
get_not_specific_scheduling_attr_from_column_names(I, NbColumns, Table, SchedAttr, NbSchedAttr, ResNbSchedAttr, [I|R]) :-
    I =< NbColumns,
    I1 is I+1,
    get_not_specific_scheduling_attr_from_column_names(I1, NbColumns, Table, SchedAttr, NbSchedAttr, ResNbSchedAttr, R).

get_duration_attr_from_column_names(Names, DurationAttr) :-
    functor(Names, name, NbColumns),
    get_duration_attr_from_column_names(1, NbColumns, Names, DurationAttr),
    DurationAttr = [_].

get_duration_attr_from_column_names(I, N, _, []) :-
    I > N, !.
get_duration_attr_from_column_names(I, N, Names, [I|S]) :-
    I =< N,
    arg(I, Names, Name),
    check_if_name_corresponds_to_scheduling_attr(duration, Name),
    !,
    I1 is I+1,
    get_duration_attr_from_column_names(I1, N, Names, S).
get_duration_attr_from_column_names(I, N, Names, S) :-
    I =< N,
    I1 is I+1,
    get_duration_attr_from_column_names(I1, N, Names, S).

% given,
%  . a term  TableOrTerm of arity NbColumns giving the names of the columns of a table, or  (used from gen_metadata.pl      when metadata not yet     created)
%  . a table TableOrTerm and the total number of columns NbColumns of that table,           (used from gen_resource_ctrs.pl when metadata was already created)
% returns the column indices (indices start from 1) of the columns whose name correspond to the start, duration or end attributes.
get_temporal_attrs_from_column_names(TableOrTerm, NbColumns, TemporalAttr) :-
    get_temporal_attrs_from_column_names(1, NbColumns, TableOrTerm, TemporalAttr).

get_temporal_attrs_from_column_names(I, NbColumns, _, []) :-
    I > NbColumns,
    !.
get_temporal_attrs_from_column_names(I, NbColumns, TableOrTerm, [I|R]) :-
    I =< NbColumns,
    member(SchedulingAttr, [start,duration,end]),
    (functor(TableOrTerm, _, NbColumns) ->        % if TableOrTerm is a term then extract the i-th argument
        arg(I, TableOrTerm, ColName)
    ;                                             % if TableOrTerm is a table then get the name of the i-th column
        tab_get_name(col(TableOrTerm,I), ColName)
    ),
    check_if_name_corresponds_to_scheduling_attr(SchedulingAttr, ColName),
    !,
    I1 is I+1,
    get_temporal_attrs_from_column_names(I1, NbColumns, TableOrTerm, R).
get_temporal_attrs_from_column_names(I, NbColumns, TableOrTerm, R) :-
    I =< NbColumns,
    I1 is I+1,
    get_temporal_attrs_from_column_names(I1, NbColumns, TableOrTerm, R).

check_if_name_corresponds_to_scheduling_attr(Name) :-
    member(SchedulingAttr, [start, duration, end]),
    check_if_name_corresponds_to_scheduling_attr(SchedulingAttr, Name),
    !.

% given a scheduling attribute SchedulingAttr (start, duration, end, resource) and a name Name, 
% succeed iff Name can possybly correspond to the scheduling attribute SchedulingAttr;
% for instance we get the following decomposition and result for these queries wrt the start attribute:
%  check_if_name_corresponds_to_scheduling_attr(start, start       )  [start]    ok(start,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, task_s      )  [s]        ok(task_s,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, taskS       )  [task,s]   ok(taskS,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, startjob    )  [start]    ok(startjob,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, startj      )  [startj]   ok(startj,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, 'START'     )  [start]    ok(START,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, duty_start_t)  [start,t]  ok(duty_start_t,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, startOfTask )  [start,of] ok(startOfTask,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, s           )  [s]        ok(s,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, starto      )  [starto]   ok(starto,succeed)
%  check_if_name_corresponds_to_scheduling_attr(start, tasks       )  [tasks]    ok(tasks,fail)
%  check_if_name_corresponds_to_scheduling_attr(start, startjo     )  [startjo]  ok(startjo,fail)
%  check_if_name_corresponds_to_scheduling_attr(start, staRt       )  [sta,rt]   ok(staRt,fail)
check_if_name_corresponds_to_scheduling_attr(SchedulingAttr, Name) :-
    check_if_scheduling_attribute(true), % fail if do not want to check that a name corresponds or not to a temporal attribute
    (SchedulingAttr = start    -> PossiblePrefix    = [start,begin,arrival,launch,debut],
                                  PossibleFullNames = [s,b,d,st,deb,start,begin,arrival,launch,debut]                                                                                   ;
     SchedulingAttr = duration -> PossiblePrefix    = [dure,duree,duration,len,leng,lengt,length,long,span,period,extent,stretch,exe,exec,execution],
                                  PossibleFullNames = [d,l,p,dur,dura,dure,duree,duration,len,leng,lengt,length,long,span,period,extent,stretch,exe,exec,execution]                     ;
     SchedulingAttr = end      -> PossiblePrefix    = [end,close,closure,terminate,termination,departure,conclusion,stop,fin,term,butoir,sortie,completion],
                                  PossibleFullNames = [e,f,end,close,closure,terminate,termination,departure,conclusion,stop,fin,term,butoir,sortie,completion]                         ;
     SchedulingAttr = resource -> PossiblePrefix    = [resource,machine,tool,device,instrument,unit,engine,motor,person,staff,work,manpower,crew,shift,nurse,team,employee,moyen],
                                  PossibleFullNames = [r,m,p,resource,machine,tool,device,instrument,unit,engine,motor,person,staff,work,manpower,crew,shift,nurse,team,employee,moyen] ;
                                  write(check_scheduling_attr), nl, halt                                                                                                                ),
     RemoveFactors = [task,      'Task',      'TASK',
                      duty,      'Duty',      'DUTY',
                      mission,   'Mission',   'MISSION',
                      job,       'Job',       'JOB',
                      operation, 'Operation', 'OPERATION',
                      op,        'Op',        'OP',
                      tache,     'Tache',     'TACHE'    ],
    latom_lcodes(RemoveFactors, CodeRemoveFactors),
    extract_words(Name, CodeRemoveFactors, NameNormalizedFactors),
    latom_lcodes(NameNormalizedFactors, CodeNameNormalizedFactors),
    latom_lcodes(PossiblePrefix,        CodePossiblePrefix),
%   write(NameNormalizedFactors), write(' '),
    (non_empty_intersection(NameNormalizedFactors, PossibleFullNames)                -> true ;
     prefix_with_suffix_of_length_one(CodePossiblePrefix, CodeNameNormalizedFactors) -> true ;
                                                                                        false).
extract_words(Name, CodeRemoveFactors, AllWords) :-
    atom_codes(Name, NameCodes),
    extract_all_words(NameCodes, CodeRemoveFactors, Words),
    latom_lcodes(AllWords, Words).

extract_all_words([], _, []) :- !.
extract_all_words(NameCodes, CodeRemoveFactors, Result) :-
    remove_non_alphabetic_prefix(NameCodes, NormalisedNameCodes),
    (NormalisedNameCodes = [] ->
        Result = []
    ;
        extract_next_word(NormalisedNameCodes, WordCodes, RestNormalisedNameCodes),
        remove_factors(CodeRemoveFactors, WordCodes, NormalisedWordCodes),
        (NormalisedWordCodes = [] ->
            Splitted = []
        ;
            split_wrt_upper_cases(NormalisedWordCodes, Splitted)
        ),
        append(Splitted, R, Result),
        extract_all_words(RestNormalisedNameCodes, CodeRemoveFactors, R)
    ).

remove_factors([], WordCodes, WordCodes) :- !.
remove_factors([CodeRemoveFactor|R], WordCodes, NormalisedWordCodes) :-
    remove_all_factor_occurrences(WordCodes, CodeRemoveFactor, NewWordCodes),
    remove_factors(R, NewWordCodes, NormalisedWordCodes).

remove_all_factor_occurrences([], _, []) :- !.
remove_all_factor_occurrences(List, Factor, Result) :-
    remove_first_factor_occurrence(List, Prefix, Factor, Rest),
    !,
    remove_all_factor_occurrences(Rest, Factor, Res),
    append(Prefix, Res, Result).
remove_all_factor_occurrences(List, _, List).

remove_first_factor_occurrence(List, Prefix, Factor, Suffix) :-
    sublist(List, Factor, _),
    append(Prefix, Factor, PrefixFactor),
    append(PrefixFactor, Suffix, List),
    !,
    length(Suffix, Len),
    (Len = 1 -> false ; true).

extract_next_word([], [], []) :- !.
extract_next_word([Code|R], [Code|S], T) :-
    alphabetic_code(Code),
    !,
    extract_next_word(R, S, T).
extract_next_word(RestNameCodes, [], RestNameCodes).

remove_non_alphabetic_prefix([], []) :- !.
remove_non_alphabetic_prefix([Code|R], [Code|R]) :-
    alphabetic_code(Code), !.
remove_non_alphabetic_prefix([_|R], Rest) :-
    remove_non_alphabetic_prefix(R, Rest).

split_wrt_upper_cases([], []) :- !.
split_wrt_upper_cases(WordCodes, [WordLowerCaseCodes]) :-
    only_upper_cases(WordCodes),
    !,
    convert_to_lower_case(WordCodes, WordLowerCaseCodes).
split_wrt_upper_cases(Codes, [WordCodes|R]) :-
    extract_next_wrt_upper_cases(Codes, 0, WordCodes, Rest),
    split_wrt_upper_cases(Rest, R).

extract_next_wrt_upper_cases([], _, [], []) :- !.
extract_next_wrt_upper_cases([Code|R], I, [NewCode|S], Rest) :-
    (I = 0 ->
        alphabetic_code(Code),
        to_lower_case(Code, NewCode) 
    ;
        lower_case(Code),
        NewCode = Code
    ),
    !,
    I1 is I+1,
    extract_next_wrt_upper_cases(R, I1, S, Rest).
extract_next_wrt_upper_cases(Rest, _, [], Rest).

latom_lcodes([], []) :- !.
latom_lcodes([Word|R], [Codes|S]) :-
    atom_codes(Word, Codes),
    latom_lcodes(R, S).

only_upper_cases([]) :- !.
only_upper_cases([Code|R]) :-
    upper_case(Code),
    only_upper_cases(R).

convert_to_lower_case([], []) :- !.
convert_to_lower_case([Code|R], [NewCode|S]) :-
    to_lower_case(Code, NewCode),
    convert_to_lower_case(R, S).

alphabetic_code(Code) :- lower_case(Code), !.
alphabetic_code(Code) :- upper_case(Code).

to_lower_case(Code, NewCode) :- (upper_case(Code) -> NewCode is Code + 32 ; NewCode = Code).

lower_case(Code) :- Code >= 97, Code =< 122.

upper_case(Code) :- Code >= 65, Code =<  90.

non_empty_intersection([X|_], L) :- memberchk(X, L), !.
non_empty_intersection([_|Y], L) :- non_empty_intersection(Y, L).

prefix_with_suffix_of_length_one([X|_], L) :-
    member(Y, L),
    sublist(Y, X, 0, _, 1),
    !.
prefix_with_suffix_of_length_one([_|Y], L) :-
    prefix_with_suffix_of_length_one(Y, L).

% test :-
%    (check_if_name_corresponds_to_scheduling_attr(start, start       ) -> write(ok(start,       succeed)), nl ; write(pb(start       )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, task_s      ) -> write(ok(task_s,      succeed)), nl ; write(pb(task_s      )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, taskS       ) -> write(ok(taskS,       succeed)), nl ; write(pb(taskS       )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, startjob    ) -> write(ok(startjob,    succeed)), nl ; write(pb(startjob    )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, startj      ) -> write(ok(startj,      succeed)), nl ; write(pb(startj      )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, 'START'     ) -> write(ok('START',     succeed)), nl ; write(pb('START'     )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, duty_start_t) -> write(ok(duty_start_t,succeed)), nl ; write(pb(duty_start_t)), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, startOfTask ) -> write(ok(startOfTask, succeed)), nl ; write(pb(startOfTask )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, s           ) -> write(ok(s,           succeed)), nl ; write(pb(s           )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, starto      ) -> write(ok(starto,      succeed)), nl ; write(pb(starto      )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, tasks       ) -> write(pb(tasks               )), nl ; write(ok(tasks,fail  )), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, startjo     ) -> write(pb(startjo             )), nl ; write(ok(startjo,fail)), nl),
%    (check_if_name_corresponds_to_scheduling_attr(start, staRt       ) -> write(pb(staRt               )), nl ; write(ok(staRt,fail  )), nl).
