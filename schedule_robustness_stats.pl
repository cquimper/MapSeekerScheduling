% PURPOSE: USED TO AGGREGATE RESULTS FOR THE CP 2023 SUBMITTED PAPER

:- use_module(library(lists)).
:- use_module(table_access).
:- use_module(tables).
:- use_module(utility).
:- use_module(gen_candidate_tables).

columns_disjunctive_columns([resource,start_time,end_time,duration]).
columns_diffn_columns([resource,resource_choices,start_time,end_time,duration]).
columns_temporal_columns([start_time,end_time,duration]).

% set_prolog_flag(informational,off), [schedule_robustness_stats], create_stats.
% [schedule_robustness_stats], create_stats, halt.

%top :- create_examples('existing_maps/graph/found_conjectures_graph_low_mina.pl').

create_stats:-
    statistics(total_runtime,[Start|_]),
    Directory = 'schedule_robustness/',
    FileNameDimensions = 'schedule_robustness_dimensions.pl',
    FileNameConjecture = 'found_model_1_1.pl',
    FileNameStatistics = 'stats.csv',
    atoms_concat([Directory, FileNameDimensions], FilePathDimensions),
    atoms_concat([Directory, FileNameConjecture], FilePathConjecture),
    atoms_concat([Directory, FileNameStatistics], FilePathStatistics),
    consult(FilePathDimensions),
    consult(FilePathConjecture),
    open(FilePathStatistics, write, Sout),

    % create csv-file header
    format(Sout, 'table_name,noise_columns,nb_rows,', []),
    format(Sout, 'task_option,temporal_option,schedule_option,noise_option,entries_option,', []),
    format(Sout, 'is_formula_end_time,is_formula_duration,is_precedence,is_schedule_resource_disj,is_schedule_resource_diffn,is_schedule_calendar_shift,is_schedule_calendar_cal,', []),
    format(Sout, 'found_formula_exact_end_time,found_formula_partial_end_time,found_extra_formulae_end_time,found_formula_exact_duration,found_formula_partial_duration,found_extra_formulae_duration,', []),
    % _exact_   - the fully correct conjecture
    % _partial_ - for all disj and diffn ctrs: a) at least one of the attributes is temporal, other attributes are not temporal
    %                                          b) all attributes are correct but the positions are wrong
    %             for shift and calendar ctrs:    uses temporal attributes but the positions or flags are wrong
    % _extra_   - everything else
    % found_extra_formulae - if end_time and duration use any attribute besides start_time, end_time, duration, qty, speed
    % found_extra_precedence, found_extra_schedule_resource, found_extra_schedule_calendar_shift - if all their attributes do not mention start_time, end_time, duration
    format(Sout, 'found_exact_precedence,found_partial_precedence,found_extra_precedence,', []),
    format(Sout, 'found_exact_schedule_resource_disj,found_partial_schedule_resource_disj,found_extra_schedule_resource_disj,', []),
    format(Sout, 'found_exact_schedule_resource_diffn,found_partial_schedule_resource_diffn,found_extra_schedule_resource_diffn,', []),
    format(Sout, 'found_exact_schedule_calendar_shift,found_partial_schedule_calendar_shift,found_extra_schedule_calendar_shift,', []),
    format(Sout, 'found_exact_schedule_calendar_cal,found_partial_schedule_calendar_cal,found_extra_schedule_calendar_cal', []),
    
    write('Header created.'),nl,
    
    findall(Table, (functor(TableOptions, schedule_robustness_dimensions, 7),
                    call(TableOptions),
                    arg(1, TableOptions, Table),
                    check_table_conjectures(Sout, TableOptions)%, false
                   ),
            _),
    close(Sout),
    statistics(total_runtime,[Stop|_]),
    Runtime is Stop - Start,
    write(Runtime), write('ms'), nl.


check_table_conjectures(Sout, TableOptions) :-
    format(Sout, '~n', []),
    arg(1, TableOptions, Table),
    arg(2, TableOptions, TaskOption),
    arg(3, TableOptions, TemporalOption),
    arg(4, TableOptions, ScheduleOption),
    arg(5, TableOptions, NoiseOption),
    arg(6, TableOptions, EntriesOption),
    arg(7, TableOptions, TableCtrs),
    
    (NoiseOption = a -> NoiseStat is 0 ; NoiseStat is 1),
    (EntriesOption = a ->
         NbRows is 10
    ;
     EntriesOption = b ->
         NbRows is 100
    ;
     EntriesOption = c ->
         NbRows is 1000
    ;
         NbRows is 10000
    ),
    
    % format(Sout, 'table_name,noise_columns,nb_rows,', []),
    format(Sout, '~w,~w,~w,', [Table, NoiseStat, NbRows]),
    % format(Sout, 'task_option,temporal_option,schedule_option,noise_option,entries_option,', []),
    format(Sout, '~w,~w,~w,~w,~w,', [TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption]),

    (memberchk(TaskOption, [d,f,h]) ->
         IsFormulaEndTime is 1
    ;
         IsFormulaEndTime is 0
    ),
    (memberchk(TaskOption, [e,g,h]) ->
         IsFormulaDuration is 1
    ;
         IsFormulaDuration is 0
    ),
    (TemporalOption = a ->
         IsPrecedence is 0
    ;
         IsPrecedence is 1
    ),
    (memberchk(ScheduleOption, [b,f,h]) ->
         IsScheduleResourceDisj is 1
    ;
         IsScheduleResourceDisj is 0
    ),
    (memberchk(ScheduleOption, [c,g,i]) ->
         IsScheduleResourceDiffn is 1
    ;
         IsScheduleResourceDiffn is 0
    ),
    (memberchk(ScheduleOption, [d,f,g]) ->
         IsScheduleCalendarShift is 1
    ;
         IsScheduleCalendarShift is 0
    ),
    (memberchk(ScheduleOption, [e,h,i]) ->
         IsScheduleCalendarCal is 1
    ;
         IsScheduleCalendarCal is 0
    ),
    %  format(Sout, 'is_formula_end_time,is_formula_duration,is_precedence,is_schedule_resource_disj,
    %  is_schedule_resource_diffn,is_schedule_calendar_shift,is_schedule_calendar_cal,', []),
    format(Sout, '~w,~w,~w,~w,~w,~w,~w,', [IsFormulaEndTime, IsFormulaDuration, IsPrecedence, IsScheduleResourceDisj, IsScheduleResourceDiffn, IsScheduleCalendarShift, IsScheduleCalendarCal]),
    
    findall(CostET-ConjectureETCandidate,
            (ConjectureETCandidate = conjecture(_, secondary, col(Table,_), end_time, _, CostET, _, _, _),
             call(ConjectureETCandidate)),
            ConjecturesET),
    (sort(ConjecturesET, [_-ConjectureET|_]) ->
         ConjectureET = conjecture(_, secondary, _, end_time, _, _, t(_, InputsET, _, _), FamilyET, _),
         ((memberchk(FamilyET, [formula, cst]),
           memberchk(InputsET, [[start_time,duration],
                                [start_time,qty,speed]])) ->
              FoundFormulaExactEndTime is 1 * IsFormulaEndTime,
              FoundFormulaPartialEndTime is 0,
              FoundExtraFormulaeEndTime is 1 - FoundFormulaExactEndTime
         ;
          (memberchk(FamilyET, [formula, cst]),
           memberchk(InputsET, [[],              % if it's a constant
                                [duration],      % if start_time is a constant
                                [qty,speed],     % if start_time is a constant
                                [start_time,qty],
                                [start_time,speed],
                                [qty],
                                [speed]])) ->
          %write('partial_end_time'),nl,
          %write(partial-ConjectureET),nl,nl,
              FoundFormulaExactEndTime is 0,
              FoundFormulaPartialEndTime is 1 * IsFormulaEndTime,
              FoundExtraFormulaeEndTime is 1 - FoundFormulaPartialEndTime
         ;
          %write('extra_end_time'),nl,
          %write(extra-ConjectureET),nl,
              FoundFormulaExactEndTime is 0,
              FoundFormulaPartialEndTime is 0,
              FoundExtraFormulaeEndTime is 1
         )
    ;
         FoundFormulaExactEndTime is 0,
         FoundFormulaPartialEndTime is 0,
         FoundExtraFormulaeEndTime is 0
    ),
    
    findall(CostD-ConjectureDCandidate,
            (ConjectureDCandidate = conjecture(_, secondary, col(Table,_), duration, _, CostD, _, _, _),
             call(ConjectureDCandidate)),
            ConjecturesD),
    (sort(ConjecturesD, [_-ConjectureD|_]) ->
         ConjectureD = conjecture(_, secondary, _, duration, _, _, t(_, InputsD, _, _), FamilyD, _),
         ((memberchk(FamilyD, [formula, cst]),
           memberchk(InputsD, [[qty,speed]])) ->
              FoundFormulaExactDuration is 1 * IsFormulaDuration,
              FoundFormulaPartialDuration is 0,
              FoundExtraFormulaeDuration is 1 - FoundFormulaExactDuration
         ;
          (memberchk(FamilyD, [formula, cst]),
           memberchk(InputsD, [[],          % if it's a constant
                               [qty],       % if one of the inputs is a constant
                               [speed]])) -> % if one of the inputs is a constant
          %write('partial_duration'),nl,
          %write(partial-ConjectureD),nl,nl,
              FoundFormulaExactDuration is 0,
              FoundFormulaPartialDuration is 1 * IsFormulaDuration,
              FoundExtraFormulaeDuration is 1 - FoundFormulaPartialDuration
         ;
          %write('extra_duration:'),nl,
          %write(ConjectureD),nl,nl,
              FoundFormulaExactDuration is 0,
              FoundFormulaPartialDuration is 0,
              FoundExtraFormulaeDuration is 1 
         )
    ;
         FoundFormulaExactDuration is 0,
         FoundFormulaPartialDuration is 0,
         FoundExtraFormulaeDuration is 0
    ),
    
    % format(Sout, 'found_formula_exact_end_time,found_formula_partial_end_time,found_extra_formulae_end_time,found_formula_exact_duration,found_formula_partial_duration,found_extra_formulae_duration,', []),
    format(Sout, '~w,~w,~w,~w,~w,~w,', [FoundFormulaExactEndTime,FoundFormulaPartialEndTime,FoundExtraFormulaeEndTime,FoundFormulaExactDuration,FoundFormulaPartialDuration,FoundExtraFormulaeDuration]),

    collect_precedence_ctrs(Table, IsPrecedence, TableCtrs, FoundExactPrecedence, FoundPartialPrecedence, FoundExtraPrecedence),
    %FoundExactPrecedence is 0, FoundPartialPrecedence is 0, FoundExtraPrecedence is 0,
    
    %format(Sout, 'found_exact_precedence,found_partial_precedence,found_extra_precedence,', []),
    format(Sout, '~w,~w,~w,', [FoundExactPrecedence, FoundPartialPrecedence, FoundExtraPrecedence]),

    findall(CostDisj-ConjectureCostDisjCandidate,
            (ConjectureCostDisjCandidate = conjecture(_, resource, col(Table,0), none, _, CostDisj, _, disjunctive, []),
             call(ConjectureCostDisjCandidate)),
            ConjecturesCostDisj),
    (sort(ConjecturesCostDisj, [_-ConjectureCostDisj|_]) ->
         ConjectureCostDisj = conjecture(_, resource, col(Table,0), none, _, _, t(_, InputsDisj, _, disjunctive(FlagDisj)), _, _),
         (memberchk([FlagDisj|InputsDisj], [[0,resource,start_time,duration],
                                            [1,resource,start_time,end_time]]) ->
              FoundExactScheduleResourceDisj is 1 * IsScheduleResourceDisj,             % check the conjecture and assign it in according with the flag
              FoundPartialScheduleResourceDisj is 0,
              FoundExtraScheduleResourceDisj is 1 - FoundExactScheduleResourceDisj
         ;
          check_if_disjunctive_partial(FlagDisj, InputsDisj) ->
          write('disjunctive_partial:'),nl,
          write(partial-[FlagDisj-InputsDisj]),nl,nl,
              FoundExactScheduleResourceDisj is 0,
              FoundPartialScheduleResourceDisj is 1 * IsScheduleResourceDisj,
              FoundExtraScheduleResourceDisj is 1 - FoundPartialScheduleResourceDisj
         ;
          write('disjunctive_extra:'),nl,
          write(extra-[FlagDisj-InputsDisj]),nl,nl,
              FoundExactScheduleResourceDisj is 0,
              FoundPartialScheduleResourceDisj is 0,
              FoundExtraScheduleResourceDisj is 1
         )
    ;
         FoundExactScheduleResourceDisj is 0,
         FoundPartialScheduleResourceDisj is 0,
         FoundExtraScheduleResourceDisj is 0
    ),
    % format(Sout, 'found_exact_schedule_resource_disj,found_partial_schedule_resource_disj,found_extra_schedule_resource_disj,', []),
    format(Sout, '~w,~w,~w,', [FoundExactScheduleResourceDisj, FoundPartialScheduleResourceDisj,FoundExtraScheduleResourceDisj]),

    findall(CostDiffn-ConjectureCostDiffnCandidate,
            (ConjectureCostDiffnCandidate = conjecture(_, resource, col(Table,0), none, _, CostDiffn, _, diffn, []),
             call(ConjectureCostDiffnCandidate)),
            ConjecturesCostDiffn),
    (sort(ConjecturesCostDiffn, [_-ConjectureCostDiffn|_]) ->
         ConjectureCostDiffn = conjecture(_, resource, col(Table,0), none, _, _, t(_, InputsDiffn, _, diffn(FlagDiffn)), _, _),
         (memberchk([FlagDiffn|InputsDiffn], [[0,resource,resource_choices,start_time,duration],
                                              [1,resource,resource_choices,start_time,end_time]]) ->
              FoundExactScheduleResourceDiffn is 1 * IsScheduleResourceDiffn,             % check the conjecture and assign it in according with the flag
              FoundPartialScheduleResourceDiffn is 0,
              FoundExtraScheduleResourceDiffn is 1 - FoundExactScheduleResourceDiffn
         ;
          check_if_diffn_partial(FlagDiffn, InputsDiffn) ->
          write('diffn_partial:'),nl,
          write(partial-[FlagDiffn, InputsDiffn]),nl,nl,
              FoundExactScheduleResourceDiffn is 0,
              FoundPartialScheduleResourceDiffn is 1 * IsScheduleResourceDiffn,
              FoundExtraScheduleResourceDiffn is 1 - FoundPartialScheduleResourceDiffn
         ;
          write('diffn_extra:'),nl,
          write(extra-[FlagDiffn, InputsDiffn]),nl,nl,
              FoundExactScheduleResourceDiffn is 0,
              FoundPartialScheduleResourceDiffn is 0,
              FoundExtraScheduleResourceDiffn is 1
         )
    ;
         FoundExactScheduleResourceDiffn is 0,
         FoundPartialScheduleResourceDiffn is 0,
         FoundExtraScheduleResourceDiffn is 0
    ),
    % format(Sout, 'found_exact_schedule_resource_diffn,found_partial_schedule_resource_diffn,found_extra_schedule_resource_diffn,', []),
    format(Sout, '~w,~w,~w,', [FoundExactScheduleResourceDiffn,FoundPartialScheduleResourceDiffn,FoundExtraScheduleResourceDiffn]),

    findall(ConjectureShiftCandidate,
            (ConjectureShiftCandidate = conjecture(_, resource, col(Table,0), none, _, _, t(_,[shift_times,_,_],[],calendar(_,_,_,_)), calendar, []),
             call(ConjectureShiftCandidate)),
            ConjecturesShift),
    (ConjecturesShift = [_|_] ->
         (find_exact_shift_ctr(ConjecturesShift, ConjecturesShiftRest) ->
              find_partial_ctrs(ConjecturesShiftRest, NPartialS, NExtraS),
              FoundExactCalendarShift is 1 * IsScheduleCalendarShift,             % check the conjecture and assign it in according with the flag
              FoundPartialCalendarShift is NPartialS,
              FoundExtraCalendarShift is NExtraS + 1 - FoundExactCalendarShift
         ;
          find_partial_ctrs(ConjecturesShift, NPartialS, NExtraS) ->
          write('shift_partial:'),nl,
          write(ConjecturesShift),nl,nl,
              FoundExactCalendarShift is 0,
              FoundPartialCalendarShift is NPartialS * IsScheduleCalendarShift,
              FoundExtraCalendarShift is NExtraS + NPartialS - FoundPartialCalendarShift
         )
    ;
         FoundExactCalendarShift is 0,
         FoundPartialCalendarShift is 0,
         FoundExtraCalendarShift is 0
    ),
    
    % format(Sout, 'found_exact_schedule_calendar_shift,found_partial_schedule_calendar_shift,found_extra_schedule_calendar_shift,', []),
    format(Sout, '~w,~w,~w,', [FoundExactCalendarShift,FoundPartialCalendarShift,FoundExtraCalendarShift]),

    findall(ConjectureCalendarCandidate,
            (ConjectureCalendarCandidate = conjecture(_, resource, col(Table,0), none, _, _, t(_,[calendar_times,_,_],[],calendar(_,_,_,_)), calendar, []),
             call(ConjectureCalendarCandidate)),
            ConjecturesCalendar),
    (ConjecturesCalendar = [_|_] ->
         (find_exact_calendar_ctr(ConjecturesCalendar, ConjecturesCalendarRest) ->
              find_partial_ctrs(ConjecturesCalendarRest, NPartialC, NExtraC),
              FoundExactCalendarCalendar is 1 * IsScheduleCalendarCal,             % check the conjecture and assign it in according with the flag
              FoundPartialCalendarCalendar is NPartialC,
              FoundExtraCalendarCalendar is NExtraC + 1 - FoundExactCalendarCalendar
         ;
          find_partial_ctrs(ConjecturesCalendar, NPartialC, NExtraC) ->
          write('calendar_partial:'),nl,
          write(ConjecturesCalendar),nl,nl,
              FoundExactCalendarCalendar is 0,
              FoundPartialCalendarCalendar is NPartialC * IsScheduleCalendarCal,
              FoundExtraCalendarCalendar is NExtraC + NPartialC - FoundPartialCalendarCalendar
         )
    ;
         FoundExactCalendarCalendar is 0,
         FoundPartialCalendarCalendar is 0,
         FoundExtraCalendarCalendar is 0
    ),

    findall(ConjectureCalendarCandidate,
            (ConjectureCalendarCandidate = conjecture(_, resource, col(Table,0), none, _, _, t(_,[CalWrong,_,_],[],calendar(_,_,_,_)), calendar, []),
             nonmember(CalWrong, [calendar_times, shift_times]),
             call(ConjectureCalendarCandidate)),
            ConjecturesCalendarWrongs),
    length(ConjecturesCalendarWrongs, FoundExtraCalendarCalendarWrong),

    (FoundExtraCalendarCalendarWrong >= 1 ->
         write('all the extra calendars:'),nl,
         write_list(ConjecturesCalendarWrongs),nl,nl
    ;
         true
    ),
    
    FoundExtraCalendarCalendar1 is min(1,max(FoundExtraCalendarCalendar, FoundExtraCalendarCalendarWrong)),
    
    % format(Sout, 'found_exact_schedule_calendar_cal,found_partial_schedule_calendar_cal,found_extra_schedule_calendar_cal', []),
    format(Sout, '~w,~w,~w', [FoundExactCalendarCalendar,FoundPartialCalendarCalendar,FoundExtraCalendarCalendar1]),
    true.

check_formula_inputs_extra_formula([], 0) :- !.
check_formula_inputs_extra_formula(Inputs, FoundExtraFormulae) :-
    member(Input, Inputs),
    nonmember(Input, [start_time, duration, qty, speed]),
    !,
    FoundExtraFormulae is 1.
check_formula_inputs_extra_formula(_, 0) :- !.

collect_precedence_ctrs(Table, IsPrecedence, TableCtrs, FoundExactPrecedence, FoundPartialPrecedence, FoundExtraPrecedence) :-
    % 1 - read metadata
    DirData = 'data/model_seeker/data',
    gen_table_and_metadata_file_names(DirData, 0, Table, _TableFile, MetadataFile),
    consult(MetadataFile),
    % 2 - find precedence ctrs
    (IsPrecedence = 1 ->
         memberchk(precedence(CorrectCtrs), TableCtrs)
    ;
         CorrectCtrs = []
    ),
    tab_get_ctr(col(Table,1), Ctrs),
    (memberchk(include_except_default_value_no_cycle(1, _, -1, TemporalCtrs), Ctrs) ->
         true
    ;
         TemporalCtrs = []
    ),
    % 3 - assign to exact, partial or extra
    % 3a - collect attributes (aggregated and by ctrs), signs and coefficients from both versions (remember to convert start+duration to end)
    collect_given_temporals(CorrectCtrs, CorrectAttrsAll, CorrectCtrsTransformed),
    collect_acquired_temporals(TemporalCtrs, TemporalAttrsAll, TemporalCtrsTransformed),
    % 3b - make comparison
    (TemporalCtrs = [] ->
         FoundExactPrecedence is 0,
         FoundPartialPrecedence is 0,
         FoundExtraPrecedence is 0
    ;
     (CorrectCtrs = [], TemporalCtrs = [_|_]) ->
         FoundExactPrecedence is 0,
         FoundPartialPrecedence is 0,
         FoundExtraPrecedence is 1
    ;
     CorrectCtrsTransformed = TemporalCtrsTransformed ->
         FoundExactPrecedence is 1,
         FoundPartialPrecedence is 0,
         FoundExtraPrecedence is 0
    ;
     compare_precedence_attrs(CorrectAttrsAll, TemporalAttrsAll) ->
     %write('partial constraint:'),nl,
     %write(correct-CorrectCtrs),nl,
     %write(partial-TemporalCtrs),nl,nl,
         FoundExactPrecedence is 0,
         FoundPartialPrecedence is 1,
         FoundExtraPrecedence is 0
    ;
     %write('extra constraint:'),nl,
     %write(CorrectAttrsAll-TemporalAttrsAll),nl,
     %write(correct-CorrectCtrs),nl,
     %write(extra-TemporalCtrs),nl,nl,
         FoundExactPrecedence is 0,
         FoundPartialPrecedence is 0,
         FoundExtraPrecedence is 1
    ),
    % 4 - unload the metadata
    remove_metadata_facts(Table).

collect_given_temporals(Ctrs, AttrsAll, CtrsTransformed) :-
    collect_given_temporals1(Ctrs, AttrsAll0, CtrsTransformed),
    sort(AttrsAll0, AttrsAll).

collect_given_temporals1([], [], []) :- !.
collect_given_temporals1([[left(LeftAttr,Coef),Sign,right(RightAttr)]|R],
                         [LeftAttr, RightAttr|S],
                         [[[LeftAttr], Coef, Sign, [RightAttr]] |T]) :-
    collect_given_temporals1(R, S, T).

collect_acquired_temporals(Ctrs, AttrsAll, CtrsTransformed) :-
    collect_acquired_temporals1(Ctrs, AttrsAll0, CtrsTransformed),
    sort(AttrsAll0,AttrsAll).

collect_acquired_temporals1([], [], []) :- !.
collect_acquired_temporals1([precedence(ColsLeft,Coef,Sign,ColsRight)|R],
                            Attributes,
                            [[LeftAttrs,Coef,Sign,RightAttrs]|T]) :-
    transform_precedence_cols( ColsLeft, LeftAttrs),
    transform_precedence_cols(ColsRight,RightAttrs),
    collect_acquired_temporals1(R, S, T),
    append([LeftAttrs, RightAttrs, S], Attributes).

transform_precedence_cols( Cols, Attrs) :-
    (foreach(Col, Cols), foreach(Name, Names) do tab_get_name(Col, Name)),
    (memberchk(Names, [[start_time, duration],
                       [duration, start_time]]) ->
         Attrs = [end_time]
    ;
         Attrs = Names
    ).

compare_precedence_attrs(CorrectAttrsAll, TemporalAttrsAll) :-
    CorrectAttrsAll = TemporalAttrsAll, !.
compare_precedence_attrs(CorrectAttrsAll, TemporalAttrsAll) :-
    memberchk(CorrectAttrsAll,  [[start_time], [end_time], [start_time,end_time], [end_time,start_time]]),
    memberchk(TemporalAttrsAll, [[start_time], [end_time], [start_time,end_time], [end_time,start_time]]).
       

check_if_disjunctive_partial(FlagDisj, InputsDisj) :-   % wrong Flag is assigned
    memberchk([FlagDisj|InputsDisj], [[1,resource,start_time,duration],
                                      [0,resource,start_time,end_time]]),
    !.
check_if_disjunctive_partial(_, InputsDisj) :-          % all columns are correct but the order of temporal cols is wrong
    columns_temporal_columns(ListDisj),
    InputsDisj = [resource|R],
    (foreach(Input, R), param(ListDisj)
    do
     memberchk(Input, ListDisj)
    ).

check_if_diffn_partial(FlagDiffn, InputsDiffn) :-   % wrong Flag is assigned
    memberchk([FlagDiffn|InputsDiffn], [[1,resource,resource_choices,start_time,duration],
                                        [0,resource,resource_choices,start_time,end_time]]),
    !.
check_if_diffn_partial(_, InputsDiffn) :-          % all columns are correct but the order of temporal cols is wrong
    InputsDiffn = [resource, resource_choices|R],
    columns_temporal_columns(ListDiffn),
    (foreach(Input, R), param(ListDiffn)
    do
     memberchk(Input, ListDiffn)
    ),
    !.

find_exact_shift_ctr(ConjecturesShift, ConjecturesShiftRest) :-
    member(ConjectureShift, ConjecturesShift),
    ConjectureShift = conjecture(_, resource, _, none, _, _, t(_,Inputs,[],calendar(start_end,in,Flag,all)), calendar, []),
    memberchk([Flag|Inputs], [[0,shift_times,start_time,duration],
                              [1,shift_times,start_time,end_time]]),
    selectchk(ConjectureShift, ConjecturesShift, ConjecturesShiftRest),
    !.

find_exact_calendar_ctr(ConjecturesCalendar, ConjecturesCalendarRest) :-
    member(ConjectureCalendar, ConjecturesCalendar),
    ConjectureCalendar = conjecture(_, resource, _, none, _, _, t(_,Inputs,[],calendar(start_end,in,Flag,all)), calendar, []),
    memberchk([Flag|Inputs], [[0,calendar_times,start_time,duration],
                              [1,calendar_times,start_time,end_time]]),
    selectchk(ConjectureCalendar, ConjecturesCalendar, ConjecturesCalendarRest),
    !.

find_partial_ctrs(ConjecturesShift, NPartial, NExtra) :-
    find_partial_ctrs1(ConjecturesShift, Partial, Extra),
    length(Partial, NPartial),
    length(Extra,   NExtra).

find_partial_ctrs1([], [], []) :- !.
find_partial_ctrs1([ConjectureShift|R], [ConjectureShift|S], T) :-
    check_shift_partial(ConjectureShift),
    !, find_partial_ctrs1(R, S, T).
find_partial_ctrs1([ConjectureShift|R], S, [ConjectureShift|T]) :-
    find_partial_ctrs1(R, S, T).

check_shift_partial(ConjectureShift) :-
    ConjectureShift = conjecture(_, resource, _, none, _, _, t(_,[_|Inputs],[],calendar(_,_,_,_)), calendar, []),
    columns_temporal_columns(ListCalendar),
    (foreach(Input, Inputs), param(ListCalendar)
    do
     memberchk(Input, ListCalendar)
    ),
    !.
