% Purpose: GENERATE SCHEDULES TABLES FOR ROBUSTNESS TEST
% Author : Ramiz Gindullin, IMT Atlantique

:- use_module(library(lists)).
:- use_module(library(samsort)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(timeout)).
:- use_module(utility).


% [gen_data_schedules], generate_file_name(a, a, a, a, 2, TableName, TableFile), write(TableName-TableFile), nl, halt.

% [gen_data_schedules], generate_all_tables, halt.

% [gen_data_schedules].

% [gen_data_schedules], top, halt.

top :- generate_all_tables([a,b,c,d,e,f,g,h]).

% as an alternative solution you can use these predicates to parallelise the table generation. See the predicate write_info/2 and modify it accordingly
% then merge files 'schedule_robustness/schedule_robustness_dimensions_a.pl'--'schedule_robustness/schedule_robustness_dimensions_h.pl' into one file 'schedule_robustness/schedule_robustness_dimensions.pl'
top1:- generate_all_tables([a]).
top2:- generate_all_tables([b]).
top3:- generate_all_tables([c]).
top4:- generate_all_tables([d]).
top5:- generate_all_tables([e]).
top6:- generate_all_tables([f]).
top7:- generate_all_tables([g]).
top8:- generate_all_tables([h]).

generate_all_tables(List) :-
    %open(..., write, ROut), % store information about each generated table
    findall([TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, Ctrs],
            (member(TaskOption,     List),
             member(TemporalOption, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q]),
             member(ScheduleOption, [a,b,c,d,e,f,g,h,i]),
             member(NoiseOption,    [a,b]),
             member(EntriesOption,  [a,b,c,d]),
             no_contradictory_options(TaskOption, TemporalOption, ScheduleOption, EntriesOption),
             setrand(200),
             ExNumber in 0..9, indomain(ExNumber),
             generate_table(TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, Ctrs)
            ),
            AllInfo),
    write_info(List, AllInfo),
    nl.

no_contradictory_options(TaskOption, TemporalOption, ScheduleOption, _EntriesOption) :-
    ((memberchk(TaskOption, [c,g]),
      memberchk(TemporalOption, [b,c,d,e,f,g,h,i,j,k,l,m])) ->
         false
    ;
     (memberchk(TaskOption, [c,g]),
      memberchk(ScheduleOption, [b,c,d,e,f,g,h,i])) ->
         false
    ;
     (memberchk(TaskOption, [a,b,c,d]),
      TemporalOption = a, ScheduleOption = a) ->
         false
    ;
     (memberchk(TemporalOption, [c,d,e,g,h,i,o,p,q]),
      memberchk(ScheduleOption, [b,c,f,g,h,i])) ->
         false
    ;
         true
    ).

%write_info(List, AllInfo) :-																			% to parallelise the table generation uncomment this line
    %List = [Suffix|_],																					% to parallelise the table generation uncomment this line
    %atoms_concat(['schedule_robustness/schedule_robustness_dimensions_',Suffix,'.pl'], FileName),		% to parallelise the table generation uncomment this line
write_info(_List, AllInfo) :-																			% to parallelise the table generation   comment this line
	atoms_concat(['schedule_robustness/schedule_robustness_dimensions','.pl'], FileName),				% to parallelise the table generation   comment this line
    open(FileName, write, SOut),
    (foreach([TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, Ctrs],
             AllInfo),
     param(SOut)
    do
     write_table_info(SOut, TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, Ctrs)
    ),
    close(SOut).
write_table_info(SOut, TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, Ctrs) :- % for tests purposes, disabled by default
    !,
    generate_file_name(TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, TableName, _TableFile),
    ScheduleTerm = schedule_robustness_dimensions(TableName, TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, Ctrs),
    format(SOut, '~w.~n', [ScheduleTerm]),
    true.

generate_table(TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, Ctrs) :-
    %write(commence(TaskOption, TemporalOption, ScheduleOption, EntriesOption, ExNumber)),nl,
    generate_file_name(TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, TableName, TableFile),

    %write(name(TableName)),nl,
    % 1. Generate and solve model
    generate_schedule(TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, TableName, Ctrs, AllCols),

    %write(generated),nl,

    % 2. Write model to the file
    open(TableFile, write, SOut),
    write_schedule(SOut, TableName, AllCols),
    close(SOut),
    true.


generate_file_name(TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, ExNumber, TableName, TableFile) :-
    KindCombi = model_seeker,
    atom_concat('data/', KindCombi, PrefixName0),
    atom_concat(PrefixName0, '/data0/', PrefixName),
    (foreach(X,  [TaskOption,  TemporalOption,  ScheduleOption,  NoiseOption,  EntriesOption,  ExNumber]),
     foreach(XA, [TaskOptionA, TemporalOptionA, ScheduleOptionA, NoiseOptionA, EntriesOptionA, ExNumberA])
    do
     to_atom(X, XA)
    ),
    atoms_concat(['schedule_robustness_',TaskOptionA, '_', TemporalOptionA, '_', ScheduleOptionA,
                  '_', NoiseOptionA, '_', EntriesOptionA, '_', ExNumberA],
                 TableName),
    atom_concat(PrefixName, TableName, PrefixNameFunctor),
    atom_concat(PrefixNameFunctor, '.pl', TableFile).


generate_schedule(TaskOption, TemporalOption, ScheduleOption, NoiseOption, EntriesOption, TableName, Ctrs, AllCols) :-
    nl,
    %write(TableName),
    statistics(total_runtime,[Start|_]),

    %write(a1),nl,

    % N is the number of entries
    % M is the number of resources to assign to
    (EntriesOption = a ->
         N is 10,
         M is 2
    ;
     EntriesOption = b ->
         N is 100,
         M is 10
    ;
     EntriesOption = c ->
         N is 1000,
         M is 20
    ;
     EntriesOption = d ->
         N is 10000,
         M is 100
    ),
    (for(I,1,N), foreach(PK, PrimaryKeys)
    do
     PK is I + 1200000
    ),
    PKCols = [col(PrimaryKeys, task_id, primary, input)],
    length(StartTimes, N),
    length(  EndTimes, N),
    length( Durations, N),
    MaxTime is 200 * N div M,
    (foreach(ST, StartTimes), foreach(ET, EndTimes), foreach(DT, Durations),
     param(MaxTime)
    do
     ST in 0..MaxTime,
     ET in 0..MaxTime,
     DT in 0..60,
     ET #= ST + DT
    ),
    %write(a2),nl,
    
    length( Resources, N),
    (foreach(ResourceId, Resources), param(M)
    do
     M1 is M + 1,
     random(1, M1, Resource),
     generate_resource_id(Resource, ResourceId)
    ),
    (for(I1,1,M),
     foreach(Id-Speed, ResourceSpeeds),
     foreach(Id, ResourceIds)
    do
     generate_resource_id(I1, Id),
     random(1,11,Speed)
    ),
    (foreach(Resource1, Resources), foreach(Speed1, Speeds),
     param(ResourceSpeeds)
    do
     memberchk(Resource1-Speed1, ResourceSpeeds)
    ),
    %write(a3),nl,
    
    (TemporalOption = a ->
         TemporalCols = [],
         (for(_I,1,N), foreach(Quantity, Quantities)
         do
          random(1, 6, Quantity)
         ),
         CtrPrecedence = []
    ;
         generate_successors(PrimaryKeys, 0, Successors),
         random(1, 11,  MinCst),
         random(1, 21, DiffCst),
         MaxCst is MinCst + DiffCst,
         generate_quantities(Successors, Quantities),
         generate_temporal_ctrs(TemporalOption, MinCst, MaxCst, PrimaryKeys, Successors, StartTimes, EndTimes),
         TemporalCols = [col(Successors, successors, primary, output)],
         form_precedence_ctr(TemporalOption,MinCst,MaxCst,CtrPrecedence)
    ),

    TaskResourceCols = [col(Quantities,     qty, primary,  input),
                        col(Resources, resource, primary, output),
                        col(Speeds,       speed, primary,  input)],

    %write(a4(TaskOption)),nl,
    
    (memberchk(TaskOption, [a,b,c,d]) ->
         DurationFlag = primary,
         EndTimeFlag = variable,
         (foreach(DT0, Durations)
         do
          random(1, 61, DT0)
         )
    ;
         DurationFlag = secondary,
         EndTimeFlag = secondary,
         random(0, 10, CstSpeed),
         (foreach(DTF, Durations),
          foreach(Qty, Quantities),
          foreach(Speed2, Speeds),
          param(CstSpeed)
         do
          DTF #= Qty * Speed2 + CstSpeed
         )
    ),

    %write(a5),nl,
    (memberchk(TaskOption, [a,e]) ->
         TaskCols = [col(StartTimes, start_time,     variable, input),
                     col( Durations,   duration, DurationFlag, input)]
    ;
     memberchk(TaskOption, [b,f]) ->
         TaskCols = [col(StartTimes, start_time,     variable, input),
                     col(  EndTimes,   end_time,  EndTimeFlag, output)]
    ;
     memberchk(TaskOption, [c,g]) ->
         TaskCols = [col(  EndTimes,   end_time,     variable, output),
                     col( Durations,   duration, DurationFlag, input)]
    ;
     memberchk(TaskOption, [d,h]) ->
         TaskCols = [col(StartTimes, start_time,     variable, input),
                     col(  EndTimes,   end_time,    secondary, output),
                     col( Durations,   duration, DurationFlag, input)]
    ),
    % format: output_name-formula_to_acquire
    (TaskOption = d -> CtrFormulae = [formulae([end_time-plus(start_time,duration)])]                   ;
     TaskOption = e -> CtrFormulae = [formulae([duration-plus(prod(qty,speed),CstSpeed)])]              ;
     TaskOption = f -> CtrFormulae = [formulae([end_time-plus(start_time,prod(qty,speed),CstSpeed)])]   ;
     TaskOption = g -> CtrFormulae = [formulae([duration-plus(prod(qty,speed),CstSpeed)])]              ;
     TaskOption = h -> CtrFormulae = [formulae([end_time-plus(start_time,duration),
                                                duration-plus(prod(qty,speed),CstSpeed)])]              ;
                       CtrFormulae = [formulae([])]                                                     ),
    %write(a6),nl,
    
    (memberchk(ScheduleOption, [b,f,h]) ->
         (memberchk(TaskOption, [a,e]) ->
              CtrDisj = [disjunctive([[0,resource,start_time,duration]])]
         ;
          memberchk(TaskOption, [b,f]) ->
              CtrDisj = [disjunctive([[1,resource,start_time,end_time]])]
         ;
          memberchk(TaskOption, [c,g]) ->
              CtrDisj = []
         ;
              CtrDisj = [disjunctive([[0,resource,start_time,duration],         % two equivalent options
                                      [1,resource,start_time,end_time]])]
         )
    ;
         CtrDisj = []
    ),

    %write(a7),nl,

    % generate additional column for DIFFN
    (memberchk(ScheduleOption, [c,g,i]) ->
         (foreach(ResourceId2, Resources),
          foreach(TaskResource, TaskChoices),
          param(ResourceIds)
         do
          selectchk(ResourceId2, ResourceIds, Rest),
          random_subseq(Rest, RestRandom, _),
          TaskResourceUnsorted = [ResourceId2|RestRandom],
          sort(TaskResourceUnsorted, TaskResource)
         ),
         ScheduleColsDiffn = [col(TaskChoices, resource_choices, primary, input)],
         (memberchk(TaskOption, [a,e]) ->
              CtrDiffn = [diffn([[0,resource,resource_choices,start_time,duration]])]
         ;
          memberchk(TaskOption, [b,f]) ->
              CtrDiffn = [diffn([[1,resource,resource_choices,start_time,end_time]])]
         ;
          memberchk(TaskOption, [c,g]) ->
              CtrDiffn = []
         ;
              CtrDiffn = [diffn([[0,resource,resource_choices,start_time,duration],         % two equivalent options
                                 [1,resource,resource_choices,start_time,end_time]])]
         )
    ;
         ScheduleColsDiffn = [],
         CtrDiffn = []
    ),

    %write(a8),nl,
    
    % put SHIFT column, add a constraint
    (memberchk(ScheduleOption, [d,f,g]) ->
         generate_shift(MaxTime, Shift),
         (ScheduleOption = d ->
             (foreach(Shift, ShiftTimes), foreach(ST1, StartTimes), foreach(ET1, EndTimes), foreach(DT1, Durations),
              param([Shift, MaxTime])
             do
              post_calendar_ctr(Shift, ST1, ET1, DT1, MaxTime)
             )
         ;
             (foreach(_, StartTimes), foreach(Shift, ShiftTimes), param(Shift) do true),
             collect_calendar_gaps(Shift, MaxTime, _MaxTimeR, ShiftGaps)%,
             %nl,write(MaxTime-Shift),nl,write(ShiftGaps),nl
         ),
         ShiftCols = [col(ShiftTimes, shift_times, primary, input)],
         (memberchk(TaskOption, [a,e]) ->
              CtrShift = [shift([[start_end,in,all,0,start_time,duration]])]
         ;
          memberchk(TaskOption, [b,f]) ->
              CtrShift = [shift([[start_end,in,all,1,start_time,end_time]])]
         ;
          memberchk(TaskOption, [c,g]) ->
              CtrShift = []
         ;
              CtrShift = [shift([[start_end,in,all,0,start_time,duration],      % two equivalent options
                                 [start_end,in,all,1,start_time,end_time]])]
         )
    ;
         ShiftCols = [],
         CtrShift = []
    ),

    %write(a9),nl,

    % put calendar column, add a constraint
    (memberchk(ScheduleOption, [e,h,i]) ->
         % generate times for all resources
         (for(I3, 1, M), foreach(Id1-Calendar, ResourceCalendars),
          param(MaxTime)
         do
          generate_resource_id(I3, Id1),
          generate_calendar(MaxTime, Calendar)
         ),
         % for each task of a resource generate the ctr
         (ScheduleOption = e ->
             (foreach(ST2, StartTimes), foreach(ET2, EndTimes), foreach(DT2, Durations),
              foreach(Resource3, Resources), foreach(Calendar1, CalendarTimes),
              param(ResourceCalendars, MaxTime)
             do
              memberchk(Resource3-Calendar1, ResourceCalendars),
              post_calendar_ctr(Calendar1, ST2, ET2, DT2, MaxTime)
             )
         ;
             (foreach(Resource4, Resources), foreach(Calendar2, CalendarTimes),
              param(ResourceCalendars)
             do
              memberchk(Resource4-Calendar2, ResourceCalendars)
             ),
             (foreach(Id2-Calendar2,     ResourceCalendars),
              foreach(Id2-CalendarGaps, ResourceCalendarGaps),
              param(MaxTime)
             do
              collect_calendar_gaps(Calendar2, MaxTime, _, CalendarGaps)
             )
         ),
         CalendarCols = [col(CalendarTimes, calendar_times, primary, input)],
         (memberchk(TaskOption, [a,e]) ->
              CtrCal = [calendar([[start_end,in,all,0,start_time,duration]])]
         ;
          memberchk(TaskOption, [b,f]) ->
              CtrCal = [calendar([[start_end,in,all,1,start_time,end_time]])]
         ;
          memberchk(TaskOption, [c,g]) ->
              CtrCal = []
         ;
              CtrCal = [calendar([[start_end,in,all,0,start_time,duration],     % two equivalent options
                                  [start_end,in,all,1,start_time,end_time]])]
         )
    ;
         CalendarCols = [],
         CtrCal = []
    ),
    
    %write(a10),nl,
    
    % Post DISJUNCTIVE constraint on each resource
    (memberchk(ScheduleOption, [b,c]) ->
         (for(I2, 1, M),
          param([StartTimes, Durations, Resources])
         do
          generate_resource_id(I2, ResourceId1),
          collect_tasks_per_resource(ResourceId1, StartTimes, Durations, Resources, ResourceIdStartTimesDurations),
          disjoint1(ResourceIdStartTimesDurations) 
         )
    ;
     memberchk(ScheduleOption, [f,g]) ->
         (for(I2, 1, M),
          param([StartTimes, Durations, Resources, ShiftGaps])
         do
          generate_resource_id(I2, ResourceId3),
          collect_tasks_per_resource(ResourceId3, StartTimes, Durations, Resources, ResourceIdStartTimesDurations),
          append(ResourceIdStartTimesDurations, ShiftGaps, ResourceIdStartTimesDurationsGaps),
          disjoint1(ResourceIdStartTimesDurationsGaps) 
         )
    ;
     memberchk(ScheduleOption, [h,i]) ->
         (foreach(ResourceId4-CalendarGaps1, ResourceCalendarGaps),
          param([StartTimes, Durations, Resources])
         do
          collect_tasks_per_resource(ResourceId4, StartTimes, Durations, Resources, ResourceIdStartTimesDurations),
          append(ResourceIdStartTimesDurations, CalendarGaps1, ResourceIdStartTimesDurationsGaps),
          disjoint1(ResourceIdStartTimesDurationsGaps) 
         )
    ;
         true
    ),

    %write(a11),nl,

    (NoiseOption = a ->
         NoiseCols = []
    ;
         (for(_,1,N),
          foreach(WhiteNoise1, WhiteNoises1),
          foreach(WhiteNoise2, WhiteNoises2),
          foreach(WhiteNoise3, WhiteNoises3)
         do
          random(0, 1000, WhiteNoise1),
          random(0, 1000, WhiteNoise2),
          random(0, 1000, WhiteNoise3)
         ),
         NoiseCols = [col(WhiteNoises1, noise1, primary, input),
                      col(WhiteNoises2, noise2, primary, input),
                      col(WhiteNoises3, noise3, primary, input)]
    ),

    %write(a12),nl,
    
    append([PKCols, TaskCols, TaskResourceCols, TemporalCols, NoiseCols, ScheduleColsDiffn, ShiftCols, CalendarCols], AllCols),
    %write(a12a(CtrPrecedence,CtrFormulae,CtrDisj,CtrDiffn,CtrShift,CtrCal)),nl,
    append([CtrPrecedence,CtrFormulae,CtrDisj,CtrDiffn,CtrShift,CtrCal], Ctrs),
    %write(a12b),nl,
    length(AllCols, NbCol),
    TableTerm = table(model_seeker, TableName, [0-NbCol], 0, [pk([1]), no_fk]),
    write(TableTerm), write('. % '),
    write(' Model created...DONE.'),

    %write(a13),nl,
    
    labeling([], Durations),

    TimeOut = 180000,
    (time_out(label_task_variables(StartTimes, EndTimes),
              TimeOut,
              success) ->
        write(' Solved....Done! The time: ')
    ;
     label_task_variables_alt(StartTimes, EndTimes) ->
        write(' Solved....Done! The time: ')
    ;
        write(' NOT SOLVED AT ALL!!!!')
    ),
    statistics(total_runtime,[Stop|_]),
    Runtime is Stop - Start,
    write(Runtime), write('ms'),
    %write_list([Successors, StartTimes, EndTimes, Durations]),nl,
    !.

generate_resource_id(Resource, Id) :-
    Id is Resource * 10 + 2.

generate_successors([_], _, [-1]) :- !.
generate_successors([PrimaryKey|R], Count, [Successor|S]) :-
    ((Count =< 5, maybe(0.75)) ->
         Successor is PrimaryKey + 1,
         Count1 is Count + 1
    ;
         Successor is -1,
         Count1 is 0
    ),
    generate_successors(R, Count1, S).

generate_quantities(Successors, Quantities) :-
    random(1, 6, Qty),
    generate_quantities(Successors, Quantities, Qty).

generate_quantities([], [], _) :- !.
generate_quantities([-1|R], [Qty|S], Qty) :-
    !,
    random(1, 6, Qty1),
    generate_quantities(R, S, Qty1).
generate_quantities([_|R], [Qty|S], Qty) :-
    generate_quantities(R, S, Qty).

generate_temporal_ctrs(_TemporalOption, _MinCst, _MaxCst, [_], [-1], [_], [_]) :- !.
generate_temporal_ctrs(TemporalOption, MinCst, MaxCst, [_|R], [-1|S], [_|X], [_|Y]):-
    !,
    generate_temporal_ctrs(TemporalOption, MinCst, MaxCst, R, S, X, Y).
generate_temporal_ctrs(TemporalOption, MinCst, MaxCst, [_PK1,PK2|R], [_Successor1,Successor2|S],
                       [StartTime1,StartTime2|X], [EndTime1,EndTime2|Y]):-
    (TemporalOption = b ->
         StartTime1 + MinCst #=< StartTime2
    ;
     TemporalOption = c ->
         StartTime1 + MinCst #>= StartTime2
    ;
     TemporalOption = d ->
         (StartTime1 + MinCst #=< StartTime2) #/\
         (StartTime1 + MaxCst #>= StartTime2)
    ;
     TemporalOption = e ->
         StartTime1 + MinCst #= StartTime2
    ;
     
     TemporalOption = f ->
         StartTime1 + MinCst #=< EndTime2
    ;
     TemporalOption = g ->
         StartTime1 + MinCst #>= EndTime2
    ;
     TemporalOption = h ->
         (StartTime1 + MinCst #=< EndTime2) #/\
         (StartTime1 + MaxCst #>= EndTime2)
    ;
     TemporalOption = i ->
         StartTime1 + MinCst #= EndTime2
    ;
     
     TemporalOption = j ->
         EndTime1 + MinCst #=< StartTime2
    ;
     TemporalOption = k ->
         EndTime1 + MinCst #>= StartTime2
    ;
     TemporalOption = l ->
         (EndTime1 + MinCst #=< StartTime2) #/\
         (EndTime1 + MaxCst #>= StartTime2)
    ;
     TemporalOption = m ->
         EndTime1 + MinCst #= StartTime2
    ;
     
     TemporalOption = n ->
         EndTime1 + MinCst #=< EndTime2
    ;
     TemporalOption = o ->
         EndTime1 + MinCst #>= EndTime2
    ;
     TemporalOption = p ->
         (EndTime1 + MinCst #=< EndTime2) #/\
         (EndTime1 + MaxCst #>= EndTime2)
    ;
     TemporalOption = q ->
         EndTime1 + MinCst #= EndTime2
    ;
         write(generate_temporal_ctrs), nl, halt
    ),
    generate_temporal_ctrs(TemporalOption, MinCst, MaxCst, [PK2|R], [Successor2|S], [StartTime2|X], [EndTime2|Y]).

collect_tasks_per_resource(_, [], [], [], []) :- !.
collect_tasks_per_resource(ResourceId, [StartTime|R], [Duration|S], [ResourceId|T], [t(StartTime, Duration)|U]) :-
    !, collect_tasks_per_resource(ResourceId, R, S, T, U).
collect_tasks_per_resource(ResourceId, [_|R], [_|S], [_|T], U) :-
    !, collect_tasks_per_resource(ResourceId, R, S, T, U).

generate_shift(MaxTime, Shift) :-
    generate_shift(0, 0, MaxTime, Shift).

generate_shift(Time, _, MaxTime, []) :-
    Time >= MaxTime, !.
generate_shift(_, 20, _, []) :- !.
generate_shift(Time, Count, MaxTime, [TimeS-TimeE|ShiftNew]) :-
    TimeS is Time + 1,
    TimeE is Time + 500,
    Count1 is Count + 1,
    generate_shift(TimeE, Count1, MaxTime, ShiftNew).

generate_calendar(MaxTime, Calendar) :-
    (for(_, 1, 20), foreach(CalendarS-CalendarD, CalendarPairsUnprocessed)
    do
     random(11, 41, CalendarS),
     random(476, 776, CalendarD)
    ),
    aggregate_shift_pairs(CalendarPairsUnprocessed, MaxTime, Calendar0),
    remove_error_shifts(Calendar0, Calendar).

aggregate_shift_pairs(CalendarPairsUnprocessed, MaxTime, Shift) :-
    aggregate_shift_pairs(CalendarPairsUnprocessed, 0, MaxTime, Shift).

aggregate_shift_pairs([ShiftS-_], StartTime, MaxTime, [ShiftS1-MaxTime]) :-
    !,
    ShiftS1 is StartTime + ShiftS.
aggregate_shift_pairs([ShiftS-ShiftD|R], StartTime, MaxTime, [ShiftS1-ShiftE1|S]) :-
    ShiftS1 is StartTime + ShiftS,
    ShiftE1 is ShiftS1 + ShiftD,
    aggregate_shift_pairs(R, ShiftE1, MaxTime, S).

remove_error_shifts([], []) :- !.
remove_error_shifts([Low-Up|R], [Low-Up|S]) :-
    Low =< Up, !,
    remove_error_shifts(R, S).
remove_error_shifts([_|R], S) :-
    remove_error_shifts(R, S).



post_calendar_ctr(Calendar, ST, ET, DT, MaxTime) :-
%    write(a1),nl,
    collect_calendar_gaps(Calendar, MaxTime, MaxTimeR, CalendarGaps),
%    write(a2),nl,
    ST in 0..MaxTimeR,
    ET in 0..MaxTimeR,
%    write(CalendarGaps),nl,
    disjoint1([t(ST, DT)|CalendarGaps]).

collect_calendar_gaps([CalendarS-CalendarE|R], MaxTime, MaxTimeR, CalendarGaps) :-
%    write(b0),nl,
    (CalendarS = 0 ->
         CalendarGaps = CalendarGaps1
    ;
         CalendarGaps = [t(0, CalendarS)|CalendarGaps1]
    ),
%    write(b1),nl,
    collect_calendar_gaps1([CalendarS-CalendarE|R], MaxTime, MaxTimeR, CalendarGaps1). % the task must not intersect with gaps between shifts

collect_calendar_gaps1([_-CalendarE], MaxTime, CalendarE, [t(CalendarE, Duration)]) :-
%    write(c3),nl,
    !, Duration is MaxTime - CalendarE.
collect_calendar_gaps1([_-CalendarE1, CalendarS2-CalendarE2|R], MaxTime, MaxTimeR, [t(CalendarE1, CalendarDuration)|S]) :-
%    write(c1),nl,
    CalendarDuration is CalendarS2 - CalendarE1,
    collect_calendar_gaps1([CalendarS2-CalendarE2|R], MaxTime, MaxTimeR, S).

label_task_variables(StartTimes, EndTimes) :-
    labeling([], StartTimes),
    labeling([up], EndTimes).

label_task_variables_alt(StartTimes, EndTimes) :-
    labeling([ffc,middle], StartTimes),
    labeling([], EndTimes).

write_schedule(SOut, TableName, AllCols) :-
    length(AllCols, N),
    format(SOut, ':- multifile signature/3.~n', []),
    format(SOut, ':- multifile ~w/~w.~n', [TableName, N]),
    format(SOut, ':- dynamic signature/3.~n', []),
    format(SOut, ':- dynamic ~w/~w.~n~n', [TableName, N]),

    (foreach(col(ColValues, ColName, Kind, InOut), AllCols),
     foreach(t(ColName, Kind, InOut), SignatureCols),
     foreach(ColValues, Columns)
    do
     true
    ),
    functor(SignatureTerm, t, N),
    (for(I, 1, N),
     foreach(SignatureColTerm, SignatureCols),
     param(SignatureTerm)
    do
     arg(I, SignatureTerm, SignatureColTerm)
    ),
    format(SOut, '~w.~n~n', [signature(TableName, 0, SignatureTerm)]),
      
    transpose(Columns, Entries),
    (foreach(Entry, Entries),
     param([SOut, TableName, N])
    do
     functor(EntryTerm, TableName, N),
     (for(I, 1, N), foreach(Val, Entry), param(EntryTerm)
     do
      arg(I, EntryTerm, Val)
     ),
     format(SOut, '~w.~n', [EntryTerm])
    ).

form_precedence_ctr(a,     _,     _,[precedence([])]) :- !.

form_precedence_ctr(b,MinCst,     _,[precedence([[left(start_time, MinCst),=<,right(start_time)]])]) :- !.
form_precedence_ctr(c,MinCst,     _,[precedence([[left(start_time, MinCst),>=,right(start_time)]])]) :- !.
form_precedence_ctr(d,MinCst,MaxCst,[precedence([[left(start_time, MinCst),=<,right(start_time)],
                                                 [left(start_time, MaxCst),>=,right(start_time)]])]) :- !.
form_precedence_ctr(e,MinCst,     _,[precedence([[left(start_time, MinCst), =,right(start_time)]])]) :- !.

form_precedence_ctr(f,MinCst,     _,[precedence([[left(start_time, MinCst),=<,right(  end_time)]])]) :- !.
form_precedence_ctr(g,MinCst,     _,[precedence([[left(start_time, MinCst),>=,right(  end_time)]])]) :- !.
form_precedence_ctr(h,MinCst,MaxCst,[precedence([[left(start_time, MinCst),=<,right(  end_time)],
                                                 [left(start_time, MaxCst),>=,right(  end_time)]])]) :- !.
form_precedence_ctr(i,MinCst,     _,[precedence([[left(start_time, MinCst), =,right(  end_time)]])]) :- !.

form_precedence_ctr(j,MinCst,     _,[precedence([[left(  end_time, MinCst),=<,right(start_time)]])]) :- !.
form_precedence_ctr(k,MinCst,     _,[precedence([[left(  end_time, MinCst),>=,right(start_time)]])]) :- !.
form_precedence_ctr(l,MinCst,MaxCst,[precedence([[left(  end_time, MinCst),=<,right(start_time)],
                                                 [left(  end_time, MaxCst),>=,right(start_time)]])]) :- !.
form_precedence_ctr(m,MinCst,     _,[precedence([[left(  end_time, MinCst), =,right(start_time)]])]) :- !.

form_precedence_ctr(n,MinCst,     _,[precedence([[left(  end_time, MinCst),=<,right(  end_time)]])]) :- !.
form_precedence_ctr(o,MinCst,     _,[precedence([[left(  end_time, MinCst),>=,right(  end_time)]])]) :- !.
form_precedence_ctr(p,MinCst,MaxCst,[precedence([[left(  end_time, MinCst),=<,right(  end_time)],
                                                 [left(  end_time, MaxCst),>=,right(  end_time)]])]) :- !.
form_precedence_ctr(q,MinCst,     _,[precedence([[left(  end_time, MinCst), =,right(  end_time)]])]) :- !.
