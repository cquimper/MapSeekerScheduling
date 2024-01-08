% Purpose: Program to run to generate resource constraints (disjunctive, diffn, cumulative, calendar) in the context of the model seeker (not used for combinatorial objects);
%          this has to be executed AFTER running the main program that generates formulae for some attributes of the tables (as used formula to identify some task attributes).
%          Handle the different tables which are mentionned in some acquired formula.
% Authors: Nicolas Beldiceanu, Ramiz Gindullin, IMT Atlantique

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(tables).
:- use_module(table_access).
:- use_module(utility).
:- use_module(gen_candidate_tables).
:- use_module(test_if_scheduling_attribute). % NEW

:- multifile table_metadata/31.
:- dynamic table_metadata/31.

enable_conditional_calendar(false).     % set to true to use conditional calendars

% TODO: use Mode to calculate resource ctrs in the cost calculation so we have slight preference to duration flag
top :-
    top(1, 1).

top(SplitDiv, SplitMod) :-
    ConjectureFilePrefix = 'data/model_seeker/found_model',
    atoms_concat([ConjectureFilePrefix, '_', SplitDiv, '_', SplitMod, '.pl'], ConjectureFile),
    consult(ConjectureFile),
    findall(ConjId, conjecture(id(_,_,ConjId),_,_,_,_,_,_,_,_), LConjId), % get last used id as may create new conjectures
    (LConjId = [] -> LastConjId = 0 ; max_member(LastConjId, LConjId)),
    init_cpt(conjecture_id, LastConjId),
    get_tables(model_seeker, _, _, _, ListTables),
    sort(ListTables, SortListTables),
    findall(TableName, member_split(SortListTables, SplitDiv, SplitMod, TableName), ListTablesSlices),
    gen_resource_ctrs(ListTablesSlices, ResourcesCtrs),
    open(ConjectureFile, append, SOut),
    (foreach(Table, ListTablesSlices), foreach(ResourceCtrs, ResourcesCtrs), param(SOut)
    do
     (foreach(ResourceCtr, ResourceCtrs), param([SOut,Table])
     do
      (ResourceCtr = Cost-diffn(CtrMode,CtrMachineAttr,CtrMachinesAttr,CtrStartAttr,CtrDurEndAttr) ->
          InputCols = [col(Table, CtrMachineAttr),
                       col(Table, CtrMachinesAttr),
                       col(Table, CtrStartAttr),
                       col(Table, CtrDurEndAttr)],
          ResourceCtrConjecture = diffn(CtrMode)
      ;
       ResourceCtr = Cost-disjunctive(CtrMode,CtrMachineAttr,CtrStartAttr,CtrDurEndAttr) ->
          InputCols = [col(Table, CtrMachineAttr),
                       col(Table, CtrStartAttr),
                       col(Table, CtrDurEndAttr)],
          ResourceCtrConjecture = disjunctive(CtrMode)
      ;
       ResourceCtr = calendar(CtrStartEnd,CtrInOut,CtrMode,CtrCalendarAttr,CtrStartAttr,CtrDurEndAttr,CtrTypeVal) ->
          InputCols = [col(Table, CtrCalendarAttr),
                       col(Table, CtrStartAttr),
                       col(Table, CtrDurEndAttr)],
          ResourceCtrConjecture = calendar(CtrStartEnd,CtrInOut,CtrMode,CtrTypeVal)
      ;
          write(gen_resource_ctrs1(ResourceCtr)), nl, halt
      ),
      (foreach(InputCol,InputCols), foreach(ColName, InputColsNames), param(Table)
      do
       tab_get_name(InputCol, ColName)
      ),
      increment_cpt(conjecture_id, NextConjId),
      functor(ResourceCtrConjecture, ResourceCtrName, _),
      ResourceConjecture = conjecture(id(model_seeker, 0, NextConjId),
                                      resource,
                                      col(Table,0), % Output col.is set to 0 because this type of conjecture doesn't have output cols
                                      none,
                                      0,
                                      Cost,
                                      t(InputCols, InputColsNames, [], ResourceCtrConjecture),
                                      ResourceCtrName,
                                      []),
      portray_clause(SOut, ResourceConjecture)
     )
    ),
    close(SOut).

gen_resource_ctrs([], []) :- !.
gen_resource_ctrs([Table|R], [ResourceCtrs|S]) :-
    DirData = 'data/model_seeker/data',
    gen_table_and_metadata_file_names(DirData, 0, Table, TableFile, MetadataFile),
    consult(TableFile),
    consult(MetadataFile),
    findall(OutputCol-Formula, conjecture(_,_Kind,col(Table,OutputCol),_OutputName,_MaxN,_Cost,Formula,_,_), Conjectures),
    extract_non_machine_attrs(Conjectures, NonMachineAttrs1),                                         % first use conjectures to some non machine attributes                     % NEW
    tab_get_arity(col(Table,_),  NbColumns),                                                          % get number of columns of table Table                                     % NEW
    get_temporal_attrs_from_column_names_filtered(Table, NbColumns, TempAttrs),                       % try to identify start, duration, end attributes                          % NEW    
    append(NonMachineAttrs1, TempAttrs, NonMachineAttrs2),                                            % start, duration, end attributes cannot correspond to a machine attribute % NEW
    tab_get_arity(col(Table,_),  NbColumns),                                                          % get number of columns of table Table                                     % NEW
    get_not_specific_scheduling_attr_from_column_names_filtered(Table, NbColumns,
                                                                resource, NonMachineAttrs3),          % try to use column names to identify non machine attributes               % NEW
    append(NonMachineAttrs2, NonMachineAttrs3, NonMachineAttrs),                                      % put together non machine attributes                                      % NEW    
    sort(NonMachineAttrs, SortedNonMachineAttrs),
    gen_resource_ctr(Table, Conjectures, SortedNonMachineAttrs, ResourceCtrs),
    remove_signature_and_table_facts(Table),    
    gen_resource_ctrs(R, S).

gen_resource_ctr(Table, Conjectures, NonMachineAttrs, ResourceCtr) :-
    nl, write('TABLE: '), write(Table), nl,
    write('----------------------------------------------------------------------------------------------------'), nl,
    tab_get_pks(col(Table,_),    Pks),
    tab_get_arity(col(Table,_),  NbColumns),
    tab_get_names(col(Table,_),  Names),
    tab_get_types(col(Table,_),  Types),
    tab_get_equals(col(Table,_), Equals),
    tab_get_cmps(col(Table,_),   Cmps),
    tab_get_ctrs(col(Table,_),   Ctrs),
    gen_disjunctive_ctrs(Pks, Table, Conjectures, NonMachineAttrs, NbColumns, Names, Types, Equals, Cmps, Ctrs, DisjCalCtrs),
    ResourceCtr = DisjCalCtrs,
    write('disj cal ctrs:   '), write(DisjCalCtrs), nl.

extract_non_machine_attrs([], []) :- !.
extract_non_machine_attrs([Conjecture|R], Res) :-
    extract_non_machine_attrs1(Conjecture, CurRes),
    extract_non_machine_attrs(R, NextRes),
    append(CurRes, NextRes, Res).

extract_non_machine_attrs1(Conjecture, [OutputCol]) :-
    Conjecture = OutputCol-t(_, _, _, Formula),
    functor(Formula, Functor, _),
    memberchk(Functor, [bool, if, cases]),
    !.
extract_non_machine_attrs1(Conjecture, [OutputCol|Attrs]) :-
    Conjecture = OutputCol-t(InputCols, _, _, _),
    findall(Attr, member(col(_,Attr),InputCols), Attrs).

% search disjunctive constraints (used only for the model seeker) as well as calendar constraints
gen_disjunctive_ctrs(Pks, TableName, Conjectures, NonMachineAttrs, NbColumns, Names, Types, Equals, Cmps, Ctrs, DisjCalCtrs) :-
    (get_specific_temporal_attr_from_column_names_filtered(TableName, Names, start,    StartAttrFromNames) -> true ; StartAttrFromNames = []), % get start    attribute from the column names if this is possible
    (get_specific_temporal_attr_from_column_names_filtered(TableName, Names, duration,   DurAttrFromNames) -> true ;   DurAttrFromNames = []), % get duration attribute from the column names if this is possible
    (get_specific_temporal_attr_from_column_names_filtered(TableName, Names, end,        EndAttrFromNames) -> true ;   EndAttrFromNames = []), % get end      attribute from the column names if this is possible
    append(DurAttrFromNames, EndAttrFromNames, DurEndAttrFromNames0),
    sort(DurEndAttrFromNames0, DurEndAttrFromNames),
    (Pks = [] ->                                                                                                  % if no primary key
%write(a0), nl,
        NotMachine = NonMachineAttrs,
        get_not_specific_scheduling_attr_from_column_names_filtered(TableName, NbColumns,
                                                                    duration, NoDurationAttrs),                   % use only column names to try identifying non-duration attributes                 % NEW
        collect_temporal_attrs(1, NbColumns, 0, 0, TableName, Names, Types, Equals, Cmps, TempAttrs1),            % search all potential temporal attributes                                         % NEW
        get_temporal_attrs_from_column_names_filtered(TableName, NbColumns, TempAttrs2),                          % use also column names to search temporal attributes                              % NEW
        append(TempAttrs1, TempAttrs2, TempAttrs12Unsorted),                                                      % (use append as the potential temporal attribute may be too restricted as use     % NEW
        sort(TempAttrs12Unsorted, TempAttrs),                                                                     %  information of the precedence acquired in gen_metadata which can involve only   % NEW
                                                                                                                  %  a subset of the temporal attributes)                                            % NEW
        NoTaskTypeAttrs1 = TempAttrs                                                                              % these attributes can not correspond to a task type attribute
    ;                                                                                                             % get index of the primary key attribute when it exist
     Pks = [_-[IndPk]|_] ->                                                                                       % (take most likely one, the first one as sorted by dec. likeliness)
        arg(IndPk, Ctrs, CtrsIndPk),                                                                              % get constraints attached to the primary key attribute
        (memberchk(include_except_default_value_no_cycle(IndPk,IndSucc,_,ListPrecs), CtrsIndPk) ->                % if found a successor attribute
%write(a1), nl,
            findall(Attr, (member(precedence(LeftAttrs,_,_,_),  ListPrecs), member(col(TableName,Attr), LeftAttrs )), IndLeftAttrs ),
            findall(Attr, (member(precedence(_,_,_,RightAttrs), ListPrecs), member(col(TableName,Attr), RightAttrs)), IndRightAttrs),
            append(IndLeftAttrs, IndRightAttrs, TAttrs),
            sort(TAttrs, TempAttrs0),

            % given the attributes used on the left hand side of a generalised precedence constraint and the attributes used on the right hand side
            % of the same generalised precedence constraint extract the list of attributes which cannot correspond to a duration attribute
            findall(NoDurationLeftAttr,  (member(precedence(LeftAttrs,_,_,_),  ListPrecs),                        % if left hand side is a single attr.it cannot be a duration
                                          LeftAttrs  = [col(TableName,NoDurationLeftAttr)]),  NoDurationLeftAttrs),                                          
            findall(NoDurationRightAttr, (member(precedence(_,_,_,RightAttrs),  ListPrecs),                       % if right hand side is a single attr.it cannot be a duration
                                          RightAttrs = [col(TableName,NoDurationRightAttr)]), NoDurationRightAttrs),
            append(NoDurationLeftAttrs, NoDurationRightAttrs, NoDurationAttrs1),                                                                                                                     % NEW
            get_not_specific_scheduling_attr_from_column_names_filtered(TableName, NbColumns,
                                                                        duration, NoDurationAttrs2),              % use only column names to try identifying non-duration attributes                 % NEW
            append(NoDurationAttrs1, NoDurationAttrs2, NoDurationAttrs),                                                                                                                             % NEW
            get_duration_attr_from_conjectures(TableName, Conjectures, TempAttrs0,                                % as when possible resource constraints prefer using a start and
                                               NoDurationAttrs, DurAttr0),                                        % duration attribute (rather than start and end) try to extract the
            (DurAttr0         = [_|_] -> DurAttr = DurAttr0         ;                                             % duration attribute from the acquired conjectures
             DurAttrFromNames = []    -> DurAttr = DurAttrFromNames ;                                             % otherwise, use name to acquire duration attr.
                                         DurAttr = []               ),
            append(DurAttr, TempAttrs0, TempAttrs1),                                                             
            sort(TempAttrs1, TempAttrs2),                                                                         % remove duplicated attributes                                                     % NEW
            get_temporal_attrs_from_column_names_filtered(TableName, NbColumns, TempAttrs3),                      % use also column names to search temporal attributes                              % NEW
            append(TempAttrs2, TempAttrs3, TempAttrs23Unsorted),                                                  % (use append as the potential temporal attribute may be too restricted as use     % NEW
            sort(TempAttrs23Unsorted, TempAttrs),                                                                 %  information of the precedence acquired in gen_metadata which can involve only   % NEW
                                                                                                                  %  a subset of the temporal attributes)                                            % NEW
            write(no_duration_attrs(NoDurationAttrs)), nl,
            write(non_machine_attrs(NonMachineAttrs)), nl,
            write(dur_attr(DurAttr)), nl,
            write(temp_attrs0(TempAttrs0)), nl,
            write(temp_attrs1(TempAttrs1)), nl,
            write(temp_attrs2(TempAttrs2)), nl,
            write(temp_attrs3(TempAttrs3)), nl,
            
            append([[IndPk,IndSucc],IndLeftAttrs,IndRightAttrs,NonMachineAttrs], NotMachine),                     % the primary key, the successor attribute, and the temporal
            append([[IndPk,IndSucc],TempAttrs], NoTaskTypeAttrs1)                                                 % these attributes can not correspond to a task type attribute
        ;                                                                                                         % attributes cannot correspond to the machine attribute
%write(a2), nl,
            NotMachine = [IndPk|NonMachineAttrs],                                                                 % forbid only the primary key attribute
            NoTaskTypeAttrs1 = [IndPk|TempAttrs],                                                                 % these attributes can not correspond to a task type attribute
            NoDurationAttrs = [],                                                                                 % do not know any attribute that cannot be a duration attribute
            collect_temporal_attrs(1, NbColumns, IndPk, 0, TableName, Names, Types, Equals, Cmps, TempAttrs1),    % search all potential temporal attributes                                         % NEW
            get_temporal_attrs_from_column_names_filtered(TableName, NbColumns, TempAttrs2),                      % use also column names to search temporal attributes                              % NEW
            append(TempAttrs1, TempAttrs2, TempAttrs12Unsorted),                                                  % (use append as the potential temporal attribute may be too restricted as use     % NEW
            sort(TempAttrs12Unsorted, TempAttrs)                                                                  %  information of the precedence acquired in gen_metadata which can involve only   % NEW
        )                                                                                                         %  a subset of the temporal attributes)                                            % NEW
    ;
        Pks = [_-ListIndsPk|_],
        append(ListIndsPk, NonMachineAttrs, NotMachine),
        collect_temporal_attrs(1, NbColumns, 0, 0, TableName, Names, Types, Equals, Cmps, TempAttrs0),            % search all potential temporal attributes
        remove_elements_from_unsorted_list(TempAttrs0, ListIndsPk, TempAttrs1),                                   % attributes of a compound primary key are not temporal attributes                 % NEW
        get_temporal_attrs_from_column_names_filtered(TableName, NbColumns, TempAttrs2),                          % use also column names to search temporal attributes                              % NEW
        append(TempAttrs1, TempAttrs2, TempAttrs12Unsorted),                                                      % (use append as the potential temporal attribute may be too restricted as use     % NEW
        sort(TempAttrs12Unsorted, TempAttrs),                                                                     %  information of the precedence acquired in gen_metadata which can involve only   % NEW
                                                                                                                  %  a subset of the temporal attributes)                                            % NEW
        append(ListIndsPk, TempAttrs, NoTaskTypeAttrs1)                                                           % these attributes can not correspond to a task type attribute
    ),
    findall(M, (M in 1..NbColumns,indomain(M),arg(M,Types,set)), IndSetAttrs),                                    % all attributes which are sets cannot be machine attributes
    append(NotMachine, IndSetAttrs, NotMachine0),
    sort(NotMachine0, NotMachineAttr),
    findall(M, (M in 1..NbColumns,indomain(M),\+memberchk(M,NotMachineAttr)), MachineAttr),                       % search all potential machine attributes
    findall(t(Machine,Start,DurationOrEnd), (member(Machine, MachineAttr),                                        % search the candidates attributes for a disjunctive constraint
                                             (StartAttrFromNames = [] ->
                                                member(Start, TempAttrs)
                                             ;
                                                member(Start, StartAttrFromNames)
                                             ),
                                             Start \== Machine,
                                             (DurEndAttrFromNames = [] ->
                                                member(DurationOrEnd, TempAttrs)
                                             ;
                                                member(DurationOrEnd, DurEndAttrFromNames)
                                             ),
                                             DurationOrEnd \== Machine,
                                             DurationOrEnd \== Start),          CandidatesAttrs),
    CostDisjCtrMax = 5000000000000,
    CostDisjCtr in 0..CostDisjCtrMax,
    (minimize(check_disjunctive_ctrs(NbColumns, TableName, CandidatesAttrs, NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, CostDisjCtrMax, CostDisjCtr, DisjunctiveCtr), CostDisjCtr) ->
        (DisjunctiveCtr = t(CtrMode,CtrMachineAttr,CtrStartAttr,CtrDurEndAttr) ->
            arg(CtrMachineAttr, Cmps, BinaryCtrs),
            (memberchk(within(col(TableName,CtrMachineAttr),col(TableName,CtrMachinesAttr)), BinaryCtrs) ->
                DisjCtrs = [CostDisjCtr-diffn(CtrMode,CtrMachineAttr,CtrMachinesAttr,CtrStartAttr,CtrDurEndAttr)]
            ;
                DisjCtrs = [CostDisjCtr-disjunctive(CtrMode,CtrMachineAttr,CtrStartAttr,CtrDurEndAttr)]
            )
        ;
            DisjCtrs = []
        )
    ;
        DisjCtrs = [], CostDisjCtr = CostDisjCtrMax
    ),
    extract_calendar_attrs(IndSetAttrs, TableName, NbColumns, CalendarAttrs),
    findall([col(TableName,OutAttr)|InAttrs], conjecture(_,_,col(TableName,OutAttr),_,_,_,t(InAttrs,_,_,polynom(_)),_,_), AttrsInFormula),
    append(AttrsInFormula, FlatAttrsInFormula),                                                                   % flatify lists of attributes
    findall(Attr, member(col(_,Attr),FlatAttrsInFormula), FormulaAttrs),
    append([IndSetAttrs,FormulaAttrs,NoTaskTypeAttrs1,MachineAttr], NoTaskTypeAttrs),                             % set of attributes which cannot correspond to a task type
    sort(NoTaskTypeAttrs, SortedNoTaskTypeAttrs),                                                                 % (no temporal, no machine, no primary key, no successor, no set, not in formula)s
    (enable_conditional_calendar(true) ->
        findall(Col, (Col in 1..NbColumns,                                                                        % compute set of attributes which can correspond to a task type
                      indomain(Col),                                                                              % iterate over the different attributes
                      \+member(Col, NoTaskTypeAttrs),                                                             % filter those attributes which cannot correspond to a task type
                      tab_get_nval(col(TableName,Col), Nval),                                                     % get number of distinct values for this attribute
                      Nval > 1,                                                                                   % check that more than one type and not too many types
                      Nval < 11),                             TaskTypeAttrs)
    ;
        TaskTypeAttrs = []
    ),
    write('calendar:        '), write(CalendarAttrs),         nl,
    write('machine attr:    '), write(MachineAttr),           nl,
    write('temp attrs:      '), write(TempAttrs),             nl,
    write('formula attrs:   '), write(FormulaAttrs),          nl,
    write('no type attrs:   '), write(SortedNoTaskTypeAttrs), nl,
    write('task type attrs: '), write(TaskTypeAttrs),         nl,
    findall(TypeAttrVal,                                                                                          % compute list of possible task type attribute and task type value
                         (functor(TypeAttrVal, task_type, 2),                                                     %  . build term of the form task_type(task type attribute,task type value)
                          member(TypeAttr,TaskTypeAttrs),                                                         %  . enumerate on possible task type attributes
                          arg(1, TypeAttrVal, TypeAttr),                                                          %  . record task type attribute
                          tab_get_min_max(col(TableName,TypeAttr), MinTypeVal, MaxTypeVal),                       %  . get minimum and maximum value for current task type attribute
                          TypeVal in MinTypeVal..MaxTypeVal,                                                      %  . enumerate on possible task type values
                          indomain(TypeVal),                                                                      %  . record task type value
                          arg(2, TypeAttrVal, TypeVal)), ListTypeAttrVal),
    (DisjCtrs = [] ->                                                                                             % if did not find any disjunctive constraint then compute
        findall(t(_Mode,Calendar,Start,DurationOrEnd,TypeVal), (member(TypeVal, [all|ListTypeAttrVal]),           % Start and DurationOrEnd for the potential calendar ctr.
                                                                member(Calendar,CalendarAttrs),                   % (Mode not fixed as do not know how to interpret DurationOrEnd)
                                                                (StartAttrFromNames = [] ->                       % enumerate on possible task type attributes (all if consider all tasks)
                                                                    member(Start, TempAttrs)
                                                                ;
                                                                    member(Start, StartAttrFromNames)
                                                                ),                                                                
                                                                \+ memberchk(Start, MachineAttr),
                                                                (DurEndAttrFromNames = [] ->
                                                                    member(DurationOrEnd, TempAttrs)
                                                                ;
                                                                    member(DurationOrEnd, DurEndAttrFromNames)
                                                                ),
                                                                DurationOrEnd \== Machine,
                                                                DurationOrEnd \== Start), CandidatesAttrsCalCtrs),
        declare_mode_variables(CandidatesAttrsCalCtrs)                                                            % declare domain of mode variables as cannot be done in the findall
    ;                                                                                                             % if found some disjunctive ctr then reuse Start and DurationOrEnd
        extract_start_duration_or_end_from_disj_ctrs(DisjCtrs, ListModeStartDurationOrEnd),                       % (in this case Mode will be fixed as taken from disjunctive ctr)
        findall(t(Mode,Calendar,Start,DurationOrEnd,TypeVal), (member(TypeVal, [all|ListTypeAttrVal]),            % enumerate on possible task type attributes (all if consider all tasks)
                                                               member(Calendar,CalendarAttrs),
                                                               member(t(Mode,Start,DurationOrEnd), ListModeStartDurationOrEnd)), CandidatesAttrsCalCtrs)
    ),
%   write('calendar candidates: '), write(CandidatesAttrsCalCtrs), nl,
    check_calendar_ctrs(CandidatesAttrsCalCtrs, NbColumns, TableName, NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, CalCtrs), % NEWW
    remove_dominated_cal_ctrs(CalCtrs, NonDominatedCalCtrs),
%   write('non dominated cal ctrs: '), write(NonDominatedCalCtrs), nl,
    remove_equivalent_calendar_ctrs(NonDominatedCalCtrs, FilteredCalCtrs), % NEWW
%   write('cal ctrs: '), write(FilteredCalCtrs), nl,                       % NEWW
    append(DisjCtrs, FilteredCalCtrs, DisjCalCtrs),
    true.

get_specific_temporal_attr_from_column_names_filtered(TableName, Names, KeyWord, AttrsFiltered) :-
    get_specific_temporal_attr_from_column_names(Names, KeyWord, Attrs),
    findall(Attr, (member(Attr, Attrs),
                   tab_get_type(col(TableName,Attr), Type),
                   Type \== set), AttrsFiltered).

get_not_specific_scheduling_attr_from_column_names_filtered(TableName, NbColumns, KeyWord, AttrsFiltered) :-
    get_not_specific_scheduling_attr_from_column_names(TableName, NbColumns, KeyWord, Attrs),
    findall(Attr, (member(Attr, Attrs),
                   tab_get_type(col(TableName,Attr), Type),
                   Type \== set), AttrsFiltered).

get_temporal_attrs_from_column_names_filtered(TableName, NbColumns, AttrsFiltered) :-
    get_temporal_attrs_from_column_names(TableName, NbColumns, Attrs),
    findall(Attr, (member(Attr, Attrs),
                   tab_get_type(col(TableName,Attr), Type),
                   Type \== set), AttrsFiltered).

declare_mode_variables([]) :- !.
declare_mode_variables([t(Mode,_,_,_,_)|R]) :-
    Mode in 0..1,
    declare_mode_variables(R).

extract_start_duration_or_end_from_disj_ctrs([], []) :- !.
extract_start_duration_or_end_from_disj_ctrs([_-diffn(Mode,_,_,Start,DurationOrEnd)|R], [t(Mode,Start,DurationOrEnd)|S]) :- !,
    extract_start_duration_or_end_from_disj_ctrs(R, S).
extract_start_duration_or_end_from_disj_ctrs([_-disjunctive(Mode,_,Start,DurationOrEnd)|R], [t(Mode,Start,DurationOrEnd)|S]) :-
    extract_start_duration_or_end_from_disj_ctrs(R, S).

% extract of columns which can correspond to a calendar (sorted list of intervals)
extract_calendar_attrs([], _, _, []) :- !.
extract_calendar_attrs([SetAttr|R], TableName, NbColumns, [SetAttr|S]) :- % iterate through columns corresponding to sets
    functor(Term, TableName, NbColumns),                                  % create a term corresponding to an entry of current table
    findall(Set, (call(Term), arg(SetAttr, Term, Set)), Sets),            % extract all the sets of column SetAttr
    memberchk([_|_], Sets),                                               % check that at least one set is not empty
    check_if_calendar_attr(Sets),                                         % check that current sets correspond to a calendar
    !,                                                                    % record current column index as it correspond to a calendar
    extract_calendar_attrs(R, TableName, NbColumns, S).                   % check next columns corresponding to sets
extract_calendar_attrs([_|R], TableName, NbColumns, S) :-                 % skip current column as it is not a calendar
    extract_calendar_attrs(R, TableName, NbColumns, S).                   % check next columns corresponding to sets

check_if_calendar_attr([]) :- !.
check_if_calendar_attr([Set|R]) :-
    check_if_calendar(Set),
    check_if_calendar_attr(R).

% check that we have a sorted list of non-overlaping intervals
check_if_calendar([]) :- !.
check_if_calendar([Low-Up]) :-              % if on last interval
    !,                                      % then check that interval start
    integer(Low),                           % is less than or equal to interval end
    integer(Up),
    Low =< Up,  
    true.
check_if_calendar([Low1-Up1,Low2-Up2|R]) :- % if at least two intervals
    Low1 =< Up1,                            % then check that interval start of first interval is less than or equal to interval end of first interval
    Up1   < Low2,                           % check that interval end of first interval is smaller than interval start of second interval
    check_if_calendar([Low2-Up2|R]).        % continue to check next intervals

% try to extract a duration attribute from the generated conjectures (as sometimes precedence constraints just mention the start and end attributes, and
% as resource constraints like disjunctive or diffn prefer using start and duration attributes.
%  TableName      : name of the table from which extract constraint.
%  Conjectures    : list of conjectures that we already acquired.
%  TempAttrs      : list of temporal attributes we could extract by using the already generated precedence constraints.
%  NoDurationAttrs: list of attributes which can for sure not correspond to a duration attribute (extracted from the generated precedence constraints).
%  DurAttr        : result that will be a list (possibly empty) of potential duration attributes
get_duration_attr_from_conjectures(TableName, Conjectures, TempAttrs, NoDurationAttrs, DurAttr) :-
    findall(Dur, (member(Conjecture, Conjectures),
                  Conjecture = OutputAttr-t([col(TableName,InputAttr1),col(TableName,InputAttr2)],
                                          _, _, polynom([monome([t(_,1)],1),monome([t(_,1)],1)])),
                  memberchk(OutputAttr, TempAttrs),
                  ((memberchk(InputAttr1,TempAttrs), nonmember(InputAttr2,TempAttrs), nonmember(InputAttr2,NoDurationAttrs)) ->
                        Dur = InputAttr2
                  ;
                   (memberchk(InputAttr2,TempAttrs), nonmember(InputAttr1,TempAttrs), nonmember(InputAttr1,NoDurationAttrs)) ->
                        Dur = InputAttr1
                  ;
                        false
                  )),
                  DurAttr).

check_disjunctive_ctrs(_, _, [], _, _, _, CostDisjCtrMax, CostDisjCtrMax, []) :- !. % NEWW
check_disjunctive_ctrs(NbColumns, TableName, CandidatesAttrs, NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, _CostDisjCtrMax, CostDisjCtr, DisjunctiveCtr) :- % NEWW
    member(CandidatesAttr, CandidatesAttrs),
    check_disjunctive_ctr(NbColumns, TableName, CandidatesAttr, NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, CostDisjCtr, DisjunctiveCtr). % NEWW

check_disjunctive_ctr(NbColumns, TableName, CandidatesAttr, NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, CostDisjCtr, DisjunctiveCtr) :- % NEWW
    CandidatesAttr = t(Machine,Start,DurationOrEnd),
    findall(TermSet,
            (functor(TermSource, TableName, NbColumns),
             functor(TermSet, t, 3),
             unify_variables([Machine,Start,DurationOrEnd], 1, TermSource, TermSet),
             call(TermSource)),
            Projected),
    sort(Projected, Sorted),
    length(Projected, M),
    length(Sorted,    M),
    (memberchk(DurationOrEnd, NoDurationAttrs) ->               % if DurationOrEnd cannot be a duration attribute % NEWW
        Mode = 1                                                % then force Mode to 1, that is force DurationOrEnd to be an end attribute
    ;
     (memberchk(DurationOrEnd, DurAttrFromNames),               % if DurationOrEnd can both be an duration attribute and an end attribute from the analysis of the columns names
      memberchk(DurationOrEnd, EndAttrFromNames)) ->            % then do not fix Mode to permit both to have a duration or an end attribute
        Mode in 0..1
    ;
      memberchk(DurationOrEnd, DurAttrFromNames) ->             % if DurationOrEnd is a duration attribute from the analysis of the columns names
        Mode = 0                                                % then force Mode to 0, that is DurationOrEnd to be a duration attribute
    ;
      memberchk(DurationOrEnd, EndAttrFromNames) ->             % if DurationOrEnd is an end attribute from the analysis of the columns names
        Mode = 1                                                % then force Mode to 1, that is force DurationOrEnd to be an end attribute
    ;
        Mode in 0..1                                            % if DurationOrEnd may be a duration attribute
    ),                                                          % then do not fix Mode to permit both to have a duration or an end attribute
    check_disjunctive_ctr1(Sorted, _, Mode, 0, 1, CostDisjCtr), % put a penalty of 1 when we use the task end rather than the task duration
    DisjunctiveCtr = t(Mode,Machine,Start,DurationOrEnd).

check_disjunctive_ctr1([t(CurM,_,_)], PrevM, Mode, Cost0, Cost1, CostDisjCtr) :-
    (PrevM = CurM -> Inc = 0 ; Inc = 1),
    (integer(Mode)  -> (Mode = 0 -> CostDisjCtr is Cost0+Inc ; CostDisjCtr is Cost1+Inc) ;
     Cost0 =< Cost1 ->  Mode = 0,   CostDisjCtr is Cost0+Inc                             ;
                        Mode = 1,   CostDisjCtr is Cost1+Inc                             ).
check_disjunctive_ctr1([t(Mi,Si,DEi),t(Mj,Sj,DEj)|R], PrevM, Mode, CostCur0, CostCur1, CostDisjCtr) :-
    (DEi < Si -> Mode #= 0 ;              % as the end of a task i cannot be located before the start of task i force DEi to be interpreted as the duration of the task
     DEj < Sj -> Mode #= 0 ;              % as the end of a task j cannot be located before the start of task j force DEi to be interpreted as the duration of the task
                 true      ),
    fd_max(CostDisjCtr, CostDisjCtrMax),
    (Mi = Mj ->
         fd_min(Mode, ModeMin),
         (ModeMin = 0 ->
              Ei0 is Si+DEi,
              (Ei0 =< Sj ->
                   CostNext0 is CostCur0 + Sj - Ei0,
                   (CostNext0 > CostDisjCtrMax -> Mode = 1 ; true)
              ;
                   Mode = 1
              )
         ;
              true
         ),
         fd_max(Mode, ModeMax),
         (ModeMax = 1 ->
              Ei1 = DEi,
              (Ei1 =< Sj ->
                   CostNext1 is CostCur1 + Sj - Ei1,
                   (CostNext1 > CostDisjCtrMax -> Mode = 0 ; true)
              ;
                   Mode = 0
              )
         ;
              true
         )
    ;
         (integer(PrevM) ->
              ((PrevM < Mi, Mi < Mj) -> Inc = 1 ; Inc = 0)
         ;
          Mi < Mj  -> Inc = 1
         ;
                      Inc = 0
         ),
         (fd_min(Mode, 0) -> CostNext0 is CostCur0 + Inc ; true),
         (fd_max(Mode, 1) -> CostNext1 is CostCur1 + Inc ; true)
    ),
    check_disjunctive_ctr1([t(Mj,Sj,DEj)|R], Mi, Mode, CostNext0, CostNext1, CostDisjCtr).

% a calendar constraint is a constraint that fullfil one of the following conditions:
%  1) all task starts               are included     within     an interval of the calendar that is attached to the task
%  2) all task ends                 are included     within     an interval of the calendar that is attached to the task
%  3) both all task starts and ends are included     within a same interval of the calendar that is attached to the task
%  4) all task starts               are not included within    any interval of the calendar that is attached to the task
%  5) all task ends                 are not included within    any interval of the calendar that is attached to the task
%  6) both all task starts and ends are do not overlap         any interval of the calendar that is attached to the task
% note that a calendar may also only be applicable for a subset of tasks which have a specific type, where a type is defined by a type attribute and a type attribute value
check_calendar_ctrs(CandidatesAttrs, NbColumns, TableName, NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, CalendarCtrs) :- % NEWW
    findall(CalendarCtr, (member(CandidatesAttr, CandidatesAttrs),                                       % iterate through the list of candidates of the form t(Mode,Calendar,Start,DurationOrEnd)
                          member(StartEnd, [start_end,                                                   % focus successively both on task start and task end (has to be done first because of domination,
                                            start,end]),                                                 % see remove_dominated_cal_ctrs), on task start, and finally son task end
                          member(InOut,    [in,   out]),                                                 % focus successively on inclusion and on exclusion
                          check_calendar_ctr(CandidatesAttr, NbColumns, TableName, StartEnd, InOut,      % check whether we can extract a calendar constraint or not
                                             NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, CalendarCtr)), CalendarCtrs). % NEWW

check_calendar_ctr(CandidatesAttr, NbColumns, TableName, StartEnd, InOut, NoDurationAttrs, DurAttrFromNames, EndAttrFromNames, CalendarCtr) :- % NEWW
    CandidatesAttr = t(Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal),                       % current candidate to check for a calendar ctr.candidate
    CalendarCtr = calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal),    % parameters of calendar constraint we try to acquire
    findall(TermSet,                                                                                     % extract relevant attributes for the table TableName
            (functor(TermSource, TableName, NbColumns),
             functor(TermSet, t, 4),
             (TypeAttrVal = all ->                                                                       % if has to consider all tasks then create a dummy fourth entry in the term
                unify_variables([CalendarAttr,StartAttr,DurationOrEndAttr,CalendarAttr], 1, TermSource, TermSet)
             ;                                                                                           % if has to consider the tasks type attribute then extract it
% write('type attr val: '), write(TypeAttrVal), nl,
                TypeAttrVal = task_type(TypeAttr, _TypeVal),
                unify_variables([CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttr], 1, TermSource, TermSet)
             ),
% write('term source: '), write(TermSet-TermSource), nl,
             call(TermSource)),
            Projected),
    sort(Projected, Sorted),                                                                             % eliminate duplicates from the list of projected terms  % NEWW
    (memberchk(DurationOrEndAttr, NoDurationAttrs) ->                                                    % if DurationOrEnd cannot be a duration attribute
        Mode = 1                                                                                         % then Mode=1, i.e.force DurationOrEnd to be an end attr.
    ;
     (memberchk(DurationOrEndAttr, DurAttrFromNames),                                                    % if DurationOrEndAttr can both be an duration attribute and an end attribute from the analysis of the columns names
      memberchk(DurationOrEndAttr, EndAttrFromNames)) ->                                                 % then do not fix Mode to permit both to have a duration or an end attribute
        true
    ;
      memberchk(DurationOrEndAttr, DurAttrFromNames) ->                                                  % if DurationOrEndAttr is a duration attribute from the analysis of the columns names
        Mode = 0                                                                                         % then force Mode to 0, that is DurationOrEnd to be a duration attribute
    ;
      memberchk(DurationOrEndAttr, EndAttrFromNames) ->                                                  % if DurationOrEndAttr is an end attribute from the analysis of the columns names
        Mode = 1                                                                                         % then force Mode to 1, that is force DurationOrEnd to be an end attribute
    ;                                                                                                    % if DurationOrEndAttr may be a duration attribute
        true                                                                                             % then do not touch Mode that we got from CandidatesAttr
    ),                                                                                                   % check whether we have a calendar constraint or not
%   write(before(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)), nl,
%   write(sorted(Sorted)), nl,
    check_calendar_ctr1(Sorted, calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)),
%   write(after(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)), nl,
    true.

check_calendar_ctr1([], calendar(_,_,Mode,_,_,_,_)) :- !,                                                % no more table entry wrt current calendar
    fd_min(Mode, Mode).                                                                                  % fix mode to its minimum value if Mode was not already fixed
check_calendar_ctr1([t(Calendar,S,DE,T)|R],                                                              % current table entry wrt current calendar ctr.candidate
                    calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)) :-
% write(TypeAttrVal), write('  '), write(task(Calendar,S,DE,T)), nl,
    (TypeAttrVal = all ->                                                                                % if has to consider all tasks in the calendar constraint we currently check
        TaskToConsider = 1
    ;                                                                                                    % if has only to consider those tasks of a specific type
        TypeAttrVal = task_type(_TypeAttr, TypeVal),                                                     % in the calendar constraint we currently check
        (T = TypeVal ->                                                                                  % then consider task only if the value of its type attribute is the exepected type value
            TaskToConsider = 1
        ;
            TaskToConsider = 0
        )
    ),
    (TaskToConsider = 1 ->                                                                               % if has to consider this task in the calendar constraint we currently check
        fd_min(Mode, ModeMin),
        (ModeMin = 0 ->                                                                                  % if can still use Mode 0 (DE interpted as a duration)
            E0 is S+DE-1, % FIX                                                                          % then compute the end-1 from the start and the duration
            (S =< E0 ->                                                                                  % if precedence between start and end-1 is ok
                (StartEnd = start -> Instant10 = S,  Instant20 = S  ;
                 StartEnd = end   -> Instant10 = E0, Instant20 = E0 ;
                                     Instant10 = S,  Instant20 = E0 ),
                (InOut = in ->
                    generate_in_calendar_ctr(Calendar,  Instant10, Instant20, CalCtr0)                   % gen.calendar ctr.between cur.task and all calendar intervals
                ;
                    generate_out_calendar_ctr(Calendar, Instant10, Instant20, CalCtr0)                   % gen.calendar ctr.between cur.task and all calendar intervals
                ),
                (call(CalCtr0) ->                                                                        % state calendar constraint for current task 
                    true
                ;
                    Mode = 1                                                                             % fix Mode to the other mode if calendar constraint did fail
                )                                                          
            ;                                                                                            % if precedence between start and end is not ok
                Mode = 1                                                                                 % then fix Mode to the other mode
            )
        ;
            true
        ),
        fd_max(Mode, ModeMax),
        (ModeMax = 1 ->                                                                                  % if can still use Mode 1 (DE interpted as an end)
            E1 is DE-1, % FIX                                                                            % then get the end-1 directly from the end
            (S =< E1 ->                                                                                  % if precedence between start and end is ok
                (StartEnd = start -> Instant11 = S,  Instant21 = S  ;
                 StartEnd = end   -> Instant11 = E1, Instant21 = E1 ;
                                     Instant11 = S,  Instant21 = E1 ),
                (InOut = in ->
                    generate_in_calendar_ctr(Calendar, Instant11, Instant21,  CalCtr1)                   % gen.calendar ctr.between cur.task and all calendar intervals
                ;
                    generate_out_calendar_ctr(Calendar, Instant11, Instant21, CalCtr1)                   % gen.calendar ctr.between cur.task and all calendar intervals
                ),
                (call(CalCtr1) ->                                                                        % state calendar constraint for current task 
                    true
                ;
                    Mode = 0                                                                             % fix Mode to the other mode if calendar constraint did fail
                )                                                          
            ;                                                                                            % if precedence between start and end is not ok
                Mode = 0                                                                                 % then fix Mode to the other mode
            )
        ;
            true
        )
    ;
        true
    ),
    check_calendar_ctr1(R, calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)).

% generate a constraint enforcing Instant1 and Instant2 to be included within a same interval of the calendar
% (note that Instant1 and Instant2 may correspond to the same Instant)
generate_in_calendar_ctr([], _, _, false) :- !. % use false rather than 0 as on an empty calendar call(0) would give a runtime error (and not fail as expected)
generate_in_calendar_ctr([Low-Up|RestCalendar], Instant1, Instant2, (Low #=< Instant1 #/\ Instant2 #=< Up) #\/ R) :-
    generate_in_calendar_ctr(RestCalendar, Instant1, Instant2, R).

generate_out_calendar_ctr([], _, _, true) :- !. % use true because if we provide an empty calendar for unavailability periods then the constraint is always true
generate_out_calendar_ctr(Calendar, Instant1, Instant2, CalCtr) :-
    complement_intervals(Calendar, Complement),
    generate_in_calendar_ctr(Complement, Instant1, Instant2, CalCtr).

% if we have a calendar constraint on two instants start and end this constraint dominates both
% a calendar constraint on the start instant, and
% a calendar constraint on the end instant.
% remove such dominated calendar constraint (assumes that start_end occurs before start and end)
remove_dominated_cal_ctrs([], []) :- !.
remove_dominated_cal_ctrs([calendar(start_end,in,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|Rest0],
                          [calendar(start_end,in,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|Rest3]) :- !,
    delete(Rest0, calendar(start,in,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal), Rest1),
    delete(Rest1, calendar(end,  in,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal), Rest2),
    remove_dominated_cal_ctrs(Rest2, Rest3).
remove_dominated_cal_ctrs([calendar(start_end,out,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|Rest0],
                          [calendar(start_end,out,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|Rest3]) :- !,
    delete(Rest0, calendar(start,out,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal), Rest1),
    delete(Rest1, calendar(end,  out,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal), Rest2),
    remove_dominated_cal_ctrs(Rest2, Rest3).
remove_dominated_cal_ctrs([calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|R],
                          [calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|S]) :-
    remove_dominated_cal_ctrs(R, S).

% some calendar constraints are equivalent as they only differ wrt the fact that they use either (i) the start and the duration attributes or (ii) the start and the end attributes:
% keep only those equivalent calendar constraints which use the start and duration attributes if we have two such calendar constraints.
remove_equivalent_calendar_ctrs(CalCtrs, FilteredCalCtrs) :- % NEWW
    create_key_of_calendar_ctrs(CalCtrs, CalCtrsWithKey),
    keysort(CalCtrsWithKey, SortedCalCtrsWithKey),
    remove_adjacent_equivalent_calendar_ctrs(SortedCalCtrsWithKey, FilteredCalCtrs).

create_key_of_calendar_ctrs([], []) :- !.
create_key_of_calendar_ctrs([calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|R],
                            [key(StartEnd,InOut,CalendarAttr,StartAttr,TypeAttrVal,Mode)-calendar(StartEnd,InOut,Mode,CalendarAttr,StartAttr,DurationOrEndAttr,TypeAttrVal)|S]) :-
    create_key_of_calendar_ctrs(R, S).

remove_adjacent_equivalent_calendar_ctrs([], []) :- !.
remove_adjacent_equivalent_calendar_ctrs([_-CalendarCtr], [CalendarCtr]) :- !.
remove_adjacent_equivalent_calendar_ctrs([key(StartEnd,InOut,CalendarAttr,StartAttr,TypeAttrVal,0)-calendar(StartEnd,InOut,0,CalendarAttr,StartAttr, DurationOrEndAttr0,TypeAttrVal),
                                          key(StartEnd,InOut,CalendarAttr,StartAttr,TypeAttrVal,1)-calendar(StartEnd,InOut,1,CalendarAttr,StartAttr,_DurationOrEndAttr1,TypeAttrVal)|R],
                                                                                                  [calendar(StartEnd,InOut,0,CalendarAttr,StartAttr, DurationOrEndAttr0,TypeAttrVal)|S]) :- !,
    remove_adjacent_equivalent_calendar_ctrs(R, S).
remove_adjacent_equivalent_calendar_ctrs([_-CalendarCtr1,Key2-CalendarCtr2|R], [CalendarCtr1|S]) :-
    remove_adjacent_equivalent_calendar_ctrs([Key2-CalendarCtr2|R], S).
