% Purpose: Convert table data files, table metadata files, \data\found_model.pl to corresponding MiniZinc model files
% Author : Ramiz Gindullin, IMT Atlantique

% TODO: Think about making an UI in QT6 wrt to creating and selecting a KPI (start w/ min makespan)

:- use_module(library(lists)).
:- use_module(table_access).
:- use_module(tables).
:- use_module(utility).

% set_prolog_flag(informational,off).

top :-
    get_tables(model_seeker, _, _, _, ListTables),
    sort(ListTables, SortListTables),
    (foreach(Table,SortListTables)
    do
     convert_conjectures_metadata_to_minizinc(model_seeker, Table, 1, 1)
    ).

% [convert_to_mzn], convert_conjectures_metadata_to_minizinc(model_seeker, calendar_a_1), halt.
top1 :- top1(model_seeker).
top1(KindCombi) :-
    convert_conjectures_metadata_to_minizinc(KindCombi, use_case_tasks_sc1_use_case_machines, 1, 1),
    convert_conjectures_metadata_to_minizinc(KindCombi, use_case_tasks_sc2_use_case_machines, 1, 1),
    convert_conjectures_metadata_to_minizinc(KindCombi, use_case_tasks_sc2, 1 , 1).

convert_conjectures_metadata_to_minizinc(KindCombi, Table, SplitDiv, SplitMod) :-
    %load table Table with metadata
    write('Current table: '),write(Table),nl,
    load_table_metadata_mzn(KindCombi, Table, MZNFile-DZNFile),
    atoms_concat(['found_model_',SplitDiv,'_',SplitMod,'.pl'], FoundModelFile),
    load_conjectures(KindCombi, FoundModelFile),
    open(MZNFile, write, SOut),
    open(DZNFile, write, DOut),
    convert_metadata_to_minizinc(KindCombi, SOut-DOut, Table),
    close(SOut), close(DOut).

convert_metadata_to_minizinc(KindCombi, SOut-DOut, Table) :-
% the user cannot change the values for the number of
% columns in the tables, but can change other values
% consistently.
    
    format(SOut, '% the user cannot change the values for the number of~n',[]),
    format(SOut, '% columns in the tables, but can change other values~n',[]),
    format(SOut, '% consistently.~n~n',[]),
    format(DOut, '% The user can modify any integer or set value.~n', []),
    format(DOut, '% The size of each array must be modified consistently.~n~n', []),
    format(SOut, 'include "table.mzn";~n',   []),
    format(SOut, 'include "globals.mzn";~n', []),
    tab_get_arity(col(Table,_), NbCols),

    % Step 1 - load information about child table and parent table(-s)
    NbCols1 is NbCols + 1,
    functor(MergeTableInfo, merge_table_info, NbCols1),
    arg(1,MergeTableInfo,Table),
    (catch(call(MergeTableInfo),                                          % call the merge_table_info for the table;
          error(existence_error(procedure,user:merge_table_info/_),_),    % if there are none loaded (because the table in question was not combined from others)
          generate_merge_table_fact(Table, NbCols, MergeTableInfo)) ->    % then the existence error is raised and the procedure to generate the fact is called instead
        true
    ;
     call(MergeTableInfo) ->
        true
    ;
        generate_merge_table_fact(Table, NbCols, MergeTableInfo)
    ),
    (for(I1,2,NbCols1), for(I,1,NbCols),
     foreach([I, ChildParent, col(TableSource, ColSource), Kind], MergeTableCols),
     param(MergeTableInfo)
    do
     arg(I1, MergeTableInfo, t(TableSource,ChildParent,Kind,ColSource))
    ),
    collect_child_name(MergeTableCols, TableChild),
    collect_parent_names(MergeTableCols, TableParents, TableParentsNames),
    write(step1(MergeTableCols)), nl,
    
    % Step 2 - create vars and table constraint for child table. Its number is 1
    load_table_metadata(KindCombi, TableChild),
    print_column_vars_table_constraint(SOut-DOut, TableChild, Table, MergeTableCols, 1, RowsVarNameChild),
    write(step2), nl,
    
    % Step 3 - create vars and table constraint for each parent table. Their numbers from 2..N. Same as step 2 but with either fk sequence list or
    %          fk sequence constraint (scenario 2), depending if we have a in set constraint for fk column
    (foreach(IndexNum-TableParent, TableParents), foreach(FKName-TableParent, FKNamesList), foreach(RowsVarName-TableParent, RowsVarNameParents),
     param([KindCombi, MergeTableCols, TableChild, Table, SOut, DOut, RowsVarNameChild])
    do
     load_table_metadata(KindCombi, TableParent),
     print_column_vars_table_constraint(SOut-DOut, TableParent, Table, MergeTableCols, IndexNum, RowsVarName),
     print_foreign_key_vars_ctrs(SOut, TableChild, TableParent, MergeTableCols, RowsVarNameChild, FKName)
    ),
    write(step3(FKNamesList)), nl,
    
    % Step 4 - convert conjectures from a conjecture file, starting with formula constraints
    convert_conjectures(SOut-DOut, Table, MergeTableCols, ConjectureListFiltered, FKNamesList, RowsVarNameChild, RowsVarNameParents, DurationAttrsAll),
    write(step4), nl,

    % Step 5 - collect all sequence and temporal constraints from the combined table. 
    print_sequence_constraints(0, NbCols, SOut-DOut, Table, MergeTableCols, ConjectureListFiltered, FKNamesList, RowsVarNameChild, DurationAttrsAll),
    write(step5), nl,
    
    % unload all signatures and table rows
    remove_signature_merge_and_table_facts_conjectures([TableChild|TableParentsNames]),
    true.                                           

print_column_vars_table_constraint(SOut-DOut, Table, TableSource, MergeTableCols, Num, RowsVarName) :-
    format(SOut, '~n% declare variables and the table constraint for table "~w.pl"', [Table]),
    to_atom(Num, NumAtom),
    atom_concat('ROWS',NumAtom,RowsVarName),
    tab_get_arity(col(Table,_), NbCols),
    tab_get_nb_rows(col(Table,_), NbRows),
    format(SOut, '~nset of int: ~w = 1..~w;~n', [RowsVarName,NbRows]), % put into print column_vars_table_constraint?
    print_column_vars(0, NbCols, SOut-DOut, Table, TableSource, RowsVarName, NumAtom, MergeTableCols, ColumnsInt),
    print_table_constraint(SOut, ColumnsInt, Table, RowsVarName, MergeTableCols, NumAtom),
    true.

% 1. prints column vars with integer data from the child table
% 2. collects all columns with integer data in one list for non-secondary columns
% 3. if it's a secondary column, add all it's global constraints right after its declaration (all_different, within etc)
print_column_vars(N, N, _, _, _, _, _, _, []) :- !.
print_column_vars(I, N, SOut-DOut, Table, TableSource, RowsVarName, NumAtom, MergeTableCols, ColumnsInt) :-
    I1 is I + 1,
    tab_get_type(col(Table, I1), Type),
    (Type = int ->
        (NumAtom = '1' ->
             memberchk([ISource, _ChildParent, col(Table, I1), _Kind], MergeTableCols),
             tab_get_kind(col(Table, I1), Kind),
             findall(conjecture(col(TableSource,ISource)),
                     (functor(Conjecture,conjecture,9),            % cmodif
                      call(Conjecture),
                      arg(3,Conjecture,col(TableSource,ISource))), % cmodif
                     ConjectureList)
        ;
             Kind = primary,
             ConjectureList = []
        ),
        get_var_name(col(Table, I1), Name),

        ColumnsInt = [col(Table,I1)|R],
        check_within(Table, I1, ColSets),
        ((NumAtom \== '1'
         ;
          (Kind = primary,   ColSets = [])
         ;
          (Kind = secondary, ConjectureList = [])
         ) -> % print the row with all the values
            collect_col_values(Table,I1,Values),
            format(SOut,'array[~w] of int: ~w;~n',[RowsVarName,Name]), % for now all the data is stored in the same file.
            format(DOut, '~w = ~w;~n', [Name, Values])
        ;
         ColSets = [_|_] -> % print WITHIN constraint
            (foreach(ColSet, ColSets),foreach(SetsValues,ColSetsValues),
             param([SOut,I1,Name,Table,RowsVarName])
            do
             collect_col_values(Table,  ColSet, SetsValues)
            ),
            
            flatten(ColSetsValues,ColSetsValuesFlattened),
            sort(ColSetsValuesFlattened,ColSetsValuesSorted),
            transform_set_to_string(ColSetsValuesSorted, SetString),                  % aggregate values from all sets and use them as domain for the variable 
            format(SOut,'array[~w] of var ~w: ~w;~n',[RowsVarName,SetString,Name]),   % and use them print the variable
            
            (foreach(ColSet, ColSets),foreach(SetsValues,ColSetsValues),              % for each WITHIN constraint:
             param([SOut,DOut,I1,Name,Table,RowsVarName])
            do
             transform_sets_to_string(SetsValues, SetsString),
             get_var_name(col(Table,ColSet), NameSet),                                % create a corresponding set
             format(SOut,'array[~w] of set of int: ~w;~n',[RowsVarName, NameSet]),
             format(DOut, '~w = ~w;~n', [NameSet, SetsString]),
             format(SOut,'constraint forall(i in ~w) (~w[i] in ~w[i]);~n',            % print the constraint itself
                    [RowsVarName, Name, NameSet])
            )
        ;
            tab_get_min_max(col(Table,I1),Min,Max),
            format(SOut,'array[~w] of var ~w..~w: ~w;~n',[RowsVarName,Min,Max,Name]), % for now - Min-Max are bounds. Needs pondering. TODO: discuss with Nicolas
            print_global_constraints(SOut,Table,I1,Name)
        )
    ;
        ColumnsInt = R
    ),
    print_column_vars(I1, N, SOut-DOut, Table, TableSource, RowsVarName, NumAtom, MergeTableCols, R).

print_global_constraints(SOut,Table,I, Name) :-
    tab_get_ctr(col(Table,I), Ctrs),
    (memberchk(alldifferent, Ctrs) ->
        format(SOut, '%constraint all_different(~w);~n', [Name])
    ;
     memberchk(alldifferent_except_0, Ctrs) ->
        format(SOut, '%constraint all_different_except_0(~w);~n', [Name])
    ;
     memberchk(alldifferent_consecutive_values, Ctrs) ->
        format(SOut, '%constraint all_different(~w);~n', [Name])
    ;
     memberchk(strictly_decreasing_values,Ctrs) ->
        format(SOut, '%constraint strictly_decreasing(~w);~n', [Name])
    ;
     memberchk(strictly_increasing_values,Ctrs) ->
        format(SOut, '%constraint strictly_increasing(~w);~n', [Name])
    ;
        true
    ).

% print sequence constraints - when one column is used to determine a successor for each entry of the table (if there's one) 
print_sequence_constraints(N, N, _, _, _, _, _, _, _) :- !.
print_sequence_constraints(I, N, SOut-DOut, Table, MergeTableCols, ConjectureListFiltered, FKNames, RowsVarNameChild, DurationAttrs) :-
    I1 is I + 1,
    tab_get_ctr(col(Table,I1), Ctrs),
    tab_get_pks(col(Table,_), PKs),
    (PKs = [] -> PK = [] ; PKs = [_-PK|_]),
    (memberchk(I1,PK) ->                                                 % only print the cst is the column is a part of the primary key
        (foreach(Ctr,Ctrs), param([SOut-DOut, Table, MergeTableCols, ConjectureListFiltered, FKNames, RowsVarNameChild, DurationAttrs])
        do
         (Ctr = include_except_default_value_no_cycle(ColI, ColJ, Default, TemporalCtrs) ->
                                                                        % sequence constraints; there's an important assumption:
                                                                        % sequence will always include columns from the child table only
                                                                        % it's impossible (or wouldn't make any sense) for it to include
                                                                        % columns from a parent table
             write([ColI,ColJ,Default,TemporalCtrs]),nl,
             collect_col_values(Table, ColI, ValuesI),
             collect_col_values(Table, ColJ, ValuesJ),
             (foreach(J, ValuesJ), foreach(J1, Sequence), param([Default,ValuesI]) % for each successor
             do
              (J = Default -> J1 is J ; nth1(J1, ValuesI, J)),
              true
             ),
             memberchk([ColI, _, col(TableI, ColI1), _], MergeTableCols),
             memberchk([ColJ, _, col(TableJ, ColJ1), _], MergeTableCols),
             get_var_name(col(TableI, ColI1), NameI),
             tab_get_name(col(TableJ, ColJ1), NameJ),
             atom_concat(NameI, '_', NameI1),
             atom_concat(NameI1, NameJ, NameSequenceVar),
             format(SOut,'~n~narray[~w] of int: ~w;~n',[RowsVarNameChild,NameSequenceVar]), % for now all the data is stored in the same file.
             format(DOut, '~w = ~w;~n', [NameSequenceVar, Sequence]),
            %format(SOut,'~n~narray[~w] of int: ~w = ~w;~n',[RowsVarNameChild, NameSequenceVar, Sequence]),
             % check if there are any temporal constraints, if yes print them
             print_temporal_constraints(TemporalCtrs, SOut-DOut, NameSequenceVar, Default, Table, RowsVarNameChild, MergeTableCols, ConjectureListFiltered,
                                        FKNames, DurationAttrs)
         ;
             true
         )
        )
    ;
        true
    ),
    print_sequence_constraints(I1, N, SOut-DOut, Table, MergeTableCols, ConjectureListFiltered, FKNames, RowsVarNameChild, DurationAttrs).

% print a temporal constraint
print_temporal_constraints([], _SOut-_DOut, _NameSequenceVar, _Default, _Table, _RowsVarNameChild, _MergeTableCols, _ConjectureListFiltered,
                           _FKName, _DurationAttrs) :- !.
print_temporal_constraints([TemporalCtr|R], SOut-DOut, NameSequenceVar, Default, Table, RowsVarNameChild, MergeTableCols, ConjectureListFiltered,
                           FKName, DurationAttrs) :-
    (TemporalCtr = precedence(ColsLeft0,Coef,Sign,ColsRight0) ->
        get_source_cols(ColsLeft0,  MergeTableCols, ColsLeft),
        get_source_cols(ColsRight0, MergeTableCols, ColsRight),
        % if we only have one column on the left and one column on the right we check can we, if needed, print a duration attribute
        ((ColsLeft0 = [CtrEndCol], ColsRight0 = [CtrStartCol]) ->
             (foreach(col(Table, I), [CtrStartCol, CtrEndCol]),
              foreach(NameVarIndex,  [CtrStartAttr, CtrEndAttr]),
              param([Table, MergeTableCols, FKNamesList])
             do
              memberchk([I, ChildParent, col(TableSource,ColSource), _Kind], MergeTableCols),
              (ChildParent = child ->
                   get_var_name(col(TableSource,ColSource), NameVar),
                   var_square_brackets_index_concat(NameVar,i,NameVarIndex)
              ;
                   get_var_name(col(TableSource,ColSource), NameVar),
                   memberchk(FKNameVar-TableSource, FKNamesList),
                   var_square_brackets_index_concat(FKNameVar,i,FKNameVarIndex),
                   var_square_brackets_index_concat(NameVar,FKNameVarIndex,NameVarIndex)
              )
             ),
             print_duration_variable_constraint(SOut-DOut, MergeTableCols, RowsVarNameChild, ConjectureListFiltered,
                                                CtrStartAttr, CtrEndAttr, CtrStartCol, CtrEndCol, DurationAttrs, DurationAttrsNew, _DurationAttrIndex)
        ;
             true
        ),
        concat_cols_sign(ColsLeft,i,'+', MergeTableCols, FKName,StringColsLeft),
        (Coef = 0 ->
             StringLeftSide = StringColsLeft
        ;
             to_atom(Coef,CoefAtom),
             atom_concat(StringColsLeft, '+', StringColsLeftPlus),
             atom_concat(StringColsLeftPlus, CoefAtom, StringLeftSide)
        ),
        var_square_brackets_index_concat(NameSequenceVar, i, SequenceIndex),
        concat_cols_sign(ColsRight, SequenceIndex, '+', MergeTableCols, FKName, StringColsRight),
        (Sign = '=<' -> Sign1 = '<=' ; Sign1 = Sign),
        format(SOut, '% PRECEDENCE constraint:~n', []),  
        format(SOut, 'constraint forall(i in ROWS1 where ~w != ~w) ((~w) ~w (~w));~n',
               [SequenceIndex, Default, StringLeftSide, Sign1, StringColsRight])
    ;
        true
    ), 
    print_temporal_constraints(R, SOut-DOut, NameSequenceVar, Default, Table, RowsVarNameChild, MergeTableCols, ConjectureListFiltered,
                               FKName, DurationAttrsNew).

print_table_constraint(SOut, ColumnsInt, Table, RowsVarName, MergeTableCols, NumAtom) :-
    length(ColumnsInt, NColumnsInt),
    atom_concat('COLUMNS', NumAtom, ColumnsVarName),
    format(SOut, '~nset of int: ~w = 1..~w;~narray[~w,~w] of var int: ~w;~n',
           [ColumnsVarName, NColumnsInt, RowsVarName, ColumnsVarName, Table]),
    format(SOut, 'constraint ~w = ~n[', [Table]),
    tab_get_nb_rows(col(Table,_),NbRows),
    (count(I,1,NbRows), param(ColumnsInt), param(MergeTableCols), param(SOut)
    do
     to_atom(I,IAtom),
     concat_cols_sign(ColumnsInt,IAtom,', ', MergeTableCols, none, TableString),
     (I > 1 ->  format(SOut, ' ', []) ; true),
     format(SOut, '| ~w,~n', [TableString])
    ),
    format(SOut,' |];~n~n',[]),
    true.

% convert constraints that use foreign keys:
% * WITHIN constraint
print_foreign_key_vars_ctrs(SOut, TableChild, TableParent, MergeTableCols, RowsVarNameChild, FKName) :-
    atom_concat('fk_', TableParent, FKName),
    memberchk([_I, child, col(TableChild, ColChild), foreign(TableParent, [ColParent])], MergeTableCols),
    check_within(TableChild, ColChild, ColSets), % get the list of all WITHIN constraints
    (ColSets = [_|_] -> % if at least one WITHIN constraint
         format(SOut,'~narray[~w] of var int: ~w;~n',[RowsVarNameChild, FKName]),
         get_var_name(col(TableChild,ColChild), NameChild), 
         get_var_name(col(TableParent,ColParent), NameParent),
         format(SOut, '% costraint to connect tables "~w.pl" and "~w.pl" with a foreign key "~w":~n', [TableChild, TableParent, FKName]),
         format(SOut,'constraint forall(i in ~w) (~w[~w[i]] = ~w[i]);~n',
                [RowsVarNameChild, NameParent, FKName, NameChild])
    ; % if tasks are all pre-assigned to each machine
         collect_col_values(TableChild,  ColChild,  ValuesChild),
         collect_col_values(TableParent, ColParent, ValuesParent),
         (foreach(J, ValuesChild), foreach(J1, Sequence), param(ValuesParent)
         do
          nth1(J1,ValuesParent,J),       % a small note: since ValuesParent is a primary key, all values in ValuesParent are unique
          true),                         % if this is not true then there is a possibility of backtrack and additional errors
         format(SOut, '% costraint to connect tables "~w" and "~w" with a foreign key "~w":~n', [TableChild, TableParent, FKName]),
         format(SOut,'~narray[~w] of int: ~w = ~w;~n',[RowsVarNameChild, FKName, Sequence]),
         write([col(TableChild, ColChild), col(TableParent, ColParent)]),nl
    ).

% check if a column is a part of a within constraint
check_within(TableChild, ColChild, ColSets) :-
    tab_get_cmp(col(TableChild, ColChild), Cmps),
    % it is assumed that a column can be a part of several WITHIN constraints
    findall(ColSet,
            member(within(col(TableChild,ColChild),col(TableChild,ColSet)), Cmps),
            ColSets).

% from fact MergeTableCols collec name of a child table (required for merged tables) 
collect_child_name([[_I, child, col(TableChild, _ColSource), _Kind]|_], TableChild) :- !.
collect_child_name([_|R], TableChild) :-
    collect_child_name(R, TableChild).

% from fact MergeTableCols collect names of all parent tables and return their names with an assigned table index
collect_parent_names(MergeTableCols, TableParents, TableParentsNames) :-
    collect_parent_names1(MergeTableCols, TableParentsUnsorted),
    sort(TableParentsUnsorted, TableParentsNames),
    length(TableParentsNames, N),
    N1 is N + 1,
    (for(I, 2, N1),
     foreach(TableParent,TableParentsNames),
     foreach(I-TableParent, TableParents)
    do
     true
    ).

collect_parent_names1([], []) :- !.
collect_parent_names1([[_I, parent, col(TableParent, _ColSource), _Kind]|R], [TableParent|S]) :-
    !,
    collect_parent_names1(R,S).
collect_parent_names1([_|R], TableParents) :-
    collect_parent_names1(R, TableParents).

% Currently unused. Checks if global constraints are used or not
check_globals(N, N, _, _, 0) :- !.
check_globals(I, N, Table, MergeTableCols, GlobalsFlag) :-
    I1 is I + 1,
    tab_get_ctr(col(Table,I1), Ctrs),
    (memberchk([I1, child, col(_TableSource, _ColSource), _Kind], MergeTableCols) ->
        ((memberchk(alldifferent, Ctrs)                     ;
          memberchk(alldifferent_except_0, Ctrs)            ;
          memberchk(alldifferent_consecutive_values, Ctrs)  ;
          memberchk(strictly_decreasing_values,Ctrs)        ;
          memberchk(strictly_increasing_values,Ctrs)        ) ->
             GlobalsFlag is 1
        ;
             check_globals(I1, N, Table, MergeTableCols, GlobalsFlag)
        )
    ;
        check_globals(I1, N, Table, MergeTableCols, GlobalsFlag)
    ).

% collect all values from the selected column I of Table and return as a list Values
collect_col_values(Table,I,Values) :-
    tab_get_arity(col(Table,_),N),
    findall(Value,
            (functor(Term, Table, N),
             call(Term),
             arg(I, Term, Value)),
            Values).

% conversion of sets from the internal format to MiniZinc format
% given a list of sets [S1, ..., SN] and a string Sign connect then in the single string:
% '{S1 Sign S2 Sign S3 ... Sign SN}'
% | ?- transform_sets_to_string([[5,10],[2,3],[3,5],[10],[2,5],[2,10],[1,3-6,9],[2,3,5,10],2], SetsString).
% SetsString = '[{5,10},{2,3},{3,5},{10},{2,5},{2,10},{1,3,4,5,6,9},{2,3,5,10},{2}]' ? 
% yes
transform_sets_to_string(Sets, SetsString) :-
    concat_sets_sign(Sets, ',', SetString),
    atom_concat('[', SetString, LeftBracketSet),
    atom_concat(LeftBracketSet, ']', SetsString).

% given a list of sets [S1, ..., SN] and a string Sign connect then in the single string:
% 'S1 Sign S2 Sign S3 ... Sign SN'
concat_sets_sign([Set|R], Sign, StringCols) :-
    transform_set_to_string(Set, SetString),
    concat_sets_sign1(R, Sign, RStringCols),
    atom_concat(SetString, RStringCols, StringCols).

concat_sets_sign1([], _, '') :- !.
concat_sets_sign1([Set|R], Sign, StringCols) :-
    transform_set_to_string(Set, SetString),
    concat_sets_sign1(R, Sign, RStringCols),
    atom_concat(SetString,RStringCols,SignRStringCols),
    atom_concat(Sign, SignRStringCols, StringCols).

% given a list of intervals S transform it into a string of the format:
% '{I1, I2, ..., IN} where Ii - values belonging the one of the intervals of S
transform_set_to_string(Set, SetString) :-
    (integer(Set) -> % the simplest case - instead of a set an integer number is given
        to_atom(Set,SetStringWOBraces)
    ; % otherwise transform the set into string 'I1, I2, ..., IN'
        concat_vals_sign(Set, ',', SetStringWOBraces)
    ),
    % add left and right braces
    atom_concat('{', SetStringWOBraces, SetStringWLeftBrace),
    atom_concat(SetStringWLeftBrace,'}',SetString).

% given the list of intervals [I1, I2, ..., IN] and a string Sign transform it into the string of the format:
% 'J1 Sign J2 Sign ... Sign JK' where Jj - an integer value belonging to one of the intervals Ii
% Interval Ii can be one of the two formats:
% * integer:            a single time period. Included to the result string as it is
% * integer1-integer2:  an interval of time from integer1 to integer2. All time integers between integer1 and integer2 are included in th result string
concat_vals_sign([Val|R], Sign, StringCols) :-
    val_to_atom(Val, ValAtom),  % check which of the above types of the intervals it is and output the corresponding substring
    concat_vals_sign1(R, Sign, RStringCols),
    atom_concat(ValAtom, RStringCols, StringCols).

concat_vals_sign1([], _, '') :- !.
concat_vals_sign1([Val|R], Sign, StringCols) :-
    val_to_atom(Val,ValAtom),
    concat_vals_sign1(R, Sign, RStringCols),
    atom_concat(ValAtom,RStringCols,SignRStringCols),
    atom_concat(Sign, SignRStringCols, StringCols).

% given the list of columns [Col1, Col2,...,ColN], strings Index and Sign transform it into the string of the format:
% 'Var1[Index] Sign Var2[Index] Sign ... Sign VarN[Index]', where Vark is a corresponding variable name corresponding to Colk  
concat_cols_sign([Col|R], Index, Sign, MergeTableCols, FKNames, StringCols) :-
    apply_fk(Col, Index, MergeTableCols, FKNames, ColNameIndex),        % if a column comes from a parent table, apply a corresponfing foreign key
                                                                        % to the index first to output string 'Vark[FKName[i]]' 
    concat_cols_sign1(R, Index, Sign, MergeTableCols, FKNames, RStringCols),
    atom_concat(ColNameIndex, RStringCols, StringCols).

concat_cols_sign1([],_,_,_,_,'') :- !.
concat_cols_sign1([Col|R], Index, Sign, MergeTableCols, FKNames, StringCols) :-
    apply_fk(Col, Index, MergeTableCols, FKNames, ColNameIndex),
    atom_concat(Sign,ColNameIndex,ColNamePlus),
    concat_cols_sign1(R, Index, Sign, MergeTableCols, FKNames, RStringCols),
    atom_concat(ColNamePlus, RStringCols, StringCols).

% FKNames either a list of pairs of foreign key variables and corresponding table names
% OR it's a flag none 
apply_fk(col(Table, Col), Index, MergeTableCols, FKNames, ColNameIndex) :-
    (FKNames = none ->  % if no foreign keys are given then apply the given index
        get_var_name(col(Table, Col),ColName),
        var_square_brackets_index_concat(ColName, Index, ColNameIndex)
    ; % if the column came from a parent table, apply a corresponding foreign key to the index first
     memberchk([_I, parent, col(Table, Col), _Kind], MergeTableCols) ->
        get_var_name(col(Table, Col),ColName),
        memberchk(FKName-Table, FKNames),
        var_square_brackets_index_concat(FKName,  Index,   FKIndex),
        var_square_brackets_index_concat(ColName, FKIndex, ColNameIndex)
    ; % if the column came from a child table then apply the given index 
        get_var_name(col(Table, Col),ColName),
        var_square_brackets_index_concat(ColName, Index, ColNameIndex)
    ).

% given a variable and an index return 'variable[index]'
var_square_brackets_index_concat(Var,Index,VarSquareBracketsIndex) :-
    atom_concat(Var,'[',VarSquareBracketLeft),
    atom_concat(VarSquareBracketLeft,Index,VarSquareBracketLeftIndex),
    atom_concat(VarSquareBracketLeftIndex,']',VarSquareBracketsIndex).

% is used for columns with set data
val_to_atom(Val, ValAtom) :-
    (atom(Val) ->
        ValAtom = Val
    ;
     integer(Val) ->
        to_atom(Val,ValAtom)
    ;
     Val = Min-Max ->
        (for(I,Min,Max), foreach(I,ValList) do true),
        concat_vals_sign(ValList,',',ValAtom)
    ).

% load table file, metadata file 
load_table_metadata(KindCombi, Table) :-
    load_table_metadata_mzn(KindCombi, Table, _).

% load table file, metadata file, create name for MiniZinc file
load_table_metadata_mzn(KindCombi, Table, MZNFile-DZNFile):-
    atom_concat('data/', KindCombi, PrefixName0),
    atom_concat(PrefixName0, '/data0/', PrefixName),
    atom_concat(PrefixName, Table, PrefixNameFunctor),
    atom_concat(PrefixNameFunctor, '.pl', ConsultFile),
    atom_concat(PrefixNameFunctor, '.mzn', MZNFile),
    atom_concat(PrefixNameFunctor, '.dzn', DZNFile),
    atom_concat(PrefixNameFunctor, '_metadata.pl', MetadataFile),
    consult(ConsultFile),
    consult(MetadataFile).

load_conjectures(KindCombi, ModelFile) :-
    atom_concat('data/', KindCombi, PrefixName),
    atom_concat(PrefixName, '/', PrefixName0),
    atom_concat(PrefixName0, ModelFile, ConjectureFile),
    consult(ConjectureFile).

% generate variable name in the format TableName_ColumnName
get_var_name(col(Table,Col), Name) :-
    tab_get_name(col(Table,Col), ColName),
    atom_concat(Table, '_', TableUnderscore),
    atom_concat(TableUnderscore, ColName, Name).

get_source_cols(ColsOld, MergeTableCols, ColsSource) :-
    (foreach(col(_,ColI), ColsOld),
     foreach(col(TableSource, ColSource), ColsSource),
     param(MergeTableCols)
    do
     memberchk([ColI, _, col(TableSource, ColSource), _Kind], MergeTableCols)
    ).

% if there's no merge table fact than generate it automatically (where every column are assigned as a child table column)
generate_merge_table_fact(Table, NbCols, MergeTableInfo) :-
    (tab_get_pks(col(Table,_), [_-Pk|_]) -> true ; Pk = []),
    (for(IMergeFact,1,NbCols), param(Table,Pk,MergeTableInfo)
    do
     IMergeFact1 is IMergeFact + 1,
     arg(IMergeFact1, MergeTableInfo, ColInfo),
     TypeTable = child,
     (memberchk(IMergeFact,Pk) -> TypeColumn = primary  ;
                                  TypeColumn = data     ),
     ColInfo = t(Table,TypeTable,TypeColumn,IMergeFact)
    ).

% convert formula conjectures into minizinc format
convert_conjectures(SOut-DOut, Table, MergeTableCols, ConjectureListFiltered, FKNamesList, RowsVarNameChild, RowsVarNameParents, DurationAttrsAll) :-
    findall([OutputCol,Cost,Formula],
            (conjecture(_,Type,col(Table,OutputCol),_,MaxN,Cost,Formula,Family,_), % cmodif
             MaxN is 0,
             Type = resource,
             Family \== calendar),
            ResourceConjectureList),
    findall([OutputCol,Cost,Formula],
            (conjecture(_,Type,col(Table,OutputCol),_,MaxN,Cost,Formula,Family,_), % cmodif
             MaxN is 0,
             Type = resource,
             Family = calendar),
            CalendarConjectureList),
    findall([OutputCol,Cost,Formula],
            (conjecture(_,Type,col(Table,OutputCol),_,MaxN,Cost,Formula,_,_), % cmodif
             % make sure that no calendars are collected
             MaxN is 0,
             Type = secondary),
            ConjectureList),
    sort(ResourceConjectureList,ResourceConjectureListSorted),
    sort(ConjectureList,ConjectureListSorted),
    % keep only lowest cost conjectures. TODO: let the user choose which one he wants to keep:
    filter_conjectures_by_cost(ConjectureListSorted, ConjectureListFiltered),
    filter_conjectures_by_cost(ResourceConjectureListSorted, ResourceConjectureListFiltered), % TODO: discuss with Nicolas how to select multiple non-intersecting resource constraints - what to use as an output column?
    convert_conjectures1(ConjectureListFiltered, SOut, Table, MergeTableCols, FKNamesList, RowsVarNameChild),
    % convert calendars. If DIFFN is needed, instead of printing pass required list to convert_resource_conjectures
    convert_calendar_conjectures(CalendarConjectureList, ConjectureListFiltered, ResourceConjectureListFiltered, SOut-DOut, Table, MergeTableCols,
                                 FKNamesList, RowsVarNameChild, RowsVarNameParents, [], DiffNAdditions, [], DurationAttrsCalendar),
    convert_resource_conjectures(ResourceConjectureListFiltered, ConjectureListFiltered, SOut-DOut, Table, MergeTableCols,
                                 FKNamesList, RowsVarNameChild, DiffNAdditions, DurationAttrsCalendar, DurationAttrsAll). % pass an additional parameter from calendars

% For each unique output column selects a conjecture with minimal cost
filter_conjectures_by_cost([],[]) :- !.
filter_conjectures_by_cost([Conjecture],[Conjecture]) :- !.
filter_conjectures_by_cost([[OutputCol,Cost1,Formula1],
                            [OutputCol,Cost2,Formula2]|R], ConjectureListFiltered) :-
    !,
    (Cost1 >= Cost2 ->
        filter_conjectures_by_cost([[OutputCol,Cost2,Formula2]|R], ConjectureListFiltered)
    ;
        filter_conjectures_by_cost([[OutputCol,Cost1,Formula1]|R], ConjectureListFiltered)
     ).
filter_conjectures_by_cost([[OutputCol1,Cost1,Formula1],
                            [OutputCol2,Cost2,Formula2]|R], [[OutputCol1,Cost1,Formula1]|S]) :-
    filter_conjectures_by_cost([[OutputCol2,Cost2,Formula2]|R], S).

% convert calendar constraints one by one
convert_calendar_conjectures([], _ConjectureListFiltered, _ResourceConjectureListFiltered, _SOut-_DOut, _Table, _MergeTableCols,
                             _FKNamesList, _RowsVarNameChild, _RowsVarNameParents, DiffNAdditions, DiffNAdditions, DurationAttrs, DurationAttrs) :- !.
convert_calendar_conjectures([[_OutputCol,_Cost,Conjecture]|R], ConjectureListFiltered, ResourceConjectureListFiltered, SOut-DOut, Table, MergeTableCols,
                             FKNamesList, RowsVarNameChild, RowsVarNameParents, DiffNAdditions, DiffNAdditionsRes, DurationAttrs, DurationAttrsAll) :-
    % 1. read conjecture
    Conjecture = t(InputCols, _InputNames, _InputVars, Formula),
    convert_cols_to_vars_w_indices(InputCols, MergeTableCols,  FKNamesList, InputColsIndices),
    % 2. read the kind of the calendar ctr (consult the article)
    Formula = calendar(StartEnd, InOut, CtrMode, all),             % all denotes that we're working with non-conditional calendars
    ((StartEnd =     start, InOut = in)         -> Kind is 1    ;
     (StartEnd =       end, InOut = in)         -> Kind is 2    ;
     (StartEnd = start_end, InOut = in)         -> Kind is 3    ;
     (StartEnd =     start, InOut = out)        -> Kind is 4    ;
     (StartEnd =       end, InOut = out)        -> Kind is 5    ;
                                                   Kind is 6    ),
    % 3. find which case it is - preselected or diffn (an intricate branching system)
    InputCols        = [col(Table,CtrCalendarAttrCol), CtrStartCol, CtrDurEndCol],
    InputColsIndices = [_CtrCalendarAttr, CtrStartAttr, CtrDurEndAttr],
    memberchk([CtrCalendarAttrCol, ChildParentCalendar, col(TableCalendarSource,ColCalendarSource), _Kind], MergeTableCols),

    % from the article:
    % Assigned = 0 corresponds to a
    % Assigned = 1 corresponds to b.i
    % Assigned = 2 corresponds to b.iiA
    % Assigned = 3 corresponds to b.iiB
    (ChildParentCalendar = child ->                                     % If calendar is in the child table 
         Assigned is 0
    ;
     (memberchk(Kind, [3,6]),                                           % For Kind = 3 and Kind = 6 we can combine it with an existing DIFFN ctr, IF:
      is_calendar_resource_preassigned(Table, TableCalendarSource,      % a) in the resource table with resources freely assigned,
                              MergeTableCols),                          % b) DIFFN resource constraint referencing the same table is present,
      check_same_table_diffn(TableCalendarSource,                       % and c) this FK wasn't used for another calendar constraint before.
                             MergeTableCols,                            % We consider c) as theoretically we can two calendars from two different parent tables
                             ResourceConjectureListFiltered),           % in which case they can overlap and thus cannot be used within single DIFFN constraint
      nonmember(TableCalendarSource-_, DiffNAdditions)) ->
         Assigned is 2
    ;
     is_calendar_resource_preassigned(Table, TableCalendarSource,       % If calendar is in the resource table with resources freely assigned
                             MergeTableCols) ->                         % but no appropriate DIFFN resource constraint is present
         Assigned is 3
    ;
         Assigned is 1                                                  % If resources are preassigned on multiple tables
    ),

    % 4. create ctr or pass the info to diffn
    
    % choose variable or the sum of variables depending on the Kind and CtrMode
    (memberchk(Kind, [1,4]) ->
         TempAttr = CtrStartAttr
    ;
     (memberchk(Kind, [2,5]), CtrMode = 0) ->
         atoms_concat([CtrStartAttr,' + ', CtrDurEndAttr, ' - 1'], TempAttr)
    ;
     (memberchk(Kind, [2,5]), CtrMode = 1) ->                        % we always assume that task durations are found in the child table
                                                                     % thus we always use  RowsVarNameChild here
         print_duration_variable_constraint(SOut-DOut, MergeTableCols, RowsVarNameChild, ConjectureListFiltered, CtrStartAttr,
                                                 CtrDurEndAttr, CtrStartCol, CtrDurEndCol, DurationAttrs, DurationAttrsNew, _DurationAttrIndex),
            % the idea is that if there's no relation between start time and end time, we need to fix it anyway, whenever possible
              %TempAttr = CtrDurEndAttr
         atoms_concat([CtrDurEndAttr, ' - 1'], TempAttr)
    ;
     (memberchk(Kind, [3,6]), CtrMode = 0) ->
         TempAttr = CtrDurEndAttr
    ;                                                               % we always assume that task durations are found in the child table
                                                                    % thus we always use  RowsVarNameChild here
        print_duration_variable_constraint(SOut-DOut, MergeTableCols, RowsVarNameChild, ConjectureListFiltered, CtrStartAttr,
                                                 CtrDurEndAttr, CtrStartCol, CtrDurEndCol, DurationAttrs, DurationAttrsNew, DurationAttrIndex),
        TempAttr = DurationAttrIndex
    ),

    format(SOut, '~n', []),
    
    (memberchk(Assigned, [0, 1]) ->
         % a - read calendar column, determin if it's a uniform calendar or different tasks can have different calendars,
         %     write the single calendar or all calendars into the data file
         collect_col_values(TableCalendarSource, ColCalendarSource, Calendars),
         
         % write calendar column in the files as new variable or variables
         ((Assigned = 0, sort(Calendars, [CalendarSolo])) ->
              SingleCalendar = 1,
              get_var_name(col(TableCalendarSource,ColCalendarSource), CalendarVar),
              (memberchk(Kind, [3,6]) ->
                   (Kind = 3 ->
                        tab_get_min_max(CtrStartCol, MinTime, MaxStart),
                        (CtrMode = 0 ->
                             tab_get_max(CtrDurEndCol, MaxDur),
                             MaxTime is MaxStart + MaxDur
                        
                        ;
                             tab_get_max(CtrDurEndCol, MaxTime)
                        ),                     
                        collect_calendar_gaps_complement(CalendarSolo, MinTime, MaxTime, CalendarStarts, CalendarDurations)
                   ;
                    Kind = 6 ->
                        collect_calendar_gaps(CalendarSolo, CalendarStarts, CalendarDurations)
                   ),
                   atoms_concat([CalendarVar,'_start'], CalendarStartsVar),
                   atoms_concat([CalendarVar,'_end'], CalendarDurationsVar),
                   concat_vals_sign(CalendarStarts,',',CalendarStartsString),
                   concat_vals_sign(CalendarDurations,',',CalendarDurationsString),
                   format(SOut, 'set of int: ~w;~n', [CalendarStartsVar]),
                   format(SOut, 'set of int: ~w;~n', [CalendarDurationsVar]),
                   format(DOut, '~w = [~w];~n', [CalendarStartsVar,    CalendarStartsString]),
                   format(DOut, '~w = [~w];~n', [CalendarDurationsVar, CalendarDurationsString])
              ;
                   convert_calendar_to_str(CalendarSolo, Kind, CalendarSoloString),
                   format(SOut, 'set of int: ~w;~n', [CalendarVar]),
                   format(DOut, '~w = ~w;~n', [CalendarVar, CalendarSoloString])
              )
         ;
          memberchk(Kind, [1,2,4,5]) ->
              SingleCalendar = 0,
              get_var_name(col(TableCalendarSource,ColCalendarSource), CalendarVar),
              (Assigned = 0 ->
                   RowsVarName = RowsVarNameChild,
                   var_square_brackets_index_concat(CalendarVar, i, CalendarVarIndex)
              ;
                   memberchk(RowsVarName-TableCalendarSource, RowsVarNameParents),
                   memberchk(FKNameVar-TableCalendarSource, FKNamesList),
                   var_square_brackets_index_concat(FKNameVar,i,FKNameVarIndex),
                   var_square_brackets_index_concat(CalendarVar,FKNameVarIndex,CalendarVarIndex)
              ),
              (foreach(Calendar,       Calendars),
               foreach(CalendarString, CalendarStrings),
               param([Kind, CtrMode, CtrStartCol, CtrDurEndCol])
              do
               convert_calendar_to_str(Calendar, Kind, CalendarString)
              ),
              concat_vals_sign(CalendarStrings,',',CalendarListString),
              format(SOut, 'array[~w] of set of int: ~w;~n', [RowsVarName, CalendarVar]),
              format(DOut, '~w = [~w];~n~n', [CalendarVar, CalendarListString])
         ;
              SingleCalendar = 0,
              get_var_name(col(TableCalendarSource,ColCalendarSource), CalendarVar),
              atoms_concat([CalendarVar,'_start'], CalendarStartsVar),
              atoms_concat([CalendarVar,'_end'], CalendarDurationsVar),
              atoms_concat([CalendarVar,'_size'], CalendarSizeVar),
              (Assigned = 0 ->
                   RowsVarName = RowsVarNameChild,
                   var_square_brackets_index_concat(CalendarStartsVar,    'i,j', CalendarStartsVarIndex),
                   var_square_brackets_index_concat(CalendarDurationsVar, 'i,j', CalendarDurationsVarIndex),
                   var_square_brackets_index_concat(CalendarSizeVar,          i, CalendarSizeVarIndex)
              ;
                   memberchk(RowsVarName-TableCalendarSource, RowsVarNameParents),
                   memberchk(FKNameVar-TableCalendarSource, FKNamesList),
                   var_square_brackets_index_concat(FKNameVar,i,FKNameVarIndex),
                   atoms_concat([FKNameVarIndex,',j'],FKNameVarIndexJ),
                   var_square_brackets_index_concat(CalendarStartsVar,    FKNameVarIndexJ, CalendarStartsVarIndex),
                   var_square_brackets_index_concat(CalendarDurationsVar, FKNameVarIndexJ, CalendarDurationsVarIndex),
                   var_square_brackets_index_concat(CalendarSizeVar,      FKNameVarIndex,  CalendarSizeVarIndex)
              ),
              
              tab_get_min_max(CtrStartCol, MinTime, MaxStart),
                    (CtrMode = 0 ->
                         tab_get_max(CtrDurEndCol, MaxDur),
                         MaxTime is MaxStart + MaxDur
                        
                    ;
                         tab_get_max(CtrDurEndCol, MaxTime)
                    ),

              find_max_size_calendar(Calendars, Kind, MinTime, MaxTime, CalendarSizes, MaxSize), % find the maximum size of a calendar within a column WRT Kind
              
              (foreach(Calendar,                   Calendars),
               foreach(CalendarStartsString1,      CalendarStartsStrings),
               foreach(CalendarDurationsString1,   CalendarDurationsStrings),
               param([Kind, CtrMode, MinTime, MaxTime, MaxSize])
              do
               (Kind = 3 ->
                    collect_calendar_gaps_complement(Calendar, MinTime, MaxTime, CalendarStarts2, CalendarDurations2)
               ;
                Kind = 6 ->
                    collect_calendar_gaps(Calendar, CalendarStarts2, CalendarDurations2)
               ),
               length(Calendar, CalendarSize),
               DiffSize is MaxSize - CalendarSize,
               length(Zeroes, DiffSize),
               (foreach(0, Zeroes) do true),
               append(CalendarStarts2,    Zeroes, CalendarStarts1),
               append(CalendarDurations2, Zeroes, CalendarDurations1),
               concat_vals_sign(CalendarStarts1,',',CalendarStartsString1),
               concat_vals_sign(CalendarDurations1,',',CalendarDurationsString1)
              ),
              concat_vals_sign(CalendarSizes,           ',', CalendarSizesString),
              concat_vals_sign(CalendarStartsStrings,   ',', CalendarStartsListString),
              concat_vals_sign(CalendarDurationsStrings,',', CalendarDurListString),
              format(SOut, 'array[~w] of int: ~w;~n',          [RowsVarName, CalendarSizeVar]),
              format(SOut, 'array[~w,int] of int: ~w;~n',      [RowsVarName, CalendarStartsVar]),
              format(SOut, 'array[~w,int] of int: ~w;~n',      [RowsVarName, CalendarDurationsVar]),
              format(DOut, '~w = [~w];~n',                     [CalendarSizeVar, CalendarSizesString]),
              format(DOut, '~w = array2d(~w,1..~w,[~w]);~n',   [CalendarStartsVar,    RowsVarName, MaxSize, CalendarStartsListString]),
              format(DOut, '~w = array2d(~w,1..~w,[~w]);~n~n', [CalendarDurationsVar, RowsVarName, MaxSize, CalendarDurListString])
         ),
         
         format(SOut, '~n% the calendar constraint of Kind = ~w with preassigned tasks~n', [Kind]),
         ((SingleCalendar = 1, memberchk(Kind, [1,2,4,5])) ->
              % print the constraint
              (memberchk(Kind, [1,2]) -> 
                   format(SOut, '~nconstraint forall (i in ~w) (~w in ~w);~n', [RowsVarNameChild, TempAttr, CalendarVar])
              ;
                   format(SOut, '~nconstraint forall (i in ~w) (not (~w in ~w));~n', [RowsVarNameChild, TempAttr, CalendarVar])
              )
         ;
          (SingleCalendar = 1, memberchk(Kind, [3, 6])) ->
              format(SOut, 'constraint forall (i in ~w) (disjunctive_strict([~w] ++ ~w, [~w] ++ ~w));',
                     [RowsVarNameChild, CtrStartAttr, CalendarStartsVar, TempAttr, CalendarDurationsVar])
         ;
          (SingleCalendar = 0, memberchk(Kind, [1,2,4,5])) ->
              (memberchk(Kind, [1,2]) -> 
                   format(SOut, '~nconstraint forall (i in ~w) (~w in ~w);~n', [RowsVarNameChild, TempAttr, CalendarVarIndex])
              ;
                   format(SOut, '~nconstraint forall (i in ~w) (not (~w in ~w));~n', [RowsVarNameChild, TempAttr, CalendarVarIndex])
              )
         ;
              format(SOut, 'constraint forall (i in ~w) (disjunctive_strict([~w] ++ [~w | j in 1..~w], [~w] ++ [~w | j in 1..~w]));~n',
                     [RowsVarNameChild, CtrStartAttr, CalendarStartsVarIndex, CalendarSizeVarIndex, TempAttr, CalendarDurationsVarIndex, CalendarSizeVarIndex])
         )
    ;
     memberchk(Assigned, [2, 3]) ->
         memberchk([_I, child, ColResource, foreign(TableCalendarSource, [TableCalendarSourcePK])], MergeTableCols),
         collect_col_values(TableCalendarSource, ColCalendarSource,     Calendars),
         collect_col_values(TableCalendarSource, TableCalendarSourcePK, Resources),

         get_var_name(col(TableCalendarSource, ColCalendarSource), CalendarVar),
         get_var_name(ColResource, ResourceVar),
         var_square_brackets_index_concat(ResourceVar, i, ResourceVarIndex),
         
         atoms_concat([CalendarVar,'_start'],    CalendarStartsVar),
         atoms_concat([CalendarVar,'_resource'], CalendarResourceVar),
         atoms_concat([CalendarVar,'_duration'], CalendarDurationsVar),
         atoms_concat([CalendarVar,'_one'],      CalendarOneVar),
         
         tab_get_min_max(CtrStartCol, MinTime, MaxStart),
               (CtrMode = 0 ->
                    tab_get_max(CtrDurEndCol, MaxDur),
                    MaxTime is MaxStart + MaxDur
               ;
                    tab_get_max(CtrDurEndCol, MaxTime)
               ),
         
         (foreach(Calendar,                   Calendars),
          foreach(Resource,                   Resources),
          foreach(CalendarStartsString2,    CalendarStartsStrings),
          foreach(CalendarResourcesString,  CalendarResourcesStrings),
          foreach(CalendarDurationsString2, CalendarDurationsStrings),
          foreach(CalendarOnesString,       CalendarOnesStrings),
          param([Kind, CtrMode, MinTime, MaxTime])
         do
          (memberchk(Kind, [1,2,3]) ->
               collect_calendar_gaps_complement(Calendar, MinTime, MaxTime, CalendarStarts2, CalendarDurations2)
          ;
               collect_calendar_gaps(Calendar, CalendarStarts2, CalendarDurations2)
          ),
          (foreach(_, CalendarStarts2),
           foreach(Resource, CalendarResources),
           foreach(1,        CalendarOnes),
           param([Resource])
          do
           true
          ),
          concat_vals_sign(CalendarStarts2,',',CalendarStartsString2),
          concat_vals_sign(CalendarResources,',',CalendarResourcesString),
          concat_vals_sign(CalendarDurations2,',',CalendarDurationsString2),
          concat_vals_sign(CalendarOnes,',',CalendarOnesString)
         ),
         concat_vals_sign(CalendarStartsStrings,    ',', CalendarStartsListString),
         concat_vals_sign(CalendarResourcesStrings, ',', CalendarRecourcesListString),
         concat_vals_sign(CalendarDurationsStrings, ',', CalendarDurationsListString),
         concat_vals_sign(CalendarOnesStrings,      ',', CalendarOnesListString),

         format(SOut, 'array[int] of int: ~w;~n', [CalendarStartsVar]),
         format(SOut, 'array[int] of int: ~w;~n', [CalendarResourceVar]),
         format(SOut, 'array[int] of int: ~w;~n', [CalendarDurationsVar]),
         format(SOut, 'array[int] of int: ~w;~n', [CalendarOneVar]),

         format(DOut, '~w = [~w];~n',             [CalendarStartsVar,    CalendarStartsListString]),
         format(DOut, '~w = [~w];~n',             [CalendarResourceVar,  CalendarRecourcesListString]),
         format(DOut, '~w = [~w];~n',             [CalendarDurationsVar, CalendarDurationsListString]),
         format(DOut, '~w = [~w];~n',             [CalendarOneVar,       CalendarOnesListString]),

         (memberchk(Kind, [1,2,4,5]) ->
              StartStringVar    = TempAttr,
              DurationStringVar = '1'
         ;
              StartStringVar    = CtrStartAttr,
              DurationStringVar = TempAttr
         ),

         (Assigned = 2 ->       % in this case we pass the information to the DIFFN constraint in convert_resource_conjectures
              DiffNAdditionsNew = [TableCalendarSource-[CalendarStartsVar, CalendarResourceVar,
                                                        CalendarDurationsVar, CalendarOneVar]|DiffNAdditions]
         ;
              DiffNAdditionsNew = DiffNAdditions,
              format(SOut, '~n% the calendar constraint of Kind = ~w~n', [Kind]),       % othwerwise we create the DIFFN constraint for each task here
              format(SOut, 'constraint forall (i in ~w)(diffn([~w] ++ ~w, [~w] ++ ~w, [~w] ++ ~w, [1] ++ ~w));~n',
                     [RowsVarNameChild,  StartStringVar,       CalendarStartsVar, ResourceVarIndex, CalendarResourceVar,
                      DurationStringVar, CalendarDurationsVar, CalendarOneVar])
         )
    ),
    % 5. recursive call
    format(SOut,'~n',[]),
    convert_calendar_conjectures(R, ConjectureListFiltered, ResourceConjectureListFiltered, SOut-DOut, Table, MergeTableCols,
                                 FKNamesList, RowsVarNameChild, RowsVarNameParents, DiffNAdditionsNew, DiffNAdditionsRes, DurationAttrsNew, DurationAttrsAll).

% first we find the corresponding foreign key column which denotes the resource
% then we check if it is a part of a WITHIN constraint
is_calendar_resource_preassigned(Table, TableParent, MergeTableCols) :-
    memberchk([ResourceAttrCol, child, _, foreign(TableParent, [_])], MergeTableCols),
    check_within(Table, ResourceAttrCol, [_|_]).

% we check if we have a diffn constraint that references a resource from the given parent table TableParent 
check_same_table_diffn(TableParent, MergeTableCols, ResourceConjectureList) :-
    memberchk([_,_,t(Inputs, _, _, diffn(_))], ResourceConjectureList),
    Inputs = [col(_, I),  _,  _,  _],
    memberchk([I, child, _, foreign(TableParent, [_])], MergeTableCols).

% create a string for the given calendar depending on the provided Kind of the calendar and the range of the variable.
% e.g., convert_calendar_to_str([0-5, 9-14, 17-20], 1, CalendarString), CalendarString = '0..5 union 9..14 union 17..20'.
% e.g., convert_calendar_to_str([],                 2, CalendarString), CalendarString = '{}'.
convert_calendar_to_str(Calendar, Kind, CalendarString) :-
    memberchk(Kind,[1,2,4,5]), !,
    convert_calendar_to_str(Calendar, CalendarString).
convert_calendar_to_str(_,Kind,_) :- write(convert_calendar_to_str1(Kind)), nl, halt.

convert_calendar_to_str([], '{}') :- !.                                      % if provided calendar is empty it's an empty list
convert_calendar_to_str([Start-End|R], CalendarString) :-                    % if calendar is non empty then create the string using all the values
    to_atom(Start, StartAtom),
    to_atom(  End,   EndAtom),
    convert_calendar_to_str1(R, RestString),
    atoms_concat([StartAtom, '..', EndAtom, RestString], CalendarString).

convert_calendar_to_str1([], '') :- !.
convert_calendar_to_str1([Start-End|R], CalendarString) :-
    to_atom(Start, StartAtom),
    to_atom(  End,   EndAtom),
    convert_calendar_to_str1(R, RestString),
    atoms_concat([' union ', StartAtom, '..', EndAtom, RestString], CalendarString).

% the task must not intersect with gaps between shifts
collect_calendar_gaps([], [], []) :- !.
collect_calendar_gaps([CalendarS-CalendarE|R], [CalendarS|S], [CalendarDuration|T]) :-
    CalendarDuration is CalendarE - CalendarS,
    collect_calendar_gaps(R, S, T).

collect_calendar_gaps_complement([CalendarS-CalendarE|R], MinTime, MaxTime, CalendarStarts, CalendarDurations) :-
    (CalendarS =< MinTime ->
         CalendarStarts = CalendarStarts1,
         CalendarDurations = CalendarDurations1
    ;
         Duration is CalendarS - MinTime,
         CalendarStarts = [MinTime|CalendarStarts1],
         CalendarDurations = [Duration|CalendarDurations1]
    ),
    collect_calendar_gaps_complement1([CalendarS-CalendarE|R], MaxTime, CalendarStarts1, CalendarDurations1).

collect_calendar_gaps_complement1([_-MaxTime], MaxTime, [], []) :- !.
collect_calendar_gaps_complement1([_-CalendarE], MaxTime, [CalendarE], [Duration]) :-
    !, Duration is abs(MaxTime - CalendarE).
collect_calendar_gaps_complement1([_-CalendarE1, CalendarS2-CalendarE2|R], MaxTime, [CalendarE1|S], [CalendarDuration|T]) :-
    CalendarDuration is CalendarS2 - CalendarE1,
    collect_calendar_gaps_complement1([CalendarS2-CalendarE2|R], MaxTime, S, T).

find_max_size_calendar(Calendars, Kind, MinTime, MaxTime, Sizes, MaxSize) :-
    (foreach(Calendar, Calendars),
     foreach(Size,     Sizes),
     param([Kind, MinTime, MaxTime])
    do
     (memberchk(Kind, [1,2,3]) ->
          collect_calendar_gaps_complement(Calendar, MinTime, MaxTime, CalendarStarts, _),
          length(CalendarStarts, Size)
     ;
          length(Calendar, Size)
     )
    ),
    max_member(MaxSize, Sizes).

% NOT used anymore - the better way is to use negation in MiniZinc itself
% calculate the complement for the calender, given a range of the variable.
% e.g., calculate_calendar_complement([0-5, 9-14], 0-15, R), R = [6-8, 15-15]. 
calculate_calendar_complement([], RangeMin-RangeMax, [RangeMin-RangeMax]) :- !.
calculate_calendar_complement(Calendar, RangeMin-RangeMax, CalendarComplement) :-
    RangeMin1 is RangeMin + 1,
    calculate_calendar_complement1(Calendar, RangeMin1-RangeMax, CalendarComplementUnfiltered),
    filter_calendar_complement(CalendarComplementUnfiltered, CalendarComplement).

calculate_calendar_complement1([], RangeMin-RangeMax, [RangeMin1-RangeMax]) :-
    !, RangeMin1 is RangeMin + 1.
calculate_calendar_complement1([Start-End|R], RangeMin-RangeMax, [RangeMin1-Start1|S]) :-
    Start1 is Start - 1,
    RangeMin1 is RangeMin + 1,
    calculate_calendar_complement1(R, End-RangeMax, S).

filter_calendar_complement([], []) :- !.
filter_calendar_complement([Start-End|R], [Start-End|S]) :-
    End >= Start, !,
    filter_calendar_complement(R, S).
filter_calendar_complement([_|R], S) :-
    filter_calendar_complement(R, S).

% convert resource constraints one by one
convert_resource_conjectures([], _ConjectureListFiltered, _SOut-_DOut, _Table, _MergeTableCols,
                             _FKNamesList, _RowsVarNameChild, _DiffNAdditions, DurationAttrs, DurationAttrs) :- !.
convert_resource_conjectures([[_OutputCol,_Cost,Conjecture]|R], ConjectureListFiltered, SOut-DOut, Table, MergeTableCols,
                             FKNamesList, RowsVarNameChild, DiffNAdditions, DurationAttrs, DurationAttrsAll) :-
    Conjecture = t(InputCols, _InputNames, _InputVars, Formula),
    convert_cols_to_vars_w_indices(InputCols, MergeTableCols,  FKNamesList, InputColsIndices),
    
    % CtrMode = 0 -> uses durations, CtrMode = 1 -> uses end_times, you need to generate additional duration variable (TODO:) 
    (Formula = disjunctive(CtrMode) ->
        write(disjunctive),nl,
        InputCols        = [col(Table,CtrMachineAttrCol), CtrStartCol, CtrDurEndCol],
        InputColsIndices = [CtrMachineAttr, CtrStartAttr, CtrDurEndAttr],
        CtrStartCol  = col(_,IStart),
        CtrDurEndCol = col(_,IDurEnd),
        memberchk([IStart,  _, col(TableSourceStart, _ColSourceStart), _], MergeTableCols),
        memberchk([IDurEnd, _, col(TableSourceDurEnd, _ColSourceEnd) , _], MergeTableCols),
        (TableSourceStart = TableSourceDurEnd -> % times and duration columns must be from the same child table
            (memberchk([CtrMachineAttrCol, child, _SourceCol, foreign(TableParent,[ColParent])], MergeTableCols) -> % if MachineAttr is a foreign key
                 get_var_name(col(TableParent,ColParent), CtrMachineAttrIdName)                               % it is possible to use variable from the parent table
            ;
                 collect_col_values(Table,CtrMachineAttrCol,CtrMachineAttrValues),                                  % otherwise collect and sort values from the columns
                 sort(CtrMachineAttrValues, CtrMachineAttrValuesSorted),
                 length(CtrMachineAttrValuesSorted, CtrMachineAttrValuesLength),
                 get_var_name(col(Table,CtrMachineAttrCol), NameSequenceVar),
                 tab_get_name(CtrStartCol,  CtrStartColName),
                 tab_get_name(CtrDurEndCol, CtrDurEndColName),
                 atoms_concat([NameSequenceVar, '_', CtrStartColName, '_', CtrDurEndColName, '_resource_ids'], CtrMachineAttrIdName),
                 format(SOut,'array[1..~w] of int: ~w;~n',[CtrMachineAttrValuesLength,CtrMachineAttrIdName]),   % for now all the data is stored in the same file.
                 format(DOut, '~w = ~w;~n', [CtrMachineAttrIdName, CtrMachineAttrValuesSorted])
            ),
            (CtrMode = 0 ->         % if CtrMode set to duration
                 format(SOut,
                        'constraint forall(j in ~w) (disjunctive([~w|i in ~w where j = ~w],[~w|i in ~w where j = ~w]));',
                        [CtrMachineAttrIdName, CtrStartAttr, RowsVarNameChild, CtrMachineAttr, CtrDurEndAttr, RowsVarNameChild, CtrMachineAttr])
            ;                       % if CtrMode set to end_time
                 print_duration_variable_constraint(SOut-DOut, MergeTableCols, RowsVarNameChild, ConjectureListFiltered,
                                                    CtrStartAttr, CtrDurEndAttr, CtrStartCol, CtrDurEndCol, DurationAttrs, DurationAttrsNew, DurationAttrIndex),
                 format(SOut, '~n% args: disjunctive(List of task start times,~n', []),
                 format(SOut,   '%       List of task durations).~n', []),
                 format(SOut,
                        'constraint forall(j in ~w) (disjunctive([~w|i in ~w where j = ~w],[~w|i in ~w where j = ~w]));',
                        [CtrMachineAttrIdName, CtrStartAttr, RowsVarNameChild, CtrMachineAttr, DurationAttrIndex, RowsVarNameChild, CtrMachineAttr])
            )
        ;
            true
        )
    ;
     Formula = diffn(CtrMode) ->
        write(diffn),nl,
        InputCols        = [CtrMachineCol,  _CtrMachinesCol,  CtrStartCol,  CtrDurEndCol ],
        InputColsIndices = [ CtrMachineAttr, _CtrMachinesAttr, CtrStartAttr, CtrDurEndAttr],
        CtrMachineCol = col(_, IMachine),
        CtrStartCol  = col(_,IStart),
        CtrDurEndCol = col(_,IDurEnd),
        %memberchk([_I, child, ColResource, foreign(TableCalendarSource, [TableCalendarSourcePK])], MergeTableCols),
        memberchk([IMachine,  _, _ColSourceMachine,                          foreign(TableCalendarSource, _)], MergeTableCols),
        memberchk([IStart,    _, col(TableSourceStart,   _ColSourceStart)  , _],                                                     MergeTableCols),
        memberchk([IDurEnd,   _, col(TableSourceDurEnd,  _ColSourceEnd)    , _],                                                     MergeTableCols),

        % check if we must add calendar restrictions to the diffn constraint. We do it only once per table
        (selectchk(TableCalendarSource-[CalendarStartsVar, CalendarResourceVar,
                                        CalendarDurationsVar, CalendarOneVar], DiffNAdditions, DiffNAdditionsNew) ->
             atom_concat(' ++ ', CalendarStartsVar,    StartPlus),
             atom_concat(' ++ ', CalendarResourceVar,  ResourcePlus),
             atom_concat(' ++ ', CalendarDurationsVar, DurationPlus),
             atom_concat(' ++ ', CalendarOneVar,       OnePlus)
        ;
             StartPlus = '',
             ResourcePlus = '',
             DurationPlus = '',
             OnePlus = ''
        ),
        (TableSourceStart = TableSourceDurEnd -> % times and duration columns must be from the same child table
            (CtrMode = 0 ->         % if CtrMode set to duration
                 format(SOut, '~n% args: diffn(List of task start times~n',[]),
                 format(SOut,   '%             List of machines for each task,~n', []),
                 format(SOut,   '%             List of task durations,~n', []),
                 format(SOut,   '%             List of 1s).~n', []),
                 (StartPlus = '' ->
                      true
                 ;
                      format(SOut, '% This DIFFN constraint also includes calendar constraint.~n', [])
                 ),
                 format(SOut,
                        'constraint (diffn([~w | i in ~w]~w,[~w | i in ~w]~w, [~w | i in ~w]~w, [1 | i in ~w]~w));',
                        [CtrStartAttr, RowsVarNameChild, StartPlus, CtrMachineAttr, RowsVarNameChild, ResourcePlus,
                         CtrDurEndAttr, RowsVarNameChild, DurationPlus, RowsVarNameChild, OnePlus])
            ;                       % if CtrMode set to end_time
                 print_duration_variable_constraint(SOut-DOut, MergeTableCols, RowsVarNameChild, ConjectureListFiltered,
                                                    CtrStartAttr, CtrDurEndAttr, CtrStartCol, CtrDurEndCol, DurationAttrs, DurationAttrsNew, DurationAttrIndex),
                 format(SOut, '~n% args: diffn(List of task start times~n',[]),
                 format(SOut,   '%             List of machines for each task,~n', []),
                 format(SOut,   '%             List of task durations,~n', []),
                 format(SOut,   '%             List of 1s).~n', []),
                 (StartPlus = '' ->
                      true
                 ;
                      format(SOut, '% This DIFFN constraint also includes calendar constraint.~n', [])
                 ),
                 format(SOut,
                        'constraint (diffn([~w | i in ~w]~w,[~w | i in ~w]~w, [~w | i in ~w]~w, [1 | i in ~w]~w));',
                        [CtrStartAttr, RowsVarNameChild, StartPlus, CtrMachineAttr, RowsVarNameChild, ResourcePlus,
                         DurationAttrIndex, RowsVarNameChild, DurationPlus, RowsVarNameChild, OnePlus])
            )
        ;
            true
        )
    ;
        true
    ),
    convert_resource_conjectures(R, ConjectureListFiltered, SOut-DOut, Table, MergeTableCols,
                                 FKNamesList, RowsVarNameChild, DiffNAdditionsNew, DurationAttrsNew, DurationAttrsAll).

% given a list of columns provide a list of strings of variable names with an index 'i'
% [col(table, 1)] -> ['child_id[i]']
convert_cols_to_vars_w_indices(InputCols, MergeTableCols,  FKNamesList, InputColsIndices) :-
    (foreach(col(_Table, I), InputCols),
     foreach(NameVarIndex,   InputColsIndices),
     param([MergeTableCols,  FKNamesList])
    do
     memberchk([I, ChildParent, col(TableSource,ColSource), _Kind], MergeTableCols),
     (ChildParent = child ->
        get_var_name(col(TableSource,ColSource), NameVar),
        var_square_brackets_index_concat(NameVar,i,NameVarIndex)
     ;
        get_var_name(col(TableSource,ColSource), NameVar),
        memberchk(FKNameVar-TableSource, FKNamesList),
        var_square_brackets_index_concat(FKNameVar,i,FKNameVarIndex),
        var_square_brackets_index_concat(NameVar,FKNameVarIndex,NameVarIndex)
     )
    ).


% print duration variable and a corresponding constraint
print_duration_variable_constraint(SOut-DOut, MergeTableCols, RowsVarName, ConjectureListFiltered, CtrStartAttr,
                                   CtrDurEndAttr, CtrStartCol, CtrDurEndCol, DurationAttrs, DurationAttrsNew, DurationAttrIndex) :-
    CtrStartCol = col(_,IStart),
    memberchk([IStart, _, col(TableSource,ColSourceStart), _], MergeTableCols),
    get_var_name(col(TableSource,ColSourceStart), DurationStart),
    atom_concat(DurationStart,'_',DurationStartUnderscore),
    tab_get_name(CtrDurEndCol, DurationEnd),
    atom_concat(DurationStartUnderscore, DurationEnd, DurationAttrCand),
    write(DurationAttrCand),nl,
    (memberchk(DurationAttrCand, DurationAttrs) -> % if the duration attribute already present in the model, no need to create it again
        DurationAttr = DurationAttrCand,
        DurationAttrsNew = DurationAttrs
    ;
        CtrDurEndCol = col(_,IEnd),
        memberchk([IEnd, _, col(TableSource,ColSourceEnd), _], MergeTableCols),
        collect_col_values(TableSource, ColSourceStart, ValStart),
        collect_col_values(TableSource, ColSourceEnd,   ValEnd),
        (foreach(XEnd, ValEnd), foreach(XStart, ValStart), foreach(X, DurationVals)
        do
         X is XEnd - XStart
        ),
     % step 1: check if there's no duration column present already. If yes, DurationAttr = that column, no printing
     % step 2: check that there's no formula of the type: end_time = f(start_time,...) OR start_time = f(end_time,...)
     %   if yes: print as before, it's a variable calculated dynamically, then DurationAttr = DurationAttrCand
     %   if  no: call print_duration_constant_constraint, print as a constant, then DurationAttr = DurationAttrCand
        (check_identical_col(TableSource, MergeTableCols, DurationVals, DurationAttr) ->
            (memberchk(DurationAttr, DurationAttrs) ->
                 DurationAttrsNew = DurationAttrs
            ;
                 DurationAttrsNew = [DurationAttr|DurationAttrs]
            )
        ;
         check_conjectures_with_start_time_end_time(ConjectureListFiltered, CtrStartCol, CtrDurEndCol) ->
            DurationAttr = DurationAttrCand,
            DurationAttrsNew = [DurationAttr|DurationAttrs],
            % precalculation of the domain of the duration variable
            min_member(DurationMin, DurationVals),
            max_member(DurationMax, DurationVals),
            format(SOut,'array[~w] of var ~w..~w: ~w;~n',[RowsVarName, DurationMin, DurationMax, DurationAttr]),    % declare the duration var
            format(SOut,
                   '~nconstraint forall (i in ~w) (~w[i] = ~w - ~w);~n',
                   [RowsVarName,DurationAttr,CtrDurEndAttr,CtrStartAttr])              % calculate the duration var
        ;
            DurationAttr = DurationAttrCand,
            DurationAttrsNew = [DurationAttr|DurationAttrs],
            % print column values
            format(SOut,'array[~w] of int: ~w;~n',[RowsVarName,DurationAttr]), % for now all the data is stored in the same file.
            format(DOut, '~w = ~w;~n', [DurationAttr, DurationVals]),
            % print column var in the main file and the constraint to calculate end time
            format(SOut,
                   '~nconstraint forall (i in ~w) (~w = ~w + ~w[i]);~n',
                   [RowsVarName,CtrDurEndAttr,CtrStartAttr,DurationAttr])      % calculate end times given start times and durations
        )
    ),
    var_square_brackets_index_concat(DurationAttr,i,DurationAttrIndex).

check_identical_col(TableSource, MergeTableCols, DurationVals, DurationAttr) :-
    findall(col(TableSource,ColSourceStart),
            member([_, _, col(TableSource,ColSourceStart), _], MergeTableCols),
            SourceColList),
    check_identical_col1(SourceColList, DurationVals, DurationAttr).

check_identical_col1([col(TableSource,ColSourceStart)|R], DurationVals, DurationAttr) :-
    collect_col_values(TableSource, ColSourceStart, SourceVals),
    (SourceVals = DurationVals ->
        get_var_name(col(TableSource,ColSourceStart), DurationAttr)
    ; 
        check_identical_col1(R, DurationVals, DurationAttr)
    ).

% check that there's at least one conjecture that is used either
%  - to calculate start time by using   end time OR
%  - to calculate end   time by using start time
check_conjectures_with_start_time_end_time([[OutputCol, _Cost, t(InputCols, _InputNames, _InputVars, _Formula)]|R],
                                           CtrStartCol, CtrDurEndCol) :-
    ((OutputCol = CtrStartCol, memberchk(CtrDurEndCol, InputCols) ; OutputCol = CtrDurEndCol, memberchk(CtrStartCol, InputCols)) ->
       true
    ;
       check_conjectures_with_start_time_end_time(R, CtrStartCol, CtrDurEndCol)
    ).

check_no_values_col_in_table(Table, MergeTableCols, DurationVals) :-
    tab_get_arity(col(Table,_), N),
    (for(I,1,N), param([MergeTableCols, DurationVals])
    do
     memberchk([I, _, col(TableI,ColI), _], MergeTableCols),
     collect_col_values(TableI,ColI,  ValI),
     ValI \== DurationVals
    ).

convert_conjectures1([], _SOut, _Table, _MergeTableCols, _FKNamesList, _RowsVarNameChild) :- !.
convert_conjectures1([[OutputCol,_Cost,Conjecture]|R], SOut, Table, MergeTableCols, FKNamesList, RowsVarNameChild) :-
    Conjecture = t(InputCols, _InputNames, InputVars, Formula),
    (foreach(col(Table, I), InputCols), foreach(Var, InputVars), param([Table, MergeTableCols, FKNamesList])
    do
     memberchk([I, ChildParent, col(TableSource,ColSource), _Kind], MergeTableCols),
     (ChildParent = child ->
        get_var_name(col(TableSource,ColSource), NameVar),
        var_square_brackets_index_concat(NameVar,i,NameVarIndex)
     ;
        get_var_name(col(TableSource,ColSource), NameVar),
        memberchk(FKNameVar-TableSource, FKNamesList),
        var_square_brackets_index_concat(FKNameVar,i,FKNameVarIndex),
        var_square_brackets_index_concat(NameVar,FKNameVarIndex,NameVarIndex)
     ),
     NameVarIndex = Var
    ),                                   % Unify variables with its names.
    memberchk([OutputCol, _ChildParent, col(TableOutput,ColOutput), _], MergeTableCols),
    get_var_name(col(TableOutput,ColOutput), OutputVar),
    format(SOut,'~n~nconstraint forall (i in ~w) (~w[i] = ',[RowsVarNameChild,OutputVar]),
    convert_conjecture(Formula, SOut),
    format(SOut,');~n',[]),
    convert_conjectures1(R, SOut, Table, MergeTableCols, FKNamesList, RowsVarNameChild).

convert_conjecture(Term, _) :- % first catch the case when Term was incorrectly generated
    \+ ground(Term),           % in order to avoid getting in an infinite recursive loop
    !,
    write(convert_conjecture(Term)), nl, halt.
convert_conjecture(Term, SOut) :-
    integer(Term),
    !,
    (Term >= 0 ->
        format(SOut,'~w', [Term])
    ;
     Term < 0 ->
        format(SOut,'~w', [Term])
    ;
        true
    ).
convert_conjecture(Term, SOut) :-     % convert an atom (variable)
    atom(Term), !,
    format(SOut, '~w', [Term]).
convert_conjecture(if(Cond, Term1, Term2),SOut) :-
    !,
    format(SOut, '(if (', []),
    convert_conjecture(Cond, SOut),
    format(SOut, ') then (', []),
    convert_conjecture(Term1, SOut),
    format(SOut, ') else (', []),
    convert_conjecture(Term2, SOut),
    format(SOut, ') endif)', []).
convert_conjecture(bool(Negated, ShiftBool, Oplus, NbTerms, Conds), SOut) :-
    !,
    ((Negated = 1, Oplus = sum) -> ShiftBoolNew is ShiftBool + NbTerms  ;
                                   ShiftBoolNew is ShiftBool            ),
    (Negated = 0 ->
        (ShiftBoolNew = 0 ->
             format(SOut, '(', [])
        ;
             format(SOut, '~w+(', [ShiftBoolNew])
        )
    ;
     (Negated = 1, Oplus = sum) ->
        (ShiftBoolNew = 0 ->
             format(SOut, '-(', [])
        ;
             format(SOut, '~w-(', [ShiftBoolNew])
        )
    ;
     Negated = 1 ->
        (ShiftBoolNew = 0 ->
             format(SOut, 'not(', [])
        ;
             format(SOut, '~w+not(', [ShiftBoolNew])
        )
    ;
        false
    ),
    (Oplus = and       ->      to_atom(/,A1),to_atom(\,A2),atom_concat(A1,A2,Sign)      ;
     Oplus = or        ->      to_atom(/,A1),to_atom(\,A2),atom_concat(A2,A1,Sign)      ;
     Oplus = sum       ->      to_atom(+,Sign)                                          ;
     Oplus = allequal  ->      to_atom(/,A1),to_atom(\,A2),atom_concat(A1,A2,Sign),
                               to_atom(/,A1),to_atom(\,A2),atom_concat(A2,A1,SignOR)    ;
     Oplus = xor       ->      Sign = xor                                               ;
     Oplus = voting    ->      to_atom(+,Sign), format(SOut, '(', [])                   ;
     Oplus = card1     ->      to_atom(+,Sign), format(SOut, '(', [])                   ;
                               false                                                    ),
    Conds = [Cond|RConds],
    format(SOut, '(', []),
    convert_conjecture(Cond, SOut),
    format(SOut, ')', []),
    (foreach(TermR,RConds), param(SOut), param(Sign)
    do
     format(SOut, '~w(', [Sign]),
     convert_conjecture(TermR, SOut),
     format(SOut, ')', [])
    ),
    (Oplus = allequal  -> %allequal case must look like (Cond1 AND Cond2 AND Cond3) OR (not Cond1 AND not Cond2 AND not Cond3)
        format(SOut, ')~w(', [SignOR]),
        format(SOut, '(', []),
        convert_conjecture(not(Cond), SOut),
        format(SOut, ')', []),
        (foreach(TermA,RConds), param(SOut), param(Sign)
        do
         format(SOut, '~w(', [Sign]),
         convert_conjecture(not(TermA), SOut),
         format(SOut, ')', [])
        ),
        format(SOut, ')', [])
    ;
     Oplus = voting  ->
        length(Conds, LConds),
        LConds1 is (LConds div 2 + 1),
        format(SOut, ') >= ~w)', [LConds1])
    ;
     Oplus = card1  ->
        format(SOut, ') = 1)', [])
    ;
        format(SOut, ')', [])
    ).
convert_conjecture(geq0(Term),SOut) :-
    !, convert_conjecture(geq(Term,0), SOut).
convert_conjecture(not(not(Term)), SOut) :-
    !, convert_conjecture(Term, SOut).
convert_conjecture(not(Term), SOut) :-
    !,
    format(SOut, 'not(',  []),
    convert_conjecture(Term, SOut),
    format(SOut, ')',  []).
convert_conjecture(minus(Term), SOut) :-
    !,
    format(SOut, '-(',  []),
    convert_conjecture(Term, SOut),
    format(SOut, ')',  []).
convert_conjecture(Term, SOut) :-
    functor(Term, Type, 2),
    memberchk(Type, [eq,geq,leq,plus,minus,div,floor,mod]),
    !,
    arg(1,Term,Term1),
    arg(2,Term,Term2),
    format(SOut, '(', []),
    convert_conjecture(Term1, SOut),
    (Type = eq          ->      format(SOut, '=',     [])       ;
     Type = geq         ->      format(SOut, '>=',    [])       ;
     Type = leq         ->      format(SOut, '<=',    [])       ;
     Type = plus        ->      format(SOut, '+',   [])         ;
     Type = minus       ->      format(SOut, '-',  [])          ;
     Type = div         ->      format(SOut, ' div ', [])       ;
     Type = floor       ->      format(SOut, ' div ', [])       ;
     Type = mod         ->      format(SOut, ' mod ',    [])    ),
    convert_conjecture(Term2, SOut),
    format(SOut, ')', []).
convert_conjecture(Term, SOut) :-
    functor(Term, Type, 2),
    memberchk(Type, [min,max]),
    !,
    arg(1,Term,Term1),
    arg(2,Term,Term2),
    format(SOut, '~w(', [Type]),
    convert_conjecture(Term1, SOut),
    format(SOut, ',',  []),
    convert_conjecture(Term2, SOut),
    format(SOut, ')', []).
convert_conjecture(prod(Term1, Term2), SOut) :-
    !,
    (Term1 =  1         ->      convert_conjecture(Term2, SOut)               ;
     Term2 =  1         ->      convert_conjecture(Term1, SOut)               ;
     Term1 = -1         ->      convert_conjecture(minus(Term2), SOut)        ;
     Term2 = -1         ->      convert_conjecture(minus(Term1), SOut)        ;
     integer(Term2)     ->      convert_conjecture(prod1(Term2, Term1), SOut) ;
                                convert_conjecture(prod1(Term1, Term2), SOut) ).
convert_conjecture(prod1(Term1, Term2), SOut) :-
    !,
    format(SOut, '(', []),
    convert_conjecture(Term1, SOut),
    format(SOut, '*', []),
    convert_conjecture(Term2, SOut),
    format(SOut, ')', []).
convert_conjecture(power(Term1, Term2), SOut) :-
    !,
    (Term2 = 1  ->      % if Power = 1 no need to apply the tag
        convert_conjecture(Term1, SOut)
    ;
        convert_conjecture(Term1, SOut),
        format(SOut,'^(',[]),
        convert_conjecture(Term2, SOut),
        format(SOut,')',[])
    ).
convert_conjecture(abs(Term1, Term2), SOut) :-
    !,
    format(SOut, 'abs(', []),
    convert_conjecture(Term1, SOut),
    format(SOut, '-', []),
    convert_conjecture(Term2, SOut),
    format(SOut, ')', []).
convert_conjecture(ceil(Term1, Term2), SOut) :-
    !,
    format(SOut, '(', []),
    convert_conjecture(Term1, SOut),
    format(SOut, '+', []),
    convert_conjecture(Term2, SOut),
    format(SOut, '-1) div (', []),
    convert_conjecture(Term2, SOut),
    format(SOut, ')', []).
convert_conjecture(cmod(Term1, Term2), SOut) :-
    !, convert_conjecture(minus(Term1, mod(Term2,Term1)), SOut).
convert_conjecture(dmod(Term1, Term2), SOut) :-
    !, convert_conjecture(minus(Term1, mod(Term1,Term2)), SOut).
convert_conjecture(fmod(Term1, Term2), SOut) :-
    !, convert_conjecture(if(eq(mod(Term1,Term2), 0), Term2, mod(Term1, Term2)),
                          SOut).
convert_conjecture(sum_consec(Term),SOut) :-
    !, convert_conjecture(div(prod(Term,plus(Term,1)),2), SOut).
convert_conjecture(Term, SOut) :-
    functor(Term, Type, 3),
    memberchk(Type, [in,plus,min_min,max_min,floor_min,mfloor,linear,plus_min,plus_floor,sum_leq_attr,minus_mod_eq0]),
    !,
    arg(1,Term,Term1),
    arg(2,Term,Term2),
    arg(3,Term,Term3),
    (Type = in                  ->      format(SOut, '(', []),
                                        convert_conjecture(Term1, SOut),
                                        format(SOut, ' in ', []),
                                        convert_conjecture(Term2, SOut),
                                        format(SOut, '..', []),
                                        convert_conjecture(Term3, SOut),
                                        format(SOut, ')', [])                                                 ;
     Type = plus                ->      format(SOut, '(', []),
                                        convert_conjecture(Term1, SOut),
                                        format(SOut, '+', []),
                                        convert_conjecture(Term2, SOut),
                                        format(SOut, '+', []),
                                        convert_conjecture(Term3, SOut),
                                        format(SOut, ')', [])                                                 ;
     Type = min_min             ->      convert_conjecture(min(minus(Term1,Term3),Term2), SOut)               ;
     Type = max_min             ->      convert_conjecture(max(minus(Term1,Term3),Term2), SOut)               ;
     Type = floor_min           ->      convert_conjecture(floor(minus(Term1,Term3),Term2), SOut)             ;
     Type = mfloor              ->      convert_conjecture(floor(Term1,max(minus(Term2,Term3),1)), SOut)      ;
     Type = linear              ->      convert_conjecture(plus(Term1,prod(Term2,Term3)), SOut)               ;
     Type = plus_min            ->      convert_conjecture(min(minus(plus(Term1,Term2),Term3),Term1), SOut)   ;
     Type = plus_floor          ->      convert_conjecture(floor(plus(Term1,Term2,Term3),Term1),SOut)         ;
     Type = sum_leq_attr        ->      convert_conjecture(leq(plus(Term1,Term2),Term3),SOut)                 ;
     Type = minus_mod_eq0       ->      convert_conjecture(eq(mod(minus(Term3,Term1),Term2),0),SOut)          ).

convert_conjecture(linear(Term1,Term2,Term3,Term4),SOut) :-
    !, convert_conjecture(plus(prod(Term4,Term1),Term2,Term3), SOut).

convert_conjecture(minus_floor(Term1,Term2,Term3,Term4),SOut) :-
    !, convert_conjecture(div(minus(plus(Term1,Term3),Term2),Term4), SOut).
convert_conjecture(polynom(Terms), SOut) :-
    !,
    format(SOut,'(', []),
    Terms = [HTerm|RTerms],
    convert_conjecture(HTerm, SOut),
    (foreach(Term, RTerms), param(SOut)
    do
     format(SOut, '+', []),
     convert_conjecture(Term, SOut)
    ),
    format(SOut,')', []).
convert_conjecture(monome([], Coef), SOut) :-
    !,
    convert_conjecture(Coef, SOut).
convert_conjecture(monome([t(Term,Degree)],Coef), SOut):-
    !,
    convert_conjecture(prod(Coef,power(Term,Degree)), SOut).
convert_conjecture(monome(Terms,Coef), SOut):-
    !,
    (Coef = 0 ->
        true
    ;
     Coef =< -1 ->
        format(SOut, '-(', []),
        Coef1 is -Coef,
        convert_conjecture(Coef1, SOut),
        (foreach(t(Term,Degree),Terms), param(SOut)
        do
         format(SOut, '*', []),
         convert_conjecture(power(Term,Degree), SOut)
        ),
        format(SOut, ')', [])
    ;
        format(SOut, '(', []),
        convert_conjecture(Coef, SOut),
        (foreach(t(Term,Degree),Terms), param(SOut)
        do
         format(SOut, '*', []),
         convert_conjecture(power(Term,Degree), SOut)
        ),
        format(SOut, ')', [])
    ).
convert_conjecture(cases(TermsList),SOut) :-
    !,
    TermsList = [if_then(HCondTerm,HThenTerm)|RTerms],
    format(SOut, '(if (', []),
    convert_conjecture(HCondTerm, SOut),
    format(SOut, ') then (', []),
    convert_conjecture(HThenTerm, SOut),
    format(SOut, ') ', []),
    (foreach(Term, RTerms), param(SOut)
    do
     convert_conjecture(Term, SOut)
    ),
    format(SOut, ' endif)', []),
    true.
convert_conjecture(if_then(CondTerm,ThenTerm),SOut) :-
    !,
    format(SOut, 'elseif (', []),
    convert_conjecture(CondTerm, SOut),
    format(SOut, ') then (', []),
    convert_conjecture(ThenTerm, SOut),
    format(SOut, ') ', []),
    true.
convert_conjecture(otherwise(ElseTerm),SOut) :-
    !,
    format(SOut, 'else (', []),
    convert_conjecture(ElseTerm, SOut),
    format(SOut, ')', []).
convert_conjecture(Term,_) :- write(Term),nl.

remove_signature_merge_and_table_facts_conjectures(Tables) :-
    Tables = [TableChild|_],
    tab_get_arity(col(TableChild,_), NbCols),
    NbCols1 is NbCols + 1,
    functor(MergeTableInfo, merge_table_info, NbCols1),
    retractall(MergeTableInfo),
    (foreach(Table, Tables)
    do
     signature(Table, _, Args),         % from the signature fact of Table get information on the columns of the table
     functor(Args, t, NbColumns),       % get number of columns of the table
     functor(Term, Table, NbColumns),   % create a term whose functor is the table name and whose arity is NbColumns
     retractall(Term)                   % remove all rows of the current table as finish to deal with current table
    ),
    retractall(signature(_,_,_)),       % remove the signature fact as finish to deal with current table 
    retractall(conjecture(_,_,_,_,_,_,_,_,_)). % cmodif
