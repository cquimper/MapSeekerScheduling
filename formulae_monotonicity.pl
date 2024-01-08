% Purpose: EVALUATE MONOTONICITY OF THE BOUND CONJECTURES FUNCTIONS (used only for combinatorial objects)
% Authors: Nicolas Beldiceanu, IMT Atlantique

:- use_module(library(clpfd)).
:- use_module(tables).
:- use_module(table_access).
:- use_module(gen_candidate_tables).
:- use_module(eval_formula).
:- use_module(monotonicity).
:- use_module(utility).

top(KindCombi) :-                   % modif
    top(KindCombi, 1, 2).           % modif

top(KindCombi, NParam) :-           % modif
    top(KindCombi, NParam, NParam). % modif

% evaluate the monotonicity of each sharp bound conjecture function acquired
% for the combinatorial object KindCombi (e.g. graph, forest, forest0, tree, partition, partition0, group, cgroup) with respect to
% the different input parameters of the function.
top(KindCombi, MinNParam, MaxNParam, SplitDiv, SplitMod) :- % modif
    low_up_attributes_combi(KindCombi, ListPairsBoundAttr),     % iterate over Bound-Attribute pairs wrt KindCombi
    member(Bound-Attr, ListPairsBoundAttr),                     % get the next pair
    atoms_concat(['data/',KindCombi,
                  '/found_conjectures_',                        % construct the name of the conjecture file corresponding
                  KindCombi,       '_',                         % to the triple (KindCombi,Bound,Attr), i.e. corresponding to a map
                  Bound,           '_',
                  Attr,            '_',                         % modif
                  MinNParam,       '_',                         % modif                  
                  MaxNParam,       '_',                         % modif
                  SplitDiv,        '_',                         % modif
                  SplitMod,        '.pl'], ConjecturesFileName),% modif
    consult(ConjecturesFileName),                               % consult file containing all acquired conjectures
    NParam in MinNParam..MaxNParam,  % modif
    indomain(NParam),                % modif                    % on backtrack get next number of input parameters
    handle_tables_of_current_conjecture_file(KindCombi, Bound, Attr, NParam), % modif
    remove_conjecture_facts_of_current_map,
    false.
top(_, _, _, _, _). % modif

handle_tables_of_current_conjecture_file(KindCombi, Bound, Attr, NParam) :- % modif
    functor(LowUpAttr, Bound, 1), arg(1, LowUpAttr, Attr),           % construct a term of the form low(Attr) or up(Attr)
    gen_candidate_tables(Table, KindCombi, Attr, LowUpAttr, NParam), % modif % generate the next table name
%   \+ memberchk(Table, [low_c_maxc_s_maxs]),
    atoms_concat(['data/',KindCombi,'/data'],                        % construct path to directory from which we extract
                 DirData),                                           % the metadata associated with the table Table
%   max_size_combi(KindCombi, MaxMaxN),                              % get largest availabel size for which we generate data
    MaxMaxN = 10,                                                    % TODO: remove and uncomment previous line when metadata for 26 is there
    get_range_of_input_parameters(DirData, MaxMaxN, Table,           % get domain of potential input parameters of sharp bound functions
                                  InputParams, RangeInputParams),
    findall(conjecture(col(Table,Col),FormulaTerm),
            conjecture(_,Bound,col(Table,Col),_Name,_MaxN,_Cost,FormulaTerm,_,_), % cmodif
            Conjectures),                                            % extract sharp bound conjectures related to current table Table
    check_monotonicity_of_sharp_bounds(Conjectures, Table, InputParams, RangeInputParams),
    false.
handle_tables_of_current_conjecture_file(_, _, _, _). % modif

get_range_of_input_parameters(DirData, MaxMaxN, Table, InputParams, RangeInputParams) :-
    gen_table_and_metadata_file_names(DirData, MaxMaxN, Table, % get name of the file containing the metadata
                                      _, MetadataFile),        % of current table Table
    consult(MetadataFile),                                     % read the metadata of the table Table
    get_metadata_arity(Arity),                                 % extract arity of the metadata term
    get_metadata_attributes(IndexNames),                       % extract list of pairs of index in the metadata term and corresponding name
    functor(TableMetaData, table_metadata, Arity),             % build a metadata term
    memberchk(IndNbColumns-nb_columns, IndexNames),            % get index of number of columns of the table
    memberchk(IndKinds-kinds,          IndexNames),            % get index of column kinds
    memberchk(IndMins-mins,            IndexNames),            % get index of column minimum values
    memberchk(IndMaxs-maxs,            IndexNames),            % get index of column maximum values
    arg(IndNbColumns, TableMetaData, NbColumns),               % extract number of columns
    arg(IndKinds,     TableMetaData, Kinds),                   % extract kinds term
    arg(IndMins,      TableMetaData, Mins),                    % extract mins term
    arg(IndMaxs,      TableMetaData, Maxs),                    % extract maxs term
    call(TableMetaData),                                       % get the metadata fact
    findall(col(Table,Col), (Col in 1..NbColumns,              % for all input parameters (i.e. Kind=primary) 
                             indomain(Col),                    % get corresponding index
                             arg(Col, Kinds, primary)), InputParams),
    findall(t(Col,Min,Max), (Col in 1..NbColumns,              % for all input parameters (i.e. Kind=primary) 
                              indomain(Col),                   % get corresponding minimum and maximum values
                              arg(Col, Kinds, primary),
                              arg(Col, Mins,  Min    ),
                              arg(Col, Maxs,  Max    )), RangeInputParams),
    remove_metadata_fact_of_current_table.                     % remove metadata of current table before handling next table

check_monotonicity_of_sharp_bounds([], _, _, _) :- !.
check_monotonicity_of_sharp_bounds([Conjecture|R], Table, InputParams, RangeInputParams) :-
    Conjecture = conjecture(_, t(ConjParams, _, _, _)),
    ((ConjParams = [_|_],                                      % the conjecture should use at least one input parameter (not a constant)
      memberchks(ConjParams,InputParams)) ->                   % all conjecture parameters should be in the input parameters (no secondary char.)
        check_monotonicity_of_sharp_bound(Conjecture, Table, RangeInputParams)
    ;
        true                                                   % if use secondary char. do nothing
    ),
    check_monotonicity_of_sharp_bounds(R, Table, InputParams, RangeInputParams).

% compute monotonicity of current sharp bound wrt each of its input parameters
check_monotonicity_of_sharp_bound(Conjecture, _Table, RangeInputParams) :-
    nl, write('check monotonicity of conjecture '), write(Conjecture), nl,
    Conjecture = conjecture(BoundParam, FormulaTerm),
    FormulaTerm = t(ListOfInputParams, _ListOfNames, _ListOfSourceVariables, _SourceTerm),
    create_input_param_vars(ListOfInputParams, RangeInputParams, ListOfInputVars),
%   (Table = low_c_maxc_s_maxs -> ListOfInputVars = [C, _MaxC, S], S #>= C ;
%                                 true                                     ),
    del(SelectedParam, SelectedVar, ListOfInputParams, ListOfInputVars, _IndFixedParams, VarFixedParams),
    catch(check_monotonicity_of_input_param_of_sharp_bound(SelectedParam, SelectedVar, BoundParam, VarFixedParams, ListOfInputVars, BoundParam, FormulaTerm),
          error(evaluation_error(zero_divisor),_),
          undefined_function_for_infeasible_tuple(SelectedParam)),
    false.
check_monotonicity_of_sharp_bound(_, _, _).

% as we ignore the feasibility constraints when we generate the cartesian product of the input parameters
% the function may be undefined (e.g. division by 0) for some tuples corresponding to infeasible combinations of parameters
undefined_function_for_infeasible_tuple(SelectedParam) :-
    SelectedParam = col(Table,I),
    write(Table), write(': '), write('undefined function'), write(' wrt parameter '), write(I), nl.

create_input_param_vars([], _, []) :- !.
create_input_param_vars([col(_Table,Col)|R], RangeInputParams, [Var|S]) :-
    memberchk(t(Col,Min,Max), RangeInputParams),
    Var in Min..Max,
    create_input_param_vars(R, RangeInputParams, S).

% compute monotonicity of current sharp bound wrt parameter SelectedParam and bound parameter BoundParam
check_monotonicity_of_input_param_of_sharp_bound(_SelectedParam, SelectedVar, BoundParam, VarFixedParams, ListOfInputVars, BoundParam, FormulaTerm) :-
    retractall(monotony(_)),                                                         % generate next group of values for input parameters
    assert(monotony(undetermined)),                                                  % (except for selected variable)
    labeling([], VarFixedParams),                                                    % fix current group of variables
    generate_pairs_wrt_selected_param_and_bound(SelectedVar, BoundParam, VarFixedParams, ListOfInputVars, FormulaTerm, Pairs),
    monotony(PrevMonotonicity),                                                      % get value of previous cumulated monotonicity
    (PrevMonotonicity = none ->                                                      % if already not monotonic do not evaluate monotonicity for next groups
        false
    ;                                                                                % if still monotonic then 
        monotonicity(Pairs, PrevMonotonicity, Monotonicity, _),                      % evaluate monotonicity of current group
        combine_monotonicity(PrevMonotonicity, Monotonicity, CurrentMonotonicity),   % and combine it with cumulated monotonicity of previous groups
        retractall(monotony(_)),
        assert(monotony(CurrentMonotonicity)),                                       % assert new cumulated monotonicity
        false                                                                        % fail in order to compute monotonicity of next group
    ).
check_monotonicity_of_input_param_of_sharp_bound(SelectedParam, _, _, _, _, _, _) :- % succeed to compute monotonicity wrt next input parameter
    SelectedParam = col(Table,I),
    monotony(Monotonicity),
    write(Table), write(': '), write(Monotonicity), write(' wrt parameter '), write(I), nl.

generate_pairs_wrt_selected_param_and_bound(SelectedVar, _BoundParam, VarFixedParams, ListOfInputVars, FormulaTerm, Pairs) :-
    FormulaTerm = t(_ListOfInputParams, _ListOfNames, ListOfSourceVariables, SourceTerm),
    findall(VarFixedParams-t(SelectedVar,ValBoundParam), (indomain(SelectedVar),
                                                          copy_term(ListOfSourceVariables-SourceTerm, ListOfInputVars-TargetTerm),
                                                          eval_formula_term(TargetTerm, ValBoundParam)),                           Pairs).

remove_conjecture_facts_of_current_map :-
    Arity = 9,                        % set arity of a conjecture fact      % cmodif
    functor(Term, conjecture, Arity), % create an "empty" conjecture term
    retractall(Term).                 % remove conjectures of current map

remove_metadata_fact_of_current_table :-
    get_metadata_arity(Arity),            % get arity of a metadata fact
    functor(Term, table_metadata, Arity), % create an "empty" table_metadata term
    retractall(Term).                     % remove metadata of current table

del(Elem1, Elem2, [Elem1|Rest1], [Elem2|Rest2], Rest1, Rest2).
del(Elem1, Elem2, [Skip1|List1], [Skip2|List2], [Skip1|Rest1], [Skip2|Rest2]) :-
    del(Elem1, Elem2, List1, List2, Rest1, Rest2).
