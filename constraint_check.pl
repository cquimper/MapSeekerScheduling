% Purpose: Automatic generation of forbidden pairs rules that are later used in gen_bool_formula.pl
% Author : Ramiz Gindullin, IMT Atlantique

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(constraint_check_condition_generation).
:- use_module(found_condpairs_family1_symf3).
:- use_module(found_condpairs_family2_symf3).
:- use_module(found_condpairs_family1_sym).
:- use_module(found_condpairs_family2_sym).

op_plus_list([and,
              or,
              allequal,
              xor]).

% Instruction to generate proper rules that are used by forbid_specific_combinations_of_conds_generated.pl:
% Step 1: run 'top.' predicate
% Step 2: restart Sicstus PROLOG
% Step 3: run 'top(3).' predicate OR run predicates 'top(31).', 'top(33).', 'top(35).' and 'top(37).' in parallel and
% combine rules from four .pl files by hand in one file 'found_condpairs_family3_new.pl'
top :- top(11),top(22), halt.

top(1) :-       % for test purposes
    generate_constraints_family_1(asym, false, found_condpairs_family1,   condsophypothesis_f1  ).
top(10) :-      % for test purposes
    generate_constraints_family_1(asym, true,  found_condpairs_family1,   condsophypothesis_f1  ).
top(2) :-       % for test purposes
    generate_constraints_family_2(asym, false, found_condpairs_family2_new,   condsophypothesis_f2  ).
top(20) :-      % for test purposes
    generate_constraints_family_2(asym, true,  found_condpairs_family2_new,   condsophypothesis_f2  ).
top(11) :-      % for production
    generate_constraints_family_1(sym,  false, found_condpairs_family1_sym,   condsophypothesis_f1   ),
    generate_constraints_family_1(sym,  true,  found_condpairs_family1_symf3, condsophypothesis_f1all).
top(22) :-      % for production
    generate_constraints_family_2(sym,  false, found_condpairs_family2_sym,   condsophypothesis_f2   ),
    generate_constraints_family_2(sym,  true,  found_condpairs_family2_symf3, condsophypothesis_f2all).
top(3) :-      % for production
    generate_constraints_family_3(_, 'found_condpairs_family3_new.pl').

% In practice, it's better to use multiple thread - run these predicates in parallel and then combine them together in one file 'found_condpairs_family3_new.pl' 
top(31) :- generate_constraints_family_3(and,      'found_condpairs_family3_and_0.pl').
top(33) :- generate_constraints_family_3(or,       'found_condpairs_family3_or_0.pl').
%top(34) :- generate_constraints_family_3(or,       'found_condpairs_family3_or_tests.pl').
top(35) :- generate_constraints_family_3(allequal, 'found_condpairs_family3_allequal_0.pl').
top(37) :- generate_constraints_family_3(xor,      'found_condpairs_family3_xor_0.pl').

% The predicate creates the list of all the rules that belong to Family 1, selects most general ones and writes them in the specified text file
generate_constraints_family_1(Type, RecordAll, PairsFileName, PredicateName) :-
    statistics(total_runtime,[Start|_]),
    findall([Options1,Options2,Hypothesis,Check1],
            % create two conditions. For each of them we register their type, names of the attributes they use and names of the constants they use
            (select_condition(1, CondName1, AttrsNames1, CoefNamesList1), Options1 = [CondName1, AttrsNames1, CoefNamesList1],
             memberchk(AttrsNames1, [[1],[1,2],[2,1]]),
             select_condition(2, CondName2, AttrsNames2, CoefNamesList2), Options2 = [CondName2, AttrsNames2, CoefNamesList2],
             memberchk(AttrsNames2, [[1],[1,2],[2,1]]),
             % two additional symmetry breaking checks to make sure that we don't create duplicate pairs.
             nonmember([CondName1,CondName2],[[attr_in_interval,attr_not_in_interval],[attr_not_in_interval,attr_in_interval]]),
             (Type = asym -> [CondName1, AttrsNames1] @> [CondName2, AttrsNames2]
             ;
              Type =  sym -> [CondName1, AttrsNames1] \= [CondName2, AttrsNames2]
             ),
             % For the selected pair of conditions we select a hypothesis [HypothesisList, HNegationFlag] that will place up to two
             % constraints on the coefficients used in  
             select_hypothesis(CoefNamesList1, CoefNamesList2, HypothesisList, HNegationFlag), 
             % Test to see if condition 1 and condition 2 intersect while the hypothesis is inposed
             test_cond1_oplus_cond2_underhypothesis(and, 0, Options1, Options2, HypothesisList, HNegationFlag),
             % If the test is passed, save the hypothesis [HypothesisList, HNegationFlag] into a predicate
             Hypothesis = hypothesis(HNegationFlag,HypothesisList),
             % Two tests to see if either condition 1 implies condition 2 or vice versa
             (test_cond1_oplus_cond2_underhypothesis(implies1, 1, Options1, Options2, HypothesisList, HNegationFlag) ->
                 Check1 is 1
             ;
                 Check1 is 0
             ),
             (test_cond1_oplus_cond2_underhypothesis(implies2, 1, Options1, Options2, HypothesisList, HNegationFlag) ->
                 Check2 is 1
             ;
                 Check2 is 0
             ),
             CheckSum is Check1 + Check2,
             (CheckSum = 0      -> false        ; % in case there's an equivalency between them within the hypothesis, so it belongs to Family 3 and of no interest to us here
              CheckSum = 1      -> true         ;
                                   false        ),% no equivalency, no implication
             true),
            ListOfAllOptions),
    write(step1),nl,
    sort(ListOfAllOptions,ResList),
    
    statistics(total_runtime,[Stop|_]),
    Runtime is Stop - Start,
    write(Runtime), write('ms'),nl,
    atom_concat(PairsFileName, '.pl', PairsFileNameW),
    open(PairsFileNameW, write, SOut),
    format(SOut,  ':- module(', []), format(SOut, PairsFileName, []), format(SOut,',~n',        []),
    format(SOut,  '          [',[]), format(SOut, PredicateName, []), format(SOut, '/5]).~n~n', []),
    (RecordAll = true -> filter_only_most_general_hypothesis_family_one(ResList, [], SOut, PredicateName);      % Select most general hypothesises
                         write_all_hypothesis_family_one(               ResList,     SOut, PredicateName)),     % Record all the rules
    close(SOut),
    
    statistics(total_runtime,[Stop2|_]),
    Runtime2 is Stop2 - Stop,
    write(Runtime2), write('ms'), nl.

generate_constraints_family_2(Type, RecordAll, PairsFileName, PredicateName):-
    statistics(total_runtime,[Start|_]),
    op_plus_list(OpList),
    findall([Options1,Options2,Oplus,Check2,Hypothesis],
             % create two conditions. For each of them we register their type, names of the attributes they use and names of the constants they use
            (select_condition(1, CondName1, AttrsNames1, CoefNamesList1), Options1 = [CondName1, AttrsNames1, CoefNamesList1],
             memberchk(AttrsNames1, [[1],[1,2],[2,1]]),
             select_condition(2, CondName2, AttrsNames2, CoefNamesList2), Options2 = [CondName2, AttrsNames2, CoefNamesList2],
             memberchk(AttrsNames2, [[1],[1,2],[2,1]]),
             % two additional symmetry breaking checks to make sure that we don't create duplicate pairs.
             nonmember([CondName1,CondName2],[[attr_in_interval,attr_not_in_interval],[attr_not_in_interval,attr_in_interval]]),
             (Type = asym -> [CondName1, AttrsNames1] @> [CondName2, AttrsNames2]
             ;
              Type =  sym -> [CondName1, AttrsNames1] \= [CondName2, AttrsNames2]
             ),
             % For the selected pair of conditions we select an aggregator between conditions and
             % a hypothesis [HypothesisList, HNegationFlag] that will place up to two constraints on the coefficients used in
             member(Oplus,OpList),
             select_hypothesis(CoefNamesList1, CoefNamesList2, HypothesisList, HNegationFlag),
             % Save the hypothesis [HypothesisList, HNegationFlag] into a predicate 
             Hypothesis = hypothesis(HNegationFlag,HypothesisList),
             % Two tests to see if either these conditions under the hypothesis will be always True or always False for the selected aggregator
             (test_cond1_oplus_cond2_underhypothesis(Oplus, 0, Options1, Options2, HypothesisList, HNegationFlag) ->
                 Check1 is 1
             ;
                 Check1 is 0
             ),
             (test_cond1_oplus_cond2_underhypothesis(Oplus, 1, Options1, Options2, HypothesisList, HNegationFlag) ->
                 Check2 is 1
             ;
                 Check2 is 0
             ),
             CheckSum is Check1 + Check2,
             (CheckSum = 0      -> false        ; % should not be possible; a result of two incompatible hypothesis in HList that somehow conflict with one or both conditions
              CheckSum = 1      -> true         ;
                                   false        ),% at least one True and one False for this pair of conditions
             true),
            ListOfAllOptions),
    write(step1),nl,
    sort(ListOfAllOptions,ResList),
    
    statistics(total_runtime,[Stop|_]),
    Runtime is Stop - Start,
    write(Runtime), write('ms'), nl,

    atom_concat(PairsFileName, '.pl', PairsFileNameW),
    open(PairsFileNameW, write, SOut),
    format(SOut,  ':- module(', []), format(SOut, PairsFileName, []), format(SOut,',~n',        []),
    format(SOut,  '          [',[]), format(SOut, PredicateName, []), format(SOut, '/5]).~n~n', []),
    (RecordAll = true -> filter_only_most_general_hypothesis_family_two(ResList, [], SOut, PredicateName)       ; % Select most general hypothesises
                         write_all_hypothesis_family_two(               ResList,     SOut, PredicateName)       ),% Record all the rules
    close(SOut),
    
    statistics(total_runtime,[Stop2|_]),
    Runtime2 is Stop2 - Stop,
    write(Runtime2), write('ms'), nl.


generate_constraints_family_3(Oplus1,PairsFileName) :- % remove header for later
    statistics(total_runtime,[Start|_]),
    findall([Options1,Options2,Oplus1,Options3,Options4,Oplus2,Hypothesis1,Hypothesis2,LinkHypothesisList],
             % create two conditions. For each of them we register their type, names of the attributes they use and names of the constants they use
            ((Oplus1 = and -> member(Num1, [1,2]) ; Num1 is 2), Num2 is 1,
             write(nums(Num1,Num2)),nl,
             HNegationFlag1 = 1,
             select_conditions_and_hypothesis_set(0, Num1, [CondName1,      CondName2     ],
                                                           [AttrsNames1,    AttrsNames2   ],
                                                           [CoefNamesList1, CoefNamesList2],
                                                           Oplus1, HypothesisList1, HNegationFlag1),
             Options1 = [CondName1, AttrsNames1, CoefNamesList1],
             Options2 = [CondName2, AttrsNames2, CoefNamesList2],
             Hypothesis1 = hypothesis(HNegationFlag1,HypothesisList1),
             % after a conditions-hypothesis set is selected test that it has at least one True within the domain of the variables
             test_cond1_oplus_cond2_underhypothesis(Oplus1, 0, Options1, Options2, HypothesisList1, HNegationFlag1),
             length(AttrsNames1,N1), length(AttrsNames2,N2), N_Set1 is max(N1,N2),
             find_equiv_condset_and_links(Options1, Options2, Options3, Options4,       % Find a pair of conditions, an operator, hypothesis,       
                                          Hypothesis1, Hypothesis2, Oplus1, Oplus2,     % and a set of link hypothesises, that ensures equivalency
                                          N_Set1, Num1, Num2, LinkHypothesisList),      % with first set of conditions
             true),
            ListOfAllOptions),
    length(ListOfAllOptions, Lngth),
    
    write(step1(Lngth)),nl,
    statistics(total_runtime,[Stop|_]),
    Runtime is Stop - Start,
    write(Runtime), write('ms'), nl,
    sort(ListOfAllOptions,ResList),

    open(PairsFileName, write, SOut),
    format(SOut,  ':- module(', []), format(SOut, PairsFileName,        []), format(SOut,',~n',        []),
    format(SOut,  '          [',[]), format(SOut, condsophypothesis_f3, []), format(SOut, '/5]).~n~n', []),
    % Select most general hypothesis for each pair of conditions and write them in the file
    filter_only_most_general_hypothesis_family_three(ResList,[],SOut),
    
    close(SOut),
    statistics(total_runtime,[Stop2|_]),
    Runtime2 is Stop2 - Stop,
    write(Runtime2), write('ms'), nl.

find_equiv_condset_and_links([CondName1, AttrsNames1, CoefNamesList1],
                             [CondName2, AttrsNames2, CoefNamesList2],
                             [CondName3, AttrsNames3, CoefNamesList3],
                             [CondName4, AttrsNames4, CoefNamesList4],
                             hypothesis(HNegationFlag1,HypothesisList1),
                             hypothesis(HNegationFlag2,HypothesisList2),
                             Oplus1, Oplus2, N_Set1, Num1, Num2, LinkHypothesisList):-
    Options1 = [CondName1, AttrsNames1, CoefNamesList1],
    Options2 = [CondName2, AttrsNames2, CoefNamesList2],
    % Calculate Cost for the first set of conditions
    condition_cost(CondName1, CondCost1),
    condition_cost(CondName2, CondCost2),
    (Oplus1 = and       -> SetCost1 is CondCost1 + CondCost2    ;
     Oplus1 = or        -> SetCost1 is CondCost1 + CondCost2 + 1;
     Oplus1 = xor       -> SetCost1 is CondCost1 + CondCost2 + 2;
                           SetCost1 is CondCost1 + CondCost2 + 3),

    % Select a second conditions-hypothesis set:
    HNegationFlag2 = 1,
    select_conditions_and_hypothesis_set(2, Num2, [CondName3,      CondName4     ],
                                                  [AttrsNames3,    AttrsNames4   ],
                                                  [CoefNamesList3, CoefNamesList4],
                                          Oplus2, HypothesisList2, HNegationFlag2),
    % Calculate Cost for the second set of conditions
    condition_cost(CondName3, CondCost3),
    condition_cost(CondName4, CondCost4),
    (Oplus2 = and             -> SetCost2 is CondCost3 + CondCost4      ;
     Oplus2 = or              -> SetCost2 is CondCost3 + CondCost4 + 1  ;
     Oplus2 = xor             -> SetCost2 is CondCost3 + CondCost4 + 2  ;
                                 SetCost2 is CondCost3 + CondCost4 + 3  ),
    length(AttrsNames3,N3), length(AttrsNames4,N4), N_Set2 is max(N3,N4),       % Find the total number of attributes
    [N_Set2, SetCost2, Num2] @< [N_Set1, SetCost1, Num1],                       % ensure that the second set is different and preferable to the first set
    [Oplus1, CondName1, AttrsNames1, CondName2, AttrsNames2] \= [Oplus2, CondName3, AttrsNames3, CondName4, AttrsNames4],
    Options3 = [CondName3, AttrsNames3, CoefNamesList3],
    Options4 = [CondName4, AttrsNames4, CoefNamesList4],
    % after a conditions-hypothesis set is selected test that it has at least one True within the domain of the variables
    test_cond1_oplus_cond2_underhypothesis(Oplus2, 0, Options3, Options4, HypothesisList2, HNegationFlag2),
    % Select link hypothesis:
    (filter_second_set_hypothesis(HypothesisList2) ->
     % if Hypothesis of the second set doesn't have concrete values on the coefficient (e.g. coef3 = 1)
     % then link hypothesises could be selected
        true
    ; %otherwise there's no need to select any links - there are already concrete values
        HypothesisList13 = [none],
        HypothesisList14 = [none],
        HypothesisList23 = [none],
        HypothesisList24 = [none]
    ),
    select_link_hypothesis(CoefNamesList1, CoefNamesList3, HypothesisList13, HNegationFlag13),
    select_link_hypothesis(CoefNamesList1, CoefNamesList4, HypothesisList14, HNegationFlag14),
    select_link_hypothesis(CoefNamesList2, CoefNamesList3, HypothesisList23, HNegationFlag23),
    select_link_hypothesis(CoefNamesList2, CoefNamesList4, HypothesisList24, HNegationFlag24),
    LinkHypothesisList = [hypothesis(HNegationFlag13, HypothesisList13),
                          hypothesis(HNegationFlag14, HypothesisList14),
                          hypothesis(HNegationFlag23, HypothesisList23),
                          hypothesis(HNegationFlag24, HypothesisList24)],
    filter_links(HypothesisList13,HypothesisList23),    % ensure that links do not bind same coefficient twice
    filter_links(HypothesisList14,HypothesisList24),    % (thus creating an additional rule between first pair)
    ((Num1 = 1, Num2 = 1) ->
       % if we simplify one condition to another condition we must check that this case isn't already handled
       % by Family 1 and Family 2 rules
       select_condition(1, CondName1, AttrsNames1, CoefNamesList1_test),
       select_condition(2, CondName3, AttrsNames3, CoefNamesList3_test),
       select_converted_link_hypothesis(HypothesisList13, HypothesisList13Test),
       \+ condsophypothesis_f1(_,[CondName1, AttrsNames1, CoefNamesList1_test],
                              [CondName3, AttrsNames3, CoefNamesList3_test],
                              and, hypothesis(HNegationFlag13, HypothesisList13Test)),
       \+ condsophypothesis_f2(_,[CondName1, AttrsNames1, CoefNamesList1_test],
                              [CondName3, AttrsNames3, CoefNamesList3_test],
                              and, hypothesis(HNegationFlag13, HypothesisList13Test))
    ;
       true),
    % Then the check is done to see that both conditions-hypothesis sets intersect and equivalent within the domain of involved variables
    % first test is conducted as a relatively quick check to see if there's a need to run a more expensive second test or not
    test_condsets(and, 0, N_Set1,
                  Oplus1, Options1, Options2, HypothesisList1, HNegationFlag1,
                  Oplus2, Options3, Options4, HypothesisList2, HNegationFlag2,
                  HNegationFlag13, HypothesisList13, HNegationFlag14, HypothesisList14,
                  HNegationFlag23, HypothesisList23, HNegationFlag24, HypothesisList24),
    (test_condsets(allequal, 1, N_Set1,
                  Oplus1, Options1, Options2, HypothesisList1, HNegationFlag1,
                  Oplus2, Options3, Options4, HypothesisList2, HNegationFlag2,
                  HNegationFlag13, HypothesisList13, HNegationFlag14, HypothesisList14,
                  HNegationFlag23, HypothesisList23, HNegationFlag24, HypothesisList24) -> false ;
                                                                                           true  ),
    !,
    write(left(cost(CondCost1,CondCost2,SetCost1),[Oplus1, Options1, Options2, hypothesis(HNegationFlag1,HypothesisList1)])),nl,
    write(rght(cost(CondCost3,CondCost4,SetCost2),[Oplus2, Options3, Options4, hypothesis(HNegationFlag2,HypothesisList2)])),nl,
    write(links(LinkHypothesisList)), nl, nl,
    true.

% Selects a set of two conditions and a hypothesis placed upon them
% Inputs:
%  * StartNum           - the starting position of selected conditions. StartNum = 0 is used for first conditions-hypothesis set
%                                                                       StartNum = 2 is used for second conditions-hypothesis set
%                         is used for generating names for cst/coef variables
%  * Num                - the number of selected conditions. Num = 1 means that only one condition is selected while the second one is set to 'none'.
% Outputs:
%  * AllCondList        - the list of names selected conditions
%  * AttrList           - the list of used attributes by selected conditions
%  * AllCoefNamesList   - the list of used coefficients by selected conditions
%  * Oplus              - the selected aggregator. Can be used as both input and output
%  * HypothesisList     - the list of selected hypothesises placed upon selected pair of conditions
%  * HNegationFlag      - the negation flag for HypothesisList
% example of a call:
% | ?-  findall([AllCondList, AttrList, AllCoefNamesList, HypothesisList, HNegationFlag],
%               select_conditions_and_hypothesis_set(0, 2, AllCondList, AttrList, AllCoefNamesList, OPlus, HypothesisList, HNegationFlag),
%               Res), write_list(Res).
select_conditions_and_hypothesis_set(StartNum, Num, AllCondList, AttrList, AllCoefNamesList, Oplus, HypothesisList, HNegationFlag):-
    (Num = 1 ->
        NewNum is StartNum + 1,
        Oplus = and,    % only AND
        select_condition(NewNum, CondName, AttrsNames, CoefNamesList),
        (StartNum = 0 ->
            memberchk(AttrsNames, [[1],[1,2],[2,1]])
         ;
            true
        ),
        AllCondList      = [CondName,           none],
        AttrList         = [AttrsNames,         []  ],
        AllCoefNamesList = [CoefNamesList,      []  ],
        select_hypothesis(CoefNamesList, [], HypothesisList, HNegationFlag)
    ;
     Num = 2 ->
        NewNum1 is StartNum + 1,
        NewNum2 is StartNum + 2,
        op_plus_list(OpList),
        (atom(Oplus) -> memberchk(Oplus, OpList) ; member(Oplus,OpList)),       % if Oplus isn't passed down then enumerate through all operators
        select_condition(NewNum1, CondName1, AttrsNames1, CoefNamesList1),
        select_condition(NewNum2, CondName2, AttrsNames2, CoefNamesList2),
        (StartNum = 0 ->                                                        % for the first pair of conditions:
            memberchk(AttrsNames1, [[1],[1,2],[2,1]]),                          % do not select AttrsNames1 = [2]
            memberchk(AttrsNames2, [[1],[1,2],[2,1]])                           % do not select AttrsNames2 = [2]
         ;
            true
        ),
        (AttrsNames1 = [2,1] -> AttrsNames2 \= [2,1] ; true),                   % it is already covered by [1,2]-[1,2] combination
        filter_non_compatible_families_of_conditions(CondName1,CondName2),      % similar to gen_bool_formula - some combinatons of conditions
        nonmember([ CondName1,                  CondName2               ],      % aren't considered at all, for performace reasons
                  [[attr_in_interval,           attr_not_in_interval    ],
                   [attr_not_in_interval,       attr_in_interval        ]]),
        [CondName1, AttrsNames1] @> [CondName2, AttrsNames2],                   % to break symmetry, no need to consider both symmetrical pairs
        AllCondList      = [CondName1,          CondName2       ],
        AttrList         = [AttrsNames1,        AttrsNames2     ],
        AllCoefNamesList = [CoefNamesList1,     CoefNamesList2  ],
        % As of now, for optimization purposes, only singular hypothesises are considered
        select_hypothesis(CoefNamesList1, CoefNamesList2, [Hypothesis], HNegationFlag),
        HypothesisList = [Hypothesis],
        % since stored rules only contain hypothesis on coefficients with numbers 1 and 2
        % and attribute lists [1], [1,2] and [2,1] only, it is required to re-adjust the numbers before
        % searching its presence in families 1 and 2
        (AttrsNames1 = [2] -> AttrsNames1Test = [1] ; AttrsNames1Test = AttrsNames1),
        (AttrsNames2 = [2] -> AttrsNames2Test = [1] ; AttrsNames2Test = AttrsNames2),
        (StartNum = 0 ->
            CoefNamesList1_test = CoefNamesList1,
            CoefNamesList2_test = CoefNamesList2,
            HypothesisListTest  = HypothesisList
        ;
            select_condition(1, CondName1, AttrsNames1Test, CoefNamesList1_test),
            select_condition(2, CondName2, AttrsNames2Test, CoefNamesList2_test),
            select_converted_pair_hypothesis(HypothesisList, HypothesisListTest)
        ),
        % if the rule already present in Families 1 and 2, no need to check them
        \+condsophypothesis_f1all(_,[CondName1, AttrsNames1Test, CoefNamesList1_test],  % First make a check if the hypothesis already present in
                                 [CondName2, AttrsNames2Test, CoefNamesList2_test],     % families 1 and 2.
                               Oplus, hypothesis(HNegationFlag, HypothesisListTest)),
        \+condsophypothesis_f2all(_,[CondName1, AttrsNames1Test, CoefNamesList1_test],
                                 [CondName2, AttrsNames2Test, CoefNamesList2_test],
                               Oplus, hypothesis(HNegationFlag, HypothesisListTest)),
        findall(hypothesis(HNegationFlag, HypothesisRule),                                              % if the hypothesis isn't present in families 1 and 2
                ((condsophypothesis_f1(RuleType,[CondName1, AttrsNames1Test, CoefNamesList1_test],      % check if it intersects with any of the generalized
                                            [CondName2, AttrsNames2Test, CoefNamesList2_test],          % rules of families 1 and 2
                                            Oplus, hypothesis(HNegationFlag, HypothesisRule))
                 ;
                  condsophypothesis_f2(RuleType,[CondName1, AttrsNames1Test, CoefNamesList1_test],
                                            [CondName2, AttrsNames2Test, CoefNamesList2_test],
                                            Oplus, hypothesis(HNegationFlag, HypothesisRule))),
                 (RuleType = always_false -> OPNegFlag is 1 ; OPNegFlag is 0),
                 (Hypothesis \= HypothesisListTest ->
                        compare_two_hypothesis([[CondName1, AttrsNames1Test, CoefNamesList1_test],
                                                 [CondName2, AttrsNames2Test, CoefNamesList2_test],
                                                 hypothesis(HNegationFlag, HypothesisListTest)],
                                                [[CondName1, AttrsNames1Test, CoefNamesList1_test],
                                                [CondName2, AttrsNames2Test, CoefNamesList2_test],
                                                hypothesis(HNegationFlag, HypothesisRule)],
                                                Oplus, OPNegFlag, Flag)
                 ;
                        Flag = hyps_equiv),
                 Flag \= no_intersection), ListOfIntersectingRules),
        ListOfIntersectingRules = []    % if no intersecting rules found then it can be used for further checks
    ).

% Used to check if the second set of a pair conditions and a hypothesis is covered by family 1 and family 2 rules
select_converted_pair_hypothesis([], []) :- !.
select_converted_pair_hypothesis([Hypothesis|R], [HypothesisTest|S]) :-
    functor(Hypothesis,     HypothesisName, N),
    functor(HypothesisTest, HypothesisName, N),
    (N = 0 -> true
    ;
     N = 1 ->
        arg(1, Hypothesis,     Coef),
        convert_coef(Coef, CoefConverted),
        arg(1, HypothesisTest, CoefConverted)
    ;
     N = 2 ->
        arg(1, Hypothesis,     Coef1),          arg(2, Hypothesis,     Coef2),
        convert_coef(Coef1, CoefConverted1),    convert_coef(Coef2, CoefConverted2),
        arg(1, HypothesisTest, CoefConverted1), arg(2, HypothesisTest, CoefConverted2)
    ;
        false
    ),
    select_converted_pair_hypothesis(R, S).

% conversion of coefficient names from second pair to the first pair
convert_coef(Coef, CoefConverted) :-
    (append_num(_,    1,[Coef]) -> CoefConverted = Coef                 ;
     append_num(_,    2,[Coef]) -> CoefConverted = Coef                 ;
     append_num(Coef0,3,[Coef]) -> append_num(Coef0,1,[CoefConverted])  ;
     append_num(Coef0,4,[Coef]) -> append_num(Coef0,2,[CoefConverted])  ).

% conversion of link hypothesis in the situation when one condition if simplified to one simpler condition
select_converted_link_hypothesis([], []) :- !.
select_converted_link_hypothesis([Hypothesis|R], [HypothesisTest|S]) :-
    functor(Hypothesis,     HypothesisName, N),
    functor(HypothesisTest, HypothesisName, N),
    (N = 0 ->
        true
    ;
     N = 2 ->
        arg(1, Hypothesis,     Coef1),                  arg(2, Hypothesis,     Coef2),
        convert_coef_link(Coef1, CoefConverted1),       convert_coef_link(Coef2, CoefConverted2),
        arg(1, HypothesisTest, CoefConverted1),         arg(2, HypothesisTest, CoefConverted2)
    ;
        false
    ),
    select_converted_link_hypothesis(R, S).

convert_coef_link(Coef, CoefConverted) :-
    (append_num(_,    1,[Coef]) -> CoefConverted = Coef                 ;
     append_num(Coef0,3,[Coef]) -> append_num(Coef0,2,[CoefConverted])  ).

select_link_hypothesis(CoefNamesList1, CoefNamesList2, HypothesisList, HNegationFlag) :-
    select_hypothesis(CoefNamesList1, CoefNamesList2, HypothesisList, HNegationFlag),
    filter_non_link_hypothesis(HypothesisList).

% check if hypothesis of the second conditions-hypothesis set contains any hypothesis that states coefficient values explicitly 
filter_second_set_hypothesis([]) :- !.
filter_second_set_hypothesis([Hypothesis|R]) :-
    functor(Hypothesis,_HypothesisName,Arity),
    Arity =\= 1,                % TODO: think of the clever way to allow to create [x1=0] and [x2=0] (the only exception)
                                % it is irrelevant as of now - it would be the case of compound hypothesis that isn't currently generated
    filter_second_set_hypothesis(R).

filter_non_link_hypothesis([Hypothesis]) :-     % for optimisation purposes, for now, it's limited by only non-compound hypothesis.
                                                % later it can be easily modified to check on coumpound hypothesis, if there's a need for it
    functor(Hypothesis,HypothesisName,Arity),
    Arity =\= 1,       % do not generate hypothesis with values on the coefficients. The reason is quite simple -
                       % while they have their uses when we simplify a single Cond to a single Cond (e.g. [x div y =< 1] <=> [x =< y]),
                       % but with simplifying two Conds into one or two Conds it starts to become unmanagable to discern
                       % if it's done because of the specific domain we chose or there's something else:
                       % for any x >= 1, y >= 1: [x + y = 2] <=> [x = 1] and [y = 1].
                       % As of now, even if I add hypothesis of types coef_eq_mindomain_x and coef_eq_double_mindomain_x
                       % I have no real tools to automatically decide are coefficients [1,1,2] are derived from variable domains
                       % or they are domain independent.
                       % Now, there are, of course, cases where compound hypothesis with specific coefficient values can be necessary:
                       % [[coef1 = 0] /\ [coef2 = cst3]]: [[x mod y = coef1] /\ [x div y = coef2]] <=> [x = cst3 * y]
                       % but for now their search is prohibited, both for the aforementioned reason and because of the optimisation
    memberchk(HypothesisName, [none,                  % for optimisation purposes set of link hypothesises is limited
                               c1_eq_c2,
                               c1_eq_c2_plus_1,
                               c1_plus_1_eq_c2%,
                               %c1_eq_c2_plus_2,
                               %c1_plus_2_eq_c2
                               ])/*,
    nonmember(HypothesisName, [c1x2_eq_c2,            % for optimisation purposes
                               c1x2_leq_c2,
                               c1x2_geq_c2,
                               c1_eq_c2x2,
                               c1_leq_c2x2,
                               c1_geq_c2x2,
                               c1_ineq_c2,
                               c1_ineq_c2_plus_1,
                               c1_plus_1_ineq_c2,
                               c1_ineq_c2_plus_2,
                               c1_plus_2_ineq_c2])*/.

% check that two link hypothesis do not use the same coefficient in the second pair
% the reason is to prevent situations like:
%   Link13 = [c1_eq_c2(coef1,coef3)],
%   Link23 = [c1_eq_c2(coef2,coef3)]
% which would mean that coef1=coef2.
% Either the hypothesis c1_eq_c2(coef1,coef2) is stated explicitly in HypothesisList, thus one of the links is pointless
% or this hypothesis isn't stated explicitly and thus the stored rule is incomplete
filter_links([none], _) :- !.
filter_links(_, [none]) :- !.
filter_links([HLink1], [HLink2]) :-
    arg(2,HLink1, CoefName1),
    arg(2,HLink2, CoefName2),
    CoefName2 \= CoefName1.

% same behaviour to gen_bool_formula.pl
filter_non_compatible_families_of_conditions(Cnd1, Cnd2) :-
    select_cnd_family(Cnd1, Family1),
    select_cnd_family(Cnd2, Family2),
    (Family1 = 0 -> true                ;
     Family2 = 0 -> true                ;
                    Family1 = Family2   ).

% Find the family of the condition
select_cnd_family(Cnd, Family) :-
    memberchk([Cnd,                               Family        ],
             [[unary_term_eq_coef(mod),           1             ],
              [unary_term_leq_coef(mod),          1             ],
              [unary_term_geq_coef(mod),          1             ],
              [binary_term_eq_coef(min),          2             ],
              [binary_term_leq_coef(min),         2             ],
              [binary_term_geq_coef(min),         2             ],
              [binary_term_eq_coef(max),          3             ],
              [binary_term_leq_coef(max),         3             ],
              [binary_term_geq_coef(max),         3             ],
              [binary_term_eq_coef(prod),         4             ],
              [binary_term_leq_coef(prod),        4             ],
              [binary_term_geq_coef(prod),        4             ],
              [binary_term_eq_coef(abs),          5             ],
              [binary_term_leq_coef(abs),         5             ],
              [binary_term_geq_coef(abs),         5             ],
              [binary_term_eq_coef(floor),        6             ],
              [binary_term_leq_coef(floor),       6             ],
              [binary_term_geq_coef(floor),       6             ],
              [binary_term_eq_coef(ceil),         7             ],
              [binary_term_leq_coef(ceil),        7             ],
              [binary_term_geq_coef(ceil),        7             ],
              [binary_term_eq_coef(mod),          8             ],
              [binary_term_leq_coef(mod),         8             ],
              [binary_term_geq_coef(mod),         8             ],
              [binary_term_eq_coef(mfloor),       9             ],
              [binary_term_leq_coef(mfloor),      9             ],
              [binary_term_geq_coef(mfloor),      9             ],
              [binary_term_eq_coef(cmod),         10            ],
              [binary_term_leq_coef(cmod),        10            ],
              [binary_term_geq_coef(cmod),        10            ],
              [binary_term_eq_coef(dmod),         11            ],
              [binary_term_leq_coef(dmod),        11            ],
              [binary_term_geq_coef(dmod),        11            ],
              [binary_term_eq_coef(fmod),         12            ],
              [binary_term_leq_coef(fmod),        12            ],
              [binary_term_geq_coef(fmod),        12            ]]),
    !.
select_cnd_family(_, 0).

% Select a condition from a list
% Inputs:
%   *Num - appends the number to the names of coefficients. e.g. if Num = 1, then [coef,cst] will be transfored to [coef1,cst1]
% Outputs:
%   *CondName      - type of the condition
%   *AttrnNames    - attributes used by the condition 
%   *CoefNamesList - names of the coefficients used by the condition
select_condition(Num, CondName, AttrsNames, CoefNamesList) :-
    (integer(Num) -> true ; Num is 0),
    member( [CondName,                          AttrsNames,       CoefNamesListDef],
           [[attr_eq_coef,                      [1],              [coef]          ],
            [attr_leq_coef,                     [1],              [coef]          ],
            [attr_geq_coef,                     [1],              [coef]          ],
            [attr_in_interval,                  [1],              [coef,cst]      ],
            [attr_not_in_interval,              [1],              [coef,cst]      ],
            [attr_eq_coef,                      [2],              [coef]          ],
            %[attr_leq_coef,                     [2],              [coef]          ],
            %[attr_geq_coef,                     [2],              [coef]          ],
            %[attr_in_interval,                  [2],              [coef,cst]      ],
            %[attr_not_in_interval,              [2],              [coef,cst]      ],
            [attr_eq_attr,                      [1,2],            []              ],
            [attr_leq_attr,                     [1,2],            []              ],
            [attr_leq_attr,                     [2,1],            []              ],
            [attr_eq_unary(prod),               [1,2],            [cst]           ],
            [attr_eq_unary(prod),               [2,1],            [cst]           ],
            [attr_leq_unary(prod),              [1,2],            [cst]           ],
            [attr_leq_unary(prod),              [2,1],            [cst]           ],
            [unary_leq_attr(prod),              [1,2],            [cst]           ],
            [unary_leq_attr(prod),              [2,1],            [cst]           ],
            [binary_term_eq_coef(plus),         [1,2],            [coef]          ],
            [binary_term_leq_coef(plus),        [1,2],            [coef]          ],
            [binary_term_geq_coef(plus),        [1,2],            [coef]          ],
            [binary_term_eq_coef(minus),        [1,2],            [coef]          ],
            [binary_term_eq_coef(minus),        [2,1],            [coef]          ],
            [binary_term_leq_coef(minus),       [1,2],            [coef]          ],
            [binary_term_leq_coef(minus),       [2,1],            [coef]          ],
            [binary_term_geq_coef(minus),       [1,2],            [coef]          ],
            [binary_term_geq_coef(minus),       [2,1],            [coef]          ],
            [binary_term_eq_coef(min),          [1,2],            [coef]          ],
            [binary_term_leq_coef(min),         [1,2],            [coef]          ],
            [binary_term_geq_coef(min),         [1,2],            [coef]          ],
            [binary_term_eq_coef(max),          [1,2],            [coef]          ],
            [binary_term_leq_coef(max),         [1,2],            [coef]          ],
            [binary_term_geq_coef(max),         [1,2],            [coef]          ],
            [binary_term_eq_coef(prod),         [1,2],            [coef]          ],
            [binary_term_leq_coef(prod),        [1,2],            [coef]          ],
            [binary_term_geq_coef(prod),        [1,2],            [coef]          ],
            [binary_term_eq_coef(abs),          [1,2],            [coef]          ],
            [binary_term_leq_coef(abs),         [1,2],            [coef]          ],
            [binary_term_geq_coef(abs),         [1,2],            [coef]          ],
            [binary_term_eq_coef(floor),        [1,2],            [coef]          ],
            [binary_term_eq_coef(floor),        [2,1],            [coef]          ],
            [binary_term_leq_coef(floor),       [1,2],            [coef]          ],
            [binary_term_leq_coef(floor),       [2,1],            [coef]          ],
            [binary_term_geq_coef(floor),       [1,2],            [coef]          ],
            [binary_term_geq_coef(floor),       [2,1],            [coef]          ],
            [binary_term_eq_coef(ceil),         [1,2],            [coef]          ],
            [binary_term_eq_coef(ceil),         [2,1],            [coef]          ],
            [binary_term_leq_coef(ceil),        [1,2],            [coef]          ],
            [binary_term_leq_coef(ceil),        [2,1],            [coef]          ],
            [binary_term_geq_coef(ceil),        [1,2],            [coef]          ],
            [binary_term_geq_coef(ceil),        [2,1],            [coef]          ],
            [binary_term_eq_coef(mfloor),       [1,2],            [coef]          ],
            [binary_term_eq_coef(mfloor),       [2,1],            [coef]          ],
            [binary_term_leq_coef(mfloor),      [1,2],            [coef]          ],
            [binary_term_leq_coef(mfloor),      [2,1],            [coef]          ],
            [binary_term_geq_coef(mfloor),      [1,2],            [coef]          ],
            [binary_term_geq_coef(mfloor),      [2,1],            [coef]          ],
            [unary_term_eq_coef(mod),           [1],              [coef,cst]      ],
            [unary_term_leq_coef(mod),          [1],              [coef,cst]      ],
            [unary_term_geq_coef(mod),          [1],              [coef,cst]      ],
            %[unary_term_eq_coef(mod),           [2],              [coef,cst]      ],
            %[unary_term_leq_coef(mod),          [2],              [coef,cst]      ],
            %[unary_term_geq_coef(mod),          [2],              [coef,cst]      ],
            [binary_term_eq_coef(mod),          [1,2],            [coef]          ],
            [binary_term_eq_coef(mod),          [2,1],            [coef]          ],
            [binary_term_leq_coef(mod),         [1,2],            [coef]          ],
            [binary_term_leq_coef(mod),         [2,1],            [coef]          ],
            [binary_term_geq_coef(mod),         [1,2],            [coef]          ],
            [binary_term_geq_coef(mod),         [2,1],            [coef]          ]/**//*,
            [binary_term_eq_coef(cmod),         [1,2],            [coef]          ],
            [binary_term_eq_coef(cmod),         [2,1],            [coef]          ],
            [binary_term_leq_coef(cmod),        [1,2],            [coef]          ],
            [binary_term_leq_coef(cmod),        [2,1],            [coef]          ],
            [binary_term_geq_coef(cmod),        [1,2],            [coef]          ],
            [binary_term_geq_coef(cmod),        [2,1],            [coef]          ],
            [binary_term_eq_coef(dmod),         [1,2],            [coef]          ],
            [binary_term_eq_coef(dmod),         [2,1],            [coef]          ],
            [binary_term_leq_coef(dmod),        [1,2],            [coef]          ],
            [binary_term_leq_coef(dmod),        [2,1],            [coef]          ],
            [binary_term_geq_coef(dmod),        [1,2],            [coef]          ],
            [binary_term_geq_coef(dmod),        [2,1],            [coef]          ],
            [binary_term_eq_coef(fmod),         [1,2],            [coef]          ],
            [binary_term_eq_coef(fmod),         [2,1],            [coef]          ],
            [binary_term_leq_coef(fmod),        [1,2],            [coef]          ],
            [binary_term_leq_coef(fmod),        [2,1],            [coef]          ],
            [binary_term_geq_coef(fmod),        [1,2],            [coef]          ],
            [binary_term_geq_coef(fmod),        [2,1],            [coef]          ]*/
           ]),
    append_num(CoefNamesListDef, Num, CoefNamesList).

% select a cost of a condition
condition_cost(CondName, CondCost) :-
    memberchk([CondName,                          CondCost        ],
             [[none,                              0               ],
              [attr_eq_coef,                      1               ],
              [attr_leq_coef,                     1               ],
              [attr_geq_coef,                     1               ],
              [attr_in_interval,                  0               ],
              [attr_not_in_interval,              1               ],
              [unary_term_eq_coef(mod),           6               ],
              [unary_term_leq_coef(mod),          7               ],
              [unary_term_geq_coef(mod),          7               ],
              [attr_eq_attr,                      0               ],
              [attr_leq_attr,                     0               ],
              [attr_eq_unary(prod),               2               ],
              [attr_leq_unary(prod),              3               ],
              [unary_leq_attr(prod),              3               ],
              [binary_term_eq_coef(plus),         2               ],
              [binary_term_leq_coef(plus),        3               ],
              [binary_term_geq_coef(plus),        3               ],
              [binary_term_eq_coef(minus),        2               ],
              [binary_term_leq_coef(minus),       3               ],
              [binary_term_geq_coef(minus),       3               ],
              [binary_term_eq_coef(min),          3               ],
              [binary_term_leq_coef(min),         4               ],
              [binary_term_geq_coef(min),         4               ],
              [binary_term_eq_coef(max),          3               ],
              [binary_term_leq_coef(max),         4               ],
              [binary_term_geq_coef(max),         4               ],
              [binary_term_eq_coef(prod),         4               ],
              [binary_term_leq_coef(prod),        5               ],
              [binary_term_geq_coef(prod),        6               ],
              [binary_term_eq_coef(abs),          3               ],
              [binary_term_leq_coef(abs),         4               ],
              [binary_term_geq_coef(abs),         4               ],
              [binary_term_eq_coef(floor),        4               ],
              [binary_term_leq_coef(floor),       5               ],
              [binary_term_geq_coef(floor),       5               ],
              [binary_term_eq_coef(ceil),         4               ],
              [binary_term_leq_coef(ceil),        5               ],
              [binary_term_geq_coef(ceil),        5               ],
              [binary_term_eq_coef(mod),          6               ],
              [binary_term_leq_coef(mod),         7               ],
              [binary_term_geq_coef(mod),         7               ],
              [binary_term_eq_coef(mfloor),       7               ],
              [binary_term_leq_coef(mfloor),      8               ],
              [binary_term_geq_coef(mfloor),      8               ],
              [binary_term_eq_coef(cmod),         7               ],
              [binary_term_leq_coef(cmod),        8               ],
              [binary_term_geq_coef(cmod),        8               ],
              [binary_term_eq_coef(dmod),         7               ],
              [binary_term_leq_coef(dmod),        8               ],
              [binary_term_geq_coef(dmod),        8               ],
              [binary_term_eq_coef(fmod),         7               ],
              [binary_term_leq_coef(fmod),        8               ],
              [binary_term_geq_coef(fmod),        8               ]
           ]).


% select_hypothesis(CList1,CList2,HList,NegationFlag),
% Given sets of coefficient names the predicate will creates the list of hypothesis for them
% Inputs:
%    - CList1, CList2: lists with coefficient names, e.g. CList1 = [coef1, cst1], CList2 = [cst2].
% Outputs:
%    - HList: lists of selected hypothesis'. This list contains maximum two hypothesis.
%    - Negation flag: if equal to 1 - no negation, hypothesis used as usual. Always 1 if only one hypothesis is generated
%                     if equal to 0 - only applied for lists of two selected hypothesis. Negates AND of both hypothesis
%Example of a call: | ?- findall(H, (select_hypothesis([cst1],[],H,_), write(H),nl),_).
select_hypothesis([],[], [HName], 1) :-
    generate_single_hypothesis(HName, none, none, _).

select_hypothesis([CName],[], [HName], 1) :-
    generate_single_hypothesis(HName, CName-_, none, _).

select_hypothesis([],[CName], [HName], 1) :-
    generate_single_hypothesis(HName, none, CName-_, _).

select_hypothesis([CName1,_CName2],[], [HName], 1) :-
    generate_single_hypothesis(HName, CName1-_, none, _).

select_hypothesis([_CName1,CName2],[], [HName], 1) :-
    generate_single_hypothesis(HName, CName2-_, none, _).

select_hypothesis([],[CName1,_CName2], [HName], 1) :-
    generate_single_hypothesis(HName, none, CName1-_, _).

select_hypothesis([],[_CName1,CName2], [HName], 1) :-
    generate_single_hypothesis(HName, none, CName2-_, _).

select_hypothesis([CName1],[CName2], [HName], 1) :-
    generate_single_hypothesis(HName, CName1-_, CName2-_, _).

select_hypothesis([CName1],[CName2,_CName3], [HName], 1) :-
    generate_single_hypothesis(HName, CName1-_, CName2-_, _).

select_hypothesis([CName1],[_CName2,CName3], [HName], 1) :-
    generate_single_hypothesis(HName, CName1-_, CName3-_, _), HName \= c10_none.
 
select_hypothesis([CName1],[CName2,CName3], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName1-_, CName2-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName1-_, CName3-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).

select_hypothesis([CName1,_CName2],[CName3], [HName], 1) :-
    generate_single_hypothesis(HName, CName1-_, CName3-_, _).

select_hypothesis([_CName1,CName2],[CName3], [HName], 1) :-
    generate_single_hypothesis(HName, CName2-_, CName3-_, _), HName \= c10_none.

select_hypothesis([CName1,CName2],[CName3], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName1-_, CName3-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName2-_, CName3-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).

select_hypothesis([CName1,_CName2],[CName3,_CName4],  [HName], 1) :-
    generate_single_hypothesis(HName, CName1-_, CName3-_, _).

select_hypothesis([CName1,_CName2],[_CName3,CName4],  [HName], 1) :-
    generate_single_hypothesis(HName, CName1-_, CName4-_, _), HName \= c10_none.

select_hypothesis([_CName1,CName2],[CName3,_CName4],  [HName], 1) :-
    generate_single_hypothesis(HName, CName2-_, CName3-_, _), HName \= c10_none.

select_hypothesis([_CName1,CName2],[_CName3,CName4],  [HName], 1) :-
    generate_single_hypothesis(HName, CName2-_, CName4-_, _), HName \= c10_none.

select_hypothesis([CName1,CName2],[CName3,_CName4], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName1-_, CName3-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName2-_, CName3-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).

select_hypothesis([CName1,CName2],[_CName3,CName4], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName1-_, CName4-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName2-_, CName4-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).

select_hypothesis([CName1,_CName2],[CName3,CName4], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName1-_, CName3-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName1-_, CName4-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).

select_hypothesis([_CName1,CName2],[CName3,CName4], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName2-_, CName3-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName2-_, CName4-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).

select_hypothesis([CName1,CName2],[CName3,CName4], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName1-_, CName3-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName2-_, CName4-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).

select_hypothesis([CName1,CName2],[CName3,CName4], HList, NegFlag) :-
    member(NegFlag, [0,1]),
    generate_single_hypothesis(HName1, CName2-_, CName3-_, _), HName1 \= c10_none,
    generate_single_hypothesis(HName2, CName1-_, CName4-_, _), HName2 \= c10_none,
    (HName2 @> HName1 -> HList = [HName1,HName2] ; HList = [HName2,HName1]).


% Given two conditions Cond1 and Cond2, hypothesis list HypothesisList with HypothesisNegationFlag,
% the aggregator sign Oplus and the negation flag OPNegationFlag check if there's at least one solution for:
% (HypothesisList #= HypothesisNegationFlag): (Cond1 Oplus Cond2 #= (1 - OPNegationFlag))
%
% List of inputs:
% test_cond1_oplus_cond2_underhypothesis(Oplus, OPNegationFlag, Cond1, Cond2, HypothesisList, HypothesisNegationFlag) 
test_cond1_oplus_cond2_underhypothesis(Oplus, OPNegFlag, [Type1, Attrs1Names, CoefsNames1],
                                                         [Type2, Attrs2Names, CoefsNames2], HList, HNegFlag) :-
    length(Attrs1Names,N1), length(Attrs2Names,N2), N is max(N1,N2), 
    generate_attrs(N, Attrs),                                                                   % Generate attribute variables
    generate_condition_terms(2, [ConstraintTerm1,ConstraintTerm2]),                             % Generate ConditionTerm variables
    generate_constraint(ConstraintTerm1, Attrs, [Type1, Attrs1Names, CoefsVars1]),              % Generate condition 1
    generate_constraint(ConstraintTerm2, Attrs, [Type2, Attrs2Names, CoefsVars2]),              % Generate condition 2
    generate_hypothesis(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,HNegFlag,HVars),    % Generate hypothesis
    create_op_constraint(ConstraintTerm1,ConstraintTerm2,Oplus,OPNegFlag),                      % create (Cond1 Oplus Cond2 #= (1 - OPNegationFlag) constraint
    append([Attrs, CoefsVars1, CoefsVars2, HVars, [ConstraintTerm1, ConstraintTerm2]], Vars),   % collect variables
    once(labeling([], Vars)), !,                                                                % Find at least one solution
    true.

% Similar to the test_cond1_oplus_cond2_underhypothesis, with the difference that
% the goal is to extract a new constraint term HypothesisTerm for this problem:
% HypothesisTerm #= ((HypothesisList #= HypothesisNegationFlag): (Cond1 Oplus Cond2 #= (1 - OPNegationFlag)))
generate_cond1_oplus_cond2_underhypothesis(Oplus, OPNegFlag, Attrs, CoefsVars1, CoefsVars2, [Type1, Attrs1, CoefsNames1], [Type2, Attrs2, CoefsNames2],
                                           HList, HNegFlag, HypothesisTerm, Vars) :-
    generate_condition_terms(2, [ConstraintTerm1,ConstraintTerm2]),                             % Generate ConditionTerm variables
    generate_constraint(ConstraintTerm1, Attrs, [Type1, Attrs1, CoefsVars1]),
    generate_constraint(ConstraintTerm2, Attrs, [Type2, Attrs2, CoefsVars2]),
    generate_hypothesis_term(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,HNegFlag,HVars,HypothesisTerm),
    create_op_constraint(ConstraintTerm1,ConstraintTerm2,Oplus,OPNegFlag),
    append([Attrs, CoefsVars1, CoefsVars2, HVars, [ConstraintTerm1, ConstraintTerm2, HypothesisTerm]], Vars).

% test that at least one solution within the domain of attributes is present for the constraint:
% (Hypothesis1: (Cond1 Oplus1 Cond2 #= (1 - OPNegationFlag1)) Oplus (Hypothesis2: (Cond3 Oplus2 Cond4 #= (1 - OPNegationFlag2))
% examples of a call:
% test_condsets(allequal,1,2,
%               and,[attr_eq_coef,[1],[coef1]],[attr_eq_attr,[1,2],[]],[none],1,
%               and,[attr_eq_coef,[1],[coef3]],[none,[],[]],[none],1,
%               1,[none],1,[none],
%               1,[none],1,[none])
% test_condsets(allequal, 1, 1,
%               or,  [attr_geq_coef,[1],[coef1]],[attr_eq_coef,[1],[coef2]], [c1_plus_1_eq_c2(coef1,coef2)], 1,
%               and, [attr_geq_coef,[1],[coef3]], [none, [], []], [none], 1,
%               1, [none], 1, [none],
%               1, [c1_eq_c2(coef2,coef3)], 1, [none]).
test_condsets(OPlus, NegationFlag, NbAttr,
              Oplus1, Options1, Options2, HypothesisList1, HNegationFlag1,
              Oplus2, Options3, Options4, HypothesisList2, HNegationFlag2,
              HNegationFlag13, HypothesisList13, HNegationFlag14, HypothesisList14,
              HNegationFlag23, HypothesisList23, HNegationFlag24, HypothesisList24) :-
    generate_attrs(NbAttr, Attrs),                              % generate attribute variables
    Options1 = [_CondName1, _AttrsNames1, CoefNamesList1],
    Options2 = [_CondName2, _AttrsNames2, CoefNamesList2],
    Options3 = [_CondName3, _AttrsNames3, CoefNamesList3],
    Options4 = [_CondName4, _AttrsNames4, CoefNamesList4],
    generate_family3_set_term(Oplus1, Attrs, Options1, Options2,        % generate first conditions-hypothesis set
                              HypothesisList1, HNegationFlag1, CoefVars1, CoefVars2, Vars1, SetVar1),
    generate_family3_set_term(Oplus2, Attrs, Options3, Options4,        % generate second conditions-hypothesis set
                              HypothesisList2, HNegationFlag2, CoefVars3, CoefVars4, Vars2, SetVar2),
    generate_hypothesis(CoefNamesList1,CoefVars1,CoefNamesList3,CoefVars3,HypothesisList13,HNegationFlag13,HVars13),
    generate_hypothesis(CoefNamesList1,CoefVars1,CoefNamesList4,CoefVars4,HypothesisList14,HNegationFlag14,HVars14),
    generate_hypothesis(CoefNamesList2,CoefVars2,CoefNamesList3,CoefVars3,HypothesisList23,HNegationFlag23,HVars23),
    generate_hypothesis(CoefNamesList2,CoefVars2,CoefNamesList4,CoefVars4,HypothesisList24,HNegationFlag24,HVars24),
    create_op_constraint(SetVar1,SetVar2,OPlus,NegationFlag),           % create the constraint (SetVar1 Oplus SetVar2 #= (1 - OPNegationFlag))
    append([Vars1,Vars2,HVars13,HVars14,HVars23,HVars24],Vars),         % collect variables
    once(labeling([ffc], Vars)).                                        % find at least one solution

% Create term SetVar for given values of Attrs and Coefs:
% SetVar #= (Hypothesis: (Cond1 Oplus Cond2 #= (1 - OPNegationFlag))
% example of a call: 
% generate_family3_set_term(and,[_14717,_14847],[attr_eq_coef,[1],[coef1]],
%                                               [attr_eq_attr,[1,2],[]],
%                           [none], 1, _13813,_13815,_13817,_13819)
generate_family3_set_term(Oplus, Attrs, [Type1, Attrs1Names, CoefsNames1],
                                        [Type2, Attrs2Names, CoefsNames2],
                                        HList, HNegFlag, CoefVars1, CoefVars2, [SetVar|Vars], SetVar) :-
    generate_condition_terms(2, [ConstraintTerm1,ConstraintTerm2]),                                     % Generate ConditionTerm variables
    generate_constraint(ConstraintTerm1, Attrs, [Type1, Attrs1Names, CoefVars1]),                       % Generate condition 1
    generate_constraint(ConstraintTerm2, Attrs, [Type2, Attrs2Names, CoefVars2]),                       % Generate condition 2
    generate_hypothesis(CoefsNames1, CoefVars1, CoefsNames2, CoefVars2, HList, HNegFlag, HVars),        % Generate hypothesis
    create_op_constraint_term(ConstraintTerm1, ConstraintTerm2, Oplus, SetVar),                         % create constraint
    append([Attrs, CoefVars1, CoefVars2, HVars, [ConstraintTerm1, ConstraintTerm2]],Vars).              % collect variables
    
% Given a list of hypothesis, for two conditions Cond1 and Cond2 find and select only the most general Hypothesis
% Inputs:
% * CondPairAndHypothesisList   : list of lists [Cond1, Cond2, Hypothesis] in which Cond1 and Cond2 are identical for every entry
% * Oplus                       : aggregator used between conditions
% * Negation                    : negation flag, it is needed because there must be at least one True value between conditions
% * CondListRest                : List of hypothesis that were previously tested but there were either no intersection betwen hypothesis OR
%                                                                                                      no equivalency and no implication were previously found
%                                 CondListRest by is initialised by default as [] and then gradually filled with hypothesis as the program goes
%                                 through CondPairAndHypothesisList
% Output:
% * CondListResult:             : List of all most general hypothesis. Every one of them would not be implied by another one within this list

% If CondListRest is empty when CondPairAndHypothesisList is completed, stop the search
general_test_two_hypothesis([CondPairAndHypothesis], _Oplus, _Negation, [CondPairAndHypothesis], []) :- !.
                                                                                                      
general_test_two_hypothesis([CondPairAndHypothesis], Oplus, Negation, [CondPairAndHypothesis|CondListResult], CondListRest) :-
    CondListRest = [_|_], !,
    % When the last element of CondPairAndHypothesisList is reached then, go through the list CondListRest to check if it can be pruned
    % it is done because the last hypothesis of CondPairAndHypothesisList will be the most general one and it can be implied by 
    prune_list_of_hypothesis(CondListRest, CondPairAndHypothesis, Oplus, Negation, CondListRest0),
    (CondListRest0 = [] ->       % if the CondListRest pruned completely, stop the search
        CondListResult = []
    ;   % otherwise the recursive call to select only most general hypothesis within CondListRest0
        general_test_two_hypothesis(CondListRest0, Oplus, Negation, CondListResult, [])
    ).

% If we have at least two hypothesis in CondPairAndHypothesisList
general_test_two_hypothesis([Hypothesis1, Hypothesis2 | R], Oplus, Negation, CondListResult, CondListRest) :-
    % Make a comparison between them and extract information flag Flag 
    compare_two_hypothesis(Hypothesis1, Hypothesis2, Oplus, Negation, Flag),
    (Flag = no_intersection ->                          % if there's no intersection between hypothesis,
        CondListNew     = [Hypothesis1 | R],            % keep the first one in CondPairAndHypothesisList and 
        CondListRestNew = [Hypothesis2 | CondListRest]  % put the second one in CondListResult
    ;
     Flag = no_equiv_no_implication ->                  % if there are no equivalence and no implication between hypothesis
        CondListNew     = [Hypothesis1 | R],            % keep the first one in CondPairAndHypothesisList and
        CondListRestNew = [Hypothesis2 | CondListRest]  % put the second one in CondListResult
    ;
     Flag = hyp2_imply_hyp1 ->                          % if hypothesis 2 implies hypothesis 1
        CondListNew     = [Hypothesis1 | R],            % keep hypothesis 1
        CondListRestNew = CondListRest                  % discard hypothesis 2
    ;
     Flag = hyp1_imply_hyp2 ->                          % if hypothesis 1 implies hypothesus 2
        CondListNew     = [Hypothesis2 | R],            % keep hypothesis 2
        CondListRestNew = CondListRest                  % discard hypothesis 1
    ;
     Flag = hyps_equiv ->                               % if hypothesis 1 is equivalent to hypothesis 2
        check_hypothesis_length(Hypothesis1, N1),       % and they have the similar length keep the hypothesis 1
        check_hypothesis_length(Hypothesis2, N2),       % otherwise keep the one with the shorter length
        (N2 < N1 ->                                     % for the explanation of hypothesis length see: check_hypothesis_length
            CondListNew     = [Hypothesis2 | R],
            CondListRestNew = CondListRest
        ;
            CondListNew     = [Hypothesis1 | R],
            CondListRestNew = CondListRest
        )
    ;
     Flag = error ->                                    % if there's an error flag exit the program
        write(general_test_two_hypothesis(Hypothesis1, Hypothesis2, Oplus, Negation)), nl, halt
    ),
    general_test_two_hypothesis(CondListNew, Oplus, Negation, CondListResult, CondListRestNew).

% Checks hypothesis length. Hypothesis length is the number of simple hypothesis that are included in the hypothesis.
% For example, [coef1 = coef2 AND cst1 >= cst2] has the length 2, because it consists of a simple hypothesis [coef1 = coef2]
% and a simple hypothesis [cst1 >= cst2[
check_hypothesis_length([_,_,hypothesis(_, HList)], N) :- length(HList,N).

% Go through the list CondListRest and removed all the hypothesis of this list that either imply MasterHypothesis or equivalent to MasterHypothesis 
prune_list_of_hypothesis([], _MasterHypothesis, _Oplus, _Negation, []) :- !.
prune_list_of_hypothesis([TestHypothesis|R], MasterHypothesis, Oplus, Negation, S) :-
    compare_two_hypothesis(TestHypothesis, MasterHypothesis, Oplus, Negation, Flag),
    memberchk(Flag, [hyp1_imply_hyp2, hyps_equiv]), !,
    prune_list_of_hypothesis(R, MasterHypothesis, Oplus, Negation, S).
prune_list_of_hypothesis([TestHypothesis|R], MasterHypothesis, Oplus, Negation, [TestHypothesis|S]) :-
    prune_list_of_hypothesis(R, MasterHypothesis, Oplus, Negation, S).

% Given Condition 1 and Condition 2 and two hypothesis Hypothesis1 and Hypothesis 2 imposed on these conditions
% find a relation between these two hypothesis and output the type of relation as a relation flag Flag
%compare_two_hypothesis([[attr_geq_coef,[1],[coef1]],[attr_eq_coef,[1],[coef2]],hypothesis(1,[c09_c1_leq_c2(coef1,coef2)])],[[attr_geq_coef,[1],[coef1]],[attr_eq_coef,[1],[coef2]],hypothesis(1,[c07_c1_plus_1_leq_c2(coef1,coef2)])],and,0,Flag).
compare_two_hypothesis([Options1, Options2, hypothesis(HNegFlag1, HList1)],
                       [Options1, Options2, hypothesis(HNegFlag2, HList2)], Oplus, Negation, Flag) :-
    (test_two_hypothesis(and, 0, Oplus, Negation, Options1, Options2, HList1, HNegFlag1, HList2, HNegFlag2) ->             % if two hypothesis intersect
        (test_two_hypothesis(allequal, 1, Oplus, Negation, Options1, Options2, HList1, HNegFlag1, HList2, HNegFlag2) ->    % check that they are not equivalent
           (test_two_hypothesis(implies1, 1, Oplus, Negation, Options1, Options2, HList1, HNegFlag1, HList2, HNegFlag2) -> % check if hypothesis2 doesn't imply hypothesis1
                (test_two_hypothesis(implies2, 1, Oplus, Negation, Options1, Options2, HList1, HNegFlag1, HList2, HNegFlag2) -> % check if hypothesis1 doesn't imply hypothesis2
                     Flag = no_equiv_no_implication     % in this case there are no equivalency or implication between hypothesis
                ; 
                     Flag = hyp2_imply_hyp1             % Hypothesis2 implies Hypothesis1
                )
           ; 
                (test_two_hypothesis(implies2, 1, Oplus, Negation, Options1, Options2, HList1, HNegFlag1, HList2, HNegFlag2) -> % check if hypothesis1 doesn't imply hypothesis2
                     Flag = hyp1_imply_hyp2             % Hypothesis1 implies Hypothesis2
                ; % If we reach this branch it means that two hypothesis are equivalent, but we already had this check
                     Flag = error
                )
           )
        ; 
           Flag = hyps_equiv                            % Hypothesis are equivalent to each other
        )
    ;
        Flag = no_intersection                          % two hypothesis don't intersect
    ).

% given two conditions Cond1 and Cond2, two hypothesis Hypothesis1 and Hypothesis2, the aggregator CondOplus for Cond1 and Cond2 with
% the negation flag CondOPNegFlag, the aggregator TestOplus for Hypothesis1 and Hypothesis 2 with the negation flag TestOPNegFlag
% test that:
% ((Hypothesis1: (Cond1 CondOplus Cond2) #= (1 - CondOPNegFlag)) TestOplus
%  (Hypothesis2: (Cond1 CondOplus Cond2 )#= (1 - CondOPNegFlag))) #= (1 - TestOPNegFlag)
% has at least one solution
%
% Examples of how to use it:
% test_two_hypothesis(and, 0,      and, 0, [unary_leq_attr(prod),[2,1],[cst1]], [binary_term_leq_coef(ceil),[2,1],[coef2]],  [c07_c1_plus_1_geq_c2(cst1,coef2)], 1, [c07_c1_plus_2_geq_c2(cst1,coef2)], 1).
% test_two_hypothesis(and, 0,      and, 0[unary_leq_attr(prod),[2,1],[cst1]], [binary_term_leq_coef(ceil),[2,1],[coef2]],  [c07_c1_geq_c2_plus_1(cst1,coef2)], 1, [c09_c1_leq_c2(cst1,coef2)], 1).
% test_two_hypothesis(implies1, 1, and, 0 [binary_term_leq_coef(prod),[1,2],[coef1]], [binary_term_geq_coef(minus),[2,1],[coef2]],  [c07_c1_plus_1_geq_c2(coef1,coef2)], 1, [c07_c1_plus_2_geq_c2(coef1,coef2)], 1).
test_two_hypothesis(TestOplus, TestOPNegFlag, CondOplus, CondOPNegFlag, [Type1, Attrs1Names, CoefsNames1], [Type2, Attrs2Names, CoefsNames2],
                    HList1, HNegFlag1, HList2, HNegFlag2) :-
    length(Attrs1Names,N1), length(Attrs2Names,N2), N is max(N1,N2), 
    generate_attrs(N, Attrs),     % Generate attribute variables
    generate_cond1_oplus_cond2_underhypothesis(CondOplus, CondOPNegFlag, Attrs, CoefsVars1, CoefsVars2, [Type1, Attrs1, CoefsNames1],
                                               [Type2, Attrs2, CoefsNames2], HList1, HNegFlag1, HTerm1, HVars1),
    generate_cond1_oplus_cond2_underhypothesis(CondOplus, CondOPNegFlag, Attrs, CoefsVars1, CoefsVars2, [Type1, Attrs1, CoefsNames1],
                                               [Type2, Attrs2, CoefsNames2], HList2, HNegFlag2, HTerm2, HVars2),
    create_op_constraint(HTerm1,HTerm2,TestOplus,TestOPNegFlag),
    append(HVars1, HVars2, Vars),
    once(labeling([ffc], Vars)).


% Create attribute variables
generate_attrs(NbAttr, Attrs) :-
    (NbAttr = 1 ->
        Attrs = [A],
        attribute1_min(AttributeMin),
        attribute1_max(AttributeMax),
        A in AttributeMin..AttributeMax
    ;
     NbAttr = 2 ->
        Attrs = [A1,A2],
        attribute1_min(Attribute1Min),
        attribute1_max(Attribute1Max),
        attribute2_min(Attribute2Min),
        attribute2_max(Attribute2Max),
        A1 in Attribute1Min..Attribute1Max,
        A2 in Attribute2Min..Attribute2Max
    ;
        false
    ).

% create Condition terms (variables indicating that conditions are True or False)
generate_condition_terms(NbTerms,TermsList) :-
    length(TermsList, NbTerms),
    domain(TermsList,0,1).


% Tests of the type:
% (Term1) Oplus (Term2) #= (1 - NegationFlag)
% Negation flag here acts identical to how its handled in gen_bool_formulae
create_op_constraint(Term1,Term2,Oplus,0) :-
    !,
    (Oplus = and        ->     (Term1 #/\ Term2)                ;
     Oplus = or         ->     (Term1 #\/ Term2)                ;
     Oplus = allequal   ->     (Term1 #=  Term2)                ;
     Oplus = xor        ->     (Term1 + Term2) mod 2 #= 1       ;
     Oplus = implies1   ->     (Term1 #=> Term2)                ;
     Oplus = implies2   ->     (Term2 #=> Term1)                ;
                               false                            ).
create_op_constraint(Term1,Term2,Oplus,1) :-
    (Oplus = and        ->   #\(Term1 #/\ Term2)                ;
     Oplus = or         ->   #\(Term1 #\/ Term2)                ;
     Oplus = allequal   ->   #\(Term1 #=  Term2)                ;
     Oplus = xor        ->     (Term1 + Term2) mod 2 #= 0       ;
     Oplus = implies1   ->   #\(Term1 #=> Term2)                ;
     Oplus = implies2   ->   #\(Term2 #=> Term1)                ;
                               false                            ).

% Same, but the output as a Boolean term Var
create_op_constraint_term(Term1, Term2, Oplus, Var) :-
    Var in 0..1,
    (Oplus = and        ->     Var #= (Term1 #/\ Term2)                 ;
     Oplus = or         ->     Var #= (Term1 #\/ Term2)                 ;
     Oplus = allequal   ->     Var #= (Term1 #=  Term2)                 ;
     Oplus = xor        ->     Var #= ((Term1 + Term2) mod 2 #= 1)      ;
     Oplus = implies1   ->     Var #= (Term1 #=> Term2)                 ;
     Oplus = implies2   ->     Var #= (Term2 #=> Term1)                 ;
                               false                                    ).

%adds Num to the end of each atom in the list
%append_num(List,Num,ResList).
append_num([],_,[]) :- !.
append_num([H|R],Num,[HNum|S]) :-
    to_atom(Num,NumAtom), atom_concat(H,NumAtom,HNum),
    append_num(R,Num,S).

% Convert to atoms
to_atom(X,X):-
        atom(X),
        !.
to_atom(X,Y):-
        number(X),
        number_codes(X,L),
        atom_codes(Y,L).

% Write list of all family 1 rules, without filtering
write_all_hypothesis_family_one([],_, _) :- !.
write_all_hypothesis_family_one([[Ctr1,Ctr2,Hypothesis,Implication]|R], SOut, PredicateName) :-
    !,
    write_list_to_file_family_one([[Ctr1,Ctr2,Hypothesis]], SOut, PredicateName,  and, Implication), %to write without filtering.
    write_list_to_file_family_one([[Ctr1,Ctr2,Hypothesis]], SOut, PredicateName,  or,  Implication),
    write_all_hypothesis_family_one(R, SOut, PredicateName).

% Write list of all family 2 rules, without filtering
write_all_hypothesis_family_two([],_,_) :- !.
write_all_hypothesis_family_two([[Ctr1,Ctr2,Oplus,Negation,Hypothesis]|R],SOut, PredicateName) :-
    !, 
    write_list_to_file_family_two([[Ctr1,Ctr2,Hypothesis]], SOut, PredicateName, Oplus, Negation),
    write_all_hypothesis_family_two(R, SOut, PredicateName).
    
% Filter list of family 1 rules to select most general hypothesis for each pair of conditions
filter_only_most_general_hypothesis_family_one([],_, _, _) :- !.
filter_only_most_general_hypothesis_family_one([[Ctr1,Ctr2,Hypothesis,Implication]],Cond1Cond2List, SOut, PredicateName) :-
    !, general_test_two_hypothesis([[Ctr1,Ctr2,Hypothesis]|Cond1Cond2List], and, 0, CondListResult, []),
    write_list_to_file_family_one(CondListResult, SOut, PredicateName,  and, Implication),
    write_list_to_file_family_one(CondListResult, SOut, PredicateName,  or,  Implication),
    true.
filter_only_most_general_hypothesis_family_one([[Ctr1,Ctr2,Hypothesis1,Implication],[Ctr1,Ctr2,Hypothesis2,Implication]|R],Cond1Cond2List, SOut, PredicateName) :-
    !,
    filter_only_most_general_hypothesis_family_one([[Ctr1,Ctr2,Hypothesis2,Implication]|R],[[Ctr1,Ctr2,Hypothesis1]|Cond1Cond2List], SOut, PredicateName).
filter_only_most_general_hypothesis_family_one([[Ctr1,Ctr2,Hypothesis,Implication]|R],Cond1Cond2List,SOut, PredicateName) :-
    general_test_two_hypothesis([[Ctr1,Ctr2,Hypothesis]|Cond1Cond2List], and, 0, CondListResult, []),
    write_list_to_file_family_one(CondListResult, SOut, PredicateName, and, Implication),
    write_list_to_file_family_one(CondListResult, SOut, PredicateName, or,  Implication),
    filter_only_most_general_hypothesis_family_one(R,[],SOut, PredicateName).

% Filter list of family 2 rules to select most general hypothesis for each pair of conditions
filter_only_most_general_hypothesis_family_two([],_,_,_) :- !.
filter_only_most_general_hypothesis_family_two([[Ctr1,Ctr2,Oplus,Negation,Hypothesis]],Cond1Cond2List,SOut, PredicateName) :-
    !, general_test_two_hypothesis([[Ctr1,Ctr2,Hypothesis]|Cond1Cond2List], Oplus, Negation, CondListResult, []),
    write_list_to_file_family_two(CondListResult, SOut, PredicateName, Oplus, Negation),
    true.
filter_only_most_general_hypothesis_family_two([[Ctr1,Ctr2,Oplus,Negation,Hypothesis1],[Ctr1,Ctr2,Oplus,Negation,Hypothesis2]|R],Cond1Cond2List, SOut, PredicateName) :-
    !,
    filter_only_most_general_hypothesis_family_two([[Ctr1,Ctr2,Oplus,Negation,Hypothesis2]|R],[[Ctr1,Ctr2,Hypothesis1]|Cond1Cond2List], SOut, PredicateName).
filter_only_most_general_hypothesis_family_two([[Ctr1,Ctr2,Oplus,Negation,Hypothesis]|R],Cond1Cond2List,SOut, PredicateName) :-
    general_test_two_hypothesis([[Ctr1,Ctr2,Hypothesis]|Cond1Cond2List], Oplus, Negation, CondListResult, []),
    write_list_to_file_family_two(CondListResult, SOut, PredicateName, Oplus, Negation),
    filter_only_most_general_hypothesis_family_two(R,[],SOut, PredicateName).

%filter_only_most_general_hypothesis_family_three(ResList,[],SOut),
% Filter list of family 3 rules to select most general hypothesis for each pair of conditions
filter_only_most_general_hypothesis_family_three([],_, _) :- !.
filter_only_most_general_hypothesis_family_three([[Ctr1,Ctr2,Oplus1,Ctr3,Ctr4,Oplus2,Hypothesis1,Hypothesis2,LinkHypothesisList]],Cond1Cond2List,SOut) :-
    !, general_test_two_hypothesis([[Ctr1,Ctr2,Hypothesis1]|Cond1Cond2List], Oplus1, 0, CondListResult, []),
    write_list_to_file_family_three(CondListResult, SOut, Oplus1, second_set(Ctr3,Ctr4,Oplus2,[Hypothesis2|LinkHypothesisList])).
filter_only_most_general_hypothesis_family_three([[Ctr1,Ctr2,Oplus,Ctr3,Ctr4,Oplus2,Hypothesis1,_Hypothesis2,_LinkHypothesisList1],
                                                  [Ctr1,Ctr2,Oplus,Ctr3,Ctr4,Oplus2,Hypothesis3, Hypothesis4, LinkHypothesisList2]|R],
                                                 Cond1Cond2List, SOut) :-
    !, 
    filter_only_most_general_hypothesis_family_three([[Ctr1,Ctr2,Oplus,Ctr3,Ctr4,Oplus2,Hypothesis3,Hypothesis4,LinkHypothesisList2]|R],
                                                     [[Ctr1,Ctr2,Hypothesis1]|Cond1Cond2List], SOut).
filter_only_most_general_hypothesis_family_three([[Ctr1,Ctr2,Oplus1,Ctr3,Ctr4,Oplus2,Hypothesis1,Hypothesis2,LinkHypothesisList]|R],
                                                 Cond1Cond2List,SOut) :-
    general_test_two_hypothesis([[Ctr1,Ctr2,Hypothesis1]|Cond1Cond2List], Oplus1, 0, CondListResult, []),
    write_list_to_file_family_three(CondListResult, SOut, Oplus1, second_set(Ctr3,Ctr4,Oplus2,[Hypothesis2|LinkHypothesisList])),
    filter_only_most_general_hypothesis_family_three(R,[],SOut).

% Writing list of family 1 rules to file
write_list_to_file_family_one([],_,_,_,_):- !.
write_list_to_file_family_one([[H1,H2,H3]|T], SOut, PredicateName, Oplus, Implication) :-
    (Implication = 0 -> ImplicationFlag = cond1_implies_cond2 ; ImplicationFlag = cond2_implies_cond1 ),
    format(SOut, PredicateName, []),
    format(SOut, '( ~w,  ~w,  ~w,  ~w,  ~w  ).~n', [ImplicationFlag, H1,H2,Oplus,H3]),
    write_list_to_file_family_one(T, SOut, PredicateName, Oplus, Implication).

% Writing list of family 2 rules to file
write_list_to_file_family_two([],_,_,_,_):- !.
write_list_to_file_family_two([[H1,H2,H3]|T], SOut, PredicateName, Oplus, Negation) :-
    (Negation = 0 -> NegationFlag = always_true ; NegationFlag = always_false),
    format(SOut, PredicateName, []),
    format(SOut, '( ~w,  ~w,  ~w,  ~w,  ~w  ).~n', [NegationFlag, H1,H2,Oplus,H3]),  
    write_list_to_file_family_two(T, SOut, PredicateName, Oplus, Negation).

% Writing list of family 3 rules to file
write_list_to_file_family_three([],_,_,_):- !.
write_list_to_file_family_three([[H1,H2,H3]|T], SOut, Oplus, SimplifiedSetAndLinks) :-
    format(SOut, 'condsophypothesis_f3( ~w,  ~w,  ~w,  ~w,  ~w, ~w ).~n', [simplification, H1, H2, Oplus, H3, SimplifiedSetAndLinks]),  
    write_list_to_file_family_three(T, SOut, Oplus, SimplifiedSetAndLinks).

% Basic writing list to file, for test purposes. Can be deprecated
write_list_to_file(_,[]):- !.
write_list_to_file(SOut, [H|T]) :-
    (length(H,3) ->
        format(SOut, 'condpairandhypothesis(  ~w,  ~w,  ~w  ).~n', H)
    ;
     length(H,4) ->
        format(SOut, 'condsophypothesis( ~w, ~w,  ~w,  ~w  ).~n', H)   
    ),
    write_list_to_file(SOut, T).

% for test purposes. Can be deprecated
generate_constraints_test :-
    statistics(total_runtime,[Start|_]),
    findall([Options1,Options2,Hypothesis],
            (select_condition(1, CondName1, AttrsNames1, CoefNamesList1), Options1 = [CondName1, AttrsNames1, CoefNamesList1],
             select_condition(2, CondName2, AttrsNames2, CoefNamesList2), Options2 = [CondName2, AttrsNames2, CoefNamesList2],
             nonmember([CondName1,CondName2],[[attr_in_interval,attr_not_in_interval],[attr_not_in_interval,attr_in_interval]]),
             [CondName1, AttrsNames1] @> [CondName2, AttrsNames2],
             %write(Options1-Options2),nl,
             select_hypothesis(CoefNamesList1, CoefNamesList2, HypothesisList, HNegationFlag), 
             test_cond1_oplus_cond2_underhypothesis(and, 0, Options1, Options2, HypothesisList, HNegationFlag),
             Hypothesis = hypothesis(HNegationFlag,HypothesisList),
             %write(Hypothesis),nl,
             true),
            ListOfAllOptions),
    write(step1),nl,
    %sort(ListOfAllOptions,ResList),  %Remove duplicates
    PairsFileName = 'found_condpairs_all.pl',
    open(PairsFileName, write, SOut),
    format(SOut, '%All Family 1 combinations (one Ctr implies another, given a hypothesis):~n', []),
    write_list_to_file(SOut, ListOfAllOptions), % was 924199
    %format(SOut, '~n ~n%Basic filtering (to ease reading):~n', []),
    %write_list_to_file(SOut, ResListFiltered),
    close(SOut),
    statistics(total_runtime,[Stop|_]),
    %statistics(total_runtime,[Stop2|_]),
    Runtime is Stop - Start,
    write(Runtime), write('ms'), nl.
