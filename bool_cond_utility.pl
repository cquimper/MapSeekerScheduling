% Purpose: A set of utility predicates required for creation of additional performance constraints for gen_bool_formulae.pl and gen_cond_formulae.pl
% Author : Nicolas Beldiceanu, Ramiz Gindullin, IMT Atlantique

:- module(bool_cond_utility,[tab_get_unaryf_values/8,
                             tab_get_unary_term_values/8,
                             tab_get_binary_term_values/8,
                             tab_get_attribute_values/7,
                             tab_get_ternary_values/7,
                             gen_compatible_unary_and_binary_and_ternary/3,
                             generate_oplus_lists_from_val_target_pairs/5,
                             split_value_target_pairs/4,
                             get_set_of_values_for_each_mandatory_attribute/9,
                             get_set_of_unaryf_values_for_each_pair_of_mandatory_attributes/8,
                             get_set_of_unary_term_values_for_each_mandatory_attribute/12,
                             get_set_of_ternary_triplets_of_mandatory_attributes/5,
                             get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes/10]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(table_access).
:- use_module(utility).

% Returns the set of possible coefficient values for a selected attribute column Col
tab_get_attribute_values([], _, _, _, _, _, []) :- !.
tab_get_attribute_values([col(Table, Col)|R], col(Table, OutputCol), Mode, ClusterOne, ClusterZero, CoefMax, ListOfAttrValues) :- 
    ((ClusterOne = [], ClusterZero = []) ->
         Mode = precalculated_entries(TableEntries, InputCols, TargetMin, TargetMax)
    ;
         TargetMin = 0,
         TargetMax = 1,
         Mode = precalculated_entries(TableEntries, InputCols, _TargetMin, _TargetMax)
    ),
    nth1(IPos, InputCols,  col(Table, Col)),
    TargetRange is TargetMax - TargetMin,
    findall(Val-Target, (member(TableEntry, TableEntries),
                         last(TableEntry, Target0),
                         nth1(IPos, TableEntry, Val),
                         ((ClusterOne = [], ClusterZero = []) ->
                              Target is Target0 - TargetMin   % (note that we get compute one pair for each row of the table if no clusters are given)
                         ;
                              (memberchk(Target0, ClusterOne ) -> Target is 1 ;
                               memberchk(Target0, ClusterZero) -> Target is 0 ;
                                                                  false       )
                         )), RowValues),
    sort(RowValues, ValueTargetList),                        % remove duplicated values

    % from the list of pairs ValueTargetList compute the different set of coefficient values for the different Boolean options
    % of the form:
    %   attr_eq([ Oplus,   Negation, Col, ListOfValues]) - for attr_eq_coef
    %   attr_neq([Oplus,   Negation, Col, ListOfValues]) - for attr_leq_coef and attr_geq_coef
    %............................................................................................................................
    (TargetRange = 1 ->
        generate_oplus_lists_from_val_target_pairs(ValueTargetList, TargetRange, 0, CoefMax,
                                                   [AndEqList,       AndNeqList,
                                                    OrEqList,        OrNeqList,
                                                    AllequalEqList,  AllequalNeqList,
                                                    AndEqNegList,    AndNeqNegList,
                                                    OrEqNegList,     OrNeqNegList]),
        ListOfAttrValues = [attr_eq([ and,      0, col(Table, Col), AndEqList]),
                            attr_neq([and,      0, col(Table, Col), AndNeqList]),
                            attr_eq([ or,       0, col(Table, Col), OrEqList]),
                            attr_neq([or,       0, col(Table, Col), OrNeqList]),
                            attr_eq([ allequal, 0, col(Table, Col), AllequalEqList]),
                            attr_neq([allequal, 0, col(Table, Col), AllequalNeqList]),
                            attr_eq([ and,      1, col(Table, Col), AndEqNegList]),
                            attr_neq([and,      1, col(Table, Col), AndNeqNegList]),
                            attr_eq([ or,       1, col(Table, Col), OrEqNegList]),
                            attr_neq([or,       1, col(Table, Col), OrNeqNegList]),
                            attr_eq([ allequal, 1, col(Table, Col), AllequalEqList]),
                            attr_neq([allequal, 1, col(Table, Col), AllequalNeqList])|S]
    ;
        generate_oplus_lists_from_val_target_pairs(ValueTargetList, TargetRange, 0, CoefMax,
                                                   [SumEqList,    SumNeqList,
                                                    SumEqNegList, SumNeqNegList,
                                                    CondEqList,   CondNeqList]),
        ListOfAttrValues = [attr_eq([ sum,       0, col(Table, Col), SumEqList]),
                            attr_neq([sum,       0, col(Table, Col), SumNeqList]),
                            attr_eq([ sum,       1, col(Table, Col), SumEqNegList]),
                            attr_neq([sum,       1, col(Table, Col), SumNeqNegList]),
                            attr_eq([ and,       0, col(Table, Col), CondEqList]),
                            attr_neq([and,       0, col(Table, Col), CondNeqList]),
                            attr_eq([ and,       1, col(Table, Col), CondEqList]),
                            attr_neq([and,       1, col(Table, Col), CondNeqList]),
                            attr_eq([ or,        0, col(Table, Col), CondEqList]),
                            attr_neq([or,        0, col(Table, Col), CondNeqList]),
                            attr_eq([ or,        1, col(Table, Col), CondEqList]),
                            attr_neq([or,        1, col(Table, Col), CondNeqList]),
                            attr_eq([ allequal,  0, col(Table, Col), CondEqList]),
                            attr_neq([allequal,  0, col(Table, Col), CondNeqList]),
                            attr_eq([ allequal,  1, col(Table, Col), CondEqList]),
                            attr_neq([allequal,  1, col(Table, Col), CondNeqList])|S]
    ),
    tab_get_attribute_values(R, col(Table, OutputCol), Mode, ClusterOne, ClusterZero, CoefMax, S).



% returns the set of possible coefficient values for the coefficient of a selected BinaryTerm (sum, minus, etc)
% for every pair of input columns wrt target column OutputCol
tab_get_binary_term_values([InputCol|RInputCol], OutputCol, Mode, ClusterOne, ClusterZero, BinaryTerm, CoefMax, Values) :- !,
    (memberchk(BinaryTerm, [plus,abs,min,max,prod]) ->                       % get all pairs of input columns for commutative binary terms
        findall(Col1-Col2, (member(Col1,[InputCol|RInputCol]), Col1 = col(_,I1),
                            member(Col2,[InputCol|RInputCol]), Col2 = col(_,I2),
                            I2 > I1), ColPairs)
    ;
     memberchk(BinaryTerm, [minus,floor,ceil,mfloor,mod,cmod,dmod,fmod]) ->  % get all pairs of input columns for non-commutative binary terms
        findall(Col1-Col2, (member(Col1,[InputCol|RInputCol]), Col1 = col(_,I1),
                            member(Col2,[InputCol|RInputCol]), Col2 = col(_,I2),
                            I2 =\= I1), ColPairs)
    ;
        write(tab_get_binary_term_values(BinaryTerm)), nl, halt
    ),
    ((ClusterOne = [], ClusterZero = []) ->
         Mode = precalculated_entries(_TableEntries, _InputCols, TargetMin, TargetMax)
    ;
         TargetMin = 0,
         TargetMax = 1
    ),
    tab_get_binary_term_values0(ColPairs, OutputCol, Mode, ClusterOne, ClusterZero, BinaryTerm, TargetMin-TargetMax, CoefMax, Values).
tab_get_binary_term_values([], _, _, _, _, _, _, _) :- write(tab_get_binary_term_values), nl, halt.

tab_get_binary_term_values0([], _, _, _, _, _, _, _, []) :- !.
tab_get_binary_term_values0([col(Table, Col1)-col(Table, Col2)|R], col(Table, OutputCol), Mode, ClusterOne, ClusterZero, BinaryTerm, TargetMin-TargetMax, CoefMax, ListOfCoefValues) :-
    Mode = precalculated_entries(TableEntries, InputCols, _TargetMin, _TargetMax) ->
    nth1(IPos1, InputCols,  col(Table,Col1)),
    nth1(IPos2, InputCols,  col(Table,Col2)),
    TargetRange is TargetMax - TargetMin,
    findall(Val-Target, (member(TableEntry, TableEntries),
                         last(TableEntry, Target0),
                         ((ClusterOne = [], ClusterZero = []) ->
                              Target is Target0 - TargetMin   % (note that we get compute one pair for each row of the table if no clusters are given)
                         ;
                              (memberchk(Target0, ClusterOne ) -> Target is 1 ;
                               memberchk(Target0, ClusterZero) -> Target is 0 ;
                                                                  false       )
                         ),
                         nth1(IPos1, TableEntry, Val1),
                         nth1(IPos2, TableEntry, Val2),
                         binary_term_calculation(BinaryTerm,Val1,Val2,col(Table, Col2), Val)), RowValues),
    sort(RowValues, ValueTargetList),                        % remove duplicated values

    % from the list of pairs ValueTargetList compute the different set of coefficient values for the different Boolean options
    % of the form:
    %   binary_eq([ BinaryTerm, Oplus,   Negation, Col1, Col2, ListOfValues]) - for binary_term_eq_coef
    %   binary_neq([BinaryTerm, Oplus,   Negation, Col1, Col2, ListOfValues]) - for binary_term_leq_coef and binary_term_geq_coef
    %............................................................................................................................
    (TargetRange = 1 ->
        generate_oplus_lists_from_val_target_pairs(ValueTargetList, TargetRange, 0, CoefMax,
                                                   [AndEqList,       AndNeqList,
                                                    OrEqList,        OrNeqList,
                                                    AllequalEqList,  AllequalNeqList,
                                                    AndEqNegList,    AndNeqNegList,
                                                    OrEqNegList,     OrNeqNegList]),
        ListOfCoefValues = [binary_eq([ BinaryTerm, and,      0, col(Table, Col1),col(Table, Col2), AndEqList]),
                            binary_neq([BinaryTerm, and,      0, col(Table, Col1),col(Table, Col2), AndNeqList]),
                            binary_eq([ BinaryTerm, or,       0, col(Table, Col1),col(Table, Col2), OrEqList]),
                            binary_neq([BinaryTerm, or,       0, col(Table, Col1),col(Table, Col2), OrNeqList]),
                            binary_eq([ BinaryTerm, allequal, 0, col(Table, Col1),col(Table, Col2), AllequalEqList]),
                            binary_neq([BinaryTerm, allequal, 0, col(Table, Col1),col(Table, Col2), AllequalNeqList]),
                            binary_eq([ BinaryTerm, and,      1, col(Table, Col1),col(Table, Col2), AndEqNegList]),
                            binary_neq([BinaryTerm, and,      1, col(Table, Col1),col(Table, Col2), AndNeqNegList]),
                            binary_eq([ BinaryTerm, or,       1, col(Table, Col1),col(Table, Col2), OrEqNegList]),
                            binary_neq([BinaryTerm, or,       1, col(Table, Col1),col(Table, Col2), OrNeqNegList]),
                            binary_eq([ BinaryTerm, allequal, 1, col(Table, Col1),col(Table, Col2), AllequalEqList]),
                            binary_neq([BinaryTerm, allequal, 1, col(Table, Col1),col(Table, Col2), AllequalNeqList])|S]
    ;
        generate_oplus_lists_from_val_target_pairs(ValueTargetList, TargetRange, 0, CoefMax,
                                                   [SumEqList,    SumNeqList,
                                                    SumEqNegList, SumNeqNegList,
                                                    CondEqList,   CondNeqList]),
        ListOfCoefValues = [binary_eq([ BinaryTerm, sum,       0, col(Table, Col1),col(Table, Col2), SumEqList]),
                            binary_neq([BinaryTerm, sum,       0, col(Table, Col1),col(Table, Col2), SumNeqList]),
                            binary_eq([ BinaryTerm, sum,       1, col(Table, Col1),col(Table, Col2), SumEqNegList]),
                            binary_neq([BinaryTerm, sum,       1, col(Table, Col1),col(Table, Col2), SumNeqNegList]),
                            binary_eq([ BinaryTerm, and,       0, col(Table, Col1),col(Table, Col2), CondEqList]),
                            binary_eq([ BinaryTerm, and,       0, col(Table, Col1),col(Table, Col2), CondEqList]),
                            binary_neq([BinaryTerm, and,       1, col(Table, Col1),col(Table, Col2), CondNeqList]),
                            binary_neq([BinaryTerm, and,       1, col(Table, Col1),col(Table, Col2), CondNeqList]),
                            binary_eq([ BinaryTerm, or,        0, col(Table, Col1),col(Table, Col2), CondEqList]),
                            binary_eq([ BinaryTerm, or,        0, col(Table, Col1),col(Table, Col2), CondEqList]),
                            binary_neq([BinaryTerm, or,        1, col(Table, Col1),col(Table, Col2), CondNeqList]),
                            binary_neq([BinaryTerm, or,        1, col(Table, Col1),col(Table, Col2), CondNeqList]),
                            binary_eq([ BinaryTerm, allequal,  0, col(Table, Col1),col(Table, Col2), CondEqList]),
                            binary_neq([BinaryTerm, allequal,  0, col(Table, Col1),col(Table, Col2), CondNeqList]),
                            binary_eq([ BinaryTerm, allequal,  1, col(Table, Col1),col(Table, Col2), CondEqList]),
                            binary_neq([BinaryTerm, allequal,  1, col(Table, Col1),col(Table, Col2), CondNeqList])|S]
    ),
    tab_get_binary_term_values0(R, col(Table, OutputCol), Mode, ClusterOne, ClusterZero, BinaryTerm, TargetMin-TargetMax, CoefMax, S).


% returns the set of possible coefficient values for a selected UnaryTerm (i.e. mod) for each input column wrt the value of the constant
tab_get_unary_term_values([InputCol|RInputCol], OutputCol, Mode, ClusterOne, ClusterZero, UnaryTerm, MaxCst, ValuesList) :- !,
   ((ClusterOne = [], ClusterZero = []) ->
         Mode = precalculated_entries(_TableEntries, _InputCols, TargetMin, TargetMax)
    ;
         TargetMin = 0,
         TargetMax = 1
    ),
    (UnaryTerm = mod ->                                             % get all pairs of input columns and constants
       findall(Column-Cst, (member(Column, [InputCol|RInputCol]),
                            Cst in 2..MaxCst, indomain(Cst)),
               ColumnCstPairs)
   ;
       write(tab_get_unary_term_values(UnaryTerm)),nl,halt
   ),                                                              % compute the different set of values
   tab_get_unary_term_values0(ColumnCstPairs, OutputCol, Mode, ClusterOne, ClusterZero, UnaryTerm, TargetMin-TargetMax, MaxCst, ValuesList).
tab_get_unary_term_values([], _, _, _, _, _, _, _) :- write(tab_get_unary_term_values), nl, halt.

tab_get_unary_term_values0([], _, _, _, _, _, _, _, []) :- !.
tab_get_unary_term_values0([col(Table, Col)-Cst|R], col(Table, OutputCol), Mode, ClusterOne, ClusterZero, UnaryTerm, TargetMin-TargetMax, CoefMax, ListOfCoefValues) :- 
    TargetRange is TargetMax - TargetMin,
    Mode = precalculated_entries(TableEntries, InputCols, _TargetMin, _TargetMax),
    (UnaryTerm = mod ->
         nth1(IPos, InputCols, col(Table, Col)),
         findall(Val-Target, (member(TableEntry, TableEntries),
                              last(TableEntry, Target0),
                              nth1(IPos, TableEntry, Val1),
                              ((ClusterOne = [], ClusterZero = []) ->
                                  Target is Target0 - TargetMin   % (note that we get compute one pair for each row of the table if no clusters are given)
                              ;
                                  (memberchk(Target0, ClusterOne ) -> Target is 1 ;
                                   memberchk(Target0, ClusterZero) -> Target is 0 ;
                                                                      false       )
                              ),
                              Val is Val1 mod Cst),
                 RowValues)
    ;
         RowValues = []
    ),
    sort(RowValues, ValueTargetList),                           % remove duplicated values
    % from the list of pairs ValueTargetList compute the different set of coefficient values for the different Boolean options
    % of the form:
    %   unary_term_eq([ UnaryTerm, Oplus, Negation, Col, Cst, ListOfValues]) - for x mod Cst = Coef
    %   unary_term_neq([UnaryTerm, Oplus, Negation, Col, Cst, ListOfValues]) - for x mod Cst =< Coef and x mod Cst >= Coef
    %............................................................................................................................
    (TargetRange = 1 ->
        generate_oplus_lists_from_val_target_pairs(ValueTargetList, TargetRange, -CoefMax, CoefMax,
                                                   [AndEqList,       AndNeqList,
                                                    OrEqList,        OrNeqList,
                                                    AllequalEqList,  AllequalNeqList,
                                                    AndEqNegList,    AndNeqNegList,
                                                    OrEqNegList,     OrNeqNegList]),
        ListOfCoefValues = [unary_term_eq([ UnaryTerm, and,      0, col(Table, Col), Cst, AndEqList]),
                            unary_term_neq([UnaryTerm, and,      0, col(Table, Col), Cst, AndNeqList]),
                            unary_term_eq([ UnaryTerm, or,       0, col(Table, Col), Cst, OrEqList]),
                            unary_term_neq([UnaryTerm, or,       0, col(Table, Col), Cst, OrNeqList]),
                            unary_term_eq([ UnaryTerm, allequal, 0, col(Table, Col), Cst, AllequalEqList]),
                            unary_term_neq([UnaryTerm, allequal, 0, col(Table, Col), Cst, AllequalNeqList]),
                            unary_term_eq([ UnaryTerm, and,      1, col(Table, Col), Cst, AndEqNegList]),
                            unary_term_neq([UnaryTerm, and,      1, col(Table, Col), Cst, AndNeqNegList]),
                            unary_term_eq([ UnaryTerm, or,       1, col(Table, Col), Cst, OrEqNegList]),
                            unary_term_neq([UnaryTerm, or,       1, col(Table, Col), Cst, OrNeqNegList]),
                            unary_term_eq([ UnaryTerm, allequal, 1, col(Table, Col), Cst, AllequalEqList]),
                            unary_term_neq([UnaryTerm, allequal, 1, col(Table, Col), Cst, AllequalNeqList])|S]
    ;
        generate_oplus_lists_from_val_target_pairs(ValueTargetList, TargetRange, -CoefMax, CoefMax,
                                                   [SumEqList,    SumNeqList,
                                                    SumEqNegList, SumNeqNegList,
                                                    CondEqList,   CondNeqList]),
        ListOfCoefValues = [unary_term_eq([ UnaryTerm, sum,       0, col(Table, Col), Cst, SumEqList]),
                            unary_term_neq([UnaryTerm, sum,       0, col(Table, Col), Cst, SumNeqList]),
                            unary_term_eq([ UnaryTerm, sum,       1, col(Table, Col), Cst, SumEqNegList]),
                            unary_term_neq([UnaryTerm, sum,       1, col(Table, Col), Cst, SumNeqNegList]),
                            unary_term_eq([ UnaryTerm, and,       0, col(Table, Col), Cst, CondEqList]),   % since conditional formulae can be for Boolean outputs as well
                            unary_term_neq([UnaryTerm, and,       0, col(Table, Col), Cst, CondNeqList]),
                            unary_term_eq([ UnaryTerm, and,       1, col(Table, Col), Cst, CondEqList]),
                            unary_term_neq([UnaryTerm, and,       1, col(Table, Col), Cst, CondNeqList]),
                            unary_term_eq([ UnaryTerm, or,        0, col(Table, Col), Cst, CondEqList]),
                            unary_term_neq([UnaryTerm, or,        0, col(Table, Col), Cst, CondNeqList]),
                            unary_term_eq([ UnaryTerm, or,        1, col(Table, Col), Cst, CondEqList]),
                            unary_term_neq([UnaryTerm, or,        1, col(Table, Col), Cst, CondNeqList]),
                            unary_term_eq([ UnaryTerm, allequal,  0, col(Table, Col), Cst, CondEqList]),
                            unary_term_neq([UnaryTerm, allequal,  0, col(Table, Col), Cst, CondNeqList]),
                            unary_term_eq([ UnaryTerm, allequal,  1, col(Table, Col), Cst, CondEqList]),
                            unary_term_neq([UnaryTerm, allequal,  1, col(Table, Col), Cst, CondNeqList])|S]
    ),
    tab_get_unary_term_values0(R,  col(Table, OutputCol), Mode, ClusterOne, ClusterZero, UnaryTerm, TargetMin-TargetMax, CoefMax, S).

% Get the set of possible values for a selected BinaryTerm (sum, minus, etc) for every pair of columns from the list Columns.
% Returns a list of values 
tab_get_unaryf_values(Columns, col(_Table, OutputCol), Mode, ClusterOne, ClusterZero, UnaryF, CoefMax, Values) :- 
    (UnaryF = prod ->
        findall(Col1-Col2, (member(Col1,Columns), Col1 = col(_,I1),
                            member(Col2,Columns), Col2 = col(_,I2),
                            I2 =\= I1), ColPairs)
    ;
        write(tab_get_unaryf_values(UnaryF)), nl, halt
    ),
    append(ClusterOne, ClusterZero, Clusters),
    tab_get_unaryf_values0(ColPairs, OutputCol, Mode, Clusters, UnaryF, CoefMax, Values).

% returns the set of possible constant values for a selected UnaryF(prod) and each pair of columns Col1 and Col2,
% where UnaryF(prod) if a condition of the form:
%  . t(cond(attr_eq_unary( [prod(2,Max)])), no_negation, 1)
%  . t(cond(attr_leq_unary([prod(2,Max)])), no_negation, 1)
%  . t(cond(unary_leq_attr([prod(2,Max)])), no_negation, 1)
tab_get_unaryf_values0([], _, _, _, _, _, []) :- !.
tab_get_unaryf_values0([col(Table, Col1)-col(Table, Col2)|R], OutputCol, Mode, Clusters, UnaryF, CoefMax,[ValuesAEqU,ValuesALeqU,ValuesULeqA|S]) :-
    Mode = precalculated_entries(TableEntries, InputCols, _TargetMin, _TargetMax),
    nth1(IPos1, InputCols,  col(Table, Col1)),
    nth1(IPos2, InputCols,  col(Table, Col2)),                                         
    (UnaryF = prod ->
        findall(Val, (Val in 2..CoefMax, indomain(Val),          % for every pair of columns
                      findall(RowValue, (member(TableEntry, TableEntries),
                                         last(TableEntry, Target),
                                         (Clusters = [] -> true ; memberchk(Target, Clusters)),
                                         nth1(IPos1, TableEntry, Val1),
                                         nth1(IPos2, TableEntry, Val2),
                                         ValVal2 is Val * Val2,
                                         (Val1 = ValVal2 -> RowValue = 1 ; RowValue = 0)), RowValues),
                      memberchk(0, RowValues),                   % keep only value Val if the condition can be made false and true
                      memberchk(1, RowValues)),
                CoefListAEqU),
        findall(Val, (Val in 2..CoefMax, indomain(Val),          % for every pair of columns
                      findall(RowValue, (member(TableEntry, TableEntries),
                                         last(TableEntry, Target),
                                         (Clusters = [] -> true ; memberchk(Target, Clusters)),
                                         nth1(IPos1, TableEntry, Val1),
                                         nth1(IPos2, TableEntry, Val2),
                                         ValVal2 is Val * Val2,
                                         (Val1 =< ValVal2 -> RowValue = 1 ; RowValue = 0)), RowValues),
                      memberchk(0, RowValues),                   % keep only value Val if the condition can be made false and true
                      memberchk(1, RowValues)),
                CoefListALeqU),
        findall(Val, (Val in 2..CoefMax, indomain(Val),          % for every pair of columns
                      findall(RowValue, (member(TableEntry, TableEntries),
                                         last(TableEntry, Target),
                                         (Clusters = [] -> true ; memberchk(Target, Clusters)),
                                         nth1(IPos1, TableEntry, Val1),
                                         nth1(IPos2, TableEntry, Val2),
                                         ValVal1 is Val * Val1,
                                         (ValVal1 =< Val2 -> RowValue = 1 ; RowValue = 0)), RowValues),
                      memberchk(0, RowValues),                   % keep only value Val if the condition can be made false and true
                      memberchk(1, RowValues)),
                CoefListULeqA)
    ;
        CoefListAEqU = []
    ),
    ValuesAEqU  = unary_aequ([ UnaryF,col(Table, Col1),col(Table, Col2),CoefListAEqU ]),
    ValuesALeqU = unary_alequ([UnaryF,col(Table, Col1),col(Table, Col2),CoefListALeqU]),
    ValuesULeqA = unary_uleqa([UnaryF,col(Table, Col1),col(Table, Col2),CoefListULeqA]),
    tab_get_unaryf_values0(R, OutputCol, Mode, Clusters, UnaryF, CoefMax, S).

% Get the set of possible values for a selected BinaryTerm (sum, minus, etc) for every pair of columns from the list Columns.
% Returns a list of values 
tab_get_ternary_values(Columns, col(_Table, OutputCol), Mode, ClusterOne, ClusterZero, TernaryType, Values) :-
    findall([Col1,Col2,Col3], (member(Col1,Columns), Col1 = col(_,I1),
                               member(Col2,Columns), Col2 = col(_,I2),
                               member(Col3,Columns), Col3 = col(_,I3),
                               I1 =\= I2, I1 =\= I3, I2 =\= I3), ColPairs),
    append(ClusterOne, ClusterZero, Clusters),
    tab_get_ternary_values0(ColPairs, OutputCol, Mode, Clusters, TernaryType, Values).

% returns the set of possible constant values for a selected UnaryF(prod) and each pair of columns Col1 and Col2,
% where UnaryF(prod) if a condition of the form:
%  . t(cond(attr_eq_unary( [prod(2,Max)])), no_negation, 1)
%  . t(cond(attr_leq_unary([prod(2,Max)])), no_negation, 1)
%  . t(cond(unary_leq_attr([prod(2,Max)])), no_negation, 1)
tab_get_ternary_values0([], _, _, _, _, []) :- !.
tab_get_ternary_values0([[col(Table, Col1),col(Table, Col2),col(Table,Col3)]|R], OutputCol, Mode, Clusters, TernaryType, Values) :-
    Mode = precalculated_entries(TableEntries, InputCols, _TargetMin, _TargetMax),
    nth1(IPos1, InputCols,  col(Table, Col1)),
    nth1(IPos2, InputCols,  col(Table, Col2)),                                         
    nth1(IPos3, InputCols,  col(Table, Col3)),
    (TernaryType = sum_leq_attr ->
        findall(RowValue, (member(TableEntry, TableEntries),
                           last(TableEntry, Target),
                           (Clusters = [] -> true ; memberchk(Target, Clusters)),
                           nth1(IPos1, TableEntry, Val1),
                           nth1(IPos2, TableEntry, Val2),
                           nth1(IPos3, TableEntry, Val3),
                           Val12 is Val1 + Val2,
                           (Val12 < Val3 ->
                                RowValue = 1
                           ;
                            Val12 = Val3 ->
                                RowValue = 2
                           ;
                                RowValue = 0
                           )
                          ), RowValuesTerm),
        ((memberchk(0, RowValuesTerm),     % keep the triplet of rows only if condition can be falsified within the data table
          memberchk(1, RowValuesTerm),     % keep the triplet of rows only if condition can be positive with an inequality within the data table
          memberchk(2, RowValuesTerm)) ->  % keep the triplet of rows only if condition can be positive with an   equality within the data table
            Values = [ternary_set([col(Table, Col1), col(Table, Col2), col(Table, Col3)], TernaryType)|S]
        ;
            Values = S
        )
    ;
     TernaryType = minus_mod_eq0 ->
        % cond(minus_mod_eq0) :     (CondAttr3-CondAttr1) mod CondAttr2 =  0
        findall(RowValue, (member(TableEntry, TableEntries),
                           last(TableEntry, Target),
                           (Clusters = [] -> true ; memberchk(Target, Clusters)),
                           nth1(IPos1, TableEntry, Val1),
                           nth1(IPos2, TableEntry, Val2),
                           nth1(IPos3, TableEntry, Val3),
                           Val31 is Val3 - Val1,
                           Val2 =\= 0,
                           Val is Val31 mod Val2,
                           (Val is 0 ->
                                RowValue = 1
                           ;
                                RowValue = 0
                           )
                          ), RowValuesTerm),
        ((memberchk(0, RowValuesTerm),    % keep the triplet of rows only if condition can be falsified within the data table
          memberchk(1, RowValuesTerm)) -> % keep the triplet of rows only if condition can be positive  within the data table
            Values = [ternary_set([col(Table, Col1), col(Table, Col2), col(Table, Col3)], TernaryType)|S]
        ;
            Values = S
        )
    ;
     TernaryType = ceil_leq_floor ->
        % cond(ceil_leq_floor): ceil(CondAttr1-CondAttr3,CondAttr2)     =< floor(CondAttr1-CondAttr2,CondAttr3)
        %Cost =  2:  c >= 1 + [not [((maxc-mins+maxs-1) div maxs)=<((maxc-maxs) div mins)]]
        findall(RowValue, (member(TableEntry, TableEntries),
                           last(TableEntry, Target),
                           (Clusters = [] -> true ; memberchk(Target, Clusters)),
                           nth1(IPos1, TableEntry, Val1),
                           nth1(IPos2, TableEntry, Val2),
                           nth1(IPos3, TableEntry, Val3),
                           Val2 =\= 0, Val3 =\= 0,
                           Val132 is (Val1 - Val3 + Val2 - 1) // Val2,
                           Val123 is (Val1 - Val2) // Val3,
                           (Val132 =< Val123 ->
                                RowValue = 1
                           ;
                                RowValue = 0
                           )
                          ), RowValuesTerm),
        ((memberchk(0, RowValuesTerm),    % keep the triplet of rows only if condition can be falsified within the data table
          memberchk(1, RowValuesTerm)) -> % keep the triplet of rows only if condition can be positive  within the data table
            Values = [ternary_set([col(Table, Col1), col(Table, Col2), col(Table, Col3)], TernaryType)|S]
        ;
            Values = S
        )
    ;
     TernaryType = attr_geq_fmod ->
        % cond(attr_geq_fmod): CondAttr1 >= fmod(CondAttr2,CondAttr3)
        findall(RowValue, (member(TableEntry, TableEntries),
                           last(TableEntry, Target),
                           (Clusters = [] -> true ; memberchk(Target, Clusters)),
                           nth1(IPos1, TableEntry, Val1),
                           nth1(IPos2, TableEntry, Val2),
                           nth1(IPos3, TableEntry, Val3),
                           Val3 =\= 0,
                           binary_term_calculation(fmod, Val2, Val3, _, Val32),
                           (Val1 >= Val32 ->
                                RowValue = 1
                           ;
                                RowValue = 0
                           )
                          ), RowValuesTerm),
        %write(TableEntries),nl,
        %write([Col1,Col2,Col3,RowValuesTerm]),nl,
        ((memberchk(0, RowValuesTerm),    % keep the triplet of rows only if condition can be falsified within the data table
          memberchk(1, RowValuesTerm)) -> % keep the triplet of rows only if condition can be positive  within the data table
            Values = [ternary_set([col(Table, Col1), col(Table, Col2), col(Table, Col3)], TernaryType)|S]
        ;
            Values = S
        )
    ;
     TernaryType = mod_gt_mod ->
        % cond(mod_gt_mod) :                     (LogAttr2 mod LogAttr3 >  LogAttr1 mod LogAttr3)
        findall(RowValue, (member(TableEntry, TableEntries),
                           last(TableEntry, Target),
                           (Clusters = [] -> true ; memberchk(Target, Clusters)),
                           nth1(IPos1, TableEntry, Val1),
                           nth1(IPos2, TableEntry, Val2),
                           nth1(IPos3, TableEntry, Val3),
                           Val3 =\= 0,
                           Val13 is Val1 mod Val3,
                           Val23 is Val2 mod Val3,
                           (Val23 > Val13 ->
                                RowValue = 1
                           ;
                                RowValue = 0
                           )
                          ), RowValuesTerm),
        ((memberchk(0, RowValuesTerm),    % keep the triplet of rows only if condition can be falsified within the data table
          memberchk(1, RowValuesTerm)) -> % keep the triplet of rows only if condition can be positive  within the data table
            Values = [ternary_set([col(Table, Col1), col(Table, Col2), col(Table, Col3)], TernaryType)|S]
        ;
            Values = S
        )
    ;
        write(tab_get_ternary_values0(TernaryType)),nl,halt
    ),
    tab_get_ternary_values0(R, OutputCol, Mode, Clusters, TernaryType, S).

gen_compatible_unary_and_binary_and_ternary(NTerm, NAttr, Sols) :-
    I in 0..NTerm,
    J in 0..NTerm,
    K in 0..NTerm,
    I+J+K #= NTerm,
    I+2*J+3*K #>= NAttr,            % maximum number of attributes we can have should not be too big
    LowI in 0..1, LowI #= min(1,I), % minimum number of attributes if use one unary condition
    LowJ in 0..2, LowJ #= min(2,J), % minimum number of attributes if use one binary condition
    LowK in 0..3, LowK #= min(3,K), % minimum number of attributes if use one ternary condition
    Low  in 0..3,
    maximum(Low, [LowI,LowJ,LowK]), % minimum number of attributes we will have
    Low #=< NAttr,                  % minimum number of attributes we will have should not be too small
    !,
    findall([I,J,K], labeling([], [I,J,K]), Sols).
gen_compatible_unary_and_binary_and_ternary(_, _, []).

% Given a list of pairs of type Value-Target where:
%  . Value  is the value of a attribute/unary term/binary term used in the Boolean condition,
%  . Target is the corresponding value of a target Boolean column
%    (the maximum value of a target Boolean column can be >1 if use a sum)
% with,
%  . ColMax being the maximum value of the target column
%  . CoefMin is the minimum value of the coefficient associated with the Boolean condition
%  . CoefMax is the maximum value of the coefficient associated with the Boolean condition
% create a list of sets of allowed coefficient values for Boolean conditions
% note that:
%  . we do not create AllequalEqNegList  as it is equal to AllequalEqList
%  . we do not create AllequalNeqNegList as it is equal to AllequalNeqList
% examples how to call it:
%  . generate_oplus_lists_from_val_target_pairs([1-0, 2-1, 3-0, 3-1, 4-1, 7-0], 1, -10, 10, Res)
%  . generate_oplus_lists_from_val_target_pairs([1-3, 2-3, 3-0, 5-3, 4-3, 7-0], 3, -10, 10, Res).
%
generate_oplus_lists_from_val_target_pairs(ValueTargetList, ColMax, CoefMin, CoefMax,
                                           [AndEqList,                                          % for type     and(cond= coef)
                                            AndNeqList,                                         % for type     and(cond<=coef) or and(cond>=coef)
                                            OrEqList,                                           % for type      or(cond= coef)
                                            OrNeqList,                                          % for type      or(cond<=coef) or  or(cond>=coef)
                                            AllequalEqList,                                     % for type       =(cond= coef)
                                            AllequalNeqList,                                    % for type       =(cond<=coef) or   =(cond>=coef)
                                            AndEqNegList,                                       % for type not and(cond= coef)
                                            AndNeqNegList,                                      % for type not and(cond<=coef) or and(cond>=coef)
                                            OrEqNegList,                                        % for type not or(cond= coef)
                                            OrNeqNegList]):-                                    % for type not or(cond<=coef)  or  or(cond>=coef)
    ColMax = 1,
    !,

    (foreach(X-_, ValueTargetList),                                                             % extract all possible values of the term
     foreach(X,     ValuesListAll) do true),                                                    % sort and remove duplicates to create the list of
    sort(ValuesListAll,ValuesListEq0),                                                          % values for the coefficient in an equality constraint
    (ValuesListEq0 = [_,_|_] ->                                                                 % if more than one value
        ValuesListEq = ValuesListEq0                                                            % then keep them
    ;                                                                                           % if less than two values
        ValuesListEq = []                                                                       % no need for using it in a constraint
    ),                                                                                          % generates the list of all possible values for the
    remove_first_last_elements(ValuesListEq,                                                    % coefficient in an inequality ctr (<=,>=): remove
                               ValuesListNeq),                                                  % min/max values so that condition not allways true or
                                                                                                % that condition is not equivalent to an equality

    findall(Val, (member(Val,ValuesListEq),  Val =< CoefMax, Val >= CoefMin), AllequalEqList),  % no more restriction when using =, except
    findall(Val, (member(Val,ValuesListNeq), Val =< CoefMax, Val >= CoefMin), AllequalNeqList), % coefficient range

    split_value_target_pairs(ValueTargetList, ColMax, ValuesForFalse, ValuesForTrue),           % get term values wrt minimum/maximum target values
    
    findall(Val, (member(Val,ValuesForTrue),                                                    % as for an and all conditions need to be true,
                  Val =< CoefMax, Val >= CoefMin),                                              % we only use values for which the target value
                                                   AndEqList),                                  % is one (i.e. the and condition holds)

    intersect_lists(AndEqList,                                                                  % remove from AndEqList min/max values if they are
                    AllequalNeqList,                                                            % present so that the condition is not allways true
                    AndNeqList),                                                                % or that condition is not equivalent to an equality

    remove_elements_from_list(AllequalEqList,                                                   % from all potential values remove term values for
                              ValuesForFalse,                                                   % which the target value is false (as such term values
                              OrEqList),                                                        % cannot be used with a true target value as this
                                                                                                % would lead to a contradiction)
    
    remove_elements_from_list(AllequalNeqList,                                                  % from all potential values remove term values for
                              ValuesForFalse,                                                   % which the target value is false (as such term values
                              OrNeqList),                                                       % cannot be used with a true target value as this
                                                                                                % would lead to a contradiction)

    findall(Val, (member(Val,ValuesForFalse),                                                   % symmetric to the case AndEqList in the sense
                  Val =< CoefMax, Val >= CoefMin),                                              % that we permute the target values true
                                                   AndEqNegList),                               % and false

    intersect_lists(AndEqNegList,                                                               % remove from AndEqNegList min/max values if they are
                    AllequalNeqList,                                                            % present so that the condition is not allways true
                    AndNeqNegList),                                                             % or that condition is not equivalent to an equality
    
    remove_elements_from_list(AllequalEqList,                                                   % symmetric to the case OrEqList in the sense
                              ValuesForTrue,                                                    % that we permute the target values false
                              OrEqNegList),                                                     % and true
    
    remove_elements_from_list(AllequalNeqList,                                                  % symmetric to the case OrNeqList in the sense
                              ValuesForTrue,                                                    % that we permute the target values false
                              OrNeqNegList).                                                    % and true


generate_oplus_lists_from_val_target_pairs(ValueTargetList, ColMax, CoefMin, CoefMax,
                                           [SumEqList,                                          % for type         sum(cond= coef)
                                            SumNeqList,                                         % for type         sum(cond<=coef) or sum(cond>=coef)
                                            SumEqNegList,                                       % for type nterm - sum(cond= coef)
                                            SumNeqNegList,                                      % for type nterm - sum(cond<=coef) or sum(cond>=coef)
                                            CondEqList,                                         % for conditional formulae
                                            CondNeqList]):-                                     % for conditional formulae
    (foreach(X-_, ValueTargetList),                                                             % extract all possible values of the term
     foreach(X,     ValuesListAll) do true),                                                    % sort and remove duplicates to create the list of
    sort(ValuesListAll,ValuesListEq),                                                           % values for the coefficient in an equality constraint
                                                                                                % generates the list of all possible values for the
    remove_first_last_elements(ValuesListEq,                                                    % coefficient in an inequality ctr (<=,>=): remove
                               ValuesListNeq),                                                  % min/max values so that condition not allways true or
                                                                                                % that condition is not equivalent to an equality
    findall(Val, (member(Val,ValuesListEq),  Val =< CoefMax, Val >= CoefMin), AllEqList),       % the only restriction for AllEqList  is coef.range
    findall(Val, (member(Val,ValuesListNeq), Val =< CoefMax, Val >= CoefMin), AllNeqList),      % the only restriction for AllNeqList is coef.range
    split_value_target_pairs(ValueTargetList, ColMax, ValuesForZero, ValuesForMax),             % get term values wrt minimum/maximum target values

    CondEqList = AllEqList, CondNeqList = AllNeqList,                                           % Further filtering for conditional formulae isn't
                                                                                                % possible

    remove_elements_from_list(AllEqList,                                                        % from all potential values remove term values for
                              ValuesForZero,                                                    % which the target value is 0 (as such term values
                              SumEqList),                                                       % cannot be used with a non-zero target value as this
                                                                                                % would lead to a contradiction)

    remove_elements_from_list(AllNeqList,                                                       % from all potential values remove term values for
                              ValuesForZero,                                                    % which the target value is 0 (as such term values
                              SumNeqList),                                                      % cannot be used with a non-zero target value as this
                                                                                                % would lead to a contradiction)
    
    remove_elements_from_list(AllEqList,  ValuesForMax, SumEqNegList),                          % next two lines are symmetric to two previous
    remove_elements_from_list(AllNeqList, ValuesForMax, SumNeqNegList).                         % statements

% split_value_target_pairs(ValueTargetPairs, TargetV, ValuesDiffTargetV, ValuesTargetV)
% given a list of pairs Value-Target and a value TargetV of Target, return two lists:
%  . a list ValuesDiffTargetV of all values for which the target is different from TargetV,
%  . a list ValuesTargetV     of all values for which the target is equal to TargetV.
%
% ?- split_value_target_pairs([1-0, 2-1, 3-0, 4-1,7-0], 1, X, Y).
% X = [1,3,7], Y = Y = [2,4]
split_value_target_pairs([], _, [], []):- !.
split_value_target_pairs([X-0|R], Max, [X|S], T):- !,
    split_value_target_pairs(R, Max, S, T).
split_value_target_pairs([X-Max|R], Max, S, [X|T]):- !,
    split_value_target_pairs(R, Max, S, T).
split_value_target_pairs([_|R], Max, S, T):- !,
    split_value_target_pairs(R, Max, S, T).

get_set_of_values_for_each_mandatory_attribute(MandatoryAttr, ShiftBool, Oplus, Negated, CondType, ColSetsBool,
                                               CondCoefMin, CondCoefMax, [[1,CondCoefMin]|CoefValuesList]):-
    length(MandatoryAttr,LenA),
    select_oplus_for_the_set_search(Oplus, OplusSearch),
    % attr_eq([ Oplus,   Negation, Col, Cst, ListOfValues]) - for x = Coef
    % attr_neq([Oplus,   Negation, Col, Cst, ListOfValues]) - for x =< Coef and x >= Coef
    findall([Attr,CoefValue],(I in 1..LenA, indomain(I),
                              nth1(I,MandatoryAttr,Col),
                              (CondType = attr_eq_coef ->
                                   memberchk(attr_eq([ OplusSearch,Negated,Col,ValuesList]), ColSetsBool)
                              ;
                               memberchk(CondType, [attr_leq_coef,attr_geq_coef]) ->
                                   memberchk(attr_neq([OplusSearch,Negated,Col,ValuesList]), ColSetsBool)
                              ;
                                   write(get_set_of_values_for_each_mandatory_attribute(I-Attr,OplusSearch,Negated)),nl,halt
                              ),
                              member(CoefValue,ValuesList),
                              CoefValue >= CondCoefMin,
                              CoefValue =< CondCoefMax,
                              Attr is I + ShiftBool), CoefValuesList).

% (cond(attr_eq_unary([prod(2,Max)])),                    no_negation,    1),
% t(cond(attr_leq_unary([prod(2,Max)])),                  no_negation,    1),
% t(cond(unary_leq_attr([prod(2,Max)])),                  no_negation,    1),
get_set_of_unaryf_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, ShiftBool, CondType, UnaryFType, ColSetsBool,
                                                               CondCstMin, CondCstMax, [[1,1,CondCstMin]|CoefValuesList]):-
    length(MandatoryAttr,LenA),
    findall([Attr1,Attr2,CstValue],(I1 in 1..LenA, I2 in 1..LenA, indomain(I1),indomain(I2), I1 =\= I2,
                                     nth1(I1,MandatoryAttr,Col1),
                                     nth1(I2,MandatoryAttr,Col2),
                                     (CondType = attr_eq_unary ->
                                        memberchk(unary_aequ([UnaryFType,Col1,Col2,ValuesList]),  ColSetsBool)
                                     ;
                                      CondType = attr_leq_unary ->
                                        memberchk(unary_alequ([UnaryFType,Col1,Col2,ValuesList]), ColSetsBool)
                                     ;
                                        memberchk(unary_uleqa([UnaryFType,Col1,Col2,ValuesList]), ColSetsBool)
                                     ),
                                     member(CstValue,ValuesList),
                                     CstValue >= CondCstMin,
                                     CstValue =< CondCstMax,
                                     Attr1 is I1 + ShiftBool,
                                     Attr2 is I2 + ShiftBool), CoefValuesList).

get_set_of_ternary_triplets_of_mandatory_attributes(MandatoryAttr, ShiftBool, CondType, ColSetsBool,
                                                    [[1,1,1]|TripletsList]):-
    length(MandatoryAttr,LenA),
    findall([Attr1,Attr2,Attr3],(I1 in 1..LenA, I2 in 1..LenA, indomain(I1),indomain(I2), I1 =\= I2,
                                 nth1(I1,MandatoryAttr,Col1),
                                 nth1(I2,MandatoryAttr,Col2),
                                 nth1(I3,MandatoryAttr,Col3),
                                 memberchk(ternary_set([Col1,Col2,Col3], CondType),  ColSetsBool),
                                 Attr1 is I1 + ShiftBool,
                                 Attr2 is I2 + ShiftBool,
                                 Attr3 is I3 + ShiftBool), TripletsList).

get_set_of_unary_term_values_for_each_mandatory_attribute(MandatoryAttr, ShiftBool, Oplus, Negated, CondType, UnaryType, ColSetsBool,
                                                          CondCstMin, CondCstMax, CondCoefMin, CondCoefMax, [[1,CondCstMin,CondCoefMin]|CoefValuesList]):-
    length(MandatoryAttr,LenA),
    select_oplus_for_the_set_search(Oplus, OplusSearch),
    % unary_term_eq([ UnaryTerm, Oplus,   Negation, Col, Cst, ListOfValues]) - for x mod Cst = Coef
    % unary_term_neq([UnaryTerm, Oplus,   Negation, Col, Cst, ListOfValues]) - for x mod Cst =< Coef and x mod Cst >= Coef
    findall([Attr,CstValue,CoefValue],(I in 1..LenA, indomain(I),
                                       nth1(I,MandatoryAttr,Col),
                                       CstValue in CondCstMin..CondCstMax,
                                       indomain(CstValue),
                                       (CondType = unary_term_eq_coef ->
                                          memberchk(unary_term_eq([UnaryType,OplusSearch,Negated,Col,CstValue,ValuesList]), ColSetsBool)
                                       ;
                                        memberchk(CondType, [unary_term_leq_coef,unary_term_geq_coef]) ->
                                          memberchk(unary_term_neq([UnaryType,OplusSearch,Negated,Col,CstValue,ValuesList]), ColSetsBool)
                                       ;
                                          write(get_set_of_unary_term_values_for_each_mandatory_attribute(UnaryType,OplusSearch,Negated)),nl,halt
                                       ),
                                       member(CoefValue,ValuesList),
                                       CoefValue >= CondCoefMin,
                                       CoefValue =< CondCoefMax,
                                       Attr is I + ShiftBool), CoefValuesList).

get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, ShiftBool, Oplus, Negated, CondType, BinaryType, ColSetsBool,
                                                                    CondCoefMin, CondCoefMax, [[1,1,CondCoefMin]|CoefValuesList]):-
    length(MandatoryAttr,LenA),
    select_oplus_for_the_set_search(Oplus, OplusSearch),
    findall([Attr1,Attr2,CoefValue],(I1 in 1..LenA, I2 in 1..LenA, indomain(I1),indomain(I2), I1 =\= I2,
                                     nth1(I1,MandatoryAttr,Col1),
                                     nth1(I2,MandatoryAttr,Col2),
                                     (CondType = binary_term_eq_coef ->
                                        memberchk(binary_eq([ BinaryType,OplusSearch,Negated,Col1,Col2,ValuesList]), ColSetsBool)
                                     ;
                                      memberchk(CondType, [binary_term_leq_coef,binary_term_geq_coef]) ->
                                        memberchk(binary_neq([BinaryType,OplusSearch,Negated,Col1,Col2,ValuesList]), ColSetsBool)
                                     ;
                                        write(get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(CondType,BinaryType,OplusSearch,Col1,Col2)), nl, halt
                                     ),
                                     member(CoefValue,ValuesList),
                                     CoefValue >= CondCoefMin,
                                     CoefValue =< CondCoefMax,
                                     Attr1 is I1 + ShiftBool,
                                     Attr2 is I2 + ShiftBool), CoefValuesList).

select_oplus_for_the_set_search(Oplus, OplusSearch) :-
    (memberchk(Oplus, [xor,voting,card1]) -> OplusSearch = allequal; OplusSearch = Oplus).      % since xor (implemented) and voting (not implemented), use the same list of values for conditions as allequal 
                                                                                                % precalculated Values for conditions we can use allequal list to use in predicates:
                                                                                                % get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes/9
                                                                                                % get_set_of_unary_term_values_for_each_mandatory_attribute/11
                                                                                                % get_set_of_unaryf_values_for_each_pair_of_mandatory_attributes/7
                                                                                                % get_set_of_values_for_each_mandatory_attribute/8
