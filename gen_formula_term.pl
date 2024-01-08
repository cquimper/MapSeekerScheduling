% Purpose: FROM A FORMULA PATTERN GENERATE A PROLOG TERM (INTERNAL FORMULA FORMAT)
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_formula_term,[formula_pattern_create_term/2]).            % write a formula in the console
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(utility).
:- use_module(table_access).

% FROM A GROUND FORMULA PATTERN CREATE A FORMULA TERM OF THE FORM t(ListOfParams,ListOfSourceVars,SourceTerm)
%------------------------------------------------------------------------------------------------------------
% For instance, for a table with 4 columns named w, x, y, z,
% and a formula of the form min(x,y)*z^2 - x^2 + y*z - 3*x + 7
% we generate the following formula term t(ListOfNames, ListOfParemeters, ListOfSourceVariables, SourceTerm), where:
%  . ListOfNames           = [x, y, z],
%  . ListOfParemeters      = [col(table, 2), col(table, 3), col(table, 4)],
%  . ListOfSourceVariables = [X, Y, Z],
%  . SourceTerm            = polynom([monome([t(min(X,Y),1),t(Z,2)],1),
%                                     monome([t(X,2)],-1),
%                                     monome([t(Y,1),t(Z,1)],1),
%                                     monome([t(X,1)],-3),
%                                     monome([],7)]).
%
% For instance, for a table with 3 columns names x, y, z,
% and a conditional formula of the form (x=2: 3 ; y)
% we generate the following formula term t(ListOfNames, ListOfParemeters, ListOfSourceVariables, SourceTerm), where:
%  . ListOfNames           = [x, y],
%  . ListOfParemeters      = [col(table, 1), col(table, 2)],
%  . ListOfSourceVariables = [X, Y],
%  . SourceTerm            = if(eq(X,2), 3, Y).

formula_pattern_create_term(FormulaPattern, FormulaTerm) :-
    (FormulaPattern = t(_,_,_,_) ->
        FormulaTerm = FormulaPattern
    ;
     formula_pattern(f(_), FormulaPattern) ->
        formula_pattern_create_term_formula(FormulaPattern, FormulaTerm) % polynomial or unary function or binary function
    ;
     formula_pattern(cond_ex, FormulaPattern) ->
        formula_pattern_create_term_cond(FormulaPattern, FormulaTerm)    % Conditional formula with Boolean condition
    ;
     formula_pattern(op(_), FormulaPattern) ->
        formula_pattern_create_term_bool(FormulaPattern, FormulaTerm)    % Boolean formula
    ;
     formula_pattern(decomp(_,_,_), FormulaPattern) ->
        formula_pattern_create_term_decomp(FormulaPattern, FormulaTerm)    % Boolean formula
    ;
     formula_pattern(option(_,_,_), FormulaPattern) ->
        formula_pattern_create_term_case(FormulaPattern, FormulaTerm)    % Case formula
    ;
        formula_pattern_create_term_cond(FormulaPattern, FormulaTerm)    % Conditional formula
    ).

% create formula term for decomposed formulae. Works recursively
formula_pattern_create_term_decomp(FormulaPattern, FormulaTerm) :-
    FormulaPattern = [decomp(cond, FormulaPatternCond-FormulaPatternThen, FormulaPatternElse)],
    !,
    formula_pattern_create_term(FormulaPatternCond,
                                t(InputColsListCond, InputNamesListCond, InputVarsListCond, FormulaTermCondTerm)),
    formula_pattern_create_term(FormulaPatternThen,
                                t(InputColsListThen, InputNamesListThen, InputVarsListThen, FormulaTermThenTerm)),
    formula_pattern_create_term(FormulaPatternElse,
                                t(InputColsListElse, InputNamesListElse, InputVarsListElse, FormulaTermElseTerm)),
    FormulaTermTerm = if(FormulaTermCondTerm, FormulaTermThenTerm, FormulaTermElseTerm),
    append([InputColsListCond,   InputColsListThen,   InputColsListElse],  InputColsList),
    append([InputNamesListCond,  InputNamesListThen,  InputNamesListElse], InputNamesList),
    append([InputVarsListCond,   InputVarsListThen,   InputVarsListElse],  InputVarsList),
    transform_col_name_var_lists(InputColsList, InputNamesList, InputVarsList,
                                 InputVarsListTransformed),
    copy_term(InputVarsList-FormulaTermTerm, InputVarsListTransformed-FormulaTermTerm2),
    collect_unique_col_name_var_lists(InputColsList,            InputNamesList,            InputVarsListTransformed,
                                      InputColsListTerm,        InputNamesListTerm,        InputVarsListTerm),
    FormulaTerm = t(InputColsListTerm, InputNamesListTerm, InputVarsListTerm, FormulaTermTerm2).
formula_pattern_create_term_decomp(FormulaPattern, FormulaTerm) :-
    FormulaPattern = [decomp(Op, FormulaPattern1, FormulaPattern2)],
    formula_pattern_create_term(FormulaPattern1,
                                t(InputColsList1, InputNamesList1, InputVarsList1, FormulaTerm1Term)),
    formula_pattern_create_term(FormulaPattern2,
                                t(InputColsList2, InputNamesList2, InputVarsList2, FormulaTerm2Term)),
    functor(FormulaTermTerm, Op, 2),
    arg(1, FormulaTermTerm, FormulaTerm1Term),
    arg(2, FormulaTermTerm, FormulaTerm2Term),
    append(InputColsList1,  InputColsList2,  InputColsList),
    append(InputNamesList1, InputNamesList2, InputNamesList),
    append(InputVarsList1,  InputVarsList2,  InputVarsList),
    transform_col_name_var_lists(InputColsList, InputNamesList, InputVarsList,
                                 InputVarsListTransformed),
    copy_term(InputVarsList-FormulaTermTerm, InputVarsListTransformed-FormulaTermTerm2),
    collect_unique_col_name_var_lists(InputColsList,            InputNamesList,            InputVarsListTransformed,
                                      InputColsListTerm,        InputNamesListTerm,        InputVarsListTerm),
    FormulaTerm = t(InputColsListTerm, InputNamesListTerm, InputVarsListTerm, FormulaTermTerm2).

% If two variables Var have same corresponding Col and Name we replace the second Var with the first one,
% ensuring that each Col and Name have only one corresponding Var. e.g.:
% BEFORE: Cols =  [Col1, Col3, Col1, Col2], Names = [name1, name3, name1, name2], Vars =  [_A, _B, _C, _D]
% AFTER:  Cols =  [Col1, Col3, Col1, Col2], Names = [name1, name3, name1, name2], Vars =  [_A, _B, _A, _D]
transform_col_name_var_lists([], [], [], []) :- !.
transform_col_name_var_lists([Col|R], [Name|S], [Var|T],
                             [Var|Y]) :-
    transform_col_name_var_lists1(R, S, T, Col, Name, Var, X),
    transform_col_name_var_lists(R, S, X, Y).

transform_col_name_var_lists1([], [], [], _, _, _, []) :- !.
transform_col_name_var_lists1([Col|R], [Name|S],   [_|T], Col, Name, Var,
                              [Var|X]) :-
    !,
    transform_col_name_var_lists1(R, S, T, Col, Name, Var, X).
transform_col_name_var_lists1([_|R], [_|S], [Var0|T], Col, Name, Var,
                              [Var0|X]) :-
    transform_col_name_var_lists1(R, S, T, Col, Name, Var, X).

% Remove corresponding entries with the same Col, Name, Var and sort them (by Col):
% BEFORE: Cols =  [Col1, Col3, Col1, Col2], Names = [name1, name2, name1, name3], Vars =  [_A, _B, _A, _D]
% AFTER:  Cols =  [Col1, Col2, Col3],       Names = [name1, name2, name3],        Vars =  [_A, _D, _B]
collect_unique_col_name_var_lists(InputColsList,            InputNamesList,            InputVarsList,
                                  InputColsListTerm,        InputNamesListTerm,        InputVarsListTerm) :-
    (foreach(X, InputColsList),
     foreach(Y, InputNamesList),
     foreach(Z, InputVarsList),
     foreach([X, Y, Z], ColsNamesVarsList)
    do
     true
    ),
    sort(ColsNamesVarsList, ColsNamesVarsListSorted),
    (foreach([X, Y, Z], ColsNamesVarsListSorted),
     foreach(X, InputColsListTerm),
     foreach(Y, InputNamesListTerm),
     foreach(Z, InputVarsListTerm)
    do
     true
    ).

formula_pattern_create_term_case(FormulaPattern, FormulaTerm) :-
    length(FormulaPattern, CaseListLength),
    length(CaseList, CaseListLength),
    formula_pattern_create_term_case1(FormulaPattern, CaseList),
    formula_pattern_create_term_case2(CaseList, InputColsList, InputNamesList, InputVarsList, FormulaTermsList),
    FormulaTerm = t(InputColsList, InputNamesList, InputVarsList, cases(FormulaTermsList)).

formula_pattern_create_term_case1([option(_,PatternThen,PatternCond)|R], [CaseTerm|S]) :-
    !,
    (integer(PatternCond) ->
        TermCond = t([],[],[],PatternCond)
    ;
        formula_pattern_create_term(PatternCond, TermCond)
    ),
    (integer(PatternThen) ->
        TermThen = t([],[],[],PatternThen)
    ;
        formula_pattern_create_term(PatternThen, TermThen)
    ),
    CaseTerm = if_then(TermCond, TermThen),
    formula_pattern_create_term_case1(R, S). 
formula_pattern_create_term_case1([option(FormulaPattern)], [CaseTerm]) :-
    (integer(FormulaPattern) ->
        CaseTerm = otherwise(t([],[],[],FormulaPattern))
    ;
        formula_pattern_create_term(FormulaPattern, CaseTerm)
    ).

formula_pattern_create_term_case2([CaseTerm|R], InputColsListNew, InputNamesListNew, InputVarsListNew, [FormulaTerm|S]) :-
    !,
    (CaseTerm = if_then(t(InputColsCond, InputNamesCond, InputVarsCond, TermCond),
                        t(InputColsThen, InputNamesThen, InputVarsThen, TermThen)) ->
        merge_lists(InputColsCond,InputColsThen,InputColsTerm),
        merge_lists(InputColsTerm,InputColsList,InputColsListNew),

        merge_lists(InputNamesCond,InputNamesThen,InputNamesTerm),
        merge_lists(InputNamesTerm,InputNamesList,InputNamesListNew),

        merge_lists(InputVarsCond,InputVarsThen,InputVarsTerm),
        merge_lists(InputVarsTerm,InputVarsList,InputVarsListNew),
        
        FormulaTerm = if_then(TermCond, TermThen)
    ;
     CaseTerm = otherwise(t(InputColsElse, InputNamesElse, InputVarsElse, TermElse)) ->
        merge_lists(InputColsElse,InputColsList,InputColsListNew),
        merge_lists(InputNamesElse,InputNamesList,InputNamesListNew),
        merge_lists(InputVarsElse,InputVarsList,InputVarsListNew),
        FormulaTerm = otherwise(TermElse)
    ;
        write(formula_pattern_create_term_case2(CaseTerm)),nl,halt
    ),
    formula_pattern_create_term_case2(R, InputColsList, InputNamesList, InputVarsList, S).
formula_pattern_create_term_case2([], [], [], [], []).


formula_pattern_create_term_bool(FormulaPattern, FormulaTerm) :-
    formula_pattern(mandatory_attr(AttrNames), FormulaPattern),                              % get parameters of Boolean formula
    formula_pattern(neg(Negated),              FormulaPattern),                              % get the fact whether Boolean formula is negated or not
    formula_pattern(shift(ShiftBool),          FormulaPattern),                              % get shift of the Boolean formula
                                                                                             % (min of output column) % NEWBOOL
    formula_pattern(op(Oplus),                 FormulaPattern),                              % get operator (and,or,allequal,sum,xor,voting,card1)
                                                                                             % of Boolean formula
    formula_pattern(nb_terms(NbTerms),         FormulaPattern),                              % get number of terms of Boolean formula
    formula_pattern(conds(Conds),              FormulaPattern),                              % get all potential conditions of Boolean formula
    findall(TabCol, member(TabCol, AttrNames),                            ListOfParemeters), % get parameters corresponding to mandatory attributes
    findall(Name,  (member(TCol,   AttrNames), tab_get_name(TCol, Name)), ListOfNames),      % get names of mandatory attributes
    length(AttrNames, LenParameters),                                                        % get number of used parameters
    length(ListOfSourceVariables, LenParameters),                                            % create a list of source variables
    formula_pattern_create_bool_conds(Conds, TConds, Oplus, ListOfSourceVariables),
    FormulaTerm = t(ListOfParemeters, ListOfNames, ListOfSourceVariables, bool(Negated,ShiftBool,Oplus,NbTerms,TConds)). % NEWBOOL

formula_pattern_create_bool_conds([], [], _, _) :- !.
formula_pattern_create_bool_conds([Cond|R], TConds, Oplus, ListOfSourceVariables) :-
    Cond = cond(CondB, CType, _LCondAttr, _LCondExtraVars, _LCondCostVars,
                CAttr1, CAttr2, CAttr3, CCst, CCoef,
                _SourceVarsLocal, _SourceVarsGlobal, _TargetVarsGlobal, _SourceCtrs, _CondCheckOrder),
    (CondB = -1 ->
        TConds = S
    ;
        UnshiftCAttr1 is CAttr1-1, % has to unshift reference to mandatory attributes in the context of Boolean formulae
        UnshiftCAttr2 is CAttr2-1, % as they were shifted in order to allow one to relax a condition of a Boolean formula
        UnshiftCAttr3 is CAttr3-1,
        formula_pattern_create_term_condition(ListOfSourceVariables, CondB, CType,
                                              UnshiftCAttr1, UnshiftCAttr2, UnshiftCAttr3,
                                              CCst, CCoef, CondTerm),
        TConds = [CondTerm|S]
    ),
    formula_pattern_create_bool_conds(R, S, Oplus, ListOfSourceVariables).

formula_pattern_create_term_cond(FormulaPattern, FormulaTerm) :-
    formula_pattern(mandatory_attr(AttrNames),                                          FormulaPattern),
    formula_pattern(then(TType, TAttr1, TAttr2,         TCst, TCoef, _, _, _, _, _, _), FormulaPattern),
    formula_pattern(else(EType, EAttr1, EAttr2,         ECst, ECoef, _, _, _, _, _, _), FormulaPattern),
    findall(TabCol, member(TabCol, AttrNames),                            ListOfParemeters), % get parameters corresponding to mandatory attributes
    findall(Name,  (member(TCol,   AttrNames), tab_get_name(TCol, Name)), ListOfNames),      % get names of mandatory attributes
    length(AttrNames, LenParameters),                                                        % get number of used parameters
    length(ListOfSourceVariables, LenParameters),                                            % create a list of source variables
    (formula_pattern(cond_ex, FormulaPattern) ->
        formula_pattern(neg(Negated),              FormulaPattern),                              % get the fact whether Boolean formula is negated or not
        formula_pattern(shift(ShiftBool),          FormulaPattern),                              % get shift of the Boolean formula
                                                                                                 % (min of output column) % NEWBOOL
        formula_pattern(op(Oplus),                 FormulaPattern),                              % get operator (and,or,allequal,sum,xor,voting,card1)
                                                                                                 % of Boolean formula
        formula_pattern(nb_terms(NbTerms),         FormulaPattern),                              % get number of terms of Boolean formula
        formula_pattern(conds(Conds),              FormulaPattern),                              % get all potential conditions of Boolean formula
        formula_pattern_create_bool_conds(Conds, TConds, Oplus, ListOfSourceVariables),
        CondTerm = bool(Negated,ShiftBool,Oplus,NbTerms,TConds),
        SimplifyThen = no
    ;
        formula_pattern(cond(CType, CAttr1, CAttr2, CAttr3, CCst, CCoef, _, _, _, _, _, _), FormulaPattern),
        formula_pattern_create_term_condition(ListOfSourceVariables, 1, CType, CAttr1, CAttr2, CAttr3, CCst, CCoef, CondTerm),
        (CType = attr_eq_coef ->         % if use condition of the form CAttr1=CCoef
                SimplifyThen = CAttr1-CCoef  % then will try to replace CAttr1 by CCoef in the then part
        ;
                SimplifyThen = no
        )
    ),
    formula_pattern_create_term_then_else(ListOfSourceVariables, TType, TAttr1, TAttr2, TCst, TCoef, ThenTerm, SimplifyThen),
    formula_pattern_create_term_then_else(ListOfSourceVariables, EType, EAttr1, EAttr2, ECst, ECoef, ElseTerm, no),
    FormulaTerm = t(ListOfParemeters, ListOfNames, ListOfSourceVariables, if(CondTerm, ThenTerm, ElseTerm)).

formula_pattern_create_term_condition(ListOfSourceVariables, 0, CType, CAttr1, CAttr2, CAttr3, CCst, CCoef, CondTerm) :- !,
    CondTerm = not(CondTermNegated),
    formula_pattern_create_term_condition(ListOfSourceVariables, 1, CType, CAttr1, CAttr2, CAttr3, CCst, CCoef, CondTermNegated).
formula_pattern_create_term_condition(ListOfSourceVariables, 1, CType, CAttr1, CAttr2, CAttr3, CCst, CCoef, CondTerm) :-
    functor(CType, CondType, _),
    (memberchk(CondType, [attr_eq_coef,       attr_eq_attr,       attr_eq_unary,
                          unary_term_eq_coef, binary_term_eq_coef              ]) ->
        CondTerm = eq(Arg1,Arg2)
    ;
     CondType = attr_in_interval ->
        CondTerm = in(Arg1,Arg2,Arg3)
    ;
     CondType = sum_leq_attr -> % cond(sum_leq_attr)  :      CondAttr1+CondAttr2                =< CondAttr3
        %CondTerm = sum_leq_attr(Arg1,Arg2,Arg3)
        CondTerm = leq(plus(Arg1,Arg2),Arg3)
    ;
     CondType = minus_mod_eq0 -> % cond(minus_mod_eq0) :     (CondAttr3-CondAttr1) mod CondAttr2 =  0
        %CondTerm = minus_mod_eq0(Arg1,Arg2,Arg3)
        CondTerm = eq(mod(minus(Arg3,Arg1),Arg2),0)
    ;
     CondType = mod_gt_mod ->
        CondTerm = gt(mod(Arg2,Arg3),mod(Arg1,Arg3))
    ;
     CondType = attr_geq_fmod -> % cond(attr_geq_fmod):                                CondAttr1 >= fmod(CondAttr2,CondAttr3)
        CondTerm = geq(Arg1,fmod(Arg2,Arg3))
    ;
     CondType = ceil_leq_floor -> %cond(ceil_leq_floor): ceil(CondAttr1-CondAttr3,CondAttr2)     =< floor(CondAttr1-CondAttr2,CondAttr3)
        %CondTerm = ceil_leq_floor(Arg1,Arg2,Arg3)
        CondTerm = leq(ceil(minus(Arg1,Arg3),Arg2),floor(minus(Arg1,Arg2),Arg3))
    ;
     memberchk(CondType, [attr_geq_coef, unary_term_geq_coef, binary_term_geq_coef]) ->
        CondTerm = geq(Arg1,Arg2)
    ;
     memberchk(CondType,[linear_eq_coef]) ->
        CondTerm = eq(Arg1,Arg2)
    ;
        CondTerm = leq(Arg1,Arg2)
    ),
    (memberchk(CondType, [attr_eq_attr,attr_leq_attr]) ->
        nth1(CAttr1, ListOfSourceVariables, Arg1),
        nth1(CAttr2, ListOfSourceVariables, Arg2)
    ;
     memberchk(CondType, [attr_eq_coef, attr_leq_coef, attr_geq_coef]) ->
        nth1(CAttr1, ListOfSourceVariables, Arg1),
        Arg2 = CCoef
    ;
     CondType = attr_in_interval ->
        nth1(CAttr1, ListOfSourceVariables, Arg1),
        Arg2 = CCst,
        Arg3 = CCoef
    ;
     memberchk(CondType, [attr_eq_unary, attr_leq_unary]) ->
        arg(1, CType, Func),
        nth1(CAttr1, ListOfSourceVariables, Arg1),
        functor(Arg2, Func, 2),
        nth1(CAttr2, ListOfSourceVariables, VAttr2),
        arg(1, Arg2, VAttr2),
        arg(2, Arg2, CCst)
    ;
     CondType = unary_leq_attr ->
        arg(1, CType, Func),
        functor(Arg1, Func, 2),
        nth1(CAttr1, ListOfSourceVariables, VAttr1),
        arg(1, Arg1, VAttr1),
        arg(2, Arg1, CCst),
        nth1(CAttr2, ListOfSourceVariables, Arg2)
    ;
     memberchk(CondType, [unary_term_eq_coef, unary_term_leq_coef, unary_term_geq_coef]) ->
        arg(1, CType, Func),
        functor(Arg1, Func, 2),
        nth1(CAttr1, ListOfSourceVariables, VAttr1),
        arg(1, Arg1, VAttr1),
        arg(2, Arg1, CCst),
        Arg2 = CCoef
    ;
     memberchk(CondType, [binary_term_eq_coef, binary_term_leq_coef, binary_term_geq_coef]) ->
        arg(1, CType, Func),
        (Func = mfloor -> arg(2, CType, MinAttr2), Arity = 3 ; Arity = 2),
        functor(Arg1, Func, Arity),
        nth1(CAttr1, ListOfSourceVariables, VAttr1),
        arg(1, Arg1, VAttr1),
        nth1(CAttr2, ListOfSourceVariables, VAttr2),
        arg(2, Arg1, VAttr2),
        (Func = mfloor -> arg(3, Arg1, MinAttr2) ; true),
        Arg2 = CCoef
    ; % CondCst*CondAttr1+CondAttr2+CondAttr3=CondCoef
     CondType = linear_eq_coef ->
        functor(Arg1, linear, 4),
        nth1(CAttr1, ListOfSourceVariables, VAttr1),
        nth1(CAttr2, ListOfSourceVariables, VAttr2),
        nth1(CAttr3, ListOfSourceVariables, VAttr3),
        arg(1, Arg1, VAttr1),
        arg(2, Arg1, VAttr2),
        arg(3, Arg1, VAttr3),
        arg(4, Arg1, CCst),
        Arg2 = CCoef
    ;
     memberchk(CondType, [sum_leq_attr, minus_mod_eq0, ceil_leq_floor,
                          attr_geq_fmod, mod_gt_mod]) ->
        nth1(CAttr1, ListOfSourceVariables, Arg1),
        nth1(CAttr2, ListOfSourceVariables, Arg2),
        nth1(CAttr3, ListOfSourceVariables, Arg3)
    ;
        write(formula_pattern_create_term_condition(CondType)), nl, halt
    ).

formula_pattern_create_term_then_else(ListOfSourceVariables, TType, TAttr1, TAttr2, TCst, TCoef, ThenTerm, SimplifyThen) :-
    functor(TType, ThenType, _),
    (ThenType = coef ->
        ThenTerm = TCoef
    ;
     ThenType = attr ->
        (SimplifyThen = TAttr1-CCoef ->                    % if use condition of the form CAttr1=CCoef
            ThenTerm = CCoef                               % and if CAttr1 = TAttr1 then put directly the value CCoef
        ;                                                  % otherwise put the name of TAttr1
            nth1(TAttr1, ListOfSourceVariables, ThenTerm)
        )
    ;
     ThenType = unary_term ->
        arg(1, TType, Func),
        (memberchk(Func,[max_min,min_min,floor_min]) -> arg(2, TType, MinAttr1), Arity = 3 ; Arity = 2),
        functor(ThenTerm, Func, Arity),
        nth1(TAttr1, ListOfSourceVariables, VAttr1),
        arg(1, ThenTerm, VAttr1),
        arg(2, ThenTerm, TCst),
        (Arity = 3 -> arg(3, ThenTerm, MinAttr1) ; true)
    ;
     ThenType = binary_term ->
        arg(1, TType, Func),
        (memberchk(Func, [linear,plus_min,plus_floor]) ->
            nth1(TAttr1, ListOfSourceVariables, VAttr1),
            nth1(TAttr2, ListOfSourceVariables, VAttr2),
            functor(ThenTerm, Func, 3),
            arg(1, ThenTerm, VAttr1),
            arg(2, ThenTerm, VAttr2),
            arg(3, ThenTerm, TCst)
        ;
         Func = minus_floor ->
            nth1(TAttr1, ListOfSourceVariables, VAttr1),
            nth1(TAttr2, ListOfSourceVariables, VAttr2),
            functor(ThenTerm, Func, 4),
            arg(1, ThenTerm, VAttr1),
            arg(2, ThenTerm, VAttr2),
            arg(3, ThenTerm, TCst),
            arg(4, ThenTerm, TCoef)
        ;
            (Func = mfloor -> arg(2, TType, MinAttr2), Arity = 3 ; Arity = 2),
            functor(ThenTerm, Func, Arity),
            nth1(TAttr1, ListOfSourceVariables, VAttr1),
            arg(1, ThenTerm, VAttr1),
            nth1(TAttr2, ListOfSourceVariables, VAttr2),
            arg(2, ThenTerm, VAttr2),
            (Func = mfloor -> arg(3, ThenTerm, MinAttr2) ; true)
        )
    ;
        write(formula_pattern_create_term_then_else), nl, halt
    ).

formula_pattern_create_term_formula(FormulaPattern, FormulaTerm) :-
    formula_pattern(f(F),                                           FormulaPattern),                  % get type of main formula
    formula_pattern(nu(NU),                                         FormulaPattern),                  % number of unary terms in the formula
    formula_pattern(unary(Unary),                                   FormulaPattern),                  % get unary terms
    formula_pattern(binary(Binary),                                 FormulaPattern),                  % get binary terms
    formula_pattern(attributes_use(AttributesUse),                  FormulaPattern),                  % get used attributes
    formula_pattern(mandatory_optional_attributes_names(AttrNames), FormulaPattern),                  % get names of the attributes
    length(AttrNames, NAttr),                                                                         % get number of attributes (mandatory + optional)
    LastU is NAttr + NU,                                                                              % index of last unary term
    CreateInfo = t(NAttr, LastU, Unary, Binary, AttributesUse),                                       % put together required params for creating a formula term
    (F = 0 ->                                                                                         % we have a constant
        formula_pattern(cst(Cst), FormulaPattern),                                                    % get the constant
        FormulaTerm = t([],[],[],Cst)                                                                 % create a formula term corresponding to a constant
    ;
        memberchk(a(ListA), AttributesUse),                                                           % get used attributes (list of 0/1)
        length(ListA, LenParameters),                                                                 % create the list of used parameters
        findall(Parameter, (I in 1..LenParameters,
                            indomain(I),
                            nth1(I, ListA, 1),
                            nth1(I, AttrNames, Parameter)), ListOfParemeters),                        % get names corresponding to the parameters
        findall(Name, (member(Parameter,ListOfParemeters),tab_get_name(Parameter,Name)), ListOfNames),
        length(ListOfSourceVariables, LenParameters),                                                 % create a list of source variables
        (F = 1 ->                                                                                     % we have a polynom
            formula_pattern(polynoms([Polynom]), FormulaPattern),                                     % get the unique polynom we have
            formula_pattern_create_polynom(Polynom, AttrNames,                                        % create monomes of the polynom 
                                           ListOfSourceVariables, 0, CreateInfo, Monomes),            % which use the source vars
            SourceTerm = polynom(Monomes),                                                            % create the source term
            FormulaTerm = t(ListOfParemeters, ListOfNames, ListOfSourceVariables, SourceTerm)         % create the formula term
        ;
        F = 2 ->                                                                                      % we have a unary function
            formula_pattern(unaryf([UnaryFunction]),       FormulaPattern),                           % get the unary function, and
            formula_pattern(polynoms([Polynom]),           FormulaPattern),                           % get the unique polynom we have
            formula_pattern_create_polynom(Polynom, AttrNames,                                        % create monomes of the polynom
                                           ListOfSourceVariables, 0, CreateInfo, Monomes),            % which use the source vars
            functor(UnaryFunction, UFunction, _),                                                     % get the type of unary function used
            (memberchk(UFunction, [min,max,floor,mod,power]) ->
                arg(1, UnaryFunction, Cst),
                functor(SourceTerm, UFunction, 2),
                arg(1, SourceTerm, polynom(Monomes)),
                arg(2, SourceTerm, Cst)
            ;
             memberchk(UFunction, [geq0, sum_consec]) ->
                functor(SourceTerm, UFunction, 1),
                arg(1, SourceTerm, polynom(Monomes))
            ;
             UFunction = in ->
                arg(1, UnaryFunction, Low),
                arg(2, UnaryFunction, Up),
                SourceTerm = in(polynom(Monomes), Low, Up)
            ;
                write(formula_pattern_create_term(UFunction)), nl, halt
            ),
            FormulaTerm = t(ListOfParemeters, ListOfNames, ListOfSourceVariables, SourceTerm)         % create the formula term
        ;
         F = 3 ->                                                                                     % we have a binary function
            formula_pattern(binaryf([BinaryFunction]),     FormulaPattern),                           % get the binary function, and
            formula_pattern(polynoms([Polynom1,Polynom2]), FormulaPattern),
            formula_pattern_create_polynom(Polynom1, AttrNames,                                       % create monomes of polynom 
                                           ListOfSourceVariables, 0, CreateInfo, Monomes1),           % which uses the source vars
            formula_pattern_create_polynom(Polynom2, AttrNames,                                       % create monomes of polynom 
                                           ListOfSourceVariables, 0, CreateInfo, Monomes2),           % which uses the source vars
            functor(BinaryFunction, BFunction, _),
            (memberchk(BFunction, [min,max,floor,ceil,mod,cmod,dmod,prod]) ->
                functor(SourceTerm, BFunction, 2),
                arg(1, SourceTerm, polynom(Monomes1)),
                arg(2, SourceTerm, polynom(Monomes2)),
                FormulaTerm = t(ListOfParemeters, ListOfNames, ListOfSourceVariables, SourceTerm)     % create the formula term
            ;
                write(formula_pattern_create_term(BFunction)), nl, halt
            )
        ;
            write(formula_pattern_create_term(F)), nl, halt
        )
    ).

formula_pattern_create_polynom([], _, _, _, _, []) :- !.
formula_pattern_create_polynom([t(_,_,0)], _, _, 1, _, []) :- !.                                   % skip last 0 coef if found some non zero coef
formula_pattern_create_polynom([t(_,_,0)], _, _, 0, _, [monome([],0)]) :- !.                       % don't skip last 0 coef if did not find non zero coef
formula_pattern_create_polynom([t(_,_,0)|R], AttrNames, ListOfSourceVariables, Diff0, CreateInfo, S) :- !, % skip 0 coef if not on last coef
    formula_pattern_create_polynom(R, AttrNames, ListOfSourceVariables, Diff0, CreateInfo, S).
formula_pattern_create_polynom([t(_,Degrees,Coef)|R], AttrNames, ListOfSourceVariables, _, CreateInfo, [monome(ListMonomeElems,Coef)|S]) :-
    get_non_zero_exponents(Degrees, 1, Exponents),
    formula_pattern_create_rest_monome(Exponents, AttrNames, ListOfSourceVariables, CreateInfo, ListMonomeElems),
    formula_pattern_create_polynom(R, AttrNames, ListOfSourceVariables, 1, CreateInfo, S).

formula_pattern_create_rest_monome([], _, _, _, []) :- !.
formula_pattern_create_rest_monome([Pos-Exponent|R], AttrNames, SourceVars, CreateInfo, [t(Source,Exponent)|S]) :-
    CreateInfo = t(NAttr, LastU, _Unary, _Binary, _AttributesUse),
    (Pos =< NAttr ->                                                            % if we are on a mandatory of optional attribute
        nth1(Pos, SourceVars, Source)                                           % then get its source variable
    ;
     Pos =< LastU ->                                                            % if we are on a unary term
        UPos is Pos - NAttr,
        formula_pattern_create_unary_term(UPos, SourceVars, CreateInfo, Source)
    ;                                                                           % if we are on a binary term
        BPos is Pos - LastU,
        formula_pattern_create_binary_term(AttrNames, BPos, SourceVars, CreateInfo, Source)
    ),
    formula_pattern_create_rest_monome(R, AttrNames, SourceVars, CreateInfo, S).

formula_pattern_create_unary_term(Pos, SourceVars, CreateInfo, Source) :-
    CreateInfo = t(_NAttr, _LastU, Unary, _Binary, AttributesUse),
    memberchk(u(ListsU), AttributesUse),
    nth1(Pos, ListsU, ListU),
    get_pos_value(ListU, 1, 1, 1, [I]),
    nth1(I, SourceVars, Var),
    nth1(Pos, Unary, UnaryTerm),
    functor(UnaryTerm, UT, _),
    (memberchk(UT, [min,max,floor,ceil,mod,geq,power]) -> arg(1, UnaryTerm, Cst), functor(Source, UT, 2), arg(1, Source, Var), arg(2, Source, Cst) ;
     UT = in                                           -> arg(1, UnaryTerm, Low), arg(2, UnaryTerm, Up), Source = in(Var, Low, Up)                 ;
     UT = sum_consec                                   -> Source = sum_consec(Var)                                                                 ;
                                                          write(formula_pattern_create_unary_term(UT)), nl, halt                                   ).

formula_pattern_create_binary_term(AttrNames, Pos, SourceVars, CreateInfo, Source) :-
    CreateInfo = t(_NAttr, _LastU, _Unary, Binary, AttributesUse),
    memberchk(b(ListsB), AttributesUse),
    nth1(Pos, ListsB, ListB),
    get_pos_value(ListB, 1, 1, 2, [I,J]),
    nth1(I, SourceVars, VarI),
    nth1(J, SourceVars, VarJ),
    nth1(Pos, Binary, BinaryTerm),
    (BinaryTerm = min(_)    -> Source = min(VarI,VarJ)                                                ;
     BinaryTerm = max(_)    -> Source = max(VarI,VarJ)                                                ;
     BinaryTerm = floor(0)  -> Source = floor(VarI,VarJ)                                              ;
     BinaryTerm = floor(1)  -> Source = floor(VarJ,VarI)                                              ;
     BinaryTerm = mfloor(0) -> tab_get_mins_attr_names(AttrNames, MinAttrs), nth1(J, MinAttrs, MinJ),
                               Source = mfloor(VarI,VarJ,MinJ)                                        ;
     BinaryTerm = mfloor(1) -> tab_get_mins_attr_names(AttrNames, MinAttrs), nth1(I, MinAttrs, MinI),
                               Source = mfloor(VarJ,VarI,MinI)                                        ;
     BinaryTerm = ceil(0)   -> Source = ceil(VarI,VarJ)                                               ;
     BinaryTerm = ceil(1)   -> Source = ceil(VarJ,VarI)                                               ;
     BinaryTerm = mod(0)    -> Source = mod(VarI,VarJ)                                                ;
     BinaryTerm = mod(1)    -> Source = mod(VarJ,VarI)                                                ;
     BinaryTerm = cmod(0)   -> Source = cmod(VarI,VarJ)                                               ;
     BinaryTerm = cmod(1)   -> Source = cmod(VarJ,VarI)                                               ;
     BinaryTerm = dmod(0)   -> Source = dmod(VarI,VarJ)                                               ;
     BinaryTerm = dmod(1)   -> Source = dmod(VarJ,VarI)                                               ;
     BinaryTerm = fmod(0)   -> Source = fmod(VarI,VarJ)                                               ;
     BinaryTerm = fmod(1)   -> Source = fmod(VarJ,VarI)                                               ;
     BinaryTerm = prod(_)   -> Source = prod(VarI,VarJ)                                               ;
                               write(formula_pattern_create_binary_term(BinaryTerm)), nl, halt        ).
