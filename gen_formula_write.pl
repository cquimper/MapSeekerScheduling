% Purpose: WRITE A FORMULA PATTERN TO THE CONSOLE
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_formula_write,[formula_pattern_write/1,           % write a formula in the console
                             formula_pattern_write_bool/1]).    % write a Boolean formula in the console

:- use_module(library(lists)).
:- use_module(utility).
:- use_module(table_access).

% Read formula pattern and translate it so it can be printed to the console
formula_pattern_write(FormulaPattern) :-
    (FormulaPattern = t(_,_,_,_) ->
        formula_pattern_write_term(FormulaPattern)
    ;
     formula_pattern(f(_), FormulaPattern) ->
        formula_pattern_write_formula(FormulaPattern) % polynomial or unary function or binary function
    ;
     formula_pattern(cond_ex, FormulaPattern) ->
        formula_pattern_write_cond(FormulaPattern)    % Conditional formula with Boolean condition
    ;
     formula_pattern(op(_), FormulaPattern) ->
        formula_pattern_write_bool(FormulaPattern)    % Boolean formula
    ;
     formula_pattern(decomp(_,_,_), FormulaPattern) ->
        formula_pattern_write_decomp(FormulaPattern)  % Decomposition formula
    ;
     formula_pattern(option(_,_,_), FormulaPattern) ->
        formula_pattern_write_case(FormulaPattern)    % case formula
    ;
        formula_pattern_write_cond(FormulaPattern)    % Conditional formula
    ).

formula_pattern_write_term(t(_,InputNames,InputVars,FormulaTerm)) :-
    copy_term(InputVars-FormulaTerm, InputVarsWrite-FormulaTermWrite),
    copy_term(InputNames, InputVarsWrite),
    formula_pattern_write_term1(FormulaTermWrite).

formula_pattern_write_term1(Term) :-
    var(Term), !,
    write(formula_pattern_write_term1(Term)),nl,halt.
formula_pattern_write_term1(Term) :-
    integer(Term), !,
    write(Term).
formula_pattern_write_term1(Term) :-
    atom(Term), !,
    write(Term).
formula_pattern_write_term1(abs(Term)) :-
    !,
    write('|'),
    formula_pattern_write_term1(Term),
    write('|').
formula_pattern_write_term1(prod(-1,Term)) :-
    !,
    write('-'),
    formula_pattern_write_term1(Term).
formula_pattern_write_term1(Term) :-
    functor(Term, Op, 2),
    memberchk(Op, [plus,minus,prod,power,min,max,floor,ceil,mod]),
    !,
    arg(1, Term, Term1),
    arg(2, Term, Term2),
    check_parenthesises(Term1, Flag1),
    check_parenthesises(Term2, Flag2),
    (Op = max     -> write('max(')      ;
     Op = min     -> write('min(')      ;
                     true               ),
    ((memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 1) -> write('(') ;
     (memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 2) -> write('[') ;
                                                                    true       ),
    formula_pattern_write_term1(Term1),
    ((memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 1) -> write(')') ;
     (memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 2) -> write(']') ;
                                                                    true       ),
    (Op = plus  -> write('+')           ;
     Op = minus -> write('-')           ;
     Op = prod  -> write('.')           ;
     Op = power -> write('^')           ;
     Op = floor -> write(' fdiv ')      ;
     Op = ceil  -> write(' cdiv ')      ;
     Op = mod   -> write(' mod ')       ;
                   write(',')           ),
    ((memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 1) -> write('(') ;
     (memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 2) -> write('[') ;
                                                                           true       ),
    formula_pattern_write_term1(Term2),
    ((memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 1) -> write(')') ;
     (memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 2) -> write(']') ;
                                                                           true       ),
    (Op = max   -> write(')')           ;
     Op = min   -> write(')')           ;
                   true                 ).
formula_pattern_write_term1(Term) :-
    functor(Term, Op, 2),
    memberchk(Op, [eq,geq,leq]),
    !,
    arg(1, Term, Term1),
    arg(2, Term, Term2),
    write('['),
    formula_pattern_write_term1(Term1),
    (Op = eq    -> write('=')                                           ;
     Op = geq   -> write('>=')                                          ;
     Op = leq   -> write('=<')                                          ;
                   write(formula_pattern_write_term1(Term)),halt        ),
    formula_pattern_write_term1(Term2),
    write(']').
formula_pattern_write_term1(Term) :-
    write(formula_pattern_write_term1(Term)),nl,halt.

check_parenthesises(Term, Flag) :-
    (var(Term) ->
         Flag = 0
    ;
     (integer(Term), Term > 0) ->
         Flag = 0
    ;
     (integer(Term), Term < 0) ->
         Flag = 1
    ;
     atom(Term) ->
         Flag = 0
    ;
     (functor(Term,Name,2),
      memberchk(Name,[prod,min,max]))->
         Flag = 0
    ;
     (memberchk(neg(0),Term), % non-negated Booleans w/o shifts
      memberchk(shift(0),Term),
      memberchk(op(Op),Term),
      nonmember(Op, [sum, xor])) ->
         Flag = 0
    ;
     (memberchk(neg(1),Term), % non-negated Booleans w/o shifts
      memberchk(shift(0),Term),
      memberchk(op(Op),Term),
      nonmember(Op, [sum, xor])) ->
         Flag = 2
    ;
     Term = t(_,_,_,TermTerm) ->
         check_parenthesises(TermTerm, Flag)
     ;
         Flag = 1
    ).

%[decomp(cond, CondTerm-ThenTerm, FormulaPatternElse)]
formula_pattern_write_decomp(FormulaPattern) :-
    FormulaPattern = [decomp(cond, FormulaPatternCond-FormulaPatternThen, FormulaPatternElse)],
    !,
    write('('),
    formula_pattern_write(FormulaPatternCond),
    write(' ? '),
    formula_pattern_write(FormulaPatternThen),
    write(' : '),
    formula_pattern_write(FormulaPatternElse),
    write(')').

formula_pattern_write_decomp(FormulaPattern) :-
    FormulaPattern = [decomp(Op, FormulaPattern1, FormulaPattern2)],
    check_parenthesises(FormulaPattern1, Flag1),
    check_parenthesises(FormulaPattern2, Flag2),
    (Op = min ->   write('min(');
     Op = max ->   write('max(');
                   true         ),
    ((memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 1) -> write('(') ;
     (memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 2) -> write('[') ;
                                                                    true),
    formula_pattern_write(FormulaPattern1),
    ((memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 1) -> write(')') ;
     (memberchk(Op, [power, prod, floor, ceil, mod]), Flag1 = 2) -> write(']') ;
                                                                    true       ),
    %write(' '), % 1:+,  2:*,  3:min,  4:max,  5:floor,  6:ceil
    (Op = plus  -> write(' + ')         ;
     Op = minus -> write(' - ')         ;
     Op = prod  -> write('.')           ;
     Op = floor -> write(' fdiv ')      ;
     Op = ceil  -> write(' cdiv ')      ;
                   write(', ')          ),
    %write(' '),
    ((memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 1) -> write('(') ;
     (memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 2) -> write('[') ;
                                                                           true       ),
    formula_pattern_write(FormulaPattern2),
    ((memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 1) -> write(')') ;
     (memberchk(Op, [minus, power, prod, floor, ceil, mod]), Flag2 = 2) -> write(']') ;
                                                                           true       ),
    (memberchk(Op, [min,max]) -> write(')') ; true).

formula_pattern_write_bool(FormulaPattern) :-
    formula_pattern(mandatory_attr(AttrNames), FormulaPattern), % get parameters of Boolean formula
    formula_pattern(neg(Negated),              FormulaPattern), % get the fact that the Boolean formula is negated or not
    formula_pattern(shift(ShiftBool),          FormulaPattern), % get shift of the Boolean formula (min of output column) % NEWBOOL
    formula_pattern(op(Oplus),                 FormulaPattern), % get operator (and,or,allequal,sum,xor,voting,card1) of Boolean formula
    formula_pattern(nb_terms(NbTerms),         FormulaPattern), % get number of terms of Boolean formula
    formula_pattern(conds(Conds),              FormulaPattern), % get all potential conditions of Boolean formula
    ((Oplus = sum, NbTerms > 1) ->
        ((Negated = 0, ShiftBool = 0) ->
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0)
        ;
         (Negated = 0, ShiftBool > 0) ->
            write(ShiftBool), write(' + '),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0)
        ;
         (Negated = 0, ShiftBool < 0) ->
            write('('),write(ShiftBool), write(') + '),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0)
        ;
         (Negated = 1) ->
            Shift is NbTerms+ShiftBool,
            (Shift < 0   -> write('('), write(Shift), write(') - (') ;
             Shift > 0   -> write(Shift), write(' - (')              ;
                            write('-(')                              ),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(')')
        ;
            write(formula_pattern_write_bool(Oplus,NbTerms,Negated,ShiftBool)), nl, halt
        )
    ;
     (Oplus = xor, NbTerms > 1) ->
        ((Negated = 0, ShiftBool = 0) ->
            write('('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(') mod 2')
        ;
         (Negated = 0, ShiftBool > 0) ->
            write(ShiftBool), write(' + ('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(') mod 2')
        ;
         (Negated = 0, ShiftBool > 0) ->
            write('('), write(ShiftBool), write(') + ('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(') mod 2')
        ;
         (Negated = 1) ->
            Shift is ShiftBool+1,
            (Shift \== 0 -> write('('), write(Shift), write(') - (') ;
                            write('-(')                              ),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(') mod 2')
        ;
            write(formula_pattern_write_bool(Oplus,NbTerms,Negated,ShiftBool)), nl, halt
        )
    ;
     (memberchk(Oplus,[voting,card1]), NbTerms > 1) ->
        ((Negated = 0, ShiftBool = 0) ->
            write(Oplus), write('('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(')')
        ;
         (Negated = 0, ShiftBool > 0) ->
            write(ShiftBool), write(' + '), write(Oplus), write('('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(')')
        ;
         (Negated = 0, ShiftBool < 0) ->
            write('('), write(ShiftBool), write(') + '), write(Oplus), write('('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(')')
        ;
         (Negated = 1, ShiftBool = 0) ->
            write('not '), write(Oplus), write('('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(')')
        ;
         (Negated = 1, ShiftBool > 0) ->
            write(ShiftBool), write(' + [not '), write(Oplus), write('('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(')]')
        ;
         (Negated = 1, ShiftBool < 0) ->
            write('('), write(ShiftBool), write(') + [not '), write(Oplus), write('('),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(')]')
        ;
            write(formula_pattern_write_bool(Oplus,NbTerms,Negated,ShiftBool)), nl, halt
        )
    ;
     (memberchk(Oplus,[or,and,allequal]), NbTerms > 1) ->
        ((Negated = 0, ShiftBool = 0) ->
            write('['),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']')
        ;
         (Negated = 0, ShiftBool > 0) ->
            write(ShiftBool), write(' + ['),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']')
        ;
         (Negated = 0, ShiftBool < 0) ->
            write('('), write(ShiftBool), write(') + ['),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']')
        ;
         (Negated = 1, ShiftBool = 0) ->
            write('not ['),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']')
        ;
         (Negated = 1, ShiftBool > 0) ->
            write(ShiftBool), write(' + [not ['),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']]')
        ;
         (Negated = 1, ShiftBool < 0) ->
            write('('), write(ShiftBool), write(') + [not ['),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']]')
        ;
            write(formula_pattern_write_bool(Oplus,NbTerms,Negated,ShiftBool)), nl, halt
        )
    ;
     (Oplus = and, NbTerms = 1) ->
        ((Negated = 0, ShiftBool = 0) ->
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0)
        ;
         (Negated = 0, ShiftBool > 0) ->
            write(ShiftBool), write(' + '),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0)
        ;
         (Negated = 0, ShiftBool < 0) ->
            write('('), write(ShiftBool), write(') + '),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0)
        ;
         (Negated = 1, ShiftBool = 0) ->
            write('not '),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0)
        ;
         (Negated = 1, ShiftBool > 0) ->
            write(ShiftBool), write(' + [not '),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']')
        ;
         (Negated = 1, ShiftBool < 0) ->
            write('('), write(ShiftBool), write(') + [not '),
            formula_pattern_write_bool_conds(Conds, AttrNames, Oplus, 0),
            write(']')
        ;
            write(formula_pattern_write_bool(Oplus,NbTerms,Negated,ShiftBool)), nl, halt
        )
    ;
        write(formula_pattern_write_bool(Oplus,NbTerms)), nl, halt
    ).

formula_pattern_write_bool_conds([], _, _, _) :- !.
formula_pattern_write_bool_conds([Cond|R], AttrNames, Oplus, NCond) :-
    Cond = cond(CondB, CondType, _LCondAttr, _LCondExtraVars, _LCondCostVars,
                CAttr1, CAttr2, CAttr3, CondCst, CondCoef,
                _SourceVarsLocal, _SourceVarsGlobal, _TargetVarsGlobal, _SourceCtrs, _CondCheckOrder),
    functor(CondType, Type, _),
    (CondB = -1 ->
        NCond1 = NCond
    ;
        NCond1 is NCond + 1,
        (memberchk(Type, [attr_eq_attr,attr_eq_coef,attr_eq_unary,unary_term_eq_coef,binary_term_eq_coef,minus_mod_eq0]) ->
            CondCmp = '='
        ;
         memberchk(Type, [attr_leq_attr,attr_leq_coef,attr_leq_unary,unary_leq_attr,unary_term_leq_coef,
                          binary_term_leq_coef,sum_leq_attr,ceil_leq_floor,attr_geq_fmod]) ->
            CondCmp = '=<'
        ;
            CondCmp = '>='
        ),
        UnshiftedCAttr1 is CAttr1-1,
        UnshiftedCAttr2 is CAttr2-1,
        UnshiftedCAttr3 is CAttr3-1,
        (UnshiftedCAttr1 > 0 -> get_attr_name(UnshiftedCAttr1, AttrNames, CName1) ; true), % when used, get name of the first  attribute
        (UnshiftedCAttr2 > 0 -> get_attr_name(UnshiftedCAttr2, AttrNames, CName2) ; true), % when used, get name of the second attribute
        (UnshiftedCAttr3 > 0 -> get_attr_name(UnshiftedCAttr3, AttrNames, CName3) ; true), % when used, get name of the third  attribute
        (NCond > 0 ->
            (Oplus = and      -> write(' and ') ;
             Oplus = or       -> write(' or ')  ;
             Oplus = allequal -> write(' = ')   ;
             Oplus = sum      -> write(' + ')   ;
             Oplus = xor      -> write(' + ')   ;
                                 write(', ')    )
        ;
            true
        ),
        (CondB = 0 -> write(' not ') ; true),
        write('['),
        (memberchk(Type,[attr_eq_attr,attr_leq_attr]) ->               % compare two attributes
            write(CName1), write(CondCmp), write(CName2)
        ;
         memberchk(Type,[attr_eq_coef,attr_leq_coef,attr_geq_coef]) -> % compare an attribute and a constant
            write(CName1), write(CondCmp), write(CondCoef)
        ;
         Type = attr_in_interval ->
            write(CName1), write(' in '), write(CondCst), write('..'), write(CondCoef)
        ;
         memberchk(Type,[attr_eq_unary,attr_leq_unary]) ->
            write(CName1), write(CondCmp), write(CondCst), write('.'), write(CName2)
        ;
         Type = unary_leq_attr ->
            write(CondCst), write('.'), write(CName1), write(CondCmp), write(CName2)
        ;
         memberchk(Type,[unary_term_eq_coef,unary_term_leq_coef,unary_term_geq_coef]) ->
            arg(1, CondType, Func),
            (Func = mod ->
                write(CName1), write(' mod '), write(CondCst), write(CondCmp), write(CondCoef)
            ;
                write(formula_pattern_write_bool_conds(CondType)), nl, halt
            )
        ;
         memberchk(Type,[binary_term_eq_coef,binary_term_leq_coef,binary_term_geq_coef]) ->
            arg(1, CondType, Func),
            (Func = plus ->
                write('('), write(CName1), write('+'), write(CName2), write(')')
            ;
             Func = minus ->
                write('('), write(CName1), write('-'), write(CName2), write(')')
            ;
             Func = abs ->
                write('|'), write(CName1), write('-'), write(CName2), write('|')
            ;
             memberchk(Func, [min, max]) ->
                write(Func), write('('), write(CName1), write(','), write(CName2), write(')')
            ;
             Func = prod ->
                write('('), write(CName1), write('.'), write(CName2), write(')')
            ;
             Func = floor ->
                write('('), write(CName1), write(' fdiv '), write(CName2), write(')')
            ;
             Func = mfloor ->
                arg(2, CondType, MinAttribute),
                write('('), write(CName1), write(' fdiv '), write('max('), write(CName2), write('-'), write(MinAttribute), write(',1))')
            ;
             Func = ceil ->
                 write('('), write(CName1), write(' cdiv '), write(CName2), write(')')
            ;
             Func = mod ->
                write('('), write(CName1), write(' mod '), write(CName2), write(')')
            ;
             Func = cmod ->
                write('('), write(CName1), write('-'), write(CName2), write(' mod '), write(CName1), write(')')
            ;
             Func = dmod ->
                write('('), write(CName1), write('-'), write(CName1), write(' mod '), write(CName2), write(')')
            ;
             Func = fmod ->
                write('('), write(CName1), write(' mod '), write(CName2), write('=0 ? '),
                write(CName2), write(' : '), write(CName1), write(' mod '), write(CName2), write(')')
            ;
                write(formula_pattern_write_bool_conds(CondType)), nl, halt
            ),
            write(CondCmp), write(CondCoef)
        ;
         Type = sum_leq_attr ->
            write('('), write(CName1), write('+'), write(CName2), write(')'), write(CondCmp), write(CName3)
        ;
         Type = attr_geq_fmod ->
            write('('), write(CName1), write('>='),
            write('('), write(CName2), write(' mod '), write(CName3), write('=0 ? '),
            write(CName3), write(' : '), write(CName2), write(' mod '), write(CName3), write(')')
        ;
         Type = minus_mod_eq0 ->
            write('(('), write(CName3), write('-'), write(CName1), write(')'), write(' mod '), write(CName2),  write(CondCmp), write('0)')
        ;
         Type = mod_gt_mod ->
            write('('), write(CName2), write(' mod '), write(CName3), write(') > ('),
            write(CName1), write(' mod '), write(CName3), write(')')
        ;
         Type = ceil_leq_floor ->
            write('(('), write(CName1), write('-'), write(CName3), write(') cdiv '), write(CName2), write(')'),
            write(CondCmp),
            write('(('), write(CName1), write('-'), write(CName2), write(') fdiv '), write(CName3), write(')')
        ;
            write(formula_pattern_write_bool_conds(CondType)), nl, halt
        ),
        write(']')
    ),
    formula_pattern_write_bool_conds(R, AttrNames, Oplus, NCond1).

formula_pattern_write_cond(FormulaPattern) :-
    formula_pattern(mandatory_attr(AttrNames),                                          FormulaPattern),
    formula_pattern(then(TType, TAttr1, TAttr2,         TCst, TCoef, _, _, _, _, _, _), FormulaPattern),
    formula_pattern(else(EType, EAttr1, EAttr2,         ECst, ECoef, _, _, _, _, _, _), FormulaPattern),
    write('('),
    (formula_pattern(cond_ex, FormulaPattern) ->
        formula_pattern_write_bool(FormulaPattern),
        SimplifyThen = no
    ;
        formula_pattern(cond(CType, CAttr1, CAttr2, CAttr3, CCst, CCoef, _, _, _, _, _, _), FormulaPattern),
        formula_pattern_write_condition(AttrNames, CType, CAttr1, CAttr2, CAttr3, CCst, CCoef),
        (CType = attr_eq_coef ->         % if use condition of the form CAttr1=CCoef
                SimplifyThen = CAttr1-CCoef  % then will try to replace CAttr1 by CCoef in the then part
        ;
                SimplifyThen = no
        )
    ),
    write(' ? '),
    formula_pattern_write_then_else(AttrNames, TType, TAttr1, TAttr2, TCst, TCoef, SimplifyThen),
    write(' : '),
    formula_pattern_write_then_else(AttrNames, EType, EAttr1, EAttr2, ECst, ECoef, no),
    write(')').

formula_pattern_write_condition(AttrNames, CType, CAttr1, CAttr2, CAttr3, CCst, CCoef) :-
    functor(CType, CondType, CondArity),
    (memberchk(CondType, [attr_eq_coef,
                          attr_eq_attr,
                          unary_term_eq_coef,
                          binary_term_eq_coef,
                          linear_eq_coef,
                          minus_mod_eq0]) ->                                                    % get comparison used in the condition
        CondCmp = '='
    ;
     CondType = attr_in_interval ->
        CondCmp = 'in'
    ;
        CondCmp = '=<'
    ),
    (CondArity = 1 -> arg(1, CType, CFunc)                          ;                           % get extra argument  of unary  func.of the comparison
     CondArity = 2 -> arg(1, CType, CFunc), arg(2, CType, CMinAttr) ;                           % get extra arguments of binary func.of the comparison
                      true                                          ),
    (CAttr1 > 0 -> get_attr_name(CAttr1, AttrNames, CName1) ; true),                            % when used, get name of the first  attribute
    (CAttr2 > 0 -> get_attr_name(CAttr2, AttrNames, CName2) ; true),                            % when used, get name of the second attribute
    (CAttr3 > 0 -> get_attr_name(CAttr3, AttrNames, CName3) ; true),                            % when used, get name of the third  attribute
    (memberchk(CondType,[attr_eq_attr,attr_leq_attr]) ->                                        % compare two attributes
        write(CName1), write(CondCmp), write(CName2)
    ;
     memberchk(CondType,[attr_eq_coef,attr_leq_coef]) ->                                        % compare an attribute with a coefficient
        write(CName1), write(CondCmp), write(CCoef)
    ;
     memberchk(CondType,[attr_in_interval]) ->                                                  % compare an attribute with an interval
        write(CName1), write(' '), write(CondCmp), write(' '),
        write(CCst), write('..'), write(CCoef)
    ;
     memberchk(CondType,[attr_leq_unary]) ->                                                    % compare an attribute with the prod unary function
        (CFunc = prod ->
            write(CName1), write(CondCmp),
            write(CCst), write('.'), write(CName2)
        ;
            write(formula_pattern_write_condition), nl, halt
        )
    ;
     memberchk(CondType,[unary_leq_attr]) ->                                                    % compare the prod unary function with an attribute
        (CFunc = prod ->
            write(CCst), write('.'), write(CName1),
            write(CondCmp), write(CName2)
        ;
            write(formula_pattern_write_condition), nl, halt
        )
    ;
     memberchk(CondType,[unary_term_eq_coef,unary_term_leq_coef]) ->                            % compare a unary function using an attribute and a
        (CFunc = plus ->
            write(CName1), write('+'), write(CCst)
        ;
         CFunc = minus ->
            write(CName1), write('-'), write(CCst)
        ;
         CFunc = prod ->
            write(CCst), write('.'), write(CName1)
        ;
         memberchk(CFunc, [min,max]) ->
            write(CFunc), write('('), write(CName1), write(','), write(CCst), write(')')
        ;
         CFunc = floor ->
            write(CName1), write(' fdiv '), write(CCst)
        ;
         CFunc = mod ->
            write(CName1), write(' mod '), write(CCst)
        ;
         CFunc = ceil ->
            write(CName1), write(' cdiv '), write(CCst)
        ;
            write(formula_pattern_write_condition), nl, halt
        ),
        write(CondCmp), write(CCoef)
    ;
     memberchk(CondType,[binary_term_eq_coef,binary_term_leq_coef]) ->                          % compare a binary function using two attributes with
        (CFunc = plus ->                                                                        % a coefficient
            write(CName1), write('+'), write(CName2)
        ;
         CFunc = minus ->
            write(CName1), write('-'), write(CName2)
        ;
         CFunc = abs ->
            write('|'), write(CName1), write('-'), write(CName2), write('|')
        ;
         memberchk(CFunc, [min,max]) ->
            write(CFunc), write('('), write(CName1), write(','), write(CName2), write(')')
        ;
         CFunc = floor ->
            write(CName1), write(' fdiv '), write(CName2)
        ;
         CFunc = mfloor ->
            write(CName1), write(' fdiv max('), write(CName2), write('-'), write(CMinAttr), write(',1)')
        ;
         CFunc = mod ->
            write(CName1), write(' mod '), write(CName2)
        ;
         CFunc = prod ->
            write(CName1), write('.'), write(CName2)
        ;
         CFunc = ceil ->
            write(CName1), write(' cdiv '), write(CName2)
        ;
         CFunc = cmod ->
            write(CName1), write('-'), write(CName2), write(' mod '), write(CName1)
        ;
         CFunc = dmod ->
            write(CName1), write('-'), write(CName1), write(' mod '), write(CName2)
        ;
         CFunc = fmod ->
            write('('), write(CName1), write(' mod '), write(CName2), write('=0 ? '),
            write(CName2), write(' : '), write(CName1), write(' mod '), write(CName2), write(')')
        ;
            write(formula_pattern_write_condition), nl, halt
        ),
        write(CondCmp), write(CCoef)
    ;
     CondType = linear_eq_coef ->
        write(CName2), write('+'), write(CName3), 
        (CCst =  1 -> write('+')                                           ;
         CCst = -1 -> write('-')                                           ;
         CCst >  0 -> write('+'), write(CCst), write('.')                  ;
         CCst <  0 -> CCst1 is -CCst, write('-'), write(CCst1), write('.') ;
                      write(formula_pattern_write_condition), nl, halt     ),
        write(CName1), write(CondCmp), write(CCoef)
    ;
     CondType = sum_leq_attr ->
        write(CName1), write('+'), write(CName2), write(CondCmp), write(CName3)
    ;
     CondType = minus_mod_eq0 ->
        write('('), write(CName3), write('-'), write(CName1), write(') mod '), write(CName2), write(CondCmp), write('0')
    ;
     CondType = attr_geq_fmod ->
        write('('), write(CName1), write('>='),
        write('('), write(CName2), write(' mod '), write(CName3), write('=0 ? '),
            write(CName3), write(' : '), write(CName2), write(' mod '), write(CName3), write(')')
    ;
     
        write(formula_pattern_write_condition), nl, halt
    ).

formula_pattern_write_then_else(AttrNames, TType, TAttr1, TAttr2, TCst, TCoef, SimplifyThen) :-
    functor(TType, ThenType, ThenArity),
    (ThenArity = 1 -> arg(1, TType, TFunc)                          ;                      % get extra arg. of unary func.(except max_min,floor_min)
     ThenArity = 2 -> arg(1, TType, TFunc), arg(2, TType, TMinAttr) ;                      % get extra arguments of binary function or of unary
                      true                                          ),                     % functions max_min or floor_min
    (TAttr1 > 0 -> get_attr_name(TAttr1, AttrNames, TName1) ; true),                       % when used, get name of the first attribute
    (TAttr2 > 0 -> get_attr_name(TAttr2, AttrNames, TName2) ; true),                       % when used, get name of the second attribute
    (ThenType = attr ->
        (SimplifyThen = TAttr1-CCoef ->                                                    % if use condition of the form CAttr1=CCoef
            write(CCoef)                                                                   % and if CAttr1 = TAttr1 then put directly the value CCoef
        ;                                                                                  % otherwise put the name of TAttr1
            write(TName1)
        )
    ;
     ThenType = coef ->
        write(TCoef)
    ;
     ThenType = unary_term ->
        (TFunc = plus ->
            write(TName1), write('+'), write(TCst)
        ;
         TFunc = minus ->
            write(TName1), write('-'), write(TCst)
        ;
         TFunc = prod ->
            write(TCst), write('.'), write(TName1)
        ;
         memberchk(TFunc,[min,max]) ->
            write(TFunc), write('('), write(TName1), write(','), write(TCst), write(')')
        ;
         TFunc = min_min ->
            (TCst > 0 -> true ; write(formula_pattern_write_then_else), nl, halt),
            write('min('), write(TName1), write('-'), write(TMinAttr), write(','), write(TCst), write(')')
        ;
         TFunc = max_min ->
            (TCst > 0 -> true ; write(formula_pattern_write_then_else), nl, halt),
            write('max('), write(TName1), write('-'), write(TMinAttr), write(','), write(TCst), write(')')
        ;
         TFunc = floor ->
            write(TName1), write(' fdiv '), write(TCst)
        ;
         TFunc = ceil ->
            write(TName1), write(' cdiv '), write(TCst)
        ;
         TFunc = floor_min ->
            (TCst > 0 -> true ; write(formula_pattern_write_then_else), nl, halt),
            write('('), write(TName1), write('-'), write(TMinAttr), write(') fdiv '), write(TCst)
        ;
         TFunc = mod ->
            write(TName1), write(' mod '), write(TCst)
        ;
            write(formula_pattern_write_then_else), nl, halt
        )
    ;
     ThenType = binary_term ->
        (TFunc = plus ->
            write(TName1), write('+'), write(TName2)
        ;
         TFunc = minus ->
            write(TName1), write('-'), write(TName2)
        ;
         TFunc = linear ->
            write(TName1),
            (TCst < 0 ->
                TCst1 is -TCst, write('-'), write(TCst1), write('.'), write(TName2)
            ;
                write('+'),  write(TCst), write('.'), write(TName2)
            )
        ;
         TFunc = abs ->
            write('|'), write(TName1), write('-'), write(TName2), write('|')
        ;
         memberchk(TFunc,[min,max]) ->
            write(TFunc), write('('), write(TName1), write(','), write(TName2), write(')')
        ;
         TFunc = plus_min ->
            write('min('), write(TName1), write('+'), write(TName2), write('-'), write(TCst), write(','), write(TName1), write(')')
        ;
         TFunc = prod ->
            write(TName1), write('.'), write(TName2)
        ;
         TFunc = floor ->
            write(TName1), write(' fdiv '), write(TName2)
        ;
         TFunc = mfloor ->
            write(TName1), write(' fdiv max('), write(TName2), write('-'), write(TMinAttr), write(',1)')
        ;
         TFunc = ceil ->
            write(TName1), write(' cdiv '), write(TName2)
        ;
         TFunc = plus_floor ->
            write('('), write(TName1), write('+'), write(TName2),
            (TCst < 0 ->
                TCst1 is -TCst, write('-'), write(TCst1)
            ;
                write('+'),  write(TCst)
            ),
            write(') fdiv '), write(TName1)
        ;
         TFunc = minus_floor ->
            write('('), write(TName1), write('-'), write(TName2),
            (TCst = 0 -> true                                    ;
             TCst > 0 -> write('+'), write(TCst)                 ;
                         write('-'), TCst1 is -TCst, write(TCst1)),
            write(') fdiv '), write(TCoef)
        ;
         TFunc = mod ->
            write(TName1), write(' mod '), write(TName2)
        ;
         TFunc = cmod ->
            write(TName1), write('-'), write(TName2), write(' mod '), write(TName1)
        ;
         TFunc = dmod ->
            write(TName1), write('-'), write(TName1), write(' mod '), write(TName2)
        ;
         TFunc = fmod ->
            write('('), write(TName1), write(' mod '), write(TName2), write('=0 ? '),
            write(TName2), write(' : '), write(TName1), write(' mod '), write(TName2), write(')')
        ;
            write(formula_pattern_write_then_else), nl, halt
        )
    ;
        write(formula_pattern_write_then_else), nl, halt
    ).

formula_pattern_write_case(FormulaPattern) :-
    write('('),
    formula_pattern_write_cases(FormulaPattern),
    write(')').

formula_pattern_write_cases([option(_,Val,BoolCond)|R]) :-
    formula_pattern_write_bool(BoolCond),
    write(' ? '), write(Val),
    formula_pattern_write_cases1(R).

formula_pattern_write_cases1([option(_,Val,BoolCond)|R]) :-
    !,
    write(' : '),
    formula_pattern_write_bool(BoolCond),
    write(' ? '), write(Val),
    formula_pattern_write_cases1(R).
formula_pattern_write_cases1([option(Val)]) :-
    write(' : '), write(Val).

formula_pattern_write_formula(FormulaPattern) :-
    formula_pattern(f(F),                                           FormulaPattern),           % get type of main formula
    formula_pattern(nu(NU),                                         FormulaPattern),           % number of unary terms in the formula
    formula_pattern(unary(Unary),                                   FormulaPattern),           % get unary terms
    formula_pattern(binary(Binary),                                 FormulaPattern),           % get binary terms
    formula_pattern(mandatory_optional_attributes_names(AttrNames), FormulaPattern),           % get names of the attributes
    length(AttrNames, NAttr),                                                                  % get number of attributes (mandatory + optional)
    LastU is NAttr + NU,                                                                       % index of last unary term
    PrintInfo = t(NAttr, LastU, Unary, Binary, AttrNames, AttributesUse),                      % put together required parameters for writing a formula
    (F = 0 ->                                                                                  % we have a constant
        formula_pattern(cst(Cst), FormulaPattern),                                             % get the constant
        write(Cst)
    ;
     F = 1 ->                                                                                  % we have a polynom
        formula_pattern(attributes_use(AttributesUse), FormulaPattern),                        % get the polynom
        formula_pattern(polynoms([Polynom]),           FormulaPattern),
        formula_pattern_write_polynom(Polynom, PrintInfo, 1, 0)
    ;
     F = 2 ->                                                                                  % we have a unary function
        formula_pattern(unaryf([UnaryFunction]),       FormulaPattern),                        % get the unary function, and
        formula_pattern(attributes_use(AttributesUse), FormulaPattern),                        % get the polynom
        formula_pattern(polynoms([Polynom]),           FormulaPattern),
        functor(UnaryFunction, UFunction, _),
        (UFunction = min ->                                                                    % min(Polynom,Cst)
            arg(1, UnaryFunction, Cst), write('min('),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 0),
            write(','), write(Cst), write(')')                       ;
         UFunction = max ->                                                                    % max(Polynom,Cst)
            arg(1, UnaryFunction, Cst), write('max('),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 0),
            write(','), write(Cst), write(')')                       ;
         UFunction = floor ->                                                                  % Polynom div Cst
            arg(1, UnaryFunction, Cst),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 1),
            write(' fdiv '), write(Cst)                              ;
         UFunction = mod ->                                                                    % Polynom mod Cst
            arg(1, UnaryFunction, Cst),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 1),
            write(' mod '), write(Cst)                               ;
         UFunction = geq0 ->                                                                   % [Polynom >= 0]
            write('['),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 1),
            write(' >= 0]')                                          ;
         UFunction = in ->                                                                     % [Polynom in Low..Up]
            arg(1, UnaryFunction, Low), arg(2, UnaryFunction, Up), write('['),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 1),
            write(' in '), write(Low), write('..'), write(Up), write(']');
         UFunction = power ->                                                                  % Polynom^power
            arg(1, UnaryFunction, Degree),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 1),
            write('^'), write(Degree)                                ;
         UFunction = sum_consec ->                                                             % Polynom.(Polynom+1)/2
            write('('),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 1),
            write('.('),
            formula_pattern_write_polynom(Polynom, PrintInfo, 1, 0),
            write(' + 1)) fdiv 2')                                   ;
            write(formula_pattern_write_formula), nl, halt
        )
    ;
     F = 3 ->                                                                                  % we have a binary function
        formula_pattern(binaryf([BinaryFunction]),     FormulaPattern),                        % get the binary function, and
        formula_pattern(attributes_use(AttributesUse), FormulaPattern),                        % get the polynom
        formula_pattern(polynoms([Polynom1,Polynom2]), FormulaPattern),
        functor(BinaryFunction, BFunction, _),
        (BFunction = min ->                                                                    % min(Polynom1,Polynom2)
            write('min('), formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 0),
            write(','), formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 0), write(')') ;
         BFunction = max ->                                                                    % max(Polynom1,Polynom2)
            write('max('), formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 0),
            write(','), formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 0), write(')') ;
         BFunction = floor ->                                                                  % Polynom1 div Polynom2
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 1), write(' fdiv '),
            formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 1)                         ;
         BFunction = ceil ->                                                                  % Polynom1 div Polynom2
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 1), write(' cdiv '),
            formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 1)                         ;
         BFunction = mod ->                                                                    % Polynom1 mod Polynom1
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 1), write(' mod '),
            formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 1)                         ;
         BFunction = cmod ->                                                                   % Polynom1 - Polynom2 mod Polynom1
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 0), write(' - '),
            formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 1), write(' mod '),
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 1)                         ;
         BFunction = dmod ->                                                                   % Polynom1 - Polynom1 mod Polynom2
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 0), write(' - '),
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 1), write(' mod '),
            formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 1)                         ;
         BFunction = prod ->                                                                   % Polynom1 . Polynom2
            formula_pattern_write_polynom(Polynom1, PrintInfo, 1, 1), write(' . '),
            formula_pattern_write_polynom(Polynom2, PrintInfo, 1, 1)                         ;
            write(formula_pattern_write_formula), nl, halt
        )
    ;
        write(formula_pattern_write_formula), nl, halt
    ).

formula_pattern_write_polynom(Monomes, PrintInfo, First, Parenthesis) :-
    ((Parenthesis=1, formula_pattern_more_than_one_monome(Monomes, 2)) ->
        write('('),
        formula_pattern_write_polynom(Monomes, PrintInfo, First),
        write(')')
    ;
        formula_pattern_write_polynom(Monomes, PrintInfo, First)
    ).

formula_pattern_more_than_one_monome(_, 0) :- !.
formula_pattern_more_than_one_monome([t(_,_,Coef)|R], I) :-
    integer(Coef), Coef = 0, !,
    formula_pattern_more_than_one_monome(R, I).
formula_pattern_more_than_one_monome([_|R], I) :-
    I1 is I-1,
    formula_pattern_more_than_one_monome(R, I1).

formula_pattern_write_polynom([], _, _) :- !.
formula_pattern_write_polynom([Monome|R], PrintInfo, First) :-
    formula_pattern_write_monome(Monome, PrintInfo, First, NewFirst),
    formula_pattern_write_polynom(R, PrintInfo, NewFirst).

formula_pattern_write_monome(t(_,_,Coef), _, First, First) :-
    integer(Coef),
    Coef = 0,
    !.
formula_pattern_write_monome(t(_,Degrees,Coef), PrintInfo, First, 0) :-
    (Coef > 0 -> Pos = 1, AbsCoef is Coef ; Pos = 0, AbsCoef is -Coef),
    get_non_zero_exponents(Degrees, 1, Exponents),
    length(Exponents, N),
    ((N = 0, First = 1)            -> write(Coef)                                                                              ;
     (N = 0, First = 0, Pos  =  1) -> write(' + '), write(AbsCoef)                                                             ;
     (N = 0, First = 0, Pos  =  0) -> write(' - '), write(AbsCoef)                                                             ;
     (N > 0, First = 1, Coef =  1) -> formula_pattern_write_rest_monome(Exponents, PrintInfo, 1)                               ;
     (N > 0, First = 1, Coef = -1) -> write('-'), formula_pattern_write_rest_monome(Exponents, PrintInfo, 1)                   ;
     (N > 0, First = 1)            -> write(Coef), formula_pattern_write_rest_monome(Exponents, PrintInfo, 0)                  ;
     (N > 0, First = 0, Coef =  1) -> write(' + '), formula_pattern_write_rest_monome(Exponents, PrintInfo, 1)                 ;
     (N > 0, First = 0, Coef = -1) -> write(' - '), formula_pattern_write_rest_monome(Exponents, PrintInfo, 1)                 ;
     (N > 0, First = 0, Pos  =  1) -> write(' + '), write(AbsCoef), formula_pattern_write_rest_monome(Exponents, PrintInfo, 0) ;
     (N > 0, First = 0, Pos  =  0) -> write(' - '), write(AbsCoef), formula_pattern_write_rest_monome(Exponents, PrintInfo, 0) ;
                                      write(formula_pattern_write_monome), nl, halt                                            ).

formula_pattern_write_rest_monome([], _, _) :- !.
formula_pattern_write_rest_monome([Pos-Exponent|R], PrintInfo, First) :-
    PrintInfo = t(NAttr, LastU, _Unary, _Binary, AttrNames, _AttributesUse),
    (First = 0 -> write('.') ; true),
    (Pos =< NAttr ->                         % if we are on a mandatory of optional attribute
        get_attr_name(Pos, AttrNames, Name), % then get its name
        write(Name)                          % and print it directly
    ;
     Pos =< LastU ->                         % if we are on a unary term
        UPos is Pos - NAttr,
        formula_pattern_write_unary_term(UPos, PrintInfo)
    ;                                        % if we are on a binary term
        BPos is Pos - LastU,
        formula_pattern_write_binary_term(BPos, PrintInfo)
    ),
    (Exponent > 1 -> write('^'), write(Exponent) ; true),
    formula_pattern_write_rest_monome(R, PrintInfo, 0).

formula_pattern_write_unary_term(Pos, PrintInfo) :-
    PrintInfo = t(_NAttr, _LastU, Unary, _Binary, AttrNames, AttributesUse),
    memberchk(u(ListsU), AttributesUse),
    nth1(Pos, ListsU, ListU),
    get_pos_value(ListU, 1, 1, 1, [I]),
    get_attr_name(I, AttrNames, Name),
    nth1(Pos, Unary, UnaryTerm),
    functor(UnaryTerm, UT, _),
    (UT = min        -> arg(1, UnaryTerm, Cst), write('min('), write(Name), write(','), write(Cst), write(')')    ;
     UT = max        -> arg(1, UnaryTerm, Cst), write('max('), write(Name), write(','), write(Cst), write(')')    ;
     UT = floor      -> arg(1, UnaryTerm, Cst), write('('), write(Name), write(' fdiv '), write(Cst), write(')')  ;
     UT = ceil       -> arg(1, UnaryTerm, Cst), write('('), write(Name), write(' cdiv '), write(Cst), write(')')  ;
     UT = mod        -> arg(1, UnaryTerm, Cst), write('('), write(Name), write(' mod '), write(Cst), write(')')   ;
     UT = geq        -> arg(1, UnaryTerm, Cst), write('['), write(Name), write(' >= '), write(Cst), write(']')    ;
     UT = in         -> arg(1, UnaryTerm, Low), arg(2, UnaryTerm, Up), write('['), write(Name), write(' in '),
                        write(Low), write('..'), write(Up), write(']')                                            ;
     UT = power      -> arg(1, UnaryTerm, Degree), write(Name), write('^'), write(Degree)                         ;
     UT = sum_consec -> write('(('), write(Name), write('.('), write(Name), write('+1)) fdiv 2)')                 ;
                        write(formula_pattern_write_unary_term), nl, halt                                         ).

formula_pattern_write_binary_term(Pos, PrintInfo) :-
    PrintInfo = t(_NAttr, _LastU, _Unary, Binary, AttrNames, AttributesUse),
    memberchk(b(ListsB), AttributesUse),
    nth1(Pos, ListsB, ListB),
    get_pos_value(ListB, 1, 1, 2, [I,J]),
    get_attr_name(I, AttrNames, NameI),
    get_attr_name(J, AttrNames, NameJ),
    nth1(Pos, Binary, BinaryTerm),
    (BinaryTerm = min(_)    -> write('min('), write(NameI), write(','), write(NameJ), write(')')                                      ;
     BinaryTerm = max(_)    -> write('max('), write(NameI), write(','), write(NameJ), write(')')                                      ;
     BinaryTerm = floor(0)  -> write('('), write(NameI), write(' fdiv '), write(NameJ), write(')')                                    ;
     BinaryTerm = floor(1)  -> write('('), write(NameJ), write(' fdiv '), write(NameI), write(')')                                    ;
     BinaryTerm = mfloor(0) -> tab_get_mins_attr_names(AttrNames, MinAttrs), nth1(J, MinAttrs, MinNameJ),
                               write('('), write(NameI), write(' fdiv max('), write(NameJ), write('-'), write(MinNameJ), write(',1))');
     BinaryTerm = mfloor(1) -> tab_get_mins_attr_names(AttrNames, MinAttrs), nth1(I, MinAttrs, MinNameI),
                               write('('), write(NameJ), write(' fdiv max('), write(NameI), write('-'), write(MinNameI), write(',1))');
     BinaryTerm = ceil(0)   -> write('('), write(NameI), write(' cdiv '), write(NameJ), write(')')                                    ;
     BinaryTerm = ceil(1)   -> write('('), write(NameJ), write(' cdiv '), write(NameI), write(')')                                    ;
     BinaryTerm = mod(0)    -> write('('), write(NameI), write(' mod '), write(NameJ), write(')')                                     ;
     BinaryTerm = mod(1)    -> write('('), write(NameJ), write(' mod '), write(NameI), write(')')                                     ;
     BinaryTerm = cmod(0)   -> write('('), write(NameI), write('-'), write(NameJ), write(' mod '), write(NameI), write(')')           ;
     BinaryTerm = cmod(1)   -> write('('), write(NameJ), write('-'), write(NameI), write(' mod '), write(NameJ), write(')')           ;
     BinaryTerm = dmod(0)   -> write('('), write(NameI), write('-'), write(NameI), write(' mod '), write(NameJ), write(')')           ;
     BinaryTerm = dmod(1)   -> write('('), write(NameJ), write('-'), write(NameJ), write(' mod '), write(NameI), write(')')           ;
     BinaryTerm = fmod(0)   -> write('('), write(NameI), write(' mod '), write(NameJ), write('=0 ? '), write(NameJ), write(' : '),
                               write(NameI), write(' mod '), write(NameJ), write(')')                                                 ;
     BinaryTerm = fmod(1)   -> write('('), write(NameJ), write(' mod '), write(NameI), write('=0 ? '), write(NameI), write(' : '),
                               write(NameJ), write(' mod '), write(NameI), write(')')                                                 ;
     BinaryTerm = prod(_)   -> write('('), write(NameI), write('.'), write(NameJ), write(')')                                         ;
                               write(formula_pattern_write_binary_term), nl, halt                                                     ).

get_attr_name(I, AttrNames, Name) :-
    nth1(I, AttrNames, TabCol), % get the reference to the corresponding table and column
    tab_get_name(TabCol, Name). % get the name associated with the column
