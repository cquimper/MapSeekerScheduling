% Purpose: Eval a formula term (used to check acquired conjectures or to learn by transfer)
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(eval_formula, [eval_formula_term/2]).

eval_formula_term(FormulaTerm, _) :- % first catch the case when FormulaTerm was incorrectly generated
    \+ ground(FormulaTerm),          % in order to avoid getting in an infinite recursive loop
    !,
    write(eval_formula_term(FormulaTerm)), nl, halt.
eval_formula_term(FormulaTerm, Val) :-
    integer(FormulaTerm),
    !,
    Val = FormulaTerm.
eval_formula_term(FormulaTerm, Val) :-
    FormulaTerm = not(NegatedFormulaTerm),
    !,
    eval_formula_term(NegatedFormulaTerm, NegVal),
    Val is 1-NegVal.
eval_formula_term(FormulaTerm, Val) :-
    FormulaTerm = abs(FormulaTerm),
    !,
    eval_formula_term(FormulaTerm, ValAbs),
    Val is abs(ValAbs).
eval_formula_term(FormulaTerm, Val) :- % see booleans in gen_bool_formula.pl
    FormulaTerm = bool(Negated, ShiftBool, Oplus, NbTerms, Conds),
    memberchk(Oplus, [and,or,allequal,xor,sum,voting,card1]),
    !,
    eval_boolean_conds(Conds, Oplus, Value),
    (Negated = 1 ->
        (Oplus = sum ->
            Val is ShiftBool+NbTerms-Value
        ;
            Val is ShiftBool+1-Value
        )
    ;
        Val is ShiftBool+Value
    ).
eval_formula_term(FormulaTerm, Val) :- % see conditional in gen_cond_formula.pl
    functor(FormulaTerm, if, 3), !,
    arg(1, FormulaTerm, Cond),
    eval_formula_term(Cond, CondVal),
    (CondVal = 1 ->
        arg(2, FormulaTerm, Then),
        eval_formula_term(Then, Val)
    ;
        arg(3, FormulaTerm, Else),
        eval_formula_term(Else, Val)
    ).
eval_formula_term(FormulaTerm, Val) :- % see conditional in gen_cond_formula.pl
    functor(FormulaTerm, in, 3), !,
    arg(1, FormulaTerm, Arg1),
    arg(2, FormulaTerm, Arg2),
    arg(3, FormulaTerm, Arg3),
    eval_formula_term(Arg1, Val1),
    ((Val1 >= Arg2, Val1 =< Arg3) -> Val = 1 ; Val = 0).
eval_formula_term(FormulaTerm, Val) :- % see conditional in gen_cond_formula.pl
    functor(FormulaTerm, Cond, 2),
    memberchk(Cond, [eq,leq,geq,gt]), !,
    arg(1, FormulaTerm, Arg1),
    arg(2, FormulaTerm, Arg2),
    eval_formula_term(Arg1, Val1),
    eval_formula_term(Arg2, Val2),
    (Cond = eq ->
        (Val1 =  Val2 -> Val = 1 ; Val  = 0)
    ;
     Cond = leq ->
        (Val1 =< Val2 -> Val = 1 ; Val  = 0)
    ;
     Cond = gt ->
        (Val1 >  Val2 -> Val = 1 ; Val  = 0)
    ;
        (Val1 >= Val2 -> Val = 1 ; Val  = 0)
    ).
eval_formula_term(FormulaTerm, Val) :-
    functor(FormulaTerm, polynom, 1), !,
    arg(1, FormulaTerm, Monomes),
    eval_and_sumup_monomes(Monomes, 0, Val).
eval_formula_term(FormulaTerm, Val) :-
    functor(FormulaTerm, Functor, 1),
    memberchk(Functor, [geq0,sum_consec,square,sqrt]), !,
    arg(1, FormulaTerm, Arg1),
    eval_formula_term(Arg1, Val1),
    (Functor = geq0       -> (Val1 >= 0 -> Val = 1 ; Val = 0 )               ;
     Functor = sum_consec -> Val is (Val1 * (Val1 + 1)) // 2                 ;
     Functor = square     -> Val is Val1*Val1                                ;
     Functor = sqrt       -> Val is sqrt(Val1)                               ;
                             write(eval_formula_term1(FormulaTerm)), nl, halt).
eval_formula_term(FormulaTerm, Val) :- % see conditional in gen_cond_formula.pl
    functor(FormulaTerm, Functor, 2),
    memberchk(Functor, [plus,minus,abs,min,max,prod,floor,ceil,mod,cmod,dmod,fmod,power,fdiv,ffloor,fceil]), !,
    arg(1, FormulaTerm, Arg1),
    arg(2, FormulaTerm, Arg2),
    eval_formula_term(Arg1, Val1),
    eval_formula_term(Arg2, Val2),
    (Functor = plus   -> Val is Val1+Val2                                                               ;
     Functor = minus  -> Val is Val1-Val2                                                               ;
     Functor = abs    -> Val is abs(Val1-Val2)                                                          ;
     Functor = min    -> Val is min(Val1,Val2)                                                          ;
     Functor = max    -> Val is max(Val1,Val2)                                                          ;
     Functor = prod   -> Val is Val1 * Val2                                                             ;
     Functor = floor  -> Val2 \== 0, Val is Val1 // Val2                                                ;
     Functor = ceil   -> Val2 \== 0, Val is (Val1 + Val2 - 1) // Val2                                   ;
     Functor = mod    -> Val2 \== 0, Val is Val1 mod Val2                                               ;
     Functor = cmod   -> Val1 \== 0, Val is Val1 - (Val2 mod Val1)                                      ;
     Functor = dmod   -> Val2 \== 0, Val is Val1 - (Val1 mod Val2)                                      ;
     Functor = fmod   -> Val2 \== 0, Mod is Val1 mod Val2, (Mod = 0 -> Val is Val2 ; Val is Mod)        ;
     Functor = power  -> Val is Val1 ^ Val2                                                             ;
     Functor = fdiv   -> Val2 \== 0, Val is Val1 / Val2                                                 ;
     Functor = ffloor -> Val2 \== 0, Tempo is Val1 / Val2, Val is floor(Tempo)                          ;
     Functor = fceil  -> Tempo is Val1 / Val2, Val is ceiling(Tempo)                                    ;
                         write(eval_formula_term2(FormulaTerm)), nl, halt                               ).
eval_formula_term(FormulaTerm, Val) :- % see conditional in gen_cond_formula.pl
    functor(FormulaTerm, Functor, 3),
    memberchk(Functor, [in,min_min,max_min,floor_min,mfloor,linear,plus_min,plus_floor,
                        sum_leq_attr,minus_mod_eq0,ceil_leq_floor]), !,
    arg(1, FormulaTerm, Arg1),
    arg(2, FormulaTerm, Arg2),
    arg(3, FormulaTerm, Arg3),
    eval_formula_term(Arg1, Val1),
    eval_formula_term(Arg2, Val2),
    eval_formula_term(Arg3, Val3),
    (Functor = in             -> ((Val1 >= Val2, Val1 =< Val3) -> Val = 1 ; Val = 0)                            ;
     Functor = min_min        -> Val is min(Val1-Val3,Val2)                                                     ;
     Functor = max_min        -> Val is max(Val1-Val3,Val2)                                                     ;
     Functor = floor_min      -> Val is (Val1-Val3) // Val2                                                     ;
     Functor = mfloor         -> Val is Val1 // max(Val2-Val3,1)                                                ;
     Functor = linear         -> Val is Val1+Val3*Val2                                                          ; 
     Functor = plus_min       -> Val is min(Val1+Val2-Val3,Val1)                                                ;
     Functor = plus_floor     -> Val1 \== 0, Val is (Val1+Val2+Val3) // Val1                                    ;
     Functor = sum_leq_attr   -> Val12 is Val1+Val2, (Val12 =< Val3 -> Val = 1 ; Val = 0)                       ;
     Functor = minus_mod_eq0  -> Val2 \== 0, Val123 is (Val3-Val1) mod Val2, (Val123 = 0 -> Val = 1 ; Val = 0)  ;
     Functor = ceil_leq_floor -> Val2 \== 0, Val132 is ((Val1 - Val3 + Val2 - 1) // Val2),
                                 Val3 \== 0, Val123 is ((Val1 - Val2) // Val3),
                                 (Val132 =< Val123 -> Val = 1; Val = 0)                                         ;
                                 write(eval_formula_term3(FormulaTerm)), nl, halt                               ).
eval_formula_term(FormulaTerm, Val) :- % see conditional linear_eq_coef in gen_cond_formula.pl
    functor(FormulaTerm, linear, 4),
    !,
    arg(1, FormulaTerm, Arg1),
    arg(2, FormulaTerm, Arg2),
    arg(3, FormulaTerm, Arg3),
    arg(4, FormulaTerm, Arg4),
    Val is Arg4*Arg1+Arg2+Arg3.
eval_formula_term(FormulaTerm, Val) :- % NEW
    functor(FormulaTerm, minus_floor, 4),
    !,
    arg(1, FormulaTerm, Arg1),
    arg(2, FormulaTerm, Arg2),
    arg(3, FormulaTerm, Arg3),
    arg(4, FormulaTerm, Arg4),
    Arg4 \== 0, 
    Val is (Arg1-Arg2+Arg3) // Arg4.
eval_formula_term(FormulaTerm, Val) :-
    FormulaTerm = cases(CasesList),
    !,
    eval_cases(CasesList,Val).
eval_formula_term(FormulaTerm, _) :-
    write(eval_formula_term(FormulaTerm)), nl, halt.

eval_boolean_conds([Cond|R], Oplus, Val) :-
    eval_formula_term(Cond, CondVal),
    (memberchk(CondVal, [0,1]) -> true ; write(eval_boolean_conds1(CondVal)), nl, halt),
    (R = [] ->
        Val = CondVal
    ;
        eval_boolean_conds(R, Oplus, CondVal, ValConds),
        (Oplus = voting ->
            length([Cond|R], Len),
            Min is (Len+1) // 2,
            (ValConds >= Min -> Val = 1 ; Val = 0)
        ;
            Val = ValConds
        )
    ).

eval_boolean_conds([Cond|R], Oplus, PrevVal, Val) :-
    eval_formula_term(Cond, CondVal),
    (memberchk(CondVal, [0,1]) -> true ; write(eval_boolean_conds2(CondVal)), nl, halt),
    (Oplus = and      -> ((CondVal = 1, PrevVal = 1) -> CurVal = 1 ; CurVal = 0)    ;
     Oplus = or       -> ((CondVal = 0, PrevVal = 0) -> CurVal = 0 ; CurVal = 1)    ;
     Oplus = allequal -> ( CondVal = PrevVal         -> CurVal = 1 ; CurVal = 0)    ;
     Oplus = sum      ->                                CurVal is CondVal + PrevVal ;
     Oplus = xor      ->                                CurVal is CondVal + PrevVal ;
     Oplus = voting   ->                                CurVal is CondVal + PrevVal ;
     Oplus = card1    ->                                CurVal is CondVal + PrevVal ;
                         write(eval_boolean_conds3(Oplus)), nl, halt                ),
    ((R = [], Oplus = xor  )        -> Val = CurVal mod 2                         ;
     (R = [], Oplus = card1)        -> (CurVal = 1 -> Val = 1 ; Val = 0)          ;
      R = []                        -> Val = CurVal                               ; % remark: for voting return the sum
     (Oplus = allequal, CurVal = 0) -> Val = 0                                    ; %         as an intermediate result
      Oplus = allequal              -> eval_boolean_conds(R, Oplus, CondVal, Val) ;
                                       eval_boolean_conds(R, Oplus, CurVal,  Val) ).

eval_and_sumup_monomes([], Val, Val) :- !.
eval_and_sumup_monomes([Monome|R], AccumatedVals, FinalVal) :-
    Monome = monome(Terms, Coef),
    eval_monome(Terms, Coef, Val),
    NewAccumatedVals is AccumatedVals + Val,
    eval_and_sumup_monomes(R, NewAccumatedVals, FinalVal).

eval_monome([], Result, Result) :- !.
eval_monome([t(Term,Degree)|R], CurVal, Result) :-
    eval_formula_term(Term, TermValue),
    Val is TermValue ^ Degree,
    NextVal is Val * CurVal,
    eval_monome(R, NextVal, Result).

eval_cases([if_then(TermCond, TermRes)|R], Val) :-
    !,
    (eval_formula_term(TermCond, 1) ->
        eval_formula_term(TermRes, ValRes),
        Val is ValRes
    ;
        eval_cases(R, Val)
    ).
eval_cases([otherwise(TermRes)], Val) :-
    eval_formula_term(TermRes, ValRes),
    Val is ValRes.
