% Purpose: Apply forbidden pairs constraints between Cond1 and any condition in the list Conds. These constraints are generated
% automatically by generate_forbid_specific_combinations_of_conds.pl
% Authors: Ramiz Gindullin, IMT Atlantique

:- module(forbid_specific_combinations_of_conds_generated,
          [forbid_specific_combinations_of_conds/14]).

:- use_module(found_condpairs_family1_sym).
:- use_module(found_condpairs_family2_sym).
:- use_module(found_condpairs_family3_new).
:- use_module(constraint_check_condition_generation).
:- use_module(utility).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

forbid_specific_combinations_of_conds(Conds, Cond1-Type1_0, Oplus, NbTerms, MandatoryAttr, LConds,  MinAttrs, MaxAttrs,
                                      ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr, PosCondCoef, PosCondCst) :-
    % Place any additonal constraints on Cond1
    findall([[B1Target,Type1,Attrs1Name,CoefList1],hypothesis(HNegFlag,HList)],
            (condsophypothesis_f3(_, [Type1,Attrs1Name,CoefList1],[none, [], []], and, hypothesis(HNegFlag,HList),_),
             convert_type_name(Type1_0, Type1, B1Target)),
            HypothesisListF3Type1),
    arg(PosCondB,     Cond1, B1),
    arg(PosLCondAttr, Cond1, Attrs1),
    arg(PosCondCoef,  Cond1, Coef1),
    arg(PosCondCst,   Cond1, Cst1),
    create_forbid_specific_combinations_of_conds_constraint(HypothesisListF3Type1,B1,Attrs1,Coef1,Cst1),
    % Place constraints on forbidden pairs taht include Cond1 in them  
    forbid_specific_combinations_of_conds1(Conds, Cond1-Type1_0, Oplus, NbTerms, MandatoryAttr, LConds,  MinAttrs, MaxAttrs,
                                           ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr, PosCondCoef, PosCondCst).

% Enumerate through list Conds to check to see if any elements of this list form a forbidden pair with Cond1
forbid_specific_combinations_of_conds1([], _, _, _, _, _, _, _, _, _, _, _, _, _) :- !.
forbid_specific_combinations_of_conds1([Cond2|R], Cond1-Type1_0, Oplus, NbTerms, MandatoryAttr, LConds,  MinAttrs, MaxAttrs,
                                      ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr, PosCondCoef, PosCondCst) :-
    % These are hand-written forbidden pairs constraints that cover cases outside of search scope of generate_forbid_specific_combinations_of_conds.pl
    arg(PosNewCondType, Cond2, Type2_0),
    ((Oplus = and,
      memberchk(Type1_0, [attr_eq_attr,attr_eq_unary(prod),binary_term_eq_coef(minus),binary_term_eq_coef(plus)])) ->   %1 Family 3. For opimization purposes. If we have 1-to-1 relation between two attributes with Oplus=and, there's no need to search for a restriction of a second attribute if we know a restriction for the first one. Granted, it can lead to problems where it would output [x2=<x4] and [x1=<x3] and [(x1 mod x4)=0] rather than [x2=<x4] and [x1=<x3] and [x1=x4] 
        arg(PosCondB,     Cond1, Beq_1),
        arg(PosLCondAttr, Cond1, [_,A2_1]),
        arg(PosLCondAttr, Cond2, Attrs),
        (foreach(A, Attrs), param(Beq_1), param(A2_1) do (Beq_1 #= 1) #=> (A #\= A2_1))
    ;
     (Oplus = and,
      memberchk(Type2_0, [attr_eq_attr,attr_eq_unary(prod),binary_term_eq_coef(minus),binary_term_eq_coef(plus)])) ->
        arg(PosCondB,     Cond2, Beq_1),
        arg(PosLCondAttr, Cond2, [_,A2_1]),
        arg(PosLCondAttr, Cond1, Attrs),
        (foreach(A, Attrs), param(Beq_1), param(A2_1) do (Beq_1 #= 1) #=> (A #\= A2_1))
    ;
     true),
    ((memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type1_0 = attr_leq_attr, 
      memberchk(Type2_0, [attr_eq_coef,attr_leq_coef])) ->                                                                %4 Family 1
        arg(PosCondB,     Cond1, Bleq_4),
        arg(PosCondB,     Cond2, B2_4),
        arg(PosLCondAttr, Cond1, [A1_4,A2_4]),
        arg(PosLCondAttr, Cond2, [A3_4]),
        arg(PosCondCoef,  Cond2, CondCoef_4),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A2_4, [CondCoefMin|MinAttrs], MinAttr2),                            % get smallest value of attribute CondAttr1
        (Bleq_4 #= 1 #/\ B2_4 #= 1 #/\ A1_4 #= A3_4) #=> (CondCoef_4 #> MinAttr2)
    ;
     (memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type2_0 = attr_leq_attr, 
      memberchk(Type1_0, [attr_eq_coef,attr_leq_coef])) ->
        arg(PosCondB,     Cond2, Bleq_4),
        arg(PosCondB,     Cond1, B2_4),
        arg(PosLCondAttr, Cond2, [A1_4,A2_4]),
        arg(PosLCondAttr, Cond1, [A3_4]),
        arg(PosCondCoef,  Cond1, CondCoef_4),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A2_4, [CondCoefMin|MinAttrs], MinAttr2),                            % get smallest value of attribute CondAttr1
        (Bleq_4 #= 1 #/\ B2_4 #= 1 #/\ A1_4 #= A3_4) #=> (CondCoef_4 #> MinAttr2)
    ;
     true
    ),
    ((memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type1_0 = attr_leq_attr,
      memberchk(Type2_0, [attr_eq_coef,attr_geq_coef])) ->                                                                %5  Family 1
        arg(PosCondB,     Cond1, Bleq_5),
        arg(PosCondB,     Cond2, Beq_5),
        arg(PosLCondAttr, Cond1, [A1_5,A2_5]),
        arg(PosLCondAttr, Cond2, [A3_5]),
        arg(PosCondCoef,  Cond2, CondCoef_5),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_5, [CondCoefMin|MaxAttrs], MaxAttr1),                            % get smallest value of attribute CondAttr1
        (Bleq_5 #= 1 #/\ Beq_5 #= 1 #/\ A2_5 #= A3_5) #=> (CondCoef_5 #< MaxAttr1)
    ;
     (memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type2_0 = attr_leq_attr,
      memberchk(Type1_0, [attr_eq_coef,attr_geq_coef])) ->
        arg(PosCondB,     Cond2, Bleq_5),
        arg(PosCondB,     Cond1, Beq_5),
        arg(PosLCondAttr, Cond2, [A1_5,A2_5]),
        arg(PosLCondAttr, Cond1, [A3_5]),
        arg(PosCondCoef,  Cond1, CondCoef_5),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_5, [CondCoefMin|MaxAttrs], MaxAttr1),                            % get smallest value of attribute CondAttr1
        (Bleq_5 #= 1 #/\ Beq_5 #= 1 #/\ A2_5 #= A3_5) #=> (CondCoef_5 #< MaxAttr1)
    ;
     true
    ),
    ((memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type1_0 = attr_leq_attr, 
      memberchk(Type2_0, [attr_eq_coef,attr_geq_coef])) ->                                                                %6  Family 1
        arg(PosCondB,     Cond1, Bleq_6),
        arg(PosCondB,     Cond2, B2_6),
        arg(PosLCondAttr, Cond1, [A1_6,A2_6]),
        arg(PosLCondAttr, Cond2, [A3_6]),
        arg(PosCondCoef,  Cond2, CondCoef_6),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A2_6, [CondCoefMin|MaxAttrs], MaxAttr2),                            % get smallest value of attribute CondAttr1
        (Bleq_6 #= 1 #/\ B2_6 #= 1 #/\ A1_6 #= A3_6) #=> (CondCoef_6 #< MaxAttr2)
    ;
     (memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type2_0 = attr_leq_attr, 
      memberchk(Type1_0, [attr_eq_coef,attr_geq_coef])) ->
        arg(PosCondB,     Cond2, Bleq_6),
        arg(PosCondB,     Cond1, B2_6),
        arg(PosLCondAttr, Cond2, [A1_6,A2_6]),
        arg(PosLCondAttr, Cond1, [A3_6]),
        arg(PosCondCoef,  Cond1, CondCoef_6),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A2_6, [CondCoefMin|MaxAttrs], MaxAttr2),                            % get smallest value of attribute CondAttr1
        (Bleq_6 #= 1 #/\ B2_6 #= 1 #/\ A1_6 #= A3_6) #=> (CondCoef_6 #< MaxAttr2)
    ;
     true
    ),
    ((memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type1_0 = attr_leq_attr,
      memberchk(Type2_0, [attr_eq_coef,attr_leq_coef])) ->                                                                %7 Family 1
        arg(PosCondB,     Cond1, Bleq_7),
        arg(PosCondB,     Cond2, Beq_7),
        arg(PosLCondAttr, Cond1, [A1_7,A2_7]),
        arg(PosLCondAttr, Cond2, [A3_7]),
        arg(PosCondCoef,  Cond2, CondCoef_7),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_7, [CondCoefMin|MinAttrs], MinAttr1_7),                            % get smallest value of attribute CondAttr1
        (Bleq_7 #= 1 #/\ Beq_7 #= 1 #/\ A2_7 #= A3_7) #=> (CondCoef_7 #> MinAttr1_7)
    ;
     (memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type2_0 = attr_leq_attr,
      memberchk(Type1_0, [attr_eq_coef,attr_leq_coef])) ->
        arg(PosCondB,     Cond2, Bleq_7),
        arg(PosCondB,     Cond1, Beq_7),
        arg(PosLCondAttr, Cond2, [A1_7,A2_7]),
        arg(PosLCondAttr, Cond1, [A3_7]),
        arg(PosCondCoef,  Cond1, CondCoef_7),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_7, [CondCoefMin|MinAttrs], MinAttr1_7),                            % get smallest value of attribute CondAttr1
        (Bleq_7 #= 1 #/\ Beq_7 #= 1 #/\ A2_7 #= A3_7) #=> (CondCoef_7 #> MinAttr1_7)
    ;
     true
    ),
    ((memberchk(Oplus,[sum,allequal,xor]),
      memberchk(Type1_0, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2_0, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->                                                                                               %28 Family 2
        arg(PosCondB,     Cond1, Bleq_28),
        arg(PosCondB,     Cond2, Bgeq_28),
        arg(PosLCondAttr, Cond1, [A1_28,A2_28]),
        arg(PosLCondAttr, Cond2, [A3_28,A4_28]),
        ((Bleq_28 #= 1) #/\ (Bgeq_28 #= 1)) #=> ((A1_28 #\= A3_28) #\/ (A2_28 #\= A4_28))
    ;
     (memberchk(Oplus,[sum,allequal,xor]),
      memberchk(Type2_0, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1_0, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->
        arg(PosCondB,     Cond2, Bleq_28),
        arg(PosCondB,     Cond1, Bgeq_28),
        arg(PosLCondAttr, Cond2, [A1_28,A2_28]),
        arg(PosLCondAttr, Cond1, [A3_28,A4_28]),
        ((Bleq_28 #= 1) #/\ (Bgeq_28 #= 1)) #=> ((A1_28 #\= A3_28) #\/ (A2_28 #\= A4_28))
    ;
     true
    ),
    ((Oplus = and,
      memberchk(Type1_0, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2_0, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->                                                                                               %29 Family 2
        arg(PosCondB,     Cond1, Bleq_29),
        arg(PosCondB,     Cond2, Bgeq_29),
        arg(PosLCondAttr, Cond1, [A1_29,A2_29]),
        arg(PosLCondAttr, Cond2, [A3_29,A4_29]),
        arg(PosCondCoef,  Cond1, C1_29),
        arg(PosCondCoef,  Cond2, C2_29),
        ((Bleq_29 #= 1) #/\ (Bgeq_29 #= 1) #/\ (A1_29 #= A3_29) #/\ (A2_29 #= A4_29)) #=> (C1_29 #> C2_29)
    ;
     (Oplus = and,
      memberchk(Type2_0, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1_0, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->
        arg(PosCondB,     Cond2, Bleq_29),
        arg(PosCondB,     Cond1, Bgeq_29),
        arg(PosLCondAttr, Cond2, [A1_29,A2_29]),
        arg(PosLCondAttr, Cond1, [A3_29,A4_29]),
        arg(PosCondCoef,  Cond2, C1_29),
        arg(PosCondCoef,  Cond1, C2_29),
        ((Bleq_29 #= 1) #/\ (Bgeq_29 #= 1) #/\ (A1_29 #= A3_29) #/\ (A2_29 #= A4_29)) #=> (C1_29 #> C2_29)
    ;
     true
    ),
    ((Oplus = or,
      memberchk(Type1_0, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2_0, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->                                                                                               %30 Family 2
        arg(PosCondB,     Cond1, Bleq_30),
        arg(PosCondB,     Cond2, Bgeq_30),
        arg(PosLCondAttr, Cond1, [A1_30,A2_30]),
        arg(PosLCondAttr, Cond2, [A3_30,A4_30]),
        arg(PosCondCoef,  Cond1, C1_30),
        arg(PosCondCoef,  Cond2, C2_30),
        ((Bleq_30 #= 1) #/\ (Bgeq_30 #= 1) #/\ (A1_30 #= A3_30) #/\ (A2_30 #= A4_30)) #=> ((C2_30 - C1_30) #> 1)
    ;
     (Oplus = or,
      memberchk(Type2_0, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1_0, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->
        arg(PosCondB,     Cond2, Bleq_30),
        arg(PosCondB,     Cond1, Bgeq_30),
        arg(PosLCondAttr, Cond2, [A1_30,A2_30]),
        arg(PosLCondAttr, Cond1, [A3_30,A4_30]),
        arg(PosCondCoef,  Cond2, C1_30),
        arg(PosCondCoef,  Cond1, C2_30),
        ((Bleq_30 #= 1) #/\ (Bgeq_30 #= 1) #/\ (A1_30 #= A3_30) #/\ (A2_30 #= A4_30)) #=> ((C2_30 - C1_30) #> 1)
    ;
     true
    ),
    ((memberchk(Oplus,[and,sum,allequal,xor]),
      (Oplus = allequal -> NbTerms = 3 ; true),
      memberchk(Type1_0, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
      memberchk(Type2_0, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->                                                                                               %31 Family 1
        arg(PosCondB,     Cond1, Beq_31),
        arg(PosCondB,     Cond2, Bgeq_31),
        arg(PosLCondAttr, Cond1, [A1_31,A2_31]),
        arg(PosLCondAttr, Cond2, [A3_31,A4_31]),
        ((Beq_31 #= 1) #/\ (Bgeq_31 #= 1)) #=> ((A1_31 #\= A3_31) #\/ (A2_31 #\= A4_31))
    ;
     (memberchk(Oplus,[and,sum,allequal,xor]),
      (Oplus = allequal -> NbTerms = 3 ; true),
      memberchk(Type2_0, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
      memberchk(Type1_0, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->
        arg(PosCondB,     Cond2, Beq_31),
        arg(PosCondB,     Cond1, Bgeq_31),
        arg(PosLCondAttr, Cond2, [A1_31,A2_31]),
        arg(PosLCondAttr, Cond1, [A3_31,A4_31]),
        ((Beq_31 #= 1) #/\ (Bgeq_31 #= 1)) #=> ((A1_31 #\= A3_31) #\/ (A2_31 #\= A4_31))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (Oplus = allequal -> NbTerms = 2 ; true),
      memberchk(Type1_0, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
      memberchk(Type2_0, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->                                                                                               %32 Family 1
        arg(PosCondB,     Cond1, Beq_32),
        arg(PosCondB,     Cond2, Bgeq_32),
        arg(PosLCondAttr, Cond1, [A1_32,A2_32]),
        arg(PosLCondAttr, Cond2, [A3_32,A4_32]),
        arg(PosCondCoef,  Cond1, C1_32),
        arg(PosCondCoef,  Cond2, C2_32),
        ((Beq_32 #= 1) #/\ (Bgeq_32 #= 1) #/\ (A1_32 #= A3_32) #/\ (A2_32 #= A4_32)) #=> ((C2_32 - C1_32) #> 1)
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (Oplus = allequal -> NbTerms = 2 ; true),
      memberchk(Type2_0, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
      memberchk(Type1_0, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->
        arg(PosCondB,     Cond2, Beq_32),
        arg(PosCondB,     Cond1, Bgeq_32),
        arg(PosLCondAttr, Cond2, [A1_32,A2_32]),
        arg(PosLCondAttr, Cond1, [A3_32,A4_32]),
        arg(PosCondCoef,  Cond2, C1_32),
        arg(PosCondCoef,  Cond1, C2_32),
        ((Beq_32 #= 1) #/\ (Bgeq_32 #= 1) #/\ (A1_32 #= A3_32) #/\ (A2_32 #= A4_32)) #=> ((C2_32 - C1_32) #> 1)
    ;
     true
    ),
    ((memberchk(Oplus,[and,sum,allequal,xor]),
      (Oplus = allequal -> NbTerms = 3 ; true),
      memberchk(Type1_0, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2_0, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->                                                                                               %33 Family 3
        arg(PosCondB,     Cond1, Bleq_33),
        arg(PosCondB,     Cond2, Beq_33),
        arg(PosLCondAttr, Cond1, [A1_33,A2_33]),
        arg(PosLCondAttr, Cond2, [A3_33,A4_33]),
        ((Bleq_33 #= 1) #/\ (Beq_33 #= 1)) #=> ((A1_33 #\= A3_33) #\/ (A2_33 #\= A4_33))
    ;
     (memberchk(Oplus,[and,sum,allequal,xor]),
      (Oplus = allequal -> NbTerms = 3 ; true),
      memberchk(Type2_0, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1_0, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
      BType1 = BType2) ->
        arg(PosCondB,     Cond2, Bleq_33),
        arg(PosCondB,     Cond1, Beq_33),
        arg(PosLCondAttr, Cond2, [A1_33,A2_33]),
        arg(PosLCondAttr, Cond1, [A3_33,A4_33]),
        ((Bleq_33 #= 1) #/\ (Beq_33 #= 1)) #=> ((A1_33 #\= A3_33) #\/ (A2_33 #\= A4_33))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (Oplus = allequal -> NbTerms = 2 ; true),
      memberchk(Type1_0, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2_0, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->                                                                                               %34 Family 1
        arg(PosCondB,     Cond1, Bleq_34),
        arg(PosCondB,     Cond2, Beq_34),
        arg(PosLCondAttr, Cond1, [A1_34,A2_34]),
        arg(PosLCondAttr, Cond2, [A3_34,A4_34]),
        arg(PosCondCoef,  Cond1, C1_34),
        arg(PosCondCoef,  Cond2, C2_34),
        ((Bleq_34 #= 1) #/\ (Beq_34 #= 1) #/\ (A1_34 #= A3_34) #/\ (A2_34 #= A4_34)) #=> ((C1_34 - C2_34) #> 1)
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (Oplus = allequal -> NbTerms = 2 ; true),
      memberchk(Type2_0, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1_0, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
      memberchk(BType1, [fmod,cmod,dmod]),
      BType1 = BType2) ->
        arg(PosCondB,     Cond2, Bleq_34),
        arg(PosCondB,     Cond1, Beq_34),
        arg(PosLCondAttr, Cond2, [A1_34,A2_34]),
        arg(PosLCondAttr, Cond1, [A3_34,A4_34]),
        arg(PosCondCoef,  Cond2, C1_34),
        arg(PosCondCoef,  Cond1, C2_34),
        ((Bleq_34 #= 1) #/\ (Beq_34 #= 1) #/\ (A1_34 #= A3_34) #/\ (A2_34 #= A4_34)) #=> ((C1_34 - C2_34) #> 1)
    ;
     true
    ),
    ((Oplus = and,
      Type1_0 = attr_eq_coef) ->   %1 Family 3. For opimization purposes.  
        arg(PosCondB,     Cond1, Beq_2),
        arg(PosLCondAttr, Cond1, [A1_2]),
        arg(PosLCondAttr, Cond2, Attrs_2),
        (foreach(A2, Attrs_2), param(Beq_2), param(A1_2) do (Beq_2 #= 1) #=> (A2 #\= A1_2))
    ;
     (Oplus = and,
      Type2_0 = attr_eq_coef) ->   %1 Family 3. For opimization purposes.
        arg(PosCondB,     Cond2, Beq_2),
        arg(PosLCondAttr, Cond2, [A1_2]),
        arg(PosLCondAttr, Cond1, Attrs_2),
        (foreach(A2, Attrs_2), param(Beq_2), param(A1_2) do (Beq_2 #= 1) #=> (A2 #\= A1_2))
    ;
     true),
    ((memberchk(Oplus,[or,allequal,xor]),
      Type1_0 = attr_geq_coef,
      Type2_0 = unary_term_eq_coef(mod)) ->                                                                               %74 Family 3: %[v>=3] = [v mod 2=0]]
        arg(PosCondB,     Cond1, B1_74),
        arg(PosCondB,     Cond2, B2_74),
        arg(PosLCondAttr, Cond1, [A1_74]),
        arg(PosLCondAttr, Cond2, [A3_74]),
        arg(PosCondCoef,  Cond1, C1_74),
        arg(PosCondCoef,  Cond2, C2_74),
        arg(PosCondCst,   Cond2, D_74),
        ((B1_74 #= 1) #/\ (B2_74 #= 1) #/\ (A1_74 #= A3_74)) #=> (((C2_74 #> 0) #\/ (C1_74 #> D_74 + 1)) #/\ (C1_74 #> C2_74 + 1))
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      Type2_0 = attr_geq_coef,
      Type1_0 = unary_term_eq_coef(mod)) ->
        arg(PosCondB,     Cond2, B1_74),
        arg(PosCondB,     Cond1, B2_74),
        arg(PosLCondAttr, Cond2, [A1_74]),
        arg(PosLCondAttr, Cond1, [A3_74]),
        arg(PosCondCoef,  Cond2, C1_74),
        arg(PosCondCoef,  Cond1, C2_74),
        arg(PosCondCst,   Cond1, D_74),
        ((B1_74 #= 1) #/\ (B2_74 #= 1) #/\ (A1_74 #= A3_74)) #=> (((C2_74 #> 0) #\/ (C1_74 #> D_74 + 1)) #/\ (C1_74 #> C2_74 + 1))
    ;
        true
    ),
    ((memberchk(Oplus,[allequal,xor]),
      memberchk(Type1_0, [attr_eq_coef,attr_leq_coef,attr_geq_coef]),
      memberchk(Type2_0, [attr_in_interval,unary_term_eq_coef(mod),unary_term_leq_coef(mod),unary_term_geq_coef(mod)])) ->      %75  %[[maxs=<3] = [maxs in 2..3]]
        arg(PosCondB,     Cond1, B1_0),
        arg(PosCondB,     Cond2, B2_0),
        arg(PosLCondAttr, Cond1, [A1_0]),
        arg(PosLCondAttr, Cond2, [A3_0]),
        ((B1_0 #= 1) #/\ (B2_0 #>= 0)) #=> (A1_0 #\= A3_0)
    ;
     (memberchk(Oplus,[allequal,xor]),
      memberchk(Type2_0, [attr_eq_coef,attr_leq_coef,attr_geq_coef]),
      memberchk(Type1_0, [attr_in_interval,unary_term_eq_coef(mod),unary_term_leq_coef(mod),unary_term_geq_coef(mod)])) ->
        arg(PosCondB,     Cond2, B1_0),
        arg(PosCondB,     Cond1, B2_0),
        arg(PosLCondAttr, Cond2, [A1_0]),
        arg(PosLCondAttr, Cond1, [A3_0]),
        ((B1_0 #= 1) #/\ (B2_0 #>= 0)) #=> (A1_0 #\= A3_0)
    ;
        true
    ),
    ((memberchk(Oplus,[and,or]),
      Type1_0 = attr_eq_attr,
      Type2_0 = attr_eq_coef) ->                                                                                           %75 Family 3:
        arg(PosCondB,     Cond1, B1_75),
        arg(PosCondB,     Cond2, B2_75),
        arg(PosLCondAttr, Cond1, [A1_75,A2_75]),
        arg(PosLCondAttr, Cond2, [A3_75]),
        arg(PosCondCoef,  Cond2, C2_75),
        get_all_col_pairs_in_cmps(2, MandatoryAttr, [leq], Table0_75),
        shift_pairs(Table0_75, Table_75),
        element(A1_75, [CondCoefMin|MinAttrs], MinAttr1_75),
        element(A2_75, [CondCoefMin|MinAttrs], MinAttr2_75),
        call(((B1_75 #= 1) #/\ (B2_75 #= 1) #/\ (A2_75 #= A3_75) #/\
         (MinAttr1_75 #= MinAttr2_75) #/\ table([[A1_75,A2_75]],Table_75)) #=> (C2_75 #> MinAttr1_75)),
        call(((B1_75 #= 1) #/\ (B2_75 #= 1) #/\ (A1_75 #= A3_75) #/\
         (MinAttr1_75 #= MinAttr2_75) #/\ table([[A2_75,A1_75]],Table_75)) #=> (C2_75 #> MinAttr1_75))
    ;
     (memberchk(Oplus,[and,or]),
      Type2_0 = attr_eq_attr,
      Type1_0 = attr_eq_coef) ->
        arg(PosCondB,     Cond2, B1_75),
        arg(PosCondB,     Cond1, B2_75),
        arg(PosLCondAttr, Cond2, [A1_75,A2_75]),
        arg(PosLCondAttr, Cond1, [A3_75]),
        arg(PosCondCoef,  Cond1, C2_75),
        get_all_col_pairs_in_cmps(2, MandatoryAttr, [leq], Table0_75),
        shift_pairs(Table0_75, Table_75),
        element(A1_75, [CondCoefMin|MinAttrs], MinAttr1_75),
        element(A2_75, [CondCoefMin|MinAttrs], MinAttr2_75),
        call(((B1_75 #= 1) #/\ (B2_75 #= 1) #/\ (A2_75 #= A3_75) #/\
         (MinAttr1_75 #= MinAttr2_75) #/\ table([[A1_75,A2_75]],Table_75)) #=> (C2_75 #> MinAttr1_75)),
        call(((B1_75 #= 1) #/\ (B2_75 #= 1) #/\ (A1_75 #= A3_75) #/\
         (MinAttr1_75 #= MinAttr2_75) #/\ table([[A2_75,A1_75]],Table_75)) #=> (C2_75 #> MinAttr1_75))
    ;
        true
    ),

    % Application of automatically generated rules by generate_forbid_specific_combinations_of_conds.pl:
    arg(PosCondB,     Cond1, B1),
    arg(PosCondB,     Cond2, B2),
    arg(PosLCondAttr, Cond1, Attrs1),
    arg(PosLCondAttr, Cond2, Attrs2),
    arg(PosCondCoef,  Cond1, Coef1),
    arg(PosCondCoef,  Cond2, Coef2),
    arg(PosCondCst,   Cond1, Cst1),
    arg(PosCondCst,   Cond2, Cst2),
    % Search all rules from all three Families:
    findall([[B1Target,Type1,Attrs1Name,CoefList1],[B2Target,Type2,Attrs2Name,CoefList2],Oplus,hypothesis(HNegFlag,HList)],
            (condsophypothesis_f1(_,[Type1,Attrs1Name,CoefList1],[Type2,Attrs2Name,CoefList2],Oplus,hypothesis(HNegFlag,HList)),
             convert_type_name(Type1_0, Type1, B1Target), % conversion is needed because of not_attr_in_interval (the negation of attr_in_interval) and
             convert_type_name(Type2_0, Type2, B2Target)  % binary_term_*eq_coef(mfloor) - it has an additional argument
            ),
            HypothesisListF1),
    findall([[B1Target,Type1,Attrs1Name,CoefList1],[B2Target,Type2,Attrs2Name,CoefList2],Oplus,hypothesis(HNegFlag,HList)],
            (condsophypothesis_f2(_,[Type1,Attrs1Name,CoefList1],[Type2,Attrs2Name,CoefList2],Oplus,hypothesis(HNegFlag,HList)),
             convert_type_name(Type1_0, Type1, B1Target), % conversion is needed because of not_attr_in_interval (the negation of attr_in_interval) and
             convert_type_name(Type2_0, Type2, B2Target)  % binary_term_*eq_coef(mfloor) - it has an additional argument
            ),
            HypothesisListF2),
    findall([[B1Target,Type1,Attrs1Name,CoefList1],[B2Target,Type2,Attrs2Name,CoefList2],Oplus,hypothesis(HNegFlag,HList)],
            (condsophypothesis_f3(_,[Type1,Attrs1Name,CoefList1],[Type2,Attrs2Name,CoefList2],Oplus,hypothesis(HNegFlag,HList),_),
             convert_type_name(Type1_0, Type1, B1Target), % conversion is needed because of not_attr_in_interval (the negation of attr_in_interval) and
             convert_type_name(Type2_0, Type2, B2Target)  % binary_term_*eq_coef(mfloor) - it has an additional argument
            ),
            HypothesisListF3a),
    findall([[B2Target,Type2,Attrs2Name,CoefList2],[B1Target,Type1,Attrs1Name,CoefList1],Oplus,hypothesis(HNegFlag,HList)],
            (condsophypothesis_f3(_,[Type2,Attrs2Name,CoefList2],[Type1,Attrs1Name,CoefList1],Oplus,hypothesis(HNegFlag,HList),_),
             convert_type_name(Type1_0, Type1, B1Target), % conversion is needed because of not_attr_in_interval (the negation of attr_in_interval) and
             convert_type_name(Type2_0, Type2, B2Target)  % binary_term_*eq_coef(mfloor) - it has an additional argument
            ),
            HypothesisListF3b),
    % create constraints from found rules:
    create_forbid_specific_combinations_of_conds_constraint(HypothesisListF1, B1,B2,Attrs1,Attrs2,Coef1,Coef2,Cst1,Cst2),
    create_forbid_specific_combinations_of_conds_constraint(HypothesisListF2, B1,B2,Attrs1,Attrs2,Coef1,Coef2,Cst1,Cst2),
    create_forbid_specific_combinations_of_conds_constraint(HypothesisListF3a,B1,B2,Attrs1,Attrs2,Coef1,Coef2,Cst1,Cst2),
    create_forbid_specific_combinations_of_conds_constraint(HypothesisListF3b,B2,B1,Attrs2,Attrs1,Coef2,Coef1,Cst2,Cst1),

    forbid_specific_combinations_of_conds1(R, Cond1-Type1_0, Oplus, NbTerms, MandatoryAttr, LConds,
                                          MinAttrs, MaxAttrs, ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr, PosCondCoef, PosCondCst).

% Creates constraints corresponding to each forbidden pair rule from a list (families 1, 2 and 3b)
create_forbid_specific_combinations_of_conds_constraint([],_B1,_B2,_Attrs1,_Attrs2,_Coef1,_Coef2,_Cst1,_Cst2) :- !.
create_forbid_specific_combinations_of_conds_constraint([[[B1Target,_Type1,Attrs1Names,CoefsNames1],
                                                          [B2Target,_Type2,Attrs2Names,CoefsNames2],
                                                          _Oplus, hypothesis(HNegFlag,HList)]|R],
                                                        B1, B2, Attrs1, Attrs2, Coef1, Coef2, Cst1, Cst2) :-
    convert_coefnames_to_coefvars(1,CoefsNames1,Coef1,Cst1,CoefsVars1), % find corresponding variables for the current rule
    convert_coefnames_to_coefvars(2,CoefsNames2,Coef2,Cst2,CoefsVars2),
    HFlag is 1 - HNegFlag,
    generate_forbidden_hypothesis_term(HList,CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HypothesisTerm),     % Create a variable for the rule
    generate_attr_term(Attrs1Names,Attrs2Names,Attrs1,Attrs2,AttrTerm),                                         % Create a variable to find a relevant relation between attributes within the pair of conditions
    (B1 #= B1Target #/\ B2 #= B2Target #/\ AttrTerm) #=> (HypothesisTerm #= HFlag),                     % The constraint itself: if both conditions are used,
                                                                                                        % attributes are matched accordingly then
                                                                                                        % the rule on coefficient variables must be negated
    create_forbid_specific_combinations_of_conds_constraint(R,B1,B2,Attrs1,Attrs2,Coef1,Coef2,Cst1,Cst2).

% Creates constraints corresponding to each single condition rule from a list (family 3a)
create_forbid_specific_combinations_of_conds_constraint([],_B1,_Attrs1,_Coef1,_Cst1) :- !.
create_forbid_specific_combinations_of_conds_constraint([[[B1Target,_Type1,Attrs1Names,CoefsNames1],hypothesis(HNegFlag,HList)]|R],
                                                        B1, Attrs1, Coef1, Cst1) :-
    (memberchk(Attrs1Names,[[1],[1,2]]) ->      
        convert_coefnames_to_coefvars(1,CoefsNames1,Coef1,Cst1,CoefsVars1),     % find corresponding variables for the current rule
        convert_coefnames_to_coefvars(2,[],_Coef2,_Cst2,CoefsVars2),            % as we don't have a second condition here we put an default variable
        HFlag is 1 - HNegFlag,
        generate_forbidden_hypothesis_term(HList,CoefsNames1,CoefsVars1,[],CoefsVars2,HypothesisTerm),  % Create a variable for the rule
        (B1 #= B1Target) #=> (HypothesisTerm #= HFlag)                                                  % The constraint itself: if both conditions are used,
                                                                                                        % attributes are related accordingly then
                                                                                                        % the rule on coefficient variables must be negated
    ; % if there's a rule for [2,1] attributes, there's a symmetrical rule for [1,2] attributes and there's no need to post this constraint twice
        true),
    create_forbid_specific_combinations_of_conds_constraint(R,B1,Attrs1,Coef1,Cst1).           

% Convert between type names used in gen_bool_formula.pl and in stored rules
convert_type_name(Type_0,Type, BTarget) :-
    (memberchk( [Type_0, Type, BTarget],
               [[attr_in_interval,               attr_not_in_interval,         0],
                [binary_term_eq_coef(mfloor,_),  binary_term_eq_coef(mfloor),  1],
                [binary_term_leq_coef(mfloor,_), binary_term_leq_coef(mfloor), 1],
                [binary_term_geq_coef(mfloor,_), binary_term_geq_coef(mfloor), 1]
               ]
              ) ->
        true
    ;
        Type = Type_0, BTarget = 1
    )/*,
    (Type_0 = attr_not_in_interval ->
        Type = attr_in_interval, BTarget = 0
    ;
     Type_0 = binary_term_eq_coef(mfloor,_) ->
        Type = binary_term_eq_coef(mfloor), BTarget = 1 
    ;
     Type_0 = binary_term_leq_coef(mfloor,_) ->
        Type = binary_term_leq_coef(mfloor), BTarget = 1 
    ;
     Type_0 = binary_term_geq_coef(mfloor,_) ->
        Type = binary_term_geq_coef(mfloor), BTarget = 1 
    ;
        Type = Type_0, BTarget = 1
    )*/.

% From the list of coefficient names select relevant variables
% TODO: not necessary, but maybe write a generalised dynamic version using to_atom(Num,NumAtom), atom_concat(H,NumAtom,HNum),
convert_coefnames_to_coefvars(1,CoefNames,Coef,Cst,CoefVars):-
    !,
    (CoefNames = []             ->      CoefVars = []           ;
     CoefNames = [coef1]        ->      CoefVars = [Coef]       ;
     CoefNames = [cst1]         ->      CoefVars = [Cst]        ;
     CoefNames = [coef1,cst1]   ->      CoefVars = [Coef,Cst]   ;
                                        false                   ).
convert_coefnames_to_coefvars(2,CoefNames,Coef,Cst,CoefVars):-
    (CoefNames = []             ->      CoefVars = []           ;
     CoefNames = [coef2]        ->      CoefVars = [Coef]       ;
     CoefNames = [cst2]         ->      CoefVars = [Cst]        ;
     CoefNames = [coef2,cst2]   ->      CoefVars = [Coef,Cst]   ;
                                        false                   ).

% Create a variable that will determine if there's an equivalency between attributes that is relevant to the current rule
generate_attr_term(Attrs1Names,Attrs2Names,Attrs1,Attrs2,AttrTerm) :-
    AttrTerm in 0..1,
    ((Attrs1Names = _,     Attrs2Names = []   ) ->                                           AttrTerm #= 1                              ;
     (Attrs1Names = [1],   Attrs2Names = [1]  ) -> Attrs1 = [A1|_],     Attrs2 = [A3|_],     AttrTerm #= (A1 #= A3)                     ;
     (Attrs1Names = [1],   Attrs2Names = [1,2]) -> Attrs1 = [A1|_],     Attrs2 = [A3|_],     AttrTerm #= (A1 #= A3)                     ;
     (Attrs1Names = [1],   Attrs2Names = [2,1]) -> Attrs1 = [A1|_],     Attrs2 = [_A3,A4|_], AttrTerm #= (A1 #= A4)                     ;
     (Attrs1Names = [1,2], Attrs2Names = [1]  ) -> Attrs1 = [A1|_],     Attrs2 = [A3|_],     AttrTerm #= (A1 #= A3)                     ;
     (Attrs1Names = [1,2], Attrs2Names = [1,2]) -> Attrs1 = [A1,A2|_],  Attrs2 = [A3,A4|_],  AttrTerm #= ((A1 #= A3) #/\ (A2 #= A4))    ;
     (Attrs1Names = [1,2], Attrs2Names = [2,1]) -> Attrs1 = [A1,A2|_],  Attrs2 = [A3,A4|_],  AttrTerm #= ((A1 #= A4) #/\ (A2 #= A3))    ;
     (Attrs1Names = [2,1], Attrs2Names = [1]  ) -> Attrs1 = [_A1,A2|_], Attrs2 = [A3|_],     AttrTerm #= (A2 #= A3)                     ;
     (Attrs1Names = [2,1], Attrs2Names = [1,2]) -> Attrs1 = [A1,A2|_],  Attrs2 = [A3,A4|_],  AttrTerm #= ((A1 #= A4) #/\ (A2 #= A3))    ;
     (Attrs1Names = [2,1], Attrs2Names = [2,1]) -> Attrs1 = [A1,A2|_],  Attrs2 = [A3,A4|_],  AttrTerm #= ((A1 #= A3) #/\ (A2 #= A4))    ;
                                                   false                                                                                ).
