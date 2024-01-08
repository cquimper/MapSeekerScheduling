% Purpose: Apply forbidden pairs constraints between Cond1 and any condition in the list Conds. These constraints are hand-written
% Authors: Nicolas Beldiceanu, Ramiz Gindullin, IMT Atlantique

:- module(forbid_specific_combinations_of_conds,
          [forbid_specific_combinations_of_conds/14]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(utility).

% Forbid specific combinations of conditions to prevent:
% - (Family 1) Superseding conditions (when one of conditions is unnecessary as it's superseded by another one).
%       Example: [x1-x2>=2] and [x1>=x2] can be simplified by using only [x1-x2>=2]
%       Example: [x1-x2>=2] or [x1>=x2]  can be simplified by using only [x1>=x2]
% - (Family 2) Always false / Always true combinations of conditions.
%       Example: [x1=<x2] and [2*x2=<x1] is always false (if we assume that both variables are non-negative).
%       Example: [x1=<x2] or [x2=<2*x1]  is always true (if we assume that both variables are non-negative).
% - (Family 3) Undesirable combinations of conditions (that can or can not be expressed by a simpler condition or combination of conditions).
%       Example: [x1>=2] and [x1=<4]       is undesirable. We can substitute it with [x1 in [2..4]]
%       Example: [x1=<2] or [x1>=4]        is undesirable. We can substitute it with not[x1 in [2..4]]
%       Example: [x1=x2] <=> [2.x2=<x1]    is undesirable. We can substitute it with not[[x1=x2] or [2.x2=<x1]] as it's easier to explain and predict
%       Example: [|x1-x2|=<2] and [x1=<x2] is undesirable. We can substitute it with [x2-x1=<2] and [x1=<x2]. The addition of an absolute value of substraction is unnecessary
forbid_specific_combinations_of_conds([], _, _, _, _, _, _, _, _, _, _, _, _, _) :- !.                                                             % added MandatoryAttr
forbid_specific_combinations_of_conds([Cond2|R], Cond1-Type1, Oplus, NbTerms, MandatoryAttr, LConds,  MinAttrs, MaxAttrs,
                                      ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr, PosCondCoef, PosCondCst) :-        %NEW: complete rewrite
    arg(PosNewCondType, Cond2, Type2),
    ((Oplus = and,
      memberchk(Type1, [attr_eq_attr,attr_eq_unary(prod),binary_term_eq_coef(minus),binary_term_eq_coef(plus)])) ->   %1 Family 3. For opimization purposes. If we have 1-to-1 relation between two attributes with Oplus=and, there's no need to search for a restriction of a second attribute if we know a restriction for the first one. Granted, it can lead to problems where it would output [x2=<x4] and [x1=<x3] and [(x1 mod x4)=0] rather than [x2=<x4] and [x1=<x3] and [x1=x4] 
        arg(PosCondB,     Cond1, Beq_1),
        arg(PosLCondAttr, Cond1, [_,A2_1]),
        arg(PosLCondAttr, Cond2, Attrs),
        (foreach(A, Attrs), param(Beq_1), param(A2_1) do (Beq_1 #= 1) #=> (A #\= A2_1))
    ;
     (Oplus = and,
      memberchk(Type2, [attr_eq_attr,attr_eq_unary(prod),binary_term_eq_coef(minus),binary_term_eq_coef(plus)])) ->
      %memberchk(Type2, [attr_eq_attr])) ->
        arg(PosCondB,     Cond2, Beq_1),
        arg(PosLCondAttr, Cond2, [_,A2_1]),
        arg(PosLCondAttr, Cond1, Attrs),
        (foreach(A, Attrs), param(Beq_1), param(A2_1) do (Beq_1 #= 1) #=> (A #\= A2_1))
    ;
     true
    ),
    ((Oplus = and,
      Type1 = attr_eq_coef) ->   %1 Family 3. For opimization purposes.  
        arg(PosCondB,     Cond1, Beq_2),
        arg(PosLCondAttr, Cond1, [A1_2]),
        arg(PosLCondAttr, Cond2, Attrs_2),
        (foreach(A2, Attrs_2), param(Beq_2), param(A1_2) do (Beq_2 #= 1) #=> (A2 #\= A1_2))
    ;
     (Oplus = and,
      Type2 = attr_eq_coef) ->   %1 Family 3. For opimization purposes.
        arg(PosCondB,     Cond2, Beq_2),
        arg(PosLCondAttr, Cond2, [A1_2]),
        arg(PosLCondAttr, Cond1, Attrs_2),
        (foreach(A2, Attrs_2), param(Beq_2), param(A1_2) do (Beq_2 #= 1) #=> (A2 #\= A1_2))
    ;
     true),
    ((memberchk(Oplus, [and,or]), Type1 = attr_eq_attr,  Type2 = attr_leq_attr) ->                                    %2 Family 1
        arg(PosCondB,     Cond1, Beq_2),
        arg(PosCondB,     Cond2, Bleq_2),
        arg(PosLCondAttr, Cond1, [A1_2,A2_2]),
        arg(PosLCondAttr, Cond2, [A3_2,A4_2]),
        ((Beq_2 #= 1) #/\ (Bleq_2 #= 1)) #=> ((#\(A1_2 #= A3_2 #/\ A2_2 #= A4_2) #/\ #\(A1_2 #= A4_2 #/\ A2_2 #= A3_2)))
    ;
     (memberchk(Oplus, [and,or]), Type2 = attr_eq_attr,  Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, Beq_2),
        arg(PosCondB,     Cond1, Bleq_2),
        arg(PosLCondAttr, Cond2, [A1_2,A2_2]),
        arg(PosLCondAttr, Cond1, [A3_2,A4_2]),
        ((Beq_2 #= 1) #/\ (Bleq_2 #= 1)) #=> ((#\(A1_2 #= A3_2 #/\ A2_2 #= A4_2) #/\ #\(A1_2 #= A4_2 #/\ A2_2 #= A3_2)))
    ;
     true
    ),
    ((memberchk(Oplus, [allequal,xor]), NbTerms = 2, Type1 = attr_eq_attr,  Type2 = attr_leq_attr) ->
        arg(PosCondB,     Cond1, Beq_2a),
        arg(PosCondB,     Cond2, Bleq_2a),
        arg(PosLCondAttr, Cond1, [A1_2a,A2_2a]),
        arg(PosLCondAttr, Cond2, [A3_2a,A4_2a]),
        ((Beq_2a #= 1) #/\ (Bleq_2a #= 1)) #=> ((#\(A1_2a #= A3_2a #/\ A2_2a #= A4_2a) #/\ #\(A1_2a #= A4_2a #/\ A2_2a #= A3_2a)))
    ;
     (memberchk(Oplus, [allequal,xor]), NbTerms = 2, Type2 = attr_eq_attr,  Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, Beq_2a),
        arg(PosCondB,     Cond1, Bleq_2a),
        arg(PosLCondAttr, Cond2, [A1_2a,A2_2a]),
        arg(PosLCondAttr, Cond1, [A3_2a,A4_2a]),
        ((Beq_2a #= 1) #/\ (Bleq_2a #= 1)) #=> ((#\(A1_2a #= A3_2a #/\ A2_2a #= A4_2a) #/\ #\(A1_2a #= A4_2a #/\ A2_2a #= A3_2a)))
    ;
     true
    ),
    ((memberchk(Oplus, [and,or,sum,allequal,xor]), Type1 = attr_leq_attr,  Type2 = attr_leq_attr) ->                           %3 Family 1 (question - why isn't this covered by automata in a break_symmetry_between_duplicated_conds)
        arg(PosCondB,     Cond1, B1_3),
        arg(PosCondB,     Cond2, B2_3),
        arg(PosLCondAttr, Cond1, [A1_3,A2_3]),
        arg(PosLCondAttr, Cond2, [A3_3,A4_3]),
        #\(B1_3 #= 1 #/\ B2_3 #= 1 #/\ A1_3 #= A4_3 #/\ A2_3 #= A3_3)
    ;
     true
    ),
    ((memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type1 = attr_leq_attr, 
      memberchk(Type2, [attr_eq_coef,attr_leq_coef])) ->                                                                %4 Family 1
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
      Type2 = attr_leq_attr, 
      memberchk(Type1, [attr_eq_coef,attr_leq_coef])) ->
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
      Type1 = attr_leq_attr,
      memberchk(Type2, [attr_eq_coef,attr_geq_coef])) ->                                                                %5  Family 1
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
      Type2 = attr_leq_attr,
      memberchk(Type1, [attr_eq_coef,attr_geq_coef])) ->
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
      Type1 = attr_leq_attr, 
      memberchk(Type2, [attr_eq_coef,attr_geq_coef])) ->                                                                %6  Family 1
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
      Type2 = attr_leq_attr, 
      memberchk(Type1, [attr_eq_coef,attr_geq_coef])) ->
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
      Type1 = attr_leq_attr,
      memberchk(Type2, [attr_eq_coef,attr_leq_coef])) ->                                                                %7 Family 1
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
      Type2 = attr_leq_attr,
      memberchk(Type1, [attr_eq_coef,attr_leq_coef])) ->
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
    ((memberchk(Oplus, [and,or,sum,allequal,xor]),
      memberchk(Type1, [attr_leq_coef,attr_geq_coef]),
      memberchk(Type2, [attr_leq_coef,attr_geq_coef])) ->                                                               %8 Family 2 / Family 3
        arg(PosCondB,     Cond1, B1_8),
        arg(PosCondB,     Cond2, B2_8),
        arg(PosLCondAttr, Cond1, [A1_8]),
        arg(PosLCondAttr, Cond2, [A2_8]),
        ((B1_8 #= 1) #/\ (B2_8 #= 1)) #=> (A1_8 #\= A2_8)
    ;
     true
    ),
    ((memberchk(Oplus, [and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true), Type1 = attr_eq_coef,
       memberchk(Type2, [attr_leq_coef,attr_geq_coef])) ->                                                              %9 Family 1
        arg(PosCondB,     Cond1, Beq_9),
        arg(PosCondB,     Cond2, B2_9),
        arg(PosLCondAttr, Cond1, [A1_9]),
        arg(PosLCondAttr, Cond2, [A2_9]),
        ((Beq_9 #= 1) #/\ (B2_9 #= 1)) #=> (A1_9 #\= A2_9)
    ;
     (memberchk(Oplus, [and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true), Type2 = attr_eq_coef,
       memberchk(Type1, [attr_leq_coef,attr_geq_coef])) ->
        arg(PosCondB,     Cond2, Beq_9),
        arg(PosCondB,     Cond1, B2_9),
        arg(PosLCondAttr, Cond2, [A1_9]),
        arg(PosLCondAttr, Cond1, [A2_9]),
        ((Beq_9 #= 1) #/\ (B2_9 #= 1)) #=> (A1_9 #\= A2_9)
    ;
     true
    ),
    ((memberchk(Oplus, [or,allequal,xor]), (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),                                              %10  Family 1. Family 3.
      Type1 = attr_eq_coef, Type2 = attr_geq_coef) ->
        arg(PosCondB,     Cond1, Beq_10),
        arg(PosCondB,     Cond2, Bgeq_10),
        arg(PosLCondAttr, Cond1, [A1_10]),
        arg(PosLCondAttr, Cond2, [A2_10]),
        arg(PosCondCoef,  Cond1, CondCoefEq_10),
        arg(PosCondCoef,  Cond2, CondCoefGeq_10),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_10, [CondCoefMin|MinAttrs], MinAttr1_10),                            % get smallest value of attribute CondAttr1
        ((Beq_10 #= 1) #/\ (Bgeq_10 #= 1) #/\ (A1_10 #= A2_10)) #=> (((CondCoefGeq_10 - CondCoefEq_10) #> 1) #/\ (CondCoefEq_10 #> MinAttr1_10))
    ;
     (memberchk(Oplus, [or,allequal,xor]), (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type2 = attr_eq_coef, Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, Beq_10),
        arg(PosCondB,     Cond1, Bgeq_10),
        arg(PosLCondAttr, Cond2, [A1_10]),
        arg(PosLCondAttr, Cond1, [A2_10]),
        arg(PosCondCoef,  Cond2, CondCoefEq_10),
        arg(PosCondCoef,  Cond1, CondCoefGeq_10),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_10, [CondCoefMin|MinAttrs], MinAttr1_10),                            % get smallest value of attribute CondAttr1
        ((Beq_10 #= 1) #/\ (Bgeq_10 #= 1) #/\ (A1_10 #= A2_10)) #=> (((CondCoefGeq_10 - CondCoefEq_10) #> 1) #/\ (CondCoefEq_10 #> MinAttr1_10))
    ;
     true
    ),
    ((memberchk(Oplus, [or,allequal,xor]), (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),                                              %11  Family 1. Family 3.
      Type1 = attr_eq_coef, Type2 = attr_leq_coef) ->
        arg(PosCondB,     Cond1, Beq_11),
        arg(PosCondB,     Cond2, Bleq_11),
        arg(PosLCondAttr, Cond1, [A1_11]),
        arg(PosLCondAttr, Cond2, [A2_11]),
        arg(PosCondCoef,  Cond1, CondCoefEq_11),
        arg(PosCondCoef,  Cond2, CondCoefLeq_11),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_11, [CondCoefMin|MaxAttrs], MaxAttr1),                            % get smallest value of attribute CondAttr1
        ((Beq_11 #= 1) #/\ (Bleq_11 #= 1) #/\ (A1_11 #= A2_11)) #=> (((CondCoefEq_11 - CondCoefLeq_11) #> 1) #/\ (CondCoefEq_11 #< MaxAttr1))
    ;
     (memberchk(Oplus, [or,allequal,xor]), (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type2 = attr_eq_coef, Type1 = attr_leq_coef) ->
        arg(PosCondB,     Cond2, Beq_11),
        arg(PosCondB,     Cond1, Bgeq_11),
        arg(PosLCondAttr, Cond2, [A1_11]),
        arg(PosLCondAttr, Cond1, [A2_11]),
        arg(PosCondCoef,  Cond2, CondCoefEq_11),
        arg(PosCondCoef,  Cond1, CondCoefGeq_11),
        memberchk(t(cond(attr_eq_coef(coef(CondCoefMin,_))),_,_), LConds),
        element(A1_11, [CondCoefMin|MaxAttrs], MaxAttr1),                            % get smallest value of attribute CondAttr1
        ((Beq_11 #= 1) #/\ (Bgeq_11 #= 1) #/\ (A1_11 #= A2_11)) #=> (((CondCoefGeq_11 - CondCoefEq_11) #> 1) #/\ (CondCoefEq_11 #< MaxAttr1))
    ;
     true
    ),
    ((memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type1 = attr_in_interval,
      memberchk(Type2, [attr_eq_coef,attr_leq_coef,attr_geq_coef])) ->                                                  %12 Family 1 
        arg(PosCondB,     Cond1, Bin_12),
        arg(PosCondB,     Cond2, B2_12),
        arg(PosLCondAttr, Cond1, [A1_12]),
        arg(PosLCondAttr, Cond2, [A2_12]),
        ((Bin_12 #> -1) #/\ (B2_12 #= 1)) #=> (A1_12 #\= A2_12)
    ;
     (memberchk(Oplus, [and,or,sum,allequal,xor]),
      Type2 = attr_in_interval,
      memberchk(Type1, [attr_eq_coef,attr_leq_coef,attr_geq_coef])) ->
        arg(PosCondB,     Cond2, Bin_12),
        arg(PosCondB,     Cond1, B2_12),
        arg(PosLCondAttr, Cond2, [A1_12]),
        arg(PosLCondAttr, Cond1, [A2_12]),
        ((Bin_12 #> -1) #/\ (B2_12 #= 1)) #=> (A1_12 #\= A2_12)
    ;
     true
    ),
    ((Oplus = or,
      Type1 = attr_eq_attr,
      Type2 = attr_leq_unary(prod)) ->                                                                                  %13 Family 1
        arg(PosCondB,     Cond1, B1_13),
        arg(PosCondB,     Cond2, B2_13),
        arg(PosLCondAttr, Cond1, [A1_13,A2_13]),
        arg(PosLCondAttr, Cond2, [A3_13,A4_13]),
        ((B1_13 #= 1) #/\ (B2_13 #= 1)) #=> (((A1_13 #\= A3_13) #\/ (A2_13 #\= A4_13)) #/\ ((A1_13 #\= A4_13) #\/ (A2_13 #\= A3_13)))
    ;
     (Oplus = or,
      Type2 = attr_eq_attr,
      Type1 = attr_leq_unary(prod)) ->
        arg(PosCondB,     Cond2, B1_13),
        arg(PosCondB,     Cond1, B2_13),
        arg(PosLCondAttr, Cond2, [A1_13,A2_13]),
        arg(PosLCondAttr, Cond1, [A3_13,A4_13]),
        ((B1_13 #= 1) #/\ (B2_13 #= 1)) #=> (((A1_13 #\= A3_13) #\/ (A2_13 #\= A4_13)) #/\ ((A1_13 #\= A4_13) #\/ (A2_13 #\= A3_13)))
    ;
     true
    ),
    ((Oplus = and,
      Type1 = attr_leq_attr,
      memberchk(Type2, [unary_leq_attr(prod),attr_eq_unary(prod)])) ->                                                  %14 Family 1 /  Family 2 (depending if [A3,A4] mirrors [A1,A2] or not)
        arg(PosCondB,     Cond1, B1_14),
        arg(PosCondB,     Cond2, B2_14),
        arg(PosLCondAttr, Cond1, [A1_14,A2_14]),
        arg(PosLCondAttr, Cond2, [A3_14,A4_14]),
        ((B1_14 #= 1) #/\ (B2_14 #= 1)) #=> (((A1_14 #\= A3_14) #\/ (A2_14 #\= A4_14)) #/\ ((A1_14 #\= A4_14) #\/ (A2_14 #\= A3_14)))
    ;
     (Oplus = and,
      Type2 = attr_leq_attr,
      memberchk(Type1, [unary_leq_attr(prod),attr_eq_unary(prod)])) ->
        arg(PosCondB,     Cond2, B1_14),
        arg(PosCondB,     Cond1, B2_14),
        arg(PosLCondAttr, Cond2, [A1_14,A2_14]),
        arg(PosLCondAttr, Cond1, [A3_14,A4_14]),
        ((B1_14 #= 1) #/\ (B2_14 #= 1)) #=> (((A1_14 #\= A3_14) #\/ (A2_14 #\= A4_14)) #/\ ((A1_14 #\= A4_14) #\/ (A2_14 #\= A3_14)))
    ;
     true
    ),
    ((Oplus = and,
      Type1 = attr_leq_attr,
      Type2 = attr_leq_unary(prod)) ->                                                                                  %15 Family 1
        arg(PosCondB,     Cond1, B1_15),
        arg(PosCondB,     Cond2, B2_15),
        arg(PosLCondAttr, Cond1, [A1_15,A2_15]),
        arg(PosLCondAttr, Cond2, [A3_15,A4_15]),
        ((B1_15 #= 1) #/\ (B2_15 #= 1)) #=> ((A1_15 #\= A3_15) #\/ (A2_15 #\= A4_15))
    ;
     (Oplus = and,
      Type2 = attr_leq_attr,
      Type1 = attr_leq_unary(prod)) ->
        arg(PosCondB,     Cond2, B1_15),
        arg(PosCondB,     Cond1, B2_15),
        arg(PosLCondAttr, Cond2, [A1_15,A2_15]),
        arg(PosLCondAttr, Cond1, [A3_15,A4_15]),
        ((B1_15 #= 1) #/\ (B2_15 #= 1)) #=> ((A1_15 #\= A3_15) #\/ (A2_15 #\= A4_15))
    ;
     true
    ),
    ((Oplus = or,
      Type1 = attr_leq_attr,
      Type2 = unary_leq_attr(prod)) ->                                                                                  %16 Family 1
        arg(PosCondB,     Cond1, B1_16),
        arg(PosCondB,     Cond2, B2_16),
        arg(PosLCondAttr, Cond1, [A1_16,A2_16]),
        arg(PosLCondAttr, Cond2, [A3_16,A4_16]),
        ((B1_16 #= 1) #/\ (B2_16 #= 1)) #=> ((A1_16 #\= A3_16) #\/ (A2_16 #\= A4_16))
    ;
     (Oplus = or,
      Type2 = attr_leq_attr,
      Type1 = unary_leq_attr(prod)) ->
        arg(PosCondB,     Cond2, B1_16),
        arg(PosCondB,     Cond1, B2_16),
        arg(PosLCondAttr, Cond2, [A1_16,A2_16]),
        arg(PosLCondAttr, Cond1, [A3_16,A4_16]),
        ((B1_16 #= 1) #/\ (B2_16 #= 1)) #=> ((A1_16 #\= A3_16) #\/ (A2_16 #\= A4_16))
    ;
     true
    ),
    ((Oplus = or,
      Type1 = attr_leq_attr,
      Type2 = attr_eq_unary(prod)) ->                                                                                  %17 Family 1
        arg(PosCondB,     Cond1, B1_17),
        arg(PosCondB,     Cond2, B2_17),
        arg(PosLCondAttr, Cond1, [A1_17,A2_17]),
        arg(PosLCondAttr, Cond2, [A3_17,A4_17]),
        ((B1_17 #= 1) #/\ (B2_17 #= 1)) #=> ((A1_17 #\= A4_17) #\/ (A2_17 #\= A3_17))
    ;
     (Oplus = or,
      Type2 = attr_leq_attr,
      Type1 = attr_eq_unary(prod)) ->  
        arg(PosCondB,     Cond2, B1_17),
        arg(PosCondB,     Cond1, B2_17),
        arg(PosLCondAttr, Cond2, [A1_17,A2_17]),
        arg(PosLCondAttr, Cond1, [A3_17,A4_17]),
        ((B1_17 #= 1) #/\ (B2_17 #= 1)) #=> ((A1_17 #\= A4_17) #\/ (A2_17 #\= A3_17))
    ;
     true
    ),
    ((Oplus = or,
      Type1 = attr_leq_attr,
      Type2 = attr_leq_unary(prod)) ->                                                                                  %18 Family 1
        arg(PosCondB,     Cond1, B1_18),
        arg(PosCondB,     Cond2, B2_18),
        arg(PosLCondAttr, Cond1, [A1_18,A2_18]),
        arg(PosLCondAttr, Cond2, [A3_18,A4_18]),
        ((B1_18 #= 1) #/\ (B2_18 #= 1)) #=> (((A1_18 #\= A3_18) #\/ (A2_18 #\= A4_18)) #/\ ((A1_18 #\= A4_18) #\/ (A2_18 #\= A3_18)))
    ;
     (Oplus = or,
      Type2 = attr_leq_attr,
      Type1 = attr_leq_unary(prod)) ->  
        arg(PosCondB,     Cond2, B1_18),
        arg(PosCondB,     Cond1, B2_18),
        arg(PosLCondAttr, Cond2, [A1_18,A2_18]),
        arg(PosLCondAttr, Cond1, [A3_18,A4_18]),
        ((B1_18 #= 1) #/\ (B2_18 #= 1)) #=> (((A1_18 #\= A3_18) #\/ (A2_18 #\= A4_18)) #/\ ((A1_18 #\= A4_18) #\/ (A2_18 #\= A3_18)))
    ;
     true
    ),
    ((memberchk(Oplus, [allequal,xor]),
      memberchk(Type1, [attr_eq_attr,attr_leq_attr]),
      memberchk(Type2, [unary_leq_attr(prod),attr_eq_unary(prod),attr_leq_unary(prod)])) ->                             %19 Family 3. Prevent comparisons between attr_cmp_attr and attr_cmp_unary(prod) for allequal,xor because it's difficult to interpret such formulae (especially with three terms)
        arg(PosCondB,     Cond1, B1_19),
        arg(PosCondB,     Cond2, B2_19),
        arg(PosLCondAttr, Cond1, [A1_19,A2_19]),
        arg(PosLCondAttr, Cond2, [A3_19,A4_19]),
        ((B1_19 #= 1) #/\ (B2_19 #= 1)) #=> (((A1_19 #\= A3_19) #\/ (A2_19 #\= A4_19)) #/\ ((A1_19 #\= A4_19) #\/ (A2_19 #\= A3_19)))
    ;
     (memberchk(Oplus, [allequal,xor]),
      memberchk(Type2, [attr_eq_attr,attr_leq_attr]),
      memberchk(Type1, [unary_leq_attr(prod),attr_eq_unary(prod),attr_leq_unary(prod)])) ->
        arg(PosCondB,     Cond2, B1_19),
        arg(PosCondB,     Cond1, B2_19),
        arg(PosLCondAttr, Cond2, [A1_19,A2_19]),
        arg(PosLCondAttr, Cond1, [A3_19,A4_19]),
        ((B1_19 #= 1) #/\ (B2_19 #= 1)) #=> (((A1_19 #\= A3_19) #\/ (A2_19 #\= A4_19)) #/\ ((A1_19 #\= A4_19) #\/ (A2_19 #\= A3_19)))
    ;
     true
    ),
    ((memberchk(Oplus, [sum,allequal,xor]),
      Type1 = unary_term_leq_coef(UType1),
      Type2 = unary_term_geq_coef(UType2),
      UType1 = UType2) ->                                                                                               %20 Family 3
        arg(PosCondB,     Cond1, Bleq_20),
        arg(PosCondB,     Cond2, Bgeq_20),
        arg(PosLCondAttr, Cond1, [A1_20]),
        arg(PosLCondAttr, Cond2, [A2_20]),
        arg(PosCondCst,   Cond1, D1_20),
        arg(PosCondCst,   Cond2, D2_20),
        ((Bleq_20 #= 1) #/\ (Bgeq_20 #= 1)) #=> ((A1_20 #\= A2_20) #\/ (D1_20 #\= D2_20))
    ;
     (memberchk(Oplus, [sum,allequal,xor]),
      Type2 = unary_term_leq_coef(UType2),
      Type1 = unary_term_geq_coef(UType1),
      UType1 = UType2) ->
        arg(PosCondB,     Cond2, Bleq_20),
        arg(PosCondB,     Cond1, Bgeq_20),
        arg(PosLCondAttr, Cond2, [A1_20]),
        arg(PosLCondAttr, Cond1, [A2_20]),
        arg(PosCondCst,   Cond2, D1_20),
        arg(PosCondCst,   Cond1, D2_20),
        ((Bleq_20 #= 1) #/\ (Bgeq_20 #= 1)) #=> ((A1_20 #\= A2_20) #\/ (D1_20 #\= D2_20))
    ;
     true
    ),
    ((Oplus = and,
      Type1 = unary_term_leq_coef(UType1),
      Type2 = unary_term_geq_coef(UType2),
      UType1 = UType2) ->                                                                                               %21 Family 2
        arg(PosCondB,     Cond1, Bleq_21),
        arg(PosCondB,     Cond2, Bgeq_21),
        arg(PosLCondAttr, Cond1, [A1_21]),
        arg(PosLCondAttr, Cond2, [A2_21]),
        arg(PosCondCst,   Cond1, D1_21),
        arg(PosCondCst,   Cond2, D2_21),
        arg(PosCondCoef,  Cond1, C1_21),
        arg(PosCondCoef,  Cond2, C2_21),
        ((Bleq_21 #= 1) #/\ (Bgeq_21 #= 1) #/\ (A1_21 #= A2_21) #/\ (D1_21 #= D2_21)) #=> (C1_21 #> C2_21)
    ;
     (Oplus = and,
      Type2 = unary_term_leq_coef(UType2),
      Type1 = unary_term_geq_coef(UType1),
      UType1 = UType2) ->
        arg(PosCondB,     Cond2, Bleq_21),
        arg(PosCondB,     Cond1, Bgeq_21),
        arg(PosLCondAttr, Cond2, [A1_21]),
        arg(PosLCondAttr, Cond1, [A2_21]),
        arg(PosCondCst,   Cond2, D1_21),
        arg(PosCondCst,   Cond1, D2_21),
        arg(PosCondCoef,  Cond2, C1_21),
        arg(PosCondCoef,  Cond1, C2_21),
        ((Bleq_21 #= 1) #/\ (Bgeq_21 #= 1) #/\ (A1_21 #= A2_21) #/\ (D1_21 #= D2_21)) #=> (C1_21 #> C2_21)
    ;
     true
    ),
    ((Oplus = or,
      Type1 = unary_term_leq_coef(UType1),
      Type2 = unary_term_geq_coef(UType2),
      UType1 = UType2) ->                                                                                               %22 Family 1
        arg(PosCondB,     Cond1, Bleq_22),
        arg(PosCondB,     Cond2, Bgeq_22),
        arg(PosLCondAttr, Cond1, [A1_22]),
        arg(PosLCondAttr, Cond2, [A2_22]),
        arg(PosCondCst,   Cond1, D1_22),
        arg(PosCondCst,   Cond2, D2_22),
        arg(PosCondCoef,  Cond1, C1_22),
        arg(PosCondCoef,  Cond2, C2_22),
        ((Bleq_22 #= 1) #/\ (Bgeq_22 #= 1) #/\ (A1_22 #= A2_22) #/\ (D1_22 #= D2_22)) #=> ((C2_22 - C1_22) #> 1)
    ;
     (Oplus = or,
      Type2 = unary_term_leq_coef(UType1),
      Type1 = unary_term_geq_coef(UType2),
      UType1 = UType2) ->
        arg(PosCondB,     Cond2, Bleq_22),
        arg(PosCondB,     Cond1, Bgeq_22),
        arg(PosLCondAttr, Cond2, [A1_22]),
        arg(PosLCondAttr, Cond1, [A2_22]),
        arg(PosCondCst,   Cond2, D1_22),
        arg(PosCondCst,   Cond1, D2_22),
        arg(PosCondCoef,  Cond2, C1_22),
        arg(PosCondCoef,  Cond1, C2_22),
        ((Bleq_22 #= 1) #/\ (Bgeq_22 #= 1) #/\ (A1_22 #= A2_22) #/\ (D1_22 #= D2_22)) #=> ((C2_22 - C1_22) #> 1)
    ;
     true
    ),
    ((memberchk(Oplus,[and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      Type1 = unary_term_eq_coef(UType1),
      Type2 = unary_term_geq_coef(UType2),
      UType1 = UType2) ->                                                                                               %23 Family 1
        arg(PosCondB,     Cond1, Beq_23),
        arg(PosCondB,     Cond2, Bgeq_23),
        arg(PosLCondAttr, Cond1, [A1_23]),
        arg(PosLCondAttr, Cond2, [A2_23]),
        arg(PosCondCst,   Cond1, D1_23),
        arg(PosCondCst,   Cond2, D2_23),
        ((Beq_23 #= 1) #/\ (Bgeq_23 #= 1)) #=> ((A1_23 #\= A2_23) #\/ (D1_23 #\= D2_23))
    ;
     (memberchk(Oplus,[and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      Type2 = unary_term_eq_coef(UType2),
      Type1 = unary_term_geq_coef(UType1),
      UType1 = UType2) ->
        arg(PosCondB,     Cond2, Beq_23),
        arg(PosCondB,     Cond1, Bgeq_23),
        arg(PosLCondAttr, Cond2, [A1_23]),
        arg(PosLCondAttr, Cond1, [A2_23]),
        arg(PosCondCst,   Cond2, D1_23),
        arg(PosCondCst,   Cond1, D2_23),
        ((Beq_23 #= 1) #/\ (Bgeq_23 #= 1)) #=> ((A1_23 #\= A2_23) #\/ (D1_23 #\= D2_23))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type1 = unary_term_eq_coef(UType1),
      Type2 = unary_term_geq_coef(UType2),
      UType1 = UType2) ->                                                                                               %24 Family 1
        arg(PosCondB,     Cond1, Beq_24),
        arg(PosCondB,     Cond2, Bgeq_24),
        arg(PosLCondAttr, Cond1, [A1_24]),
        arg(PosLCondAttr, Cond2, [A2_24]),
        arg(PosCondCst,   Cond1, D1_24),
        arg(PosCondCst,   Cond2, D2_24),
        arg(PosCondCoef,  Cond1, C1_24),
        arg(PosCondCoef,  Cond2, C2_24),
        ((Beq_24 #= 1) #/\ (Bgeq_24 #= 1) #/\ (A1_24 #= A2_24) #/\ (D1_24 #= D2_24)) #=> ((C2_24 - C1_24) #> 1)
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type2 = unary_term_eq_coef(UType2),
      Type1 = unary_term_geq_coef(UType1),
      UType1 = UType2) ->
        arg(PosCondB,     Cond2, Beq_24),
        arg(PosCondB,     Cond1, Bgeq_24),
        arg(PosLCondAttr, Cond2, [A1_24]),
        arg(PosLCondAttr, Cond1, [A2_24]),
        arg(PosCondCst,   Cond2, D1_24),
        arg(PosCondCst,   Cond1, D2_24),
        arg(PosCondCoef,  Cond2, C1_24),
        arg(PosCondCoef,  Cond1, C2_24),
        ((Beq_24 #= 1) #/\ (Bgeq_24 #= 1) #/\ (A1_24 #= A2_24) #/\ (D1_24 #= D2_24)) #=> ((C2_24 - C1_24) #> 1)
    ;
     true
    ),
    ((memberchk(Oplus,[and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      Type1 = unary_term_leq_coef(UType1),
      Type2 = unary_term_eq_coef(UType2),
      UType1 = UType2) ->                                                                                               %25 Family 1
        arg(PosCondB,     Cond1, Bleq_25),
        arg(PosCondB,     Cond2, Beq_25),
        arg(PosLCondAttr, Cond1, [A1_25]),
        arg(PosLCondAttr, Cond2, [A2_25]),
        arg(PosCondCst,   Cond1, D1_25),
        arg(PosCondCst,   Cond2, D2_25),
        ((Bleq_25 #= 1) #/\ (Beq_25 #= 1)) #=> ((A1_25 #\= A2_25) #\/ (D1_25 #\= D2_25))
    ;
     (memberchk(Oplus,[and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      Type2 = unary_term_leq_coef(UType2),
      Type1 = unary_term_eq_coef(UType1),
      UType1 = UType2) ->
        arg(PosCondB,     Cond2, Bleq_25),
        arg(PosCondB,     Cond1, Beq_25),
        arg(PosLCondAttr, Cond2, [A1_25]),
        arg(PosLCondAttr, Cond1, [A2_25]),
        arg(PosCondCst,   Cond2, D1_25),
        arg(PosCondCst,   Cond1, D2_25),
        ((Bleq_25 #= 1) #/\ (Beq_25 #= 1)) #=> ((A1_25 #\= A2_25) #\/ (D1_25 #\= D2_25))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type1 = unary_term_leq_coef(UType1),
      Type2 = unary_term_eq_coef(UType2),
      UType1 = UType2) ->                                                                                               %26 Family 1
        arg(PosCondB,     Cond1, Bleq_26),
        arg(PosCondB,     Cond2, Beq_26),
        arg(PosLCondAttr, Cond1, [A1_26]),
        arg(PosLCondAttr, Cond2, [A2_26]),
        arg(PosCondCst,   Cond1, D1_26),
        arg(PosCondCst,   Cond2, D2_26),
        arg(PosCondCoef,  Cond1, C1_26),
        arg(PosCondCoef,  Cond2, C2_26),
        ((Bleq_26 #= 1) #/\ (Beq_26 #= 1) #/\ (A1_26 #= A2_26) #/\ (D1_26 #= D2_26)) #=> ((C1_26 - C2_26) #> 1)
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type2 = unary_term_leq_coef(UType2),
      Type1 = unary_term_eq_coef(UType1),
      UType1 = UType2) ->
        arg(PosCondB,     Cond2, Bleq_26),
        arg(PosCondB,     Cond1, Beq_26),
        arg(PosLCondAttr, Cond2, [A1_26]),
        arg(PosLCondAttr, Cond1, [A2_26]),
        arg(PosCondCst,   Cond2, D1_26),
        arg(PosCondCst,   Cond1, D2_26),
        arg(PosCondCoef,  Cond2, C1_26),
        arg(PosCondCoef,  Cond1, C2_26),
        ((Bleq_26 #= 1) #/\ (Beq_26 #= 1) #/\ (A1_26 #= A2_26) #/\ (D1_26 #= D2_26)) #=> ((C1_26 - C2_26) #> 1)
    ;
     true
    ),
    ((memberchk(Oplus,[and,or,allequal,xor]),
      Type1 = attr_eq_coef,
      Type2 = unary_term_eq_coef(mod)) ->                                                                               %27 Family 1. To prevent cases like [x1=4] and [x1 mod 4 = 0]
        arg(PosCondB,     Cond1, B1_27),
        arg(PosCondB,     Cond2, B2_27),
        arg(PosLCondAttr, Cond1, [A1_27]),
        arg(PosLCondAttr, Cond2, [A2_27]),
        arg(PosCondCoef,  Cond1, C1_27),
        arg(PosCondCoef,  Cond2, C2_27),
        arg(PosCondCst,   Cond2, D_27),
        ((B1_27 #= 1) #/\ (B2_27 #= 1) #/\ (A1_27 #= A2_27)) #=> #\((C1_27 #= D_27) #/\ (C2_27 #= 0)) 
    ;
     (memberchk(Oplus,[and,or,allequal,xor]),
      Type2 = attr_eq_coef,
      Type1 = unary_term_eq_coef(mod)) ->
        arg(PosCondB,     Cond2, B1_27),
        arg(PosCondB,     Cond1, B2_27),
        arg(PosLCondAttr, Cond2, [A1_27]),
        arg(PosLCondAttr, Cond1, [A2_27]),
        arg(PosCondCoef,  Cond2, C1_27),
        arg(PosCondCoef,  Cond1, C2_27),
        arg(PosCondCst,   Cond1, D_27),
        ((B1_27 #= 1) #/\ (B2_27 #= 1) #/\ (A1_27 #= A2_27)) #=> #\((C1_27 #= D_27) #/\ (C2_27 #= 0))
    ;
     true
    ),
    ((memberchk(Oplus,[sum,allequal,xor]),
      memberchk(Type1, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
      BType1 = BType2) ->                                                                                               %28 Family 2
        arg(PosCondB,     Cond1, Bleq_28),
        arg(PosCondB,     Cond2, Bgeq_28),
        arg(PosLCondAttr, Cond1, [A1_28,A2_28]),
        arg(PosLCondAttr, Cond2, [A3_28,A4_28]),
        ((Bleq_28 #= 1) #/\ (Bgeq_28 #= 1)) #=> ((A1_28 #\= A3_28) #\/ (A2_28 #\= A4_28))
    ;
     (memberchk(Oplus,[sum,allequal,xor]),
      memberchk(Type2, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
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
      memberchk(Type1, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
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
      memberchk(Type2, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
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
      memberchk(Type1, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
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
      memberchk(Type2, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
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
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      memberchk(Type1, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
      memberchk(Type2, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
      BType1 = BType2) ->                                                                                               %31 Family 1
        arg(PosCondB,     Cond1, Beq_31),
        arg(PosCondB,     Cond2, Bgeq_31),
        arg(PosLCondAttr, Cond1, [A1_31,A2_31]),
        arg(PosLCondAttr, Cond2, [A3_31,A4_31]),
        ((Beq_31 #= 1) #/\ (Bgeq_31 #= 1)) #=> ((A1_31 #\= A3_31) #\/ (A2_31 #\= A4_31))
    ;
     (memberchk(Oplus,[and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      memberchk(Type2, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
      memberchk(Type1, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
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
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type1, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
      memberchk(Type2, [binary_term_geq_coef(BType2),binary_term_geq_coef(BType2,_)]),
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
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type2, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
      memberchk(Type1, [binary_term_geq_coef(BType1),binary_term_geq_coef(BType1,_)]),
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
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      memberchk(Type1, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
      BType1 = BType2) ->                                                                                               %33 Family 3
        arg(PosCondB,     Cond1, Bleq_33),
        arg(PosCondB,     Cond2, Beq_33),
        arg(PosLCondAttr, Cond1, [A1_33,A2_33]),
        arg(PosLCondAttr, Cond2, [A3_33,A4_33]),
        ((Bleq_33 #= 1) #/\ (Beq_33 #= 1)) #=> ((A1_33 #\= A3_33) #\/ (A2_33 #\= A4_33))
    ;
     (memberchk(Oplus,[and,sum,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 3 ; true),
      memberchk(Type2, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
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
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type1, [binary_term_leq_coef(BType1),binary_term_leq_coef(BType1,_)]),
      memberchk(Type2, [binary_term_eq_coef(BType2), binary_term_eq_coef(BType2,_)]),
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
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type2, [binary_term_leq_coef(BType2),binary_term_leq_coef(BType2,_)]),
      memberchk(Type1, [binary_term_eq_coef(BType1), binary_term_eq_coef(BType1,_)]),
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
      memberchk(Type1, [binary_term_eq_coef(BType), binary_term_eq_coef(BType,_)]),
      memberchk(BType, [plus,minus,prod,floor,ceil,min,max]),
      Type2 = attr_eq_coef) ->                                                                                          %35 Family 3. If we fix one of the attributes we can fix the second one as well (either by attr_eq_coef or attr_in_interval)
        arg(PosCondB,     Cond1, B1_35),
        arg(PosCondB,     Cond2, B2_35),
        arg(PosLCondAttr, Cond1, [A1_35,A2_35]),
        arg(PosLCondAttr, Cond2, [A3_35]),
        ((B1_35 #= 1) #/\ (B2_35 #= 1)) #=> ((A1_35 #\= A3_35) #\/ (A2_35 #\= A3_35))
    ;
     (Oplus = and,
      memberchk(Type2, [binary_term_eq_coef(BType), binary_term_eq_coef(BType,_)]),
      memberchk(BType, [plus,minus,prod,floor,ceil,min,max]),
      Type1 = attr_eq_coef) ->
        arg(PosCondB,     Cond2, B1_35),
        arg(PosCondB,     Cond1, B2_35),
        arg(PosLCondAttr, Cond2, [A1_35,A2_35]),
        arg(PosLCondAttr, Cond1, [A3_35]),
        ((B1_35 #= 1) #/\ (B2_35 #= 1)) #=> ((A1_35 #\= A3_35) #\/ (A2_35 #\= A3_35))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type1 = binary_term_leq_coef(minus),
      Type2 = attr_eq_attr) ->                                                                                          %36 Family 1
        arg(PosCondB,     Cond1, B1_36),
        arg(PosCondB,     Cond2, B2_36),
        arg(PosLCondAttr, Cond1, [A1_36,A2_36]),
        arg(PosLCondAttr, Cond2, [A3_36,A4_36]),
        ((B1_36 #= 1) #/\ (B2_36 #= 1)) #=> (((A1_36 #\= A3_36) #\/ (A2_36 #\= A4_36)) #/\ ((A1_36 #\= A4_36) #\/ (A2_36 #\= A3_36)))
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type2 = binary_term_leq_coef(minus),
      Type1 = attr_eq_attr) ->
        arg(PosCondB,     Cond2, B1_36),
        arg(PosCondB,     Cond1, B2_36),
        arg(PosLCondAttr, Cond2, [A1_36,A2_36]),
        arg(PosLCondAttr, Cond1, [A3_36,A4_36]),
        ((B1_36 #= 1) #/\ (B2_36 #= 1)) #=> (((A1_36 #\= A3_36) #\/ (A2_36 #\= A4_36)) #/\ ((A1_36 #\= A4_36) #\/ (A2_36 #\= A3_36)))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type1 = binary_term_geq_coef(minus),
      Type2 = attr_eq_attr) ->                                                                                          %37 Family 1
        arg(PosCondB,     Cond1, B1_37),
        arg(PosCondB,     Cond2, B2_37),
        arg(PosLCondAttr, Cond1, [A1_37,A2_37]),
        arg(PosLCondAttr, Cond2, [A3_37,A4_37]),
        arg(PosCondCoef,  Cond1, C_37),
        ((B1_37 #= 1) #/\ (B2_37 #= 1) #/\ (((A1_37 #= A3_37) #/\ (A2_37 #= A4_37)) #\/ ((A1_37 #= A4_37) #/\ (A2_37 #= A3_37)))) #=> (C_37 #> 1)
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type2 = binary_term_geq_coef(minus),
      Type1 = attr_eq_attr) ->
        arg(PosCondB,     Cond2, B1_37),
        arg(PosCondB,     Cond1, B2_37),
        arg(PosLCondAttr, Cond2, [A1_37,A2_37]),
        arg(PosLCondAttr, Cond1, [A3_37,A4_37]),
        arg(PosCondCoef,  Cond2, C_37),
        ((B1_37 #= 1) #/\ (B2_37 #= 1) #/\ (((A1_37 #= A3_37) #/\ (A2_37 #= A4_37)) #\/ ((A1_37 #= A4_37) #/\ (A2_37 #= A3_37)))) #=> (C_37 #> 1)
    ;
     true
    ),
    ((Oplus = and,
      Type1 = binary_term_leq_coef(minus),
      Type2 = attr_leq_attr) ->                                                                                         %38 Family 1
        arg(PosCondB,     Cond1, B1_38),
        arg(PosCondB,     Cond2, B2_38),
        arg(PosLCondAttr, Cond1, [A1_38,A2_38]),
        arg(PosLCondAttr, Cond2, [A3_38,A4_38]),
        ((B1_38 #= 1) #/\ (B2_38 #= 1)) #=> ((A1_38 #\= A3_38) #\/ (A2_38 #\= A4_38))
    ;
     (Oplus = and,
      Type2 = binary_term_leq_coef(minus),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_38),
        arg(PosCondB,     Cond1, B2_38),
        arg(PosLCondAttr, Cond2, [A1_38,A2_38]),
        arg(PosLCondAttr, Cond1, [A3_38,A4_38]),
        ((B1_38 #= 1) #/\ (B2_38 #= 1)) #=> ((A1_38 #\= A3_38) #\/ (A2_38 #\= A4_38))
    ;
     true
    ),
    ((Oplus = and,
      Type1 = binary_term_leq_coef(minus),
      Type2 = attr_leq_attr) ->                                                                                         %39 Family 2
        arg(PosCondB,     Cond1, B1_39),
        arg(PosCondB,     Cond2, B2_39),
        arg(PosLCondAttr, Cond1, [A1_39,A2_39]),
        arg(PosLCondAttr, Cond2, [A3_39,A4_39]),
        arg(PosCondCoef,  Cond1, C_39),
        ((B1_39 #= 1) #/\ (B2_39 #= 1) #/\ (A1_39 #= A4_39) #/\ (A2_39 #= A3_39)) #=> (C_39 #> 1)
    ;
     (Oplus = and,
      Type2 = binary_term_leq_coef(minus),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_39),
        arg(PosCondB,     Cond1, B2_39),
        arg(PosLCondAttr, Cond2, [A1_39,A2_39]),
        arg(PosLCondAttr, Cond1, [A3_39,A4_39]),
        arg(PosCondCoef,  Cond2, C_39),
        ((B1_39 #= 1) #/\ (B2_39 #= 1) #/\ (A1_39 #= A4_39) #/\ (A2_39 #= A3_39)) #=> (C_39 #> 1)
    ;
     true
    ),
    ((Oplus = and,
      Type1 = binary_term_geq_coef(minus),
      Type2 = attr_leq_attr) ->                                                                                         %40 Family 2
        arg(PosCondB,     Cond1, B1_40),
        arg(PosCondB,     Cond2, B2_40),
        arg(PosLCondAttr, Cond1, [A1_40,A2_40]),
        arg(PosLCondAttr, Cond2, [A3_40,A4_40]),
        ((B1_40 #= 1) #/\ (B2_40 #= 1)) #=> (((A1_40 #\= A3_40) #\/ (A2_40 #\= A4_40)) #/\ ((A1_40 #\= A4_40) #\/ (A2_40 #\= A3_40)))
    ;
     (Oplus = and,
      Type2 = binary_term_geq_coef(minus),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_40),
        arg(PosCondB,     Cond1, B2_40),
        arg(PosLCondAttr, Cond2, [A1_40,A2_40]),
        arg(PosLCondAttr, Cond1, [A3_40,A4_40]),
        ((B1_40 #= 1) #/\ (B2_40 #= 1)) #=> (((A1_40 #\= A3_40) #\/ (A2_40 #\= A4_40)) #/\ ((A1_40 #\= A4_40) #\/ (A2_40 #\= A3_40)))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type1, [binary_term_eq_coef(minus),binary_term_geq_coef(minus)]),
      Type2 = attr_leq_attr) ->                                                                                         %41 Family 1
        arg(PosCondB,     Cond1, B1_41),
        arg(PosCondB,     Cond2, B2_41),
        arg(PosLCondAttr, Cond1, [A1_41,A2_41]),
        arg(PosLCondAttr, Cond2, [A3_41,A4_41]),
        ((B1_41 #= 1) #/\ (B2_41 #= 1)) #=> ((A1_41 #\= A4_41) #\/ (A2_41 #\= A3_41))
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type2, [binary_term_eq_coef(minus),binary_term_geq_coef(minus)]),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_41),
        arg(PosCondB,     Cond1, B2_41),
        arg(PosLCondAttr, Cond2, [A1_41,A2_41]),
        arg(PosLCondAttr, Cond1, [A3_41,A4_41]),
        ((B1_41 #= 1) #/\ (B2_41 #= 1)) #=> ((A1_41 #\= A4_41) #\/ (A2_41 #\= A3_41))
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type1, [binary_term_eq_coef(minus),binary_term_geq_coef(minus)]),
      Type2 = attr_leq_attr) ->                                                                                         %42 Family 1
        arg(PosCondB,     Cond1, B1_42),
        arg(PosCondB,     Cond2, B2_42),
        arg(PosLCondAttr, Cond1, [A1_42,A2_42]),
        arg(PosLCondAttr, Cond2, [A3_42,A4_42]),
        arg(PosCondCoef,  Cond1, C_42),
        ((B1_42 #= 1) #/\ (B2_42 #= 1) #/\ (A1_42 #= A3_42) #/\ (A2_42 #= A4_42)) #=> (C_42 #> 1)
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      memberchk(Type2, [binary_term_eq_coef(minus),binary_term_geq_coef(minus)]),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_42),
        arg(PosCondB,     Cond1, B2_42),
        arg(PosLCondAttr, Cond2, [A1_42,A2_42]),
        arg(PosLCondAttr, Cond1, [A3_42,A4_42]),
        arg(PosCondCoef,  Cond2, C_42),
        ((B1_42 #= 1) #/\ (B2_42 #= 1) #/\ (A1_42 #= A3_42) #/\ (A2_42 #= A4_42)) #=> (C_42 #> 1)
    ;
     true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type1 = binary_term_leq_coef(minus),
      Type2 = attr_leq_attr) ->                                                                                         %43  Family 2
        arg(PosCondB,     Cond1, B1_43),
        arg(PosCondB,     Cond2, B2_43),
        arg(PosLCondAttr, Cond1, [A1_43,A2_43]),
        arg(PosLCondAttr, Cond2, [A3_43,A4_43]),
        ((B1_43 #= 1) #/\ (B2_43 #= 1)) #=> (((A1_43 #\= A3_43) #\/ (A2_43 #\= A4_43)) #/\ ((A1_43 #\= A4_43) #\/ (A2_43 #\= A3_43)))
    ;
     (memberchk(Oplus,[or,allequal,xor]),
      (memberchk(Oplus, [allequal,xor]) -> NbTerms = 2 ; true),
      Type2 = binary_term_leq_coef(minus),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_43),
        arg(PosCondB,     Cond1, B2_43),
        arg(PosLCondAttr, Cond2, [A1_43,A2_43]),
        arg(PosLCondAttr, Cond1, [A3_43,A4_43]),
        ((B1_43 #= 1) #/\ (B2_43 #= 1)) #=> (((A1_43 #\= A3_43) #\/ (A2_43 #\= A4_43)) #/\ ((A1_43 #\= A4_43) #\/ (A2_43 #\= A3_43)))
    ;
        true
    ),
    ((Oplus = and,
      memberchk(Type1, [binary_term_eq_coef(min),binary_term_leq_coef(min)]),
      Type2 = attr_eq_coef) ->                                                                                          %44 Family 1 /  Family 2
        arg(PosCondB,     Cond1, B1_44),
        arg(PosCondB,     Cond2, B2_44),
        arg(PosLCondAttr, Cond1, [A1_44,A2_44]),
        arg(PosLCondAttr, Cond2, [A3_44]),
        ((B1_44 #= 1) #/\ (B2_44 #= 1)) #=> ((A1_44 #\= A3_44) #/\ (A2_44 #\= A3_44))
    ;
     (Oplus = and,
      memberchk(Type2, [binary_term_eq_coef(min),binary_term_leq_coef(min)]),
      Type1 = attr_eq_coef) ->
        arg(PosCondB,     Cond2, B1_44),
        arg(PosCondB,     Cond1, B2_44),
        arg(PosLCondAttr, Cond2, [A1_44,A2_44]),
        arg(PosLCondAttr, Cond1, [A3_44]),
        ((B1_44 #= 1) #/\ (B2_44 #= 1)) #=> ((A1_44 #\= A3_44) #/\ (A2_44 #\= A3_44))
    ;
        true
    ),
    ((Oplus = and,
      memberchk(Type1, [binary_term_eq_coef(min),binary_term_leq_coef(min)]),
      Type2 = attr_leq_coef) ->                                                                                         %45 Family 1
        arg(PosCondB,     Cond1, B1_45),
        arg(PosCondB,     Cond2, B2_45),
        arg(PosLCondAttr, Cond1, [A1_45,A2_45]),
        arg(PosLCondAttr, Cond2, [A3_45]),
        arg(PosCondCoef,  Cond1, C1_45),
        arg(PosCondCoef,  Cond2, C2_45),
        ((B1_45 #= 1) #/\ (B2_45 #= 1) #/\ ((A1_45 #= A3_45) #\/ (A2_45 #= A3_45))) #=> (C2_45 #> C1_45) 
    ;
     (Oplus = and,
      memberchk(Type2, [binary_term_eq_coef(min),binary_term_leq_coef(min)]),
      Type1 = attr_leq_coef) ->
        arg(PosCondB,     Cond2, B1_45),
        arg(PosCondB,     Cond1, B2_45),
        arg(PosLCondAttr, Cond2, [A1_45,A2_45]),
        arg(PosLCondAttr, Cond1, [A3_45]),
        arg(PosCondCoef,  Cond2, C1_45),
        arg(PosCondCoef,  Cond1, C2_45),
        ((B1_45 #= 1) #/\ (B2_45 #= 1) #/\ ((A1_45 #= A3_45) #\/ (A2_45 #= A3_45))) #=> (C2_45 #> C1_45)
    ;
        true
    ),
    ((Oplus = and,
      Type1 = binary_term_leq_coef(min),
      Type2 = attr_geq_coef) ->                                                                                         %46 Family 1
        arg(PosCondB,     Cond1, B1_46),
        arg(PosCondB,     Cond2, B2_46),
        arg(PosLCondAttr, Cond1, [A1_46,A2_46]),
        arg(PosLCondAttr, Cond2, [A3_46]),
        arg(PosCondCoef,  Cond1, C1_46),
        arg(PosCondCoef,  Cond2, C2_46),
        ((B1_46 #= 1) #/\ (B2_46 #= 1) #/\ ((A1_46 #= A3_46) #\/ (A2_46 #= A3_46))) #=> (C1_46 #>= C2_46) 
    ;
     (Oplus = and,
      Type2 = binary_term_leq_coef(min),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_46),
        arg(PosCondB,     Cond1, B2_46),
        arg(PosLCondAttr, Cond2, [A1_46,A2_46]),
        arg(PosLCondAttr, Cond1, [A3_46]),
        arg(PosCondCoef,  Cond2, C1_46),
        arg(PosCondCoef,  Cond1, C2_46),
        ((B1_46 #= 1) #/\ (B2_46 #= 1) #/\ ((A1_46 #= A3_46) #\/ (A2_46 #= A3_46))) #=> (C1_46 #>= C2_46)
    ;
        true
    ),
    ((Oplus = and,
      Type1 = binary_term_eq_coef(min),
      Type2 = attr_geq_coef) ->                                                                                         %47 Family 1
        arg(PosCondB,     Cond1, B1_47),
        arg(PosCondB,     Cond2, B2_47),
        arg(PosLCondAttr, Cond1, [A1_47,A2_47]),
        arg(PosLCondAttr, Cond2, [A3_47]),
        ((B1_47 #= 1) #/\ (B2_47 #= 1)) #=> ((A1_47 #\= A3_47) #\/ (A2_47 #\= A3_47)) 
    ;
     (Oplus = and,
      Type2 = binary_term_eq_coef(min),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_47),
        arg(PosCondB,     Cond1, B2_47),
        arg(PosLCondAttr, Cond2, [A1_47,A2_47]),
        arg(PosLCondAttr, Cond1, [A3_47]),
        ((B1_47 #= 1) #/\ (B2_47 #= 1)) #=> ((A1_47 #\= A3_47) #\/ (A2_47 #\= A3_47))
    ;
        true
    ),
    ((Oplus = and,
      Type1 = binary_term_geq_coef(min),
      memberchk(Type2, [attr_leq_coef, attr_geq_coef])) ->                                                              %48 Family 1
        arg(PosCondB,     Cond1, B1_48),
        arg(PosCondB,     Cond2, B2_48),
        arg(PosLCondAttr, Cond1, [A1_48,A2_48]),
        arg(PosLCondAttr, Cond2, [A3_48]),
        ((B1_48 #= 1) #/\ (B2_48 #= 1)) #=> ((A1_48 #\= A3_48) #\/ (A2_48 #\= A3_48)) 
    ;
     (Oplus = and,
      Type2 = binary_term_geq_coef(min),
      memberchk(Type1, [attr_leq_coef, attr_geq_coef])) ->
        arg(PosCondB,     Cond2, B1_48),
        arg(PosCondB,     Cond1, B2_48),
        arg(PosLCondAttr, Cond2, [A1_48,A2_48]),
        arg(PosLCondAttr, Cond1, [A3_48]),
        ((B1_48 #= 1) #/\ (B2_48 #= 1)) #=> ((A1_48 #\= A3_48) #\/ (A2_48 #\= A3_48)) 
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_leq_coef(min),
      Type2 = attr_eq_coef) ->                                                                                         %49 Family 1
        arg(PosCondB,     Cond1, B1_49),
        arg(PosCondB,     Cond2, B2_49),
        arg(PosLCondAttr, Cond1, [A1_49,A2_49]),
        arg(PosLCondAttr, Cond2, [A3_49]),
        arg(PosCondCoef,  Cond1, C1_49),
        arg(PosCondCoef,  Cond2, C2_49),
        ((B1_49 #= 1) #/\ (B2_49 #= 1) #/\ ((A1_49 #= A3_49) #\/ (A2_49 #= A3_49))) #=> (C2_49 #> (C1_49 + 1))
    ;
     (Oplus = or,
      Type2 = binary_term_leq_coef(min),
      Type1 = attr_eq_coef) ->
        arg(PosCondB,     Cond2, B1_49),
        arg(PosCondB,     Cond1, B2_49),
        arg(PosLCondAttr, Cond2, [A1_49,A2_49]),
        arg(PosLCondAttr, Cond1, [A3_49]),
        arg(PosCondCoef,  Cond2, C1_49),
        arg(PosCondCoef,  Cond1, C2_49),
        ((B1_49 #= 1) #/\ (B2_49 #= 1) #/\ ((A1_49 #= A3_49) #\/ (A2_49 #= A3_49))) #=> (C2_49 #> (C1_49 + 1))
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_leq_coef(min),
      Type2 = attr_leq_coef) ->                                                                                         %50 Family 1
        arg(PosCondB,     Cond1, B1_50),
        arg(PosCondB,     Cond2, B2_50),
        arg(PosLCondAttr, Cond1, [A1_50,A2_50]),
        arg(PosLCondAttr, Cond2, [A3_50]),
        ((B1_50 #= 1) #/\ (B2_50 #= 1)) #=> ((A1_50 #\= A3_50) #\/ (A2_50 #\= A3_50))
    ;
     (Oplus = or,
      Type2 = binary_term_leq_coef(min),
      Type1 = attr_leq_coef) ->
        arg(PosCondB,     Cond2, B1_50),
        arg(PosCondB,     Cond1, B2_50),
        arg(PosLCondAttr, Cond2, [A1_50,A2_50]),
        arg(PosLCondAttr, Cond1, [A3_50]),
        ((B1_50 #= 1) #/\ (B2_50 #= 1)) #=> ((A1_50 #\= A3_50) #\/ (A2_50 #\= A3_50))
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_leq_coef(min),
      Type2 = attr_geq_coef) ->                                                                                         %51 Family 1
        arg(PosCondB,     Cond1, B1_51),
        arg(PosCondB,     Cond2, B2_51),
        arg(PosLCondAttr, Cond1, [A1_51,A2_51]),
        arg(PosLCondAttr, Cond2, [A3_51]),
        arg(PosCondCoef,  Cond1, C1_51),
        arg(PosCondCoef,  Cond2, C2_51),
        ((B1_51 #= 1) #/\ (B2_51 #= 1) #/\ ((A1_51 #= A3_51) #\/ (A2_51 #= A3_51))) #=> (C2_51 #> (C1_51 + 1))
    ;
     (Oplus = and,
      Type2 = binary_term_leq_coef(min),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_51),
        arg(PosCondB,     Cond1, B2_51),
        arg(PosLCondAttr, Cond2, [A1_51,A2_51]),
        arg(PosLCondAttr, Cond1, [A3_51]),
        arg(PosCondCoef,  Cond2, C1_51),
        arg(PosCondCoef,  Cond1, C2_51),
        ((B1_51 #= 1) #/\ (B2_51 #= 1) #/\ ((A1_51 #= A3_51) #\/ (A2_51 #= A3_51))) #=> (C2_51 #> (C1_51 + 1))
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_geq_coef(min),
      Type2 = attr_eq_coef) ->                                                                                         %52 Family 1
        arg(PosCondB,     Cond1, B1_52),
        arg(PosCondB,     Cond2, B2_52),
        arg(PosLCondAttr, Cond1, [A1_52,A2_52]),
        arg(PosLCondAttr, Cond2, [A3_52]),
        arg(PosCondCoef,  Cond1, C1_52),
        arg(PosCondCoef,  Cond2, C2_52),
        ((B1_52 #= 1) #/\ (B2_52 #= 1) #/\ ((A1_52 #= A3_52) #\/ (A2_52 #= A3_52))) #=> (C1_52 #> (C2_52 + 1))
    ;
     (Oplus = or,
      Type2 = binary_term_geq_coef(min),
      Type1 = attr_eq_coef) ->
        arg(PosCondB,     Cond2, B1_52),
        arg(PosCondB,     Cond1, B2_52),
        arg(PosLCondAttr, Cond2, [A1_52,A2_52]),
        arg(PosLCondAttr, Cond1, [A3_52]),
        arg(PosCondCoef,  Cond2, C1_52),
        arg(PosCondCoef,  Cond1, C2_52),
        ((B1_52 #= 1) #/\ (B2_52 #= 1) #/\ ((A1_52 #= A3_52) #\/ (A2_52 #= A3_52))) #=> (C1_52 #> (C2_52 + 1))
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_geq_coef(min),
      Type2 = attr_leq_coef) ->                                                                                         %53 Family 2
        arg(PosCondB,     Cond1, B1_53),
        arg(PosCondB,     Cond2, B2_53),
        arg(PosLCondAttr, Cond1, [A1_53,A2_53]),
        arg(PosLCondAttr, Cond2, [A3_53]),
        ((B1_53 #= 1) #/\ (B2_53 #= 1)) #=> ((A1_53 #\= A3_53) #\/ (A2_53 #\= A3_53))
    ;
     (Oplus = or,
      Type2 = binary_term_geq_coef(min),
      Type1 = attr_leq_coef) ->
        arg(PosCondB,     Cond2, B1_53),
        arg(PosCondB,     Cond1, B2_53),
        arg(PosLCondAttr, Cond2, [A1_53,A2_53]),
        arg(PosLCondAttr, Cond1, [A3_53]),
        ((B1_53 #= 1) #/\ (B2_53 #= 1)) #=> ((A1_53 #\= A3_53) #\/ (A2_53 #\= A3_53))
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_geq_coef(min),
      Type2 = attr_geq_coef) ->                                                                                         %54 Family 1
        arg(PosCondB,     Cond1, B1_54),
        arg(PosCondB,     Cond2, B2_54),
        arg(PosLCondAttr, Cond1, [A1_54,A2_54]),
        arg(PosLCondAttr, Cond2, [A3_54]),
        arg(PosCondCoef,  Cond1, C1_54),
        arg(PosCondCoef,  Cond2, C2_54),
        ((B1_54 #= 1) #/\ (B2_54 #= 1) #/\ ((A1_54 #= A3_54) #\/ (A2_54 #= A3_54))) #=> (C2_54 #> C1_54)
    ;
     (Oplus = or,
      Type2 = binary_term_geq_coef(min),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_54),
        arg(PosCondB,     Cond1, B2_54),
        arg(PosLCondAttr, Cond2, [A1_54,A2_54]),
        arg(PosLCondAttr, Cond1, [A3_54]),
        arg(PosCondCoef,  Cond2, C1_54),
        arg(PosCondCoef,  Cond1, C2_54),
        ((B1_54 #= 1) #/\ (B2_54 #= 1) #/\ ((A1_54 #= A3_54) #\/ (A2_54 #= A3_54))) #=> (C2_54 #> C1_54)
    ;
        true
    ),
    ((Oplus = and,
      memberchk(Type1,[binary_term_eq_coef(max),binary_term_leq_coef(max),binary_term_geq_coef(max)]),
      Type2 = attr_eq_coef) ->                                                                                          %55 Family 1
        arg(PosCondB,     Cond1, B1_55),
        arg(PosCondB,     Cond2, B2_55),
        arg(PosLCondAttr, Cond1, [A1_55,A2_55]),
        arg(PosLCondAttr, Cond2, [A3_55]),
        ((B1_55 #= 1) #/\ (B2_55 #= 1)) #=> ((A1_55 #\= A3_55) #\/ (A2_55 #\= A3_55))
    ;
     (Oplus = and,
      memberchk(Type2,[binary_term_eq_coef(max),binary_term_leq_coef(max),binary_term_geq_coef(max)]),
      Type1 = attr_eq_coef) ->
        arg(PosCondB,     Cond2, B1_55),
        arg(PosCondB,     Cond1, B2_55),
        arg(PosLCondAttr, Cond2, [A1_55,A2_55]),
        arg(PosLCondAttr, Cond1, [A3_55]),
        ((B1_55 #= 1) #/\ (B2_55 #= 1)) #=> ((A1_55 #\= A3_55) #\/ (A2_55 #\= A3_55))
    ;
        true
    ),
    ((Oplus = and,
      memberchk(Type1,[binary_term_eq_coef(max),binary_term_leq_coef(max)]),
      Type2 = attr_leq_coef) ->                                                                                         %56 Family 1
        arg(PosCondB,     Cond1, B1_56),
        arg(PosCondB,     Cond2, B2_56),
        arg(PosLCondAttr, Cond1, [A1_56,A2_56]),
        arg(PosLCondAttr, Cond2, [A3_56]),
        ((B1_56 #= 1) #/\ (B2_56 #= 1)) #=> ((A1_56 #\= A3_56) #\/ (A2_56 #\= A3_56))
    ;
     (Oplus = and,
      memberchk(Type2,[binary_term_eq_coef(max),binary_term_leq_coef(max)]),
      Type1 = attr_leq_coef) ->
        arg(PosCondB,     Cond2, B1_56),
        arg(PosCondB,     Cond1, B2_56),
        arg(PosLCondAttr, Cond2, [A1_56,A2_56]),
        arg(PosLCondAttr, Cond1, [A3_56]),
        ((B1_56 #= 1) #/\ (B2_56 #= 1)) #=> ((A1_56 #\= A3_56) #\/ (A2_56 #\= A3_56))
    ;
        true
    ),
    ((Oplus = and,
      Type1 = binary_term_geq_coef(max),
      Type2 = attr_leq_coef) ->                                                                                         %57 Family 1
        arg(PosCondB,     Cond1, B1_57),
        arg(PosCondB,     Cond2, B2_57),
        arg(PosLCondAttr, Cond1, [A1_57,A2_57]),
        arg(PosLCondAttr, Cond2, [A3_57]),
        arg(PosCondCoef,  Cond1, C1_57),
        arg(PosCondCoef,  Cond2, C2_57),
        ((B1_57 #= 1) #/\ (B2_57 #= 1) #/\ ((A1_57 #= A3_57) #\/ (A2_57 #= A3_57))) #=> (C2_57 #>= C1_57)
    ;
     (Oplus = and,
      Type2 = binary_term_geq_coef(max),
      Type1 = attr_leq_coef) ->
        arg(PosCondB,     Cond1, B2_57),
        arg(PosLCondAttr, Cond2, [A1_57,A2_57]),
        arg(PosLCondAttr, Cond1, [A3_57]),
        arg(PosCondCoef,  Cond2, C1_57),
        arg(PosCondCoef,  Cond1, C2_57),
        ((B1_57 #= 1) #/\ (B2_57 #= 1) #/\ ((A1_57 #= A3_57) #\/ (A2_57 #= A3_57))) #=> (C2_57 #> C1_57)
    ;
        true
    ),
    ((Oplus = and,
      memberchk(Type1, [binary_term_leq_coef(max),binary_term_geq_coef(max)]),
      Type2 = attr_geq_coef) ->                                                                                         %58 Family 1 / Family 2
        arg(PosCondB,     Cond1, B1_58),
        arg(PosCondB,     Cond2, B2_58),
        arg(PosLCondAttr, Cond1, [A1_58,A2_58]),
        arg(PosLCondAttr, Cond2, [A3_58]),
        ((B1_58 #= 1) #/\ (B2_58 #= 1)) #=> ((A1_58 #\= A3_58) #\/ (A2_58 #\= A3_58))
    ;
     (Oplus = and,
      memberchk(Type2, [binary_term_leq_coef(max),binary_term_geq_coef(max)]),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_58),
        arg(PosCondB,     Cond1, B2_58),
        arg(PosLCondAttr, Cond2, [A1_58,A2_58]),
        arg(PosLCondAttr, Cond1, [A3_58]),
        ((B1_58 #= 1) #/\ (B2_58 #= 1)) #=> ((A1_58 #\= A3_58) #\/ (A2_58 #\= A3_58))
    ;
        true
    ),
    ((Oplus = and,
      Type1 = binary_term_eq_coef(max),
      Type2 = attr_geq_coef) ->                                                                                         %59 Family 1 /  Family 2
        arg(PosCondB,     Cond1, B1_59),
        arg(PosCondB,     Cond2, B2_59),
        arg(PosLCondAttr, Cond1, [A1_59,A2_59]),
        arg(PosLCondAttr, Cond2, [A3_59]),
        arg(PosCondCoef,  Cond1, C1_59),
        arg(PosCondCoef,  Cond2, C2_59),
        ((B1_59 #= 1) #/\ (B2_59 #= 1) #/\ ((A1_59 #= A3_59) #\/ (A2_59 #= A3_59))) #=> (C1_59 #> C2_59)
    ;
     (Oplus = and,
      Type2 = binary_term_eq_coef(max),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_59),
        arg(PosCondB,     Cond1, B2_59),
        arg(PosLCondAttr, Cond2, [A1_59,A2_59]),
        arg(PosLCondAttr, Cond1, [A3_59]),
        arg(PosCondCoef,  Cond2, C1_59),
        arg(PosCondCoef,  Cond1, C2_59),
        ((B1_59 #= 1) #/\ (B2_59 #= 1) #/\ ((A1_59 #= A3_59) #\/ (A2_59 #= A3_59))) #=> (C1_59 #> C2_59)
    ;
        true
    ),    
    ((Oplus = or,
      Type1 = binary_term_leq_coef(max),
      Type2 = attr_eq_coef) ->                                                                                          %60 Family 1
        arg(PosCondB,     Cond1, B1_60),
        arg(PosCondB,     Cond2, B2_60),
        arg(PosLCondAttr, Cond1, [A1_60,A2_60]),
        arg(PosLCondAttr, Cond2, [A3_60]),
        arg(PosCondCoef,  Cond1, C1_60),
        arg(PosCondCoef,  Cond2, C2_60),
        ((B1_60 #= 1) #/\ (B2_60 #= 1) #/\ ((A1_60 #= A3_60) #\/ (A2_60 #= A3_60))) #=> (C2_60 #> (C1_60 + 1))
    ;
     (Oplus = or,
      Type2 = binary_term_leq_coef(max),
      Type1 = attr_eq_coef) ->
        arg(PosCondB,     Cond2, B1_60),
        arg(PosCondB,     Cond1, B2_60),
        arg(PosLCondAttr, Cond2, [A1_60,A2_60]),
        arg(PosLCondAttr, Cond1, [A3_60]),
        arg(PosCondCoef,  Cond2, C1_60),
        arg(PosCondCoef,  Cond1, C2_60),
        ((B1_60 #= 1) #/\ (B2_60 #= 1) #/\ ((A1_60 #= A3_60) #\/ (A2_60 #= A3_60))) #=> (C2_60 #> (C1_60 + 1))
    ;
        true
    ),    
    ((Oplus = or,
      Type1 = binary_term_leq_coef(max),
      Type2 = attr_leq_coef) ->                                                                                         %61 Family 1
        arg(PosCondB,     Cond1, B1_61),
        arg(PosCondB,     Cond2, B2_61),
        arg(PosLCondAttr, Cond1, [A1_61,A2_61]),
        arg(PosLCondAttr, Cond2, [A3_61]),
        arg(PosCondCoef,  Cond1, C1_61),
        arg(PosCondCoef,  Cond2, C2_61),
        ((B1_61 #= 1) #/\ (B2_61 #= 1) #/\ ((A1_61 #= A3_61) #\/ (A2_61 #= A3_61))) #=> (C1_61 #> C2_61)
    ;
     (Oplus = or,
      Type2 = binary_term_leq_coef(max),
      Type1 = attr_leq_coef) ->
        arg(PosCondB,     Cond2, B1_61),
        arg(PosCondB,     Cond1, B2_61),
        arg(PosLCondAttr, Cond2, [A1_61,A2_61]),
        arg(PosLCondAttr, Cond1, [A3_61]),
        arg(PosCondCoef,  Cond2, C1_61),
        arg(PosCondCoef,  Cond1, C2_61),
        ((B1_61 #= 1) #/\ (B2_61 #= 1) #/\ ((A1_61 #= A3_61) #\/ (A2_61 #= A3_61))) #=> (C1_61 #> C2_61)
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_leq_coef(max),
      Type2 = attr_geq_coef) ->                                                                                         %62 Family 1
        arg(PosCondB,     Cond1, B1_62),
        arg(PosCondB,     Cond2, B2_62),
        arg(PosLCondAttr, Cond1, [A1_62,A2_62]),
        arg(PosLCondAttr, Cond2, [A3_62]),
        arg(PosCondCoef,  Cond1, C1_62),
        arg(PosCondCoef,  Cond2, C2_62),
        ((B1_62 #= 1) #/\ (B2_62 #= 1) #/\ ((A1_62 #= A3_62) #\/ (A2_62 #= A3_62))) #=> (C2_62 #= C1_62 + 1)
    ;
     (Oplus = or,
      Type2 = binary_term_leq_coef(max),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_62),
        arg(PosCondB,     Cond1, B2_62),
        arg(PosLCondAttr, Cond2, [A1_62,A2_62]),
        arg(PosLCondAttr, Cond1, [A3_62]),
        arg(PosCondCoef,  Cond2, C1_62),
        arg(PosCondCoef,  Cond1, C2_62),
        ((B1_62 #= 1) #/\ (B2_62 #= 1) #/\ ((A1_62 #= A3_62) #\/ (A2_62 #= A3_62))) #=> (C2_62 #= C1_62 + 1)
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_geq_coef(max),
      memberchk(Type2, [attr_eq_coef,attr_leq_coef])) ->                                                                %63 Family 1
        arg(PosCondB,     Cond1, B1_63),
        arg(PosCondB,     Cond2, B2_63),
        arg(PosLCondAttr, Cond1, [A1_63,A2_63]),
        arg(PosLCondAttr, Cond2, [A3_63]),
        arg(PosCondCoef,  Cond1, C1_63),
        arg(PosCondCoef,  Cond2, C2_63),
        ((B1_63 #= 1) #/\ (B2_63 #= 1) #/\ ((A1_63 #= A3_63) #\/ (A2_63 #= A3_63))) #=> (C1_63 #> C2_63 + 1)
    ;
     (Oplus = or,
      Type2 = binary_term_geq_coef(max),
      memberchk(Type1, [attr_eq_coef,attr_leq_coef])) ->
        arg(PosCondB,     Cond2, B1_63),
        arg(PosCondB,     Cond1, B2_63),
        arg(PosLCondAttr, Cond2, [A1_63,A2_63]),
        arg(PosLCondAttr, Cond1, [A3_63]),
        arg(PosCondCoef,  Cond2, C1_63),
        arg(PosCondCoef,  Cond1, C2_63),
        ((B1_63 #= 1) #/\ (B2_63 #= 1) #/\ ((A1_63 #= A3_63) #\/ (A2_63 #= A3_63))) #=> (C1_63 #> C2_63 + 1)
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_geq_coef(max),
      Type2 = attr_geq_coef) ->                                                                                         %64 Family 1
        arg(PosCondB,     Cond1, B1_64),
        arg(PosCondB,     Cond2, B2_64),
        arg(PosLCondAttr, Cond1, [A1_64,A2_64]),
        arg(PosLCondAttr, Cond2, [A3_64]),
        ((B1_64 #= 1) #/\ (B2_64 #= 1)) #=> ((A1_64 #\= A3_64) #\/ (A2_64 #\= A3_64))
    ;
     (Oplus = or,
      Type2 = binary_term_geq_coef(max),
      Type1 = attr_geq_coef) ->
        arg(PosCondB,     Cond2, B1_64),
        arg(PosCondB,     Cond1, B2_64),
        arg(PosLCondAttr, Cond2, [A1_64,A2_64]),
        arg(PosLCondAttr, Cond1, [A3_64]),
        ((B1_64 #= 1) #/\ (B2_64 #= 1)) #=> ((A1_64 #\= A3_64) #\/ (A2_64 #\= A3_64))
    ;
        true
    ),
    ((memberchk(Oplus, [and,or]),
      Type1 = binary_term_eq_coef(floor),
      Type2 = attr_leq_attr) ->                                                                                         %65 Family 1
        arg(PosCondB,     Cond1, B1_65),
        arg(PosCondB,     Cond2, B2_65),
        arg(PosLCondAttr, Cond1, [A1_65,A2_65]),
        arg(PosLCondAttr, Cond2, [A3_65,A4_65]),
        arg(PosCondCoef,  Cond1, C_65),
        ((B1_65 #= 1) #/\ (B2_65 #= 1) #/\ (((A1_65 #= A3_65) #/\ (A2_65 #= A4_65)) #\/ ((A1_65 #= A4_65) #/\ (A2_65 #= A3_65)))) #=> (C_65 #\= 1)
    ;
     (memberchk(Oplus, [and,or]),
      Type2 = binary_term_eq_coef(floor),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_65),
        arg(PosCondB,     Cond1, B2_65),
        arg(PosLCondAttr, Cond2, [A1_65,A2_65]),
        arg(PosLCondAttr, Cond1, [A3_65,A4_65]),
        arg(PosCondCoef,  Cond2, C_65),
        ((B1_65 #= 1) #/\ (B2_65 #= 1) #/\ (((A1_65 #= A3_65) #/\ (A2_65 #= A4_65)) #\/ ((A1_65 #= A4_65) #/\ (A2_65 #= A3_65)))) #=> (C_65 #\= 1)
    ;
        true
    ),
    ((memberchk(Oplus, [and,or]),
      Type1 = binary_term_eq_coef(ceil),
      Type2 = attr_leq_attr) ->                                                                                         %66 Family 1
        arg(PosCondB,     Cond1, B1_66),
        arg(PosCondB,     Cond2, B2_66),
        arg(PosLCondAttr, Cond1, [A1_66,A2_66]),
        arg(PosLCondAttr, Cond2, [A3_66,A4_66]),
        arg(PosCondCoef,  Cond1, C_66),
        ((B1_66 #= 1) #/\ (B2_66 #= 1) #/\ (((A1_66 #= A3_66) #/\ (A2_66 #= A4_66)) #\/ ((A1_66 #= A4_66) #/\ (A2_66 #= A3_66)))) #=> (C_66 #\= 2)
    ;
     (memberchk(Oplus, [and,or]),
      Type2 = binary_term_eq_coef(ceil),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_66),
        arg(PosCondB,     Cond1, B2_66),
        arg(PosLCondAttr, Cond2, [A1_66,A2_66]),
        arg(PosLCondAttr, Cond1, [A3_66,A4_66]),
        arg(PosCondCoef,  Cond2, C_66),
        ((B1_66 #= 1) #/\ (B2_66 #= 1) #/\ (((A1_66 #= A3_66) #/\ (A2_66 #= A4_66)) #\/ ((A1_66 #= A4_66) #/\ (A2_66 #= A3_66)))) #=> (C_66 #\= 2)
    ;
        true
    ),
    ((memberchk(Oplus, [and,or]),
      Type1 = binary_term_geq_coef(floor),
      Type2 = attr_leq_attr) ->                                                                                         %67 Family 1
        arg(PosCondB,     Cond1, B1_67),
        arg(PosCondB,     Cond2, B2_67),
        arg(PosLCondAttr, Cond1, [A1_67,A2_67]),
        arg(PosLCondAttr, Cond2, [A3_67,A4_67]),
        arg(PosCondCoef,  Cond1, C_67),
        ((B1_67 #= 1) #/\ (B2_67 #= 1) #/\ (((A1_67 #= A3_67) #/\ (A2_67 #= A4_67)) #\/ ((A1_67 #= A4_67) #/\ (A2_67 #= A3_67)))) #=> (C_67 #\= 2)
    ;
     (memberchk(Oplus, [and,or]),
      Type2 = binary_term_geq_coef(floor),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_67),
        arg(PosCondB,     Cond1, B2_67),
        arg(PosLCondAttr, Cond2, [A1_67,A2_67]),
        arg(PosLCondAttr, Cond1, [A3_67,A4_67]),
        arg(PosCondCoef,  Cond2, C_67),
        ((B1_67 #= 1) #/\ (B2_67 #= 1) #/\ (((A1_67 #= A3_67) #/\ (A2_67 #= A4_67)) #\/ ((A1_67 #= A4_67) #/\ (A2_67 #= A3_67)))) #=> (C_67 #\= 2)
    ;
        true
    ),
    ((memberchk(Oplus, [and,or]),
      Type1 = binary_term_geq_coef(ceil),
      Type2 = attr_leq_attr) ->                                                                                         %68 Family 1
        arg(PosCondB,     Cond1, B1_68),
        arg(PosCondB,     Cond2, B2_68),
        arg(PosLCondAttr, Cond1, [A1_68,A2_68]),
        arg(PosLCondAttr, Cond2, [A3_68,A4_68]),
        arg(PosCondCoef,  Cond1, C_68),
        ((B1_68 #= 1) #/\ (B2_68 #= 1) #/\ (((A1_68 #= A3_68) #/\ (A2_68 #= A4_68)) #\/ ((A1_68 #= A4_68) #/\ (A2_68 #= A3_68)))) #=> (C_68 #\= 3)
    ;
     (memberchk(Oplus, [and,or]),
      Type2 = binary_term_geq_coef(ceil),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_68),
        arg(PosCondB,     Cond1, B2_68),
        arg(PosLCondAttr, Cond2, [A1_68,A2_68]),
        arg(PosLCondAttr, Cond1, [A3_68,A4_68]),
        arg(PosCondCoef,  Cond2, C_68),
        ((B1_68 #= 1) #/\ (B2_68 #= 1) #/\ (((A1_68 #= A3_68) #/\ (A2_68 #= A4_68)) #\/ ((A1_68 #= A4_68) #/\ (A2_68 #= A3_68)))) #=> (C_68 #\= 3)
    ;
        true
    ),
    ((Oplus = and,
      Type1 = binary_term_leq_coef(abs),
      Type2 = attr_leq_attr) ->                                                                                         %69 Family 3
        arg(PosCondB,     Cond1, B1_69),
        arg(PosCondB,     Cond2, B2_69),
        arg(PosLCondAttr, Cond1, [A1_69,A2_69]),
        arg(PosLCondAttr, Cond2, [A3_69,A4_69]),
        ((B1_69 #= 1) #/\ (B2_69 #= 1)) #=> (((A1_69 #\= A3_69) #\/ (A2_69 #\= A4_69)) #/\ ((A1_69 #\= A4_69) #\/ (A2_69 #\= A3_69)))
    ;
     (Oplus = and,
      Type2 = binary_term_leq_coef(abs),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_69),
        arg(PosCondB,     Cond1, B2_69),
        arg(PosLCondAttr, Cond2, [A1_69,A2_69]),
        arg(PosLCondAttr, Cond1, [A3_69,A4_69]),
        ((B1_69 #= 1) #/\ (B2_69 #= 1)) #=> (((A1_69 #\= A3_69) #\/ (A2_69 #\= A4_69)) #/\ ((A1_69 #\= A4_69) #\/ (A2_69 #\= A3_69)))
    ;
        true
    ),
    ((Oplus = or,
      Type1 = binary_term_leq_coef(abs),
      memberchk(Type2, [attr_eq_attr, attr_leq_attr])) ->
        arg(PosCondB,     Cond1, B1_69a),
        arg(PosCondB,     Cond2, B2_69a),
        arg(PosLCondAttr, Cond1, [A1_69a,A2_69a]),
        arg(PosLCondAttr, Cond2, [A3_69a,A4_69a]),
        ((B1_69a #= 1) #/\ (B2_69a #= 1)) #=> (((A1_69a #\= A3_69a) #\/ (A2_69a #\= A4_69a)) #/\ ((A1_69a #\= A4_69a) #\/ (A2_69a #\= A3_69a)))
    ;
     (Oplus = or,
      Type2 = binary_term_leq_coef(abs),
      memberchk(Type1, [attr_eq_attr, attr_leq_attr])) ->
        arg(PosCondB,     Cond2, B1_69a),
        arg(PosCondB,     Cond1, B2_69a),
        arg(PosLCondAttr, Cond2, [A1_69a,A2_69a]),
        arg(PosLCondAttr, Cond1, [A3_69a,A4_69a]),
        ((B1_69a #= 1) #/\ (B2_69a #= 1)) #=> (((A1_69a #\= A3_69a) #\/ (A2_69a #\= A4_69a)) #/\ ((A1_69a #\= A4_69a) #\/ (A2_69a #\= A3_69a)))
    ;
        true
    ),
    ((memberchk(Oplus, [and,or]),
      memberchk(Type1, [binary_term_eq_coef(abs),binary_term_geq_coef(abs)]),
      Type2 = attr_leq_attr) ->                                                                                         %70 Family 3
        arg(PosCondB,     Cond1, B1_70),
        arg(PosCondB,     Cond2, B2_70),
        arg(PosLCondAttr, Cond1, [A1_70,A2_70]),
        arg(PosLCondAttr, Cond2, [A3_70,A4_70]),
        ((B1_70 #= 1) #/\ (B2_70 #= 1)) #=> (((A1_70 #\= A3_70) #\/ (A2_70 #\= A4_70)) #/\ ((A1_70 #\= A4_70) #\/ (A2_70 #\= A3_70)))
    ;
     (memberchk(Oplus, [and,or]),
      memberchk(Type2, [binary_term_eq_coef(abs),binary_term_geq_coef(abs)]),
      Type1 = attr_leq_attr) ->
        arg(PosCondB,     Cond2, B1_70),
        arg(PosCondB,     Cond1, B2_70),
        arg(PosLCondAttr, Cond2, [A1_70,A2_70]),
        arg(PosLCondAttr, Cond1, [A3_70,A4_70]),
        ((B1_70 #= 1) #/\ (B2_70 #= 1)) #=> (((A1_70 #\= A3_70) #\/ (A2_70 #\= A4_70)) #/\ ((A1_70 #\= A4_70) #\/ (A2_70 #\= A3_70)))
    ;
        true
    ),
    ((Oplus = and,
      memberchk(Type1, [binary_term_eq_coef(abs),binary_term_geq_coef(abs)]),
      Type2 = attr_eq_attr) ->                                                                                          %71 Family 1
        arg(PosCondB,     Cond1, B1_71),
        arg(PosCondB,     Cond2, B2_71),
        arg(PosLCondAttr, Cond1, [A1_71,A2_71]),
        arg(PosLCondAttr, Cond2, [A3_71,A4_71]),
        ((B1_71 #= 1) #/\ (B2_71 #= 1)) #=> (((A1_71 #\= A3_71) #\/ (A2_71 #\= A4_71)) #/\ ((A1_71 #\= A4_71) #\/ (A2_71 #\= A3_71)))
    ;
     (Oplus = and,
      memberchk(Type1, [binary_term_eq_coef(abs),binary_term_geq_coef(abs)]),
      Type2 = attr_eq_attr) ->
        arg(PosCondB,     Cond2, B1_71),
        arg(PosCondB,     Cond1, B2_71),
        arg(PosLCondAttr, Cond2, [A1_71,A2_71]),
        arg(PosLCondAttr, Cond1, [A3_71,A4_71]),
        ((B1_71 #= 1) #/\ (B2_71 #= 1)) #=> (((A1_71 #\= A3_71) #\/ (A2_71 #\= A4_71)) #/\ ((A1_71 #\= A4_71) #\/ (A2_71 #\= A3_71)))
    ;
        true
    ),
    ((Oplus = or,
      memberchk(Type1, [binary_term_eq_coef(abs),binary_term_geq_coef(abs)]),
      Type2 = attr_eq_attr) ->                                                                                          %72 Family 2
        arg(PosCondB,     Cond1, B1_72),
        arg(PosCondB,     Cond2, B2_72),
        arg(PosLCondAttr, Cond1, [A1_72,A2_72]),
        arg(PosLCondAttr, Cond2, [A3_72,A4_72]),
        arg(PosCondCoef,  Cond1, C_72),
        ((B1_72 #= 1) #/\ (B2_72 #= 1) #/\ (((A1_72 #= A3_72) #/\ (A2_72 #= A4_72)) #\/ ((A1_72 #= A4_72) #/\ (A2_72 #= A3_72)))) #=> (C_72 #> 1)
    ;
     (Oplus = or,
      memberchk(Type2, [binary_term_eq_coef(abs),binary_term_geq_coef(abs)]),
      Type1 = attr_eq_attr) ->
        arg(PosCondB,     Cond2, B1_72),
        arg(PosCondB,     Cond1, B2_72),
        arg(PosLCondAttr, Cond2, [A1_72,A2_72]),
        arg(PosLCondAttr, Cond1, [A3_72,A4_72]),
        arg(PosCondCoef,  Cond2, C_72),
        ((B1_72 #= 1) #/\ (B2_72 #= 1) #/\ (((A1_72 #= A3_72) #/\ (A2_72 #= A4_72)) #\/ ((A1_72 #= A4_72) #/\ (A2_72 #= A3_72)))) #=> (C_72 #> 1)  
    ;
        true
    ),
    ((memberchk(Oplus,[and,or,allequal,xor]),
      memberchk(Type1, [binary_term_eq_coef(abs),binary_term_leq_coef(abs),binary_term_geq_coef(abs)]),
      memberchk(Type2, [binary_term_eq_coef(minus),binary_term_leq_coef(minus),binary_term_geq_coef(minus)])) ->        %73 Family 1
        arg(PosCondB,     Cond1, B1_73),
        arg(PosCondB,     Cond2, B2_73),
        arg(PosLCondAttr, Cond1, [A1_73,A2_73]),
        arg(PosLCondAttr, Cond2, [A3_73,A4_73]),
        ((B1_73 #= 1) #/\ (B2_73 #= 1)) #=> (((A1_73 #\= A3_73) #\/ (A2_73 #\= A4_73)) #/\ ((A1_73 #\= A4_73) #\/ (A2_73 #\= A3_73)))
    ;
     (memberchk(Oplus,[and,or,allequal,xor]),
      memberchk(Type2, [binary_term_eq_coef(abs),binary_term_leq_coef(abs),binary_term_geq_coef(abs)]),
      memberchk(Type1, [binary_term_eq_coef(minus),binary_term_leq_coef(minus),binary_term_geq_coef(minus)])) ->
        arg(PosCondB,     Cond2, B1_73),
        arg(PosCondB,     Cond1, B2_73),
        arg(PosLCondAttr, Cond2, [A1_73,A2_73]),
        arg(PosLCondAttr, Cond1, [A3_73,A4_73]),
        ((B1_73 #= 1) #/\ (B2_73 #= 1)) #=> (((A1_73 #\= A3_73) #\/ (A2_73 #\= A4_73)) #/\ ((A1_73 #\= A4_73) #\/ (A2_73 #\= A3_73)))
    ;
        true
    ),
    ((memberchk(Oplus,[or,allequal,xor]),
      Type1 = attr_geq_coef,
      Type2 = unary_term_eq_coef(mod)) ->                                                                               %74 Family 3: %[v>=3] = [v mod 2=0]]
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
      Type2 = attr_geq_coef,
      Type1 = unary_term_eq_coef(mod)) ->
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
    ((memberchk(Oplus,[and,or]),
      Type1 = attr_eq_attr,
      Type2 = attr_eq_coef) ->                                                                                           %75 Family 3:
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
      Type2 = attr_eq_attr,
      Type1 = attr_eq_coef) ->
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
    forbid_specific_combinations_of_conds(R, Cond1-Type1, Oplus, NbTerms, MandatoryAttr, LConds,
                                          MinAttrs, MaxAttrs, ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr, PosCondCoef, PosCondCst).
