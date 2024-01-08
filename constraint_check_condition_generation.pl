% Purpose: The set of predicates used for selection and generation of conditions and hypothesis that are later used to
%          generate forbidden pairs constraints
% Author : Ramiz Gindullin, IMT Atlantique

:- module(constraint_check_condition_generation,
                [generate_constraint/3,
                 generate_single_hypothesis/4,
                 generate_hypothesis/7,
                 generate_hypothesis_term/8,
                 generate_forbidden_hypothesis_term/6,
                 generate_hvars/6,
                 attribute1_min/1,
                 attribute1_max/1,
                 attribute2_min/1,
                 attribute2_max/1,
                 coef_min/1,
                 coef_max/1,
                 find_binterm_min_max/4]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

attribute1_min(1).
attribute1_max(15). %default for families 1 and 2: 100  ; default for family 3: 15
attribute2_min(1).
attribute2_max(15). %default for families 1 and 2: 100  ; default for family 3: 15
coef_min(0).
coef_max(40).       %default for families 1 and 2: 1000 ; default for family 3: 40


% Given a lists of Name-Var pairs for two conditions generate or select a singular hypothesis.
% If it's generation call then Boolean variable HVar is returned that corresponds to if the singular hypothesis is satisfied or not
% for given values of coefficients 
% example of a call:  | ?- generate_single_hypothesis(HName, CName1-CVar1, CName2-CVar2, HVar).
generate_single_hypothesis(none, _, _, HVar)                           :- HVar in 0..1, HVar #= 1.
generate_single_hypothesis(c_geq_1(CName1), CName1-CVar1, _, HVar)     :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #>= 1).
generate_single_hypothesis(c_geq_1(CName2), _, CName2-CVar2, HVar)     :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #>= 1).
generate_single_hypothesis(c_geq_2(CName1), CName1-CVar1, _, HVar)     :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #>= 2).
generate_single_hypothesis(c_geq_2(CName2), _, CName2-CVar2, HVar)     :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #>= 2).
generate_single_hypothesis(c_ineq_1(CName1), CName1-CVar1, _, HVar)    :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #\= 1).
generate_single_hypothesis(c_ineq_1(CName2), _, CName2-CVar2, HVar)    :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #\= 1).
generate_single_hypothesis(c_ineq_2(CName1), CName1-CVar1, _, HVar)    :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #\= 2).
generate_single_hypothesis(c_ineq_2(CName2), _, CName2-CVar2, HVar)    :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #\= 2).
generate_single_hypothesis(c_ineq_3(CName1), CName1-CVar1, _, HVar)    :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #\= 3).
generate_single_hypothesis(c_ineq_3(CName2), _, CName2-CVar2, HVar)    :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #\= 3).
generate_single_hypothesis(c_leq_1(CName1), CName1-CVar1, _, HVar)     :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #=< 1).
generate_single_hypothesis(c_leq_1(CName2), _, CName2-CVar2, HVar)     :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #=< 1).
generate_single_hypothesis(c_leq_2(CName1), CName1-CVar1, _, HVar)     :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #=< 2).
generate_single_hypothesis(c_leq_2(CName2), _, CName2-CVar2, HVar)     :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #=< 2).
generate_single_hypothesis(c_eq_0(CName1), CName1-CVar1, _, HVar)      :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #=  0).
generate_single_hypothesis(c_eq_0(CName2), _, CName2-CVar2, HVar)      :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #=  0).
generate_single_hypothesis(c_eq_1(CName1), CName1-CVar1, _, HVar)      :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #=  1).
generate_single_hypothesis(c_eq_1(CName2), _, CName2-CVar2, HVar)      :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #=  1).
generate_single_hypothesis(c_eq_2(CName1), CName1-CVar1, _, HVar)      :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #=  2).
generate_single_hypothesis(c_eq_2(CName2), _, CName2-CVar2, HVar)      :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #=  2).
generate_single_hypothesis(c_eq_3(CName1), CName1-CVar1, _, HVar)      :- atom(CName1), HVar in 0..1, HVar #= (CVar1 #=  3).
generate_single_hypothesis(c_eq_3(CName2), _, CName2-CVar2, HVar)      :- atom(CName2), HVar in 0..1, HVar #= (CVar2 #=  3).


% I disable this block of hypothesis for the reason that they heavily depend on domains of A1 and A2. Better suited for a separate examination.
% In any case, it seems that in majority cases these would be overridden by more general hypothesis and we only need to keep them. What is
% needed is after everything said and done is to comb through the final list and leave only the most general condition for the pair
% and delete the rest. But even then I gather that we need to check carefully these comparisons to attribure domains. So yeah. Disabled until
% a careful study for impact of these hypothesis. There are cases where their use is warranted but it had to be done carefully.
% After the hypothesis implication check is done I believe it'll be prudent to enable them again, update and test them with domains
% for A1 in [5..90] and A2 in [7..100]. With these domains it should be more or less easy to discern if these are applicable or not
/*generate_hypothesis([Coef1],_,coef1_lss_minattr1)               :- attribute1_min(A1Min), (Coef1 #<  A1Min).
generate_hypothesis(_,[Coef2],coef2_lss_minattr1)               :- attribute1_min(A1Min), (Coef2 #<  A1Min).
generate_hypothesis([Coef1],_,coef1_leq_minattr1)               :- attribute1_min(A1Min), (Coef1 #<= A1Min).
generate_hypothesis(_,[Coef2],coef2_leq_minattr1)               :- attribute1_min(A1Min), (Coef2 #<= A1Min).
generate_hypothesis([Coef1],_,coef1_grt_maxattr1)               :- attribute1_max(A1Max), (Coef1 #>  A1Max).
generate_hypothesis(_,[Coef2],coef2_grt_maxattr1)               :- attribute1_max(A1Max), (Coef2 #>  A1Max).
generate_hypothesis([Coef1],_,coef1_geq_maxattr1)               :- attribute1_max(A1Max), (Coef1 #>= A1Max).
generate_hypothesis(_,[Coef2],coef2_geq_maxattr1)               :- attribute1_max(A1Max), (Coef2 #>= A1Max).
generate_hypothesis([Coef1],_,coef1_lss_minattr2)               :- attribute2_min(A2Min), (Coef1 #<  A2Min).
generate_hypothesis(_,[Coef2],coef2_lss_minattr2)               :- attribute2_min(A2Min), (Coef2 #<  A2Min).
generate_hypothesis([Coef1],_,coef1_leq_minattr2)               :- attribute2_min(A2Min), (Coef1 #=< A2Min).
generate_hypothesis(_,[Coef2],coef2_leq_minattr2)               :- attribute2_min(A2Min), (Coef2 #=< A2Min).
generate_hypothesis([Coef1],_,coef1_grt_maxattr2)               :- attribute2_max(A2Max), (Coef1 #>  A2Max).
generate_hypothesis(_,[Coef2],coef2_grt_maxattr2)               :- attribute2_max(A2Max), (Coef2 #>  A2Max).
generate_hypothesis([Coef1],_,coef1_geq_maxattr2)               :- attribute2_max(A2Max), (Coef1 #>= A2Max).
generate_hypothesis(_,[Coef2],coef2_geq_maxattr2)               :- attribute2_max(A2Max), (Coef2 #>= A2Max).
generate_hypothesis([Coef1],_,coef1_grt_minattr1)               :- attribute1_min(A1Min), (Coef1 #>  A1Min).
generate_hypothesis(_,[Coef2],coef2_grt_minattr1)               :- attribute1_min(A1Min), (Coef2 #>  A1Min).
generate_hypothesis([Coef1],_,coef1_geq_minattr1)               :- attribute1_min(A1Min), (Coef1 #>= A1Min).
generate_hypothesis(_,[Coef2],coef2_geq_minattr1)               :- attribute1_min(A1Min), (Coef2 #>= A1Min).
generate_hypothesis([Coef1],_,coef1_lss_maxattr1)               :- attribute1_max(A1Max), (Coef1 #<  A1Max).
generate_hypothesis(_,[Coef2],coef2_lss_maxattr1)               :- attribute1_max(A1Max), (Coef2 #<  A1Max).
generate_hypothesis([Coef1],_,coef1_leq_maxattr1)               :- attribute1_max(A1Max), (Coef1 #=< A1Max).
generate_hypothesis(_,[Coef2],coef2_leq_maxattr1)               :- attribute1_max(A1Max), (Coef2 #=< A1Max).
generate_hypothesis([Coef1],_,coef1_grt_minattr2)               :- attribute2_min(A2Min), (Coef1 #>  A2Min).
generate_hypothesis(_,[Coef2],coef2_grt_minattr2)               :- attribute2_min(A2Min), (Coef2 #>  A2Min).
generate_hypothesis([Coef1],_,coef1_geq_minattr2)               :- attribute2_min(A2Min), (Coef1 #>= A2Min).
generate_hypothesis(_,[Coef2],coef2_geq_minattr2)               :- attribute2_min(A2Min), (Coef2 #>= A2Min).
generate_hypothesis([Coef1],_,coef1_lss_maxattr2)               :- attribute2_max(A2Max), (Coef1 #<  A2Max).
generate_hypothesis(_,[Coef2],coef2_lss_maxattr2)               :- attribute2_max(A2Max), (Coef2 #<  A2Max).
generate_hypothesis([Coef1],_,coef1_leq_maxattr2)               :- attribute2_max(A2Max), (Coef1 #=< A2Max).
generate_hypothesis(_,[Coef2],coef2_leq_maxattr2)               :- attribute2_max(A2Max), (Coef2 #=< A2Max).*/ %The end of this specific block


% generate_single_hypothesis(HName, CName1-CVar1, CName2-CVar2, HVar).
%generate_single_hypothesis(none, _, _, HVar)                           :- HVar in 0..1, HVar #= 1.
% TODO: add hypothesis that also include minimum domain of an attribute
generate_single_hypothesis(c1_eq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=  CVar2).
generate_single_hypothesis(c1_eq_c2_plus_1(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=  (CVar2 + 1)).
generate_single_hypothesis(c1_plus_1_eq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 1) #=  CVar2).
generate_single_hypothesis(c1_eq_c2_plus_2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=  (CVar2 + 2)).
generate_single_hypothesis(c1_plus_2_eq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 2) #=  CVar2).

generate_single_hypothesis(c1x2_eq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (2 * CVar1 #= CVar2).
generate_single_hypothesis(c1x2_leq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (2 * CVar1 #=< CVar2).
generate_single_hypothesis(c1x2_geq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (2 * CVar1 #>= CVar2).
generate_single_hypothesis(c1_eq_c2x2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=  2 * CVar2).
generate_single_hypothesis(c1_leq_c2x2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=< 2 * CVar2).
generate_single_hypothesis(c1_geq_c2x2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #>= 2 * CVar2).

generate_single_hypothesis(c1_geq_c2_plus_2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #>= (CVar2 + 2)).
generate_single_hypothesis(c1_plus_2_leq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 2) #=< CVar2).
generate_single_hypothesis(c1_leq_c2_plus_2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=< (CVar2 + 2)).
generate_single_hypothesis(c1_plus_2_geq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 2) #>= CVar2).
generate_single_hypothesis(c1_geq_c2_plus_1(CName1,CName2),                           
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #>= (CVar2 + 1)).
generate_single_hypothesis(c1_plus_1_leq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 1) #=< CVar2).
generate_single_hypothesis(c1_leq_c2_plus_1(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=< (CVar2 + 1)).
generate_single_hypothesis(c1_plus_1_geq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 1) #>= CVar2).
generate_single_hypothesis(c1_leq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #=< CVar2).
generate_single_hypothesis(c1_geq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #>= CVar2).
generate_single_hypothesis(c1_ineq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #\= CVar2).
generate_single_hypothesis(c1_ineq_c2_plus_1(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #\= (CVar2 + 1)).
generate_single_hypothesis(c1_plus_1_ineq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 1) #\= CVar2).
generate_single_hypothesis(c1_ineq_c2_plus_2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= (CVar1 #\= (CVar2 + 2)).
generate_single_hypothesis(c1_plus_2_ineq_c2(CName1,CName2),
                           CName1-CVar1, CName2-CVar2, HVar)        :- atom(CName1), atom(CName2), HVar in 0..1, HVar #= ((CVar1 + 2) #\= CVar2).

% Generates a selected constraint by creating required coefficient variables, placing constraints on the coefficients and
% creating Boolean variable ConstraintTerm that corresponds to whether or not the condition is satisfied given specific values of attributes
% Options: 
%* Type of the condition
%* List of all attribute variables.
%* [Value of Coef (if present), Value of Cst (if present)]
generate_constraint(ConstraintTerm, _Attrs, [none, [], []]) :-
    ConstraintTerm #= 1. 
generate_constraint(ConstraintTerm, [A|_], [attr_eq_coef, [1], [Coef]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    Coef #>= AttributeMin,
    Coef #=< AttributeMax,
    ConstraintTerm #= (A  #= Coef).
generate_constraint(ConstraintTerm, [A|_], [attr_leq_coef, [1], [Coef]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    ConstraintTerm #= (A #=< Coef).
generate_constraint(ConstraintTerm, [A|_], [attr_geq_coef, [1], [Coef]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    ConstraintTerm #= (A #>= Coef).
generate_constraint(ConstraintTerm, [A|_], [attr_in_interval, [1], [Coef,Cst]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    Cst  #> AttributeMin,
    Cst  #< AttributeMax,
    Cst  #> Coef,
    ConstraintTerm #= ((A #>= Coef) #/\ (A #=< Cst)).
generate_constraint(ConstraintTerm, [A|_], [attr_not_in_interval, [1], [Coef,Cst]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    Cst  #> AttributeMin,
    Cst  #< AttributeMax,
    Cst  #> Coef,
    ConstraintTerm #= #\((A #>= Coef) #/\ (A #=< Cst)).
generate_constraint(ConstraintTerm, [A|_], [unary_term_eq_coef(mod), [1], [Coef,Cst]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #>= 0,
    Cst #>= 2,
    Cst #> Coef,
    Cst #=< (AttributeMax - AttributeMin),
    ConstraintTerm #= (A mod Cst #= Coef).
generate_constraint(ConstraintTerm, [A|_], [unary_term_leq_coef(mod), [1], [Coef,Cst]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #>  0,
    Cst #>= 2,
    Cst #> Coef + 1,
    Cst #=< (AttributeMax - AttributeMin),
    ConstraintTerm #= (A mod Cst #=< Coef).
generate_constraint(ConstraintTerm, [A|_], [unary_term_geq_coef(mod), [1], [Coef,Cst]]):-
    attribute1_min(AttributeMin),
    attribute1_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #> 0,
    Cst #>= 2,
    Cst #> Coef + 1,
    Cst #=< (AttributeMax - AttributeMin),
    ConstraintTerm #= (A mod Cst #>= Coef).
generate_constraint(ConstraintTerm, [_,A|_], [attr_eq_coef, [2], [Coef]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    Coef #>= AttributeMin,
    Coef #=< AttributeMax,
    ConstraintTerm #= (A  #= Coef).
generate_constraint(ConstraintTerm, [_,A|_], [attr_leq_coef, [2], [Coef]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    ConstraintTerm #= (A #=< Coef).
generate_constraint(ConstraintTerm, [_,A|_], [attr_geq_coef, [2], [Coef]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    ConstraintTerm #= (A #>= Coef).
generate_constraint(ConstraintTerm, [_,A|_], [attr_in_interval, [2], [Coef,Cst]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    Cst  #> AttributeMin,
    Cst  #< AttributeMax,
    Cst  #> Coef,
    ConstraintTerm #= ((A #>= Coef) #/\ (A #=< Cst)).
generate_constraint(ConstraintTerm, [_,A|_], [attr_not_in_interval, [2], [Coef,Cst]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #> AttributeMin,
    Coef #< AttributeMax,
    Cst  #> AttributeMin,
    Cst  #< AttributeMax,
    Cst  #> Coef,
    ConstraintTerm #= #\((A #>= Coef) #/\ (A #=< Cst)).
generate_constraint(ConstraintTerm, [_,A|_], [unary_term_eq_coef(mod), [2], [Coef,Cst]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #>= 0,
    Cst #>= 2,
    Cst #> Coef,
    Cst #=< (AttributeMax - AttributeMin),
    ConstraintTerm #= (A mod Cst #= Coef).
generate_constraint(ConstraintTerm, [_,A|_], [unary_term_leq_coef(mod), [2], [Coef,Cst]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #>  0,
    Cst #>= 2,
    Cst #> Coef + 1,
    Cst #=< (AttributeMax - AttributeMin),
    ConstraintTerm #= (A mod Cst #=< Coef).
generate_constraint(ConstraintTerm, [_,A|_], [unary_term_geq_coef(mod), [2], [Coef,Cst]]):-
    attribute2_min(AttributeMin),
    attribute2_max(AttributeMax),
    generate_coef(0, Coef),
    generate_coef(0, Cst),
    Coef #> 0,
    Cst #>= 2,
    Cst #> Coef + 1,
    Cst #=< (AttributeMax - AttributeMin),
    ConstraintTerm #= (A mod Cst #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [attr_eq_attr,  [1,2], []]):-
    ConstraintTerm #= (A1 #= A2).
generate_constraint(ConstraintTerm, [A1,A2], [attr_leq_attr, [1,2], []]):-
    ConstraintTerm #= (A1 #=< A2).
generate_constraint(ConstraintTerm, [A1,A2], [attr_leq_attr, [2,1], []]):-
    ConstraintTerm #= (A2 #=< A1).
generate_constraint(ConstraintTerm, [A1,A2], [attr_eq_unary(prod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_unary_min_max(attr_eq_unary,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin, Coef #=< TMax,
    ConstraintTerm #= (A1 #= A2 * Coef).
generate_constraint(ConstraintTerm, [A1,A2], [attr_eq_unary(prod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_unary_min_max(attr_eq_unary,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin, Coef #=< TMax,
    ConstraintTerm #= (A2 #= A1 * Coef).
generate_constraint(ConstraintTerm, [A1,A2], [attr_leq_unary(prod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_unary_min_max(attr_leq_unary,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin, Coef #=< TMax,
    ConstraintTerm #= (A1 #=< A2 * Coef).
generate_constraint(ConstraintTerm, [A1,A2], [attr_leq_unary(prod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_unary_min_max(attr_leq_unary,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin, Coef #=< TMax,
    ConstraintTerm #= (A2 #=< A1 * Coef).
generate_constraint(ConstraintTerm, [A1,A2], [unary_leq_attr(prod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_unary_min_max(unary_leq_attr,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin, Coef #=< TMax,
    ConstraintTerm #= (A1 * Coef #=< A2).
generate_constraint(ConstraintTerm, [A1,A2], [unary_leq_attr(prod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_unary_min_max(unary_leq_attr,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin, Coef #=< TMax,
    ConstraintTerm #= (A2 * Coef #=< A1).
%                 t(cond(binary_term_eq_coef([plus],       coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(plus), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(plus,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A1 + A2 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(plus), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(plus,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 + A2 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(plus), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(plus,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 + A2 #>= Coef).
%                 t(cond(binary_term_eq_coef([minus],      coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(minus), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(minus,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    Coef #>= 1,
    ConstraintTerm #= (A1 - A2 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(minus), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(minus,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    Coef #>= 1,
    ConstraintTerm #= (A2 - A1 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(minus), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(minus,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    Coef #>= 1,
    ConstraintTerm #= (A1 - A2 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(minus), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(minus,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    Coef #>= 1,
    ConstraintTerm #= (A2 - A1 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(minus), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(minus,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #> TMin,
    Coef #< TMax,
    Coef #>= 1,
    ConstraintTerm #= (A1 - A2 #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(minus), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(minus,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    Coef #>= 1,
    ConstraintTerm #= (A2 - A1 #>= Coef).
%                 t(cond(binary_term_eq_coef([min],        coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(min), [1,2], [Coef]]):-
    generate_coef(2, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    Coef #>= max(Attribute1Min, Attribute2Min),
    Coef #=< min(Attribute1Max, Attribute2Max),
    ConstraintTerm #= (min(A1,A2) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(min), [1,2], [Coef]]):-
    generate_coef(2, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    Coef #>  max(Attribute1Min, Attribute2Min),
    Coef #<  min(Attribute1Max, Attribute2Max),
    ConstraintTerm #= (min(A1,A2) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(min), [1,2], [Coef]]):-
    generate_coef(2, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    Coef #>  max(Attribute1Min, Attribute2Min),
    Coef #<  min(Attribute1Max, Attribute2Max),
    ConstraintTerm #= (min(A1,A2) #>= Coef).
%                 t(cond(binary_term_eq_coef([max],        coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(max), [1,2], [Coef]]):-
    generate_coef(2, Coef), 
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    Coef #>= max(Attribute1Min, Attribute2Min),
    Coef #=< min(Attribute1Max, Attribute2Max),
    ConstraintTerm #= (max(A1,A2) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(max), [1,2], [Coef]]):-
    generate_coef(2, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    Coef #>  max(Attribute1Min, Attribute2Min),
    Coef #<  min(Attribute1Max, Attribute2Max),
    ConstraintTerm #= (max(A1,A2) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(max), [1,2], [Coef]]):-
    generate_coef(2, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    Coef #>  max(Attribute1Min, Attribute2Min),
    Coef #<  min(Attribute1Max, Attribute2Max),
    ConstraintTerm #= (max(A1,A2) #>= Coef).
%                 t(cond(binary_term_eq_coef([prod],       coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(prod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(prod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A1 * A2 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(prod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(prod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 * A2 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(prod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(prod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 * A2 #>= Coef).
%                 t(cond(binary_term_eq_coef([abs],        coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(abs), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(abs,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    Coef #>= 1,
    ConstraintTerm #= (abs(A1 - A2) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(abs), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(abs,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    Coef #>= 1,
    ConstraintTerm #= (abs(A1 - A2) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(abs), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    find_binterm_min_max(abs,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    Coef #>= 1,
    ConstraintTerm #= (abs(A1 - A2) #>= Coef).
%                 t(cond(binary_term_eq_coef([floor],      coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(floor), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(floor,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A1 div A2 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2],[binary_term_eq_coef(floor), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(floor,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A2 div A1 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(floor), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(floor,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 div A2 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(floor), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(floor,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A2 div A1 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(floor), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(floor,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 div A2 #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(floor), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(floor,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A2 div A1 #>= Coef).
%                 t(cond(binary_term_eq_coef([ceil],       coef(0,Max))), no_negation,    2),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(ceil), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(ceil,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A1 + A2 - 1) div A2 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2],[binary_term_eq_coef(ceil), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(ceil,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A1 + A2 - 1) div A1 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(ceil), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(ceil,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 + A2 - 1) div A2 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(ceil), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(ceil,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 + A2 - 1) div A1 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(ceil), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(ceil,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 + A2 - 1) div A2 #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(ceil), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(ceil,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 + A2 - 1) div A1 #>= Coef).

%                 t(cond(binary_term_eq_coef([mod],        coef(0,Max))), no_negation,    1),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(mod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(mod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A1 mod A2 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2],[binary_term_eq_coef(mod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(mod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A2 mod A1 #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(mod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(mod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 mod A2 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(mod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(mod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A2 mod A1 #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(mod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(mod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 mod A2 #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(mod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(mod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A2 mod A1 #>= Coef).
%                 t(cond(binary_term_eq_coef([mfloor],     coef(0,Max))), no_negation,    1),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(mfloor), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(mfloor,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A1 div max(A2 - Attribute2Min,1) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2],[binary_term_eq_coef(mfloor), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(mfloor,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= (A2 div max(A1 - Attribute1Min,1) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(mfloor), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(mfloor,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 div max(A2 - Attribute2Min,1) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(mfloor), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(mfloor,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A2 div max(A1 - Attribute1Min,1) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(mfloor), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(mfloor,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A1 div max(A2 - Attribute2Min,1) #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(mfloor), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(mfloor,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= (A2 div max(A1 - Attribute1Min,1) #>= Coef).
% Currently these are not selected, for reasons of taking too much time.
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(cmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(cmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A1 - (A2 mod A1)) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(cmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(cmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A2 - (A1 mod A2)) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(cmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(cmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 - (A2 mod A1)) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(cmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(cmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A2 - (A1 mod A2)) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(cmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(cmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 - (A2 mod A1)) #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(cmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(cmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A2 - (A1 mod A2)) #>= Coef).
%                 t(cond(binary_term_eq_coef([dmod],       coef(0,Max))), no_negation,    1),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(dmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(dmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A1 - (A1 mod A2)) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(dmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(dmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A2 - (A2 mod A1)) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(dmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(dmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 - (A1 mod A2)) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(dmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(dmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A2 - (A2 mod A1)) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(dmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(dmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 - (A1 mod A2)) #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(dmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(dmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A2 - (A2 mod A1)) #>= Coef).

%                 t(cond(binary_term_eq_coef([fmod],       coef(0,Max))), no_negation,    1),
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(fmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(fmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A1 mod A2 #= 0) * A2 + (A1 mod A2 #> 0) * (A1 mod A2) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_eq_coef(fmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(fmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>= TMin,
    Coef #=< TMax,
    ConstraintTerm #= ((A2 mod A1 #= 0) * A2 + (A2 mod A1 #> 0) * (A2 mod A1) #= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(fmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(fmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 mod A2 #= 0) * A2 + (A1 mod A2 #> 0) * (A1 mod A2) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_leq_coef(fmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(fmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A2 mod A1 #= 0) * A2 + (A2 mod A1 #> 0) * (A2 mod A1) #=< Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(fmod), [1,2], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute2Min #> 0 #/\ Attribute2Max #> 0) #\/ (Attribute2Min #< 0 #/\ Attribute2Max #< 0),
    find_binterm_min_max(fmod,Attribute1Min-Attribute1Max,Attribute2Min-Attribute2Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A1 mod A2 #= 0) * A2 + (A1 mod A2 #> 0) * (A1 mod A2) #>= Coef).
generate_constraint(ConstraintTerm, [A1,A2], [binary_term_geq_coef(fmod), [2,1], [Coef]]):-
    generate_coef(0, Coef),
    attribute1_min(Attribute1Min),
    attribute2_min(Attribute2Min),
    attribute1_max(Attribute1Max),
    attribute2_max(Attribute2Max),
    (Attribute1Min #> 0 #/\ Attribute1Max #> 0) #\/ (Attribute1Min #< 0 #/\ Attribute1Max #< 0),
    find_binterm_min_max(fmod,Attribute2Min-Attribute2Max,Attribute1Min-Attribute1Max, TMin-TMax),
    Coef #>  TMin,
    Coef #<  TMax,
    ConstraintTerm #= ((A2 mod A1 #= 0) * A2 + (A2 mod A1 #> 0) * (A2 mod A1) #>= Coef).

% Given a list of singular hypothesises HList:
% 1. generate Boolean variables that correspond to whether or not a corresponding singular hypothesis is satisfied or not
%    for given values of coefficients
% 2. Post a constraint that ensures that a conjuction of HVar variables is equal to a Boolean variable NegFlag
generate_hypothesis(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,NegFlag,HVars) :-
    generate_hvars(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,HVars),
    (HVars = [HVar]             -> (integer(HVar) -> HList = [none] ; true), HVar #= NegFlag    ;
     HVars = [HVar1,HVar2]      -> ((integer(HVar1), integer(HVar2)) -> HList = [none] ; true),
                                   (HVar1 #/\ HVar2) #= NegFlag                                 ; 
                                   false                                                        ).
    
% Given a list of singular hypothesises HList:
% 1. generate Boolean variables that correspond to whether or not a corresponding singular hypothesis is satisfied or not
%    for given values of coefficients
% 2. Generate a Boolean variable HypothesisTerm that corresponds to whether or not a conjuction of HVar variables is equal to a Boolean variable NegFlag
generate_hypothesis_term(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,NegFlag,HVars,HypothesisTerm) :-
    HypothesisTerm in 0..1,
    generate_hvars(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,HVars),
    (HVars = [HVar]             -> (integer(HVar) -> HList = [none] ; true),
                                   HypothesisTerm #= (HVar #= NegFlag)                          ;
     HVars = [HVar1,HVar2]      -> ((integer(HVar1), integer(HVar2)) -> HList = [none] ; true),
                                   HypothesisTerm #= ((HVar1 #/\ HVar2) #= NegFlag)             ; 
                                   false                                                        ).

% Same as generate_hypothesis_term/8 with the only difference that intermediate variables HVars aren't outputted
generate_forbidden_hypothesis_term(HList,CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HVar) :-
    HList = [_], !,
    generate_hvars(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,[HVar]).

generate_forbidden_hypothesis_term(HList,CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HypothesisTerm) :-
    HList = [_,_],
    generate_hvars(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,HList,[HVar1,HVar2]),
    HypothesisTerm in 0..1,
    HypothesisTerm #= (HVar1 #/\ HVar2).

% Given the lists of CoefsNames1 and CoefsNames2, lists of corresponging variables CoefsVars1 and CoefsVars2
% and the list of hypothesis HList generate list of HVars for each individual hypothesis in HList
% HVar = 0 means the the hypothesis is False for the selected values CoefVars1 and CoefVars2
% HVar = 1 means the the hypothesis is True  for the selected values CoefVars1 and CoefVars2  
generate_hvars(_CoefsNames1,_CoefsVars1,_CoefsNames2,_CoefsVars2,[],[]) :- !.
generate_hvars(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,[Hypothesis|R],[HVar|S]) :-
    functor(Hypothesis,_HypothesisName,Arity),
    (Arity = 0  -> generate_single_hypothesis(Hypothesis, none, none, HVar)
    ;
     Arity = 1  -> arg(1,Hypothesis,CoefName),
                   (find_coefname_coefvar(CoefsNames1,CoefsVars1,CoefName,CoefName-CoefVar) ->
                        generate_single_hypothesis(Hypothesis, CoefName-CoefVar, none, HVar)
                   ;
                    find_coefname_coefvar(CoefsNames2,CoefsVars2,CoefName,CoefName-CoefVar) ->
                        generate_single_hypothesis(Hypothesis, none, CoefName-CoefVar, HVar)
                   ;
                    write(generate_hvars(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,[Hypothesis|R],[HVar|S])),nl,halt
                   )
    ;
     Arity = 2  -> arg(1,Hypothesis,CoefName1),
                   arg(2,Hypothesis,CoefName2),
                   find_coefname_coefvar(CoefsNames1,CoefsVars1,CoefName1,CoefName1-CoefVar1),
                   find_coefname_coefvar(CoefsNames2,CoefsVars2,CoefName2,CoefName2-CoefVar2),
                   generate_single_hypothesis(Hypothesis, CoefName1-CoefVar1, CoefName2-CoefVar2, HVar)
    ;
                   write(generate_hvars(Arity, Hypothesis)),nl,halt),
    generate_hvars(CoefsNames1,CoefsVars1,CoefsNames2,CoefsVars2,R,S).
% ?- generate_hvars([coef1,cst1],[_A,_B],[coef2,cst2],[_C,_D],[c1_ineq_c2(cst1,coef2)],HVars).

% Given a lists of corresponding coefficient names and coefficient variables, it returns a corresponding variable to a selected variable name
% ?- find_coefname_coefvar([coef1,cst1],[_A,_B],cst1,CoefName1-CoefVar1)
find_coefname_coefvar([CoefName|_],[CoefVar|_],CoefName,CoefName-CoefVar) :- !.
find_coefname_coefvar([_|R],[_|S],CoefName,CoefName-CoefVar) :-
    find_coefname_coefvar(R,S,CoefName,CoefName-CoefVar).

% find_unary_min_max(unary_leq_attr, 1-100,1-100, TMin-TMax).
find_unary_min_max(Type, A1Min-A1Max,A2Min-A2Max, TMin-TMax) :-
    TMin is 2,
    A1 in A1Min..A1Max,
    A2 in A2Min..A2Max,
    A3 in A1Min..A1Max,
    A4 in A2Min..A2Max,
    coef_max(CoefMax),
    C in TMin..CoefMax,
    (Type = attr_eq_unary       -> (A1 #=  A2 * C) #/\ #\(A3 #=  A4 * C)                        ;
     Type = attr_leq_unary      -> (A1 #=< A2 * C) #/\ #\(A3 #=< A4 * C)                        ;
     Type = unary_leq_attr      -> (A1 * C #=< A2) #/\ #\(A3 * C #=< A4)                        ;
     false
    ),
    labeling([maximize(C),down],[C,A1,A2,A3,A4]),
    TMax is C.

find_binterm_min_max(Term,A1Min-A1Max,A2Min-A2Max, TMin-TMax) :-
    find_binterm_min(Term,A1Min-A1Max,A2Min-A2Max, TMin),
    find_binterm_max(Term,A1Min-A1Max,A2Min-A2Max, TMax).

%find_binterm_min(prod,1-10,1-10,C).
find_binterm_min(Term,A1Min-A1Max,A2Min-A2Max, TMin) :-
    A1 in A1Min..A1Max,
    A2 in A2Min..A2Max,
    (Term = plus                -> C #= A1 + A2                                                 ;
     Term = minus               -> C #= A1 - A2                                                 ;
     Term = prod                -> C #= A1 * A2                                                 ;
     Term = abs                 -> C #= abs(A1 - A2)                                            ;
     Term = mod                 -> C #= A1 mod A2                                               ;
     Term = floor               -> C #= A1 div A2                                               ;
     Term = mfloor              -> C #= A1 div max(A2-A2Min,1)                                  ;
     Term = ceil                -> C #= (A1 + A2 - 1) div A2                                    ;
     Term = cmod                -> C #= A1 - A2 mod A1                                          ;
     Term = dmod                -> C #= A1 - A1 mod A2                                          ;
     Term = fmod                -> C #= (A1 mod A2 #= 0) * A2 + (A1 mod A2 #> 0) * (A1 mod A2)  ;
     false
    ),
    labeling([minimize(C),up],[C,A1,A2]),
    % TODO: try to force best option for minimize search time_out(minimize(label(AllAttrVars, AllCstOrdInUnaryBinary, AllCoefs, AllLowUps), Cost, [all]), TimeOut, Result),
    TMin is C.

find_binterm_max(Term,A1Min-A1Max,A2Min-A2Max, TMax) :-
    A1 in A1Min..A1Max,
    A2 in A2Min..A2Max,
    (Term = plus                -> C #= A1 + A2                                                 ;
     Term = minus               -> C #= A1 - A2                                                 ;
     Term = prod                -> C #= A1 * A2                                                 ;
     Term = abs                 -> C #= abs(A1 - A2)                                            ;
     Term = mod                 -> C #= A1 mod A2                                               ;
     Term = floor               -> C #= A1 div A2                                               ;
     Term = mfloor              -> C #= A1 div max(A2-A2Min,1)                                  ;
     Term = ceil                -> C #= (A1 + A2 - 1) div A2                                    ;
     Term = cmod                -> C #= A1 - A2 mod A1                                          ;
     Term = dmod                -> C #= A1 - A1 mod A2                                          ;
     Term = fmod                -> C #= (A1 mod A2 #= 0) * A2 + (A1 mod A2 #> 0) * (A1 mod A2)  ;
     false
    ),
    labeling([maximize(C),down],[C,A1,A2]),
    TMax is C.

generate_coef(NbAttr, Coef) :-
    (NbAttr = 0 ->
        coef_min(CoefMin),
        coef_max(CoefMax),
        Coef in CoefMin..CoefMax
    ;
     NbAttr = 1 ->
        attribute1_max(AttributeMax),
        coef_min(CoefMin),
        coef_max(CoefMax),
        Coef in CoefMin..CoefMax,
        (AttributeMax = sup -> true ; Coef #=< AttributeMax)
    ;
     NbAttr = 2 ->
        attribute1_max(Attribute1Max),
        attribute2_max(Attribute2Max),
        coef_min(CoefMin),
        coef_max(CoefMax),
        Coef in CoefMin..CoefMax,
        (Attribute1Max = sup -> true ; Coef #=< Attribute1Max),
        (Attribute2Max = sup -> true ; Coef #=< Attribute2Max)
    ;
     false).
