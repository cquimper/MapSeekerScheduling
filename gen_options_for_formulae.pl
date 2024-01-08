% Purpose: Generate a list of options telling which candidate formulae to generate and which parameters to use
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_options_for_formulae,[gen_options_bool/2,
                                    gen_options_cond/3,
                                    gen_options_lists/11,
                                    gen_options_lists_decomp_base/11,
                                    gen_options_lists_decomp_else/11,
                                    gen_options_lists_decomp_diff/11]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(table_access).
:- use_module(utility).
:- use_module(bool_cond_utility).
:- use_module(gen_clusters_val_fds).

option_bool_min_cst(model_seeker,  -9) :- !. % min. val. of a cst in an option when KindCombi= model_seeker and on Boolean formula
option_bool_min_cst(_,             -5).      % min. val. of a cst in an option when KindCombi<>model_seeker and on Boolean formula

option_bool_max_cst(model_seeker,   9) :- !. % max. val. of a cst in an option when KindCombi= model_seeker and on Boolean formula
option_bool_max_cst(_,              5).      % max. val. of a cst in an option when KindCombi<>model_seeker and on Boolean formula

option_cond_min_cst(model_seeker,  -9) :- !. % min. val. of a cst in an option when KindCombi= model_seeker and on conditional formula
option_cond_min_cst(_,             -3).      % min. val. of a cst in an option when KindCombi<>model_seeker and on conditional formula

option_cond_max_cst(model_seeker,   9) :- !. % max. val. of a cst in an option when KindCombi= model_seeker and on conditional formula
option_cond_max_cst(_,              3).      % max. val. of a cst in an option when KindCombi<>model_seeker and on conditional formula

option_poly_min_cst(model_seeker,  -3) :- !. % min. val. of a cst in an option when KindCombi= model_seeker and on a formula using polynomials
option_poly_min_cst(_,             -1).      % min. val. of a cst in an option when KindCombi<>model_seeker and on a formula using polynomials

option_poly_max_cst(model_seeker,   9) :- !. % max. val. of a cst in an option when KindCombi= model_seeker and on a formula using polynomials
option_poly_max_cst(_,              1).      % max. val. of a cst in an option when KindCombi<>model_seeker and on a formula using polynomials

option_poly_min_coef(model_seeker, -4) :- !. % min. val. of a coef in an option when KindCombi= model_seeker and on a formula using polynomials
option_poly_min_coef(_,            -2).      % min. val. of a coef in an option when KindCombi<>model_seeker and on a formula using polynomials

option_poly_max_coef(model_seeker,  4) :- !. % max. val. of a coef in an option when KindCombi= model_seeker and on a formula using polynomials
option_poly_max_coef(_,             2).      % max. val. of a coef in an option when KindCombi<>model_seeker and on a formula using polynomials

% generate the list of Boolean options without the attributes part: called only once
% as this part is independant from which attributes the formula uses; passed later as
% an argument of gen_options_lists/11
% within the term t, the third entry is the maximum number of occurrences of a same condition within a Boolean term
gen_options_bool(KindCombi, OptionsBool) :-
    option_bool_min_cst(KindCombi, Min),                                                               % get minimum value of a constant in a Boolean condition
    option_bool_max_cst(KindCombi, Max),                                                               % get maximum value of a constant in a Boolean condition
    ListConds = [t(cond(attr_eq_attr),                                   no_negation,    2),
                 t(cond(attr_leq_attr),                                  no_negation,    2),
                 t(cond(attr_eq_coef(coef(Min,Max))),                    no_negation,    3),
                 t(cond(attr_leq_coef(coef(Min,Max))),                   no_negation,    3),
                 t(cond(attr_geq_coef(coef(Min,Max))),                   no_negation,    3),
                 t(cond(attr_in_interval(1,Max)),                        no_restriction, 3),
                 t(cond(attr_eq_unary([prod(2,Max)])),                   no_negation,    1),
                 t(cond(attr_leq_unary([prod(2,Max)])),                  no_negation,    1),
                 t(cond(unary_leq_attr([prod(2,Max)])),                  no_negation,    1),
                 t(cond(sum_leq_attr),                                   no_negation,    1),
                 t(cond(ceil_leq_floor),                                 no_negation,    1),
                 t(cond(unary_term_eq_coef([mod(2,Max)],  coef(0,Max))), no_negation,    1),
                 t(cond(unary_term_leq_coef([mod(2,Max)], coef(0,Max))), no_negation,    1),
                 t(cond(unary_term_geq_coef([mod(2,Max)], coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([plus],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([minus],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([min],        coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([max],        coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([prod],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([abs],        coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([floor],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([ceil],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([mfloor],     coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([plus],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([minus],     coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([min],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([max],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([prod],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([abs],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([floor],     coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([ceil],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([mfloor],    coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([plus],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([minus],     coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([min],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([max],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([prod],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([abs],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([floor],     coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([ceil],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([mfloor],    coef(0,Max))), no_negation,    1),
                 t(cond(minus_mod_eq0),                                  no_negation,    1),
                 t(cond(attr_geq_fmod),                                  no_negation,    1),
                 t(cond(mod_gt_mod),                                     no_negation,    1),
                 t(cond(binary_term_eq_coef([mod],        coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([cmod],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([dmod],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_eq_coef([fmod],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([mod],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([cmod],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([dmod],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_leq_coef([fmod],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([mod],       coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([cmod],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([dmod],      coef(0,Max))), no_negation,    1),
                 t(cond(binary_term_geq_coef([fmod],      coef(0,Max))), no_negation,    1)],

   % with a bool option, the terms neg/1, op/1, nb_terms/2 and conds/1 have to be passed in this order !
   % use_mods(0) - prevent usage of conditions with mod in them
   % use_mods(1) - forcing usage of conditions with mod in them
   % use_mods(2) - same as prior behavior - solver can use or not use conditions with mods in them
    (KindCombi \== model_seeker ->
        OptionsBool = [[bool([neg(0), op([and]),      nb_terms(1,1), conds(ListConds), use_mods(2)])], % with one term use mod or do not use mod
                       [bool([neg(1), op([and]),      nb_terms(1,1), conds(ListConds), use_mods(2)])],

                       [bool([neg(0), op([and]),      nb_terms(2,2), conds(ListConds), use_mods(0)])], % use_mods(0): do not use condition with mod
                       [bool([neg(0), op([or]),       nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([sum]),      nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([allequal]), nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([and]),      nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([or]),       nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([sum]),      nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([allequal]), nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       
                       [bool([neg(0), op([and]),      nb_terms(2,2), conds(ListConds), use_mods(1)])], % use_mods(1): do not use condition with mod
                       [bool([neg(0), op([or]),       nb_terms(2,2), conds(ListConds), use_mods(1)])],
                       [bool([neg(0), op([sum]),      nb_terms(2,2), conds(ListConds), use_mods(1)])],
                       [bool([neg(0), op([allequal]), nb_terms(2,2), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([and]),      nb_terms(2,2), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([or]),       nb_terms(2,2), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([sum]),      nb_terms(2,2), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([allequal]), nb_terms(2,2), conds(ListConds), use_mods(1)])],

                       [bool([neg(0), op([and]),      nb_terms(3,3), conds(ListConds), use_mods(0)])], % use_mods(0): do not use condition with mod
                       [bool([neg(0), op([or]),       nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([sum]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([allequal]), nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([card1]),    nb_terms(3,3), conds(ListConds), use_mods(0)])], % tried before xor as more constrained than xor
                       [bool([neg(0), op([xor]),      nb_terms(3,3), conds(ListConds), use_mods(0)])], % use_mods(0): do not use condition with mod
                       [bool([neg(0), op([voting]),   nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([and]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([or]),       nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([sum]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([allequal]), nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([card1]),    nb_terms(3,3), conds(ListConds), use_mods(0)])], % tried before xor as more constrained than xor
                       [bool([neg(1), op([xor]),      nb_terms(3,3), conds(ListConds), use_mods(0)])], % use_mods(0): do not use condition with mod
                       [bool([neg(1), op([voting]),   nb_terms(3,3), conds(ListConds), use_mods(0)])],

                       [bool([neg(0), op([and]),      nb_terms(3,3), conds(ListConds), use_mods(1)])], % use_mods(1): do not use condition with mod
                       [bool([neg(0), op([or]),       nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(0), op([sum]),      nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(0), op([allequal]), nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(0), op([card1]),    nb_terms(3,3), conds(ListConds), use_mods(1)])], % tried before xor as more constrained than xor
                       [bool([neg(0), op([xor]),      nb_terms(3,3), conds(ListConds), use_mods(1)])], % use_mods(1): do not use condition with mod
                       [bool([neg(0), op([voting]),   nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([and]),      nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([or]),       nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([sum]),      nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([allequal]), nb_terms(3,3), conds(ListConds), use_mods(1)])],
                       [bool([neg(1), op([card1]),    nb_terms(3,3), conds(ListConds), use_mods(1)])], % tried before xor as more constrained than xor
                       [bool([neg(1), op([xor]),      nb_terms(3,3), conds(ListConds), use_mods(1)])], % use_mods(1): do not use condition with mod
                       [bool([neg(1), op([voting]),   nb_terms(3,3), conds(ListConds), use_mods(1)])]]
    ; %for model seeker
        OptionsBool = [[bool([neg(0), op([and]),      nb_terms(1,1), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([and]),      nb_terms(1,1), conds(ListConds), use_mods(0)])],

                       [bool([neg(0), op([and]),      nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([or]),       nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([sum]),      nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([allequal]), nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([and]),      nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([or]),       nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([sum]),      nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([allequal]), nb_terms(2,2), conds(ListConds), use_mods(0)])],
                       
                       [bool([neg(0), op([and]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([or]),       nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([sum]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([allequal]), nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([card1]),    nb_terms(3,3), conds(ListConds), use_mods(0)])], % tried before xor as more constrained than xor
                       [bool([neg(0), op([xor]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(0), op([voting]),   nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([and]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([or]),       nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([sum]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([allequal]), nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([card1]),    nb_terms(3,3), conds(ListConds), use_mods(0)])], % tried before xor as more constrained than xor
                       [bool([neg(1), op([xor]),      nb_terms(3,3), conds(ListConds), use_mods(0)])],
                       [bool([neg(1), op([voting]),   nb_terms(3,3), conds(ListConds), use_mods(0)])]]
    ).

% generate the list of conditional options without the attributes part: called only once
% as this part is independant from which attributes the formula uses; passed later as
% an argument of gen_options_lists/11
gen_options_cond(KindCombi, OptionsCond, NParams) :-
    option_cond_min_cst(KindCombi, Min), % get minimum value of a constant in a condition of a conditional formula
    option_cond_max_cst(KindCombi, Max), % get maximum value of a constant in a condition of a conditional formula
    (KindCombi \== model_seeker ->
            OrderedCond = [cond(attr_eq_attr),
                           cond(attr_eq_coef(coef(Min,Max))),
                           cond(attr_leq_attr),
                           cond(attr_leq_coef(coef(Min,Max))),
                           cond(attr_in_interval(1,Max)),
                           cond(binary_term_eq_coef([plus],                      coef(0,Max))),
                           cond(binary_term_eq_coef([minus],                     coef(0,Max))),
                           cond(binary_term_eq_coef([min,max],                   coef(0,Max))),
                           cond(binary_term_eq_coef([prod,abs],                  coef(0,Max))),
                           cond(attr_leq_unary([prod(2,Max)])),
                           cond(unary_leq_attr([prod(2,Max)])),
                           cond(binary_term_leq_coef([plus],                     coef(0,Max))),
                           cond(binary_term_leq_coef([minus],                    coef(0,Max))),
                           cond(sum_leq_attr),
                           cond(binary_term_leq_coef([prod,abs],                 coef(0,Max))),
                           cond(binary_term_leq_coef([min,max],                  coef(0,Max))),
                           cond(binary_term_eq_coef([floor],                     coef(0,Max))),
                           cond(binary_term_eq_coef([ceil],                      coef(0,Max))),
                           cond(binary_term_eq_coef([mfloor],                    coef(0,Max))),
                           cond(linear_eq_coef(cst(Min,Max),                     coef(Min,Max))),
                           cond(binary_term_leq_coef([floor],                    coef(0,Max))),
                           cond(binary_term_leq_coef([ceil],                     coef(0,Max))),
                           cond(binary_term_leq_coef([mfloor],                   coef(0,Max))),
                           cond(unary_term_eq_coef([mod(2,Max)],                 coef(0,Max))),
                           cond(binary_term_eq_coef([mod],                       coef(0,Max))),
                           cond(binary_term_eq_coef([cmod,dmod],                 coef(0,Max))),
                           cond(binary_term_eq_coef([fmod],                      coef(1,Max))), 
                           cond(binary_term_leq_coef([mod],                      coef(0,Max))),
                           cond(unary_term_leq_coef([mod(2,Max)],                coef(0,Max))),
                           cond(binary_term_leq_coef([cmod,dmod],                coef(0,Max))),
                           cond(binary_term_leq_coef([fmod],                     coef(1,Max))),
                           cond(minus_mod_eq0)                                                ,
                           cond(ceil_leq_floor)                                               ],
            OrderedThen = [then(attr), 
                           then(coef(Min,Max)),
                           then(binary_term([plus])),
                           then(binary_term([minus])),
                           then(binary_term([linear(Min,Max)])),
                           then(unary_term([plus(1,Max)])),
                           then(unary_term([minus(1,Max)])),
                           then(binary_term([min,max])),
                           then(unary_term([min(Min,Max),max(Min,Max)])),
                           then(binary_term([prod])),
                           then(unary_term([prod(2,Max)])),
                           then(binary_term([abs])),
                           then(binary_term([floor])),
                           then(unary_term([min_min(1,Max)])),
                           then(unary_term([max_min(1,Max)])),
                           then(binary_term([plus_min(1,Max)])),
                           then(unary_term([floor(2,Max)])),
                           then(binary_term([ceil])),
                           then(binary_term([mfloor])),
                           then(binary_term([plus_floor(Min,Max)])),
                           then(binary_term([minus_floor(Min,Max,2,Max)])),
                           then(binary_term([mod])),
                           then(unary_term([ceil(2,Max)])),
                           then(unary_term([floor_min(2,Max)])),
                           then(unary_term([mod(2,Max)])),
                           then(binary_term([cmod,dmod])),
                           then(binary_term([fmod]))],
            OrderedElse = [else(attr),
                           else(coef(Min,Max)),
                           else(binary_term([plus])),
                           else(binary_term([minus])),
                           else(binary_term([linear(Min,Max)])),
                           else(unary_term([plus(1,Max)])),
                           else(unary_term([minus(1,Max)])),
                           else(binary_term([min,max])),
                           else(unary_term([min(Min,Max),max(Min,Max)])),
                           else(binary_term([prod])),
                           else(unary_term([prod(2,Max)])),
                           else(binary_term([abs])),
                           else(binary_term([floor])),
                           else(unary_term([min_min(1,Max)])),
                           else(unary_term([max_min(1,Max)])),
                           else(binary_term([plus_min(1,Max)])),
                           else(unary_term([floor(2,Max)])),
                           else(binary_term([ceil])),
                           else(binary_term([mfloor])),
                           else(binary_term([plus_floor(Min,Max)])),
                           else(binary_term([minus_floor(Min,Max,2,Max)])),
                           else(binary_term([mod])),
                           else(unary_term([ceil(2,Max)])),
                           else(unary_term([floor_min(2,Max)])),
                           else(unary_term([mod(2,Max)])),
                           else(binary_term([cmod,dmod])),
                           else(binary_term([fmod]))]
    ;
            OrderedCond = [cond(attr_eq_attr),
                           cond(attr_eq_coef(coef(Min,Max))),
                           cond(attr_leq_attr),
                           cond(attr_leq_coef(coef(Min,Max))),
                           cond(attr_in_interval(1,Max)),
                           cond(binary_term_eq_coef([plus],                      coef(0,Max))),
                           cond(binary_term_eq_coef([minus],                     coef(0,Max))),
                           cond(binary_term_eq_coef([min,max],                   coef(0,Max))),
                           cond(binary_term_eq_coef([prod,abs],                  coef(0,Max))),
                           cond(attr_leq_unary([prod(2,Max)])),
                           cond(unary_leq_attr([prod(2,Max)])),
                           cond(binary_term_leq_coef([plus],                     coef(0,Max))),
                           cond(binary_term_leq_coef([minus],                    coef(0,Max))),
                           cond(sum_leq_attr),
                           cond(binary_term_leq_coef([prod,abs],                 coef(0,Max))),
                           cond(binary_term_leq_coef([min,max],                  coef(0,Max))),
                           cond(binary_term_eq_coef([floor],                     coef(0,Max))),
                           cond(binary_term_eq_coef([ceil],                      coef(0,Max))),
                           cond(binary_term_eq_coef([mfloor],                    coef(0,Max))),
                           cond(linear_eq_coef(cst(Min,Max),                     coef(Min,Max))),
                           cond(binary_term_leq_coef([floor],                    coef(0,Max))),
                           cond(binary_term_leq_coef([ceil],                     coef(0,Max))),
                           cond(binary_term_leq_coef([mfloor],                   coef(0,Max))),
                           cond(ceil_leq_floor)                                               ],
            OrderedThen = [then(attr), 
                           then(coef(Min,Max)),
                           then(binary_term([plus])),
                           then(binary_term([minus])),
                           then(binary_term([linear(Min,Max)])),
                           then(unary_term([plus(1,Max)])),
                           then(unary_term([minus(1,Max)])),
                           then(binary_term([min,max])),
                           then(unary_term([min(Min,Max),max(Min,Max)])),
                           then(binary_term([prod])),
                           then(unary_term([prod(2,Max)])),
                           then(binary_term([abs])),
                           then(binary_term([floor])),
                           then(unary_term([min_min(1,Max)])),
                           then(unary_term([max_min(1,Max)])),
                           then(binary_term([plus_min(1,Max)])),
                           then(unary_term([floor(2,Max)])),
                           then(binary_term([ceil])),
                           then(binary_term([mfloor])),
                           then(binary_term([plus_floor(Min,Max)])),
                           then(binary_term([minus_floor(Min,Max,2,Max)])),
                           then(unary_term([ceil(2,Max)])),
                           then(unary_term([floor_min(2,Max)]))],
            OrderedElse = [else(attr),
                           else(coef(Min,Max)),
                           else(binary_term([plus])),
                           else(binary_term([minus])),
                           else(binary_term([linear(Min,Max)])),
                           else(unary_term([plus(1,Max)])),
                           else(unary_term([minus(1,Max)])),
                           else(binary_term([min,max])),
                           else(unary_term([min(Min,Max),max(Min,Max)])),
                           else(binary_term([prod])),
                           else(unary_term([prod(2,Max)])),
                           else(binary_term([abs])),
                           else(binary_term([floor])),
                           else(unary_term([min_min(1,Max)])),
                           else(unary_term([max_min(1,Max)])),
                           else(binary_term([plus_min(1,Max)])),
                           else(unary_term([floor(2,Max)])),
                           else(binary_term([ceil])),
                           else(binary_term([mfloor])),
                           else(binary_term([plus_floor(Min,Max)])),
                           else(binary_term([minus_floor(Min,Max,2,Max)])),
                           else(unary_term([ceil(2,Max)])),
                           else(unary_term([floor_min(2,Max)]))]
    ),
    length(OrderedCond, LCond),
    length(OrderedThen, LThen),
    length(OrderedElse, LElse),
    add_key(OrderedCond, KeyOrderedCond),
    add_key(OrderedThen, KeyOrderedThen),
    add_key(OrderedElse, KeyOrderedElse),
    MinSumKeys = 3,
    MaxSumKeys is LCond+LThen+LElse,
    (NParams =< 2 -> SumKeysLimit is MaxSumKeys                         ;       % use the full set of options if NParam is low, otherwise
                     SumKeysLimit is (MaxSumKeys + MinSumKeys) // 2     ),      % restrict the number of combinations of conditionals by half
    findall([C,T,E], (SumKeys in MinSumKeys..MaxSumKeys,
                      indomain(SumKeys),
                      member(K1-C, KeyOrderedCond),
                      member(K2-T, KeyOrderedThen),
                      member(K3-E, KeyOrderedElse),
                      SumKeys is K1+K2+K3,
                      SumKeys =< SumKeysLimit,
                      (check_then_else_mod(T) -> \+ check_then_else_mod(E)      ;
                       check_then_else_mod(E) -> \+ check_then_else_mod(T)      ;
                                                 true                           )),
            OptionsCond).

check_then_else_mod(ThenElse) :-
    arg(1, ThenElse, ThenElseTerm),
    functor(ThenElseTerm, ThenElseTermName, _),
    memberchk(ThenElseTermName, [unary_term, binary_term]),
    arg(1, ThenElseTerm, ThenElseType),
    findall(ThenElseType1,
            (member(ThenElseType1, ThenElseType),
             memberchk(ThenElseType1,[mod, mod(_,_), cmod, dmod, fmod])
            ),
            [_|_]).


% generate a list of options for candidate formulae:
% (1) the type of formula generated is one of the following:
%     . a Boolean formula
%     . a condition of the form "if cond then statement else statement"
%     . a set of Boolean formulae giving a condition for each output value, except the last value
%       this type of formula is called "cluster" and is only used when between 3 and 4 output values
%     . a linear term
%     . a unary function involving a linear term
%     . a binary function involving two linear terms
%     . a polynom of degree 2
%     . a linear term involving a unary term
%     . a linear term involving a binary term
% (2) the way attributes occur in the candidate formula (in all the following three cases we discard the Output column
%                                                        as it cannot be part of the input parameters of the formula):
%     . the candidate formula uses only the primary attributes (and all of them)
%     . the candidate formula uses the smallest combination of attributes (except the one which uses all primary attributes)
%       which functionally determine the target bound
%     . the candidate formula uses any combination of attributes
%
% INPUT PARAMETERS
%  KindCombi           : <> model_seeker if generate options for conjectures, model_seeker if generate options for ASSISTANT
%  col(Table,OutputCol): column OutputCol of the table Table for which generate options
%  Kind                : kind associated with column OutputCol of table Table (primary,secondary,low,up)
%  OptionsBoolCond     : list of Boolean option and list of conditional option that were computed only once
%  PrevInputParams     : when KindCombi<>model_seeker and Kind=secondary the options should not use a set of characteristics that is
%                        included in one of the sublists of PrevInputParams (as they were already tried out)
%  InputParams         : when KindCombi<>model_seeker and Kind=secondary the options should only use characteristics that belong to InputParams
%
% OUTPUT PARAMETER
%  ListsOptions        : generated list of options
%  SetsBool            : for each input parameter, set of possible values for the constant of a Boolean condition
%
gen_options_lists(KindCombi, col(Table,OutputCol), Kind, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd,
                  TableEntries, Mode, ListsOptions, SetsBool) :-
    (KindCombi \== model_seeker ->
        ListsOptions = []
                         
    ;
        gen_options_lists_by_type(bool,    KindCombi, col(Table,OutputCol), Kind, Mode, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, ListsOptionsBool),
        gen_options_lists_by_type(cond_ex, KindCombi, col(Table,OutputCol), Kind, Mode, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, [ListsOptionsCondS, ListsOptionsCondEx]),
        ((ListsOptionsBool = [], ListsOptionsCondEx = [], ListsOptionsCondS = [], _ListsOptionsCondA = []) ->
            SetsBool = []
        ;
            gen_value_sets_wrt_boolean_operators(KindCombi, TableEntries, Mode, InputParams, col(Table,OutputCol), [], [], SetsBool)
        ),
        % formula(MinNbOfMonomes,MaxNbOfMonomes) - we set minimum and maximum numbers of monomes in a linear formulae
        %                                          (quadratic formulae have from MinNbOfMonomes - 1 to MaxNbOfMonomes - 1 of monomes).
        gen_options_lists_by_type(formula(1,1), KindCombi, col(Table,OutputCol), Kind, Mode, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, ListsOptionsPoly1),
        gen_options_lists_by_type(formula(2,2), KindCombi, col(Table,OutputCol), Kind, Mode, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, ListsOptionsPoly2),
        gen_options_lists_by_type(formula(3,3), KindCombi, col(Table,OutputCol), Kind, Mode, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, ListsOptionsPoly3),
        ListsOptions = [     bool(ListsOptionsBool),
                         formula(ListsOptionsPoly1),
                         formula(ListsOptionsPoly2),
                         formula(ListsOptionsPoly3),
                            cond(ListsOptionsCondS),
                        cond_ex(ListsOptionsCondEx)%,
                       ]
    ).


gen_options_lists_by_type(bool, _KindCombi, col(Table,OutputCol), _Kind, Mode, _OptionsBoolCond, _PrevInputParams, _InputParams, _, []) :-
    (Mode = preprocessed(Min, Max, _, _) ->
         Range is Max-Min
    ;
         tab_get_min_max(col(Table,OutputCol), Min, Max), % if generate options for Boolean formulae then catch the cases
         Range is Max-Min                                 % where the output column cannot be obtained from a Boolean formula
    ),
    (Range = 0 -> true ;                               % then return an empty list
     Range > 3 -> true ;                               % allow a range of 3 as we generate Boolean expressions having at most 3 terms
                  false),                              % FIX Nicolas 04-03-2022: Range > 2 RATHER THAN Range > 3
    !.
gen_options_lists_by_type(cluster, _KindCombi, col(Table,OutputCol), _Kind, _Mode, _OptionsBoolCond, _PrevInputParams, _InputParams, _, []) :- % CLUSTER
    tab_get_vals_fd(col(Table,OutputCol), []),         % return empty list if the metadata generation phase saw that we cannot use
    !.                                                 % a case formula for this column
gen_options_lists_by_type(CondBool, KindCombi, col(Table,OutputCol), Kind, Mode, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, ListsOptions) :-
    memberchk(CondBool,[bool,cond,cond_ex]),           % if generate options for Boolean formulae or for conditional formulae
    !,                                                 % extract functional dependencies wrt the output column
    (Mode = preprocessed(_,_, LimitBool, _) ->
        FilteredFd = [mandatory_attr(InputParams)]
    ;
     ForceToUseOnlySmallFd = 1 ->                      % if force to use an fd which is included in the original input parameters
        FilteredFd = [mandatory_attr(InputParams)],    % (see predicate search_secondaries1_small_fd in main.pl for that)
        LimitBool = 0
    ;                                                  % then force to use it (in this case InputParams corresponds to the small fd)
        gen_filtered_fd(KindCombi, col(Table,OutputCol), Kind, PrevInputParams, InputParams, FilteredFd),
        LimitBool = 0
    ),
    ClusterTypes = [[attr,                       coef(Min,Max)             ],
                    [coef(Min,Max),              attr                      ],
                    [coef(Min,Max),              coef(Min,Max)             ],
                    [attr,                       attr                      ],
                    [coef(Min,Max),              unary_term([minus(1,Max)])],
                    [attr,                       unary_term([minus(1,Max)])],
                    [unary_term([minus(1,Max)]), coef(Min,Max)             ],
                    [unary_term([minus(1,Max)]), attr                      ],
                    [coef(Min,Max),              binary_term([minus])      ],
                    [attr,                       binary_term([minus])      ],
                    [binary_term([minus]),       coef(Min,Max)             ],
                    [binary_term([minus]),       attr                      ]
                   ], % can add binary and unary terms with plus, minus and div
    (FilteredFd = [] ->
        (CondBool = cond_ex ->                         % cond_ex group together simple conditional and conditional with Boolean conditions
            ListsOptions = [[], []]                    % first list is the options for simple conditional, second list is option
        ;                                              % for conditional with Boolean conditions
            ListsOptions = []
        )
    ;
        (CondBool = bool ->                            % if generate options for Boolean formulae
            OptionsBoolCond = OptionsBool-_,           % get set of potential Boolean formulae that was generated once and for all
            (Mode = preprocessed(Min,Max, _, _) ->
                 Range is Max-Min
            ;
                 tab_get_min_max(col(Table,OutputCol), Min, Max),
                 Range is Max-Min
            ),
            (Range = 1 ->                              % if the output column is in Min + 0..1
                findall([bool([neg(Negative), op(LFiltered), nb_terms(Low,Up), conds(Conds), use_mods(UseMods)])],
                        (member([bool([neg(Negative), op(L), nb_terms(Low,Up), conds(Conds), use_mods(UseMods)])], OptionsBool),
                         delete(L, sum, LFiltered),    % can only use operators "and", "or", "allequal", "xor", "voting", "card1" (but not "sum")
                         LFiltered = [_|_],            % should have at least one operator in the filtered list
                         (LimitBool = 1 ->
                              Up =< 2,
                              memberchk(UseMods, [0,2])
                         ;
                              true
                         )
                        ),
                        OptionsFormula)                % get all Boolean operators of the options list that are different from sum
            ;                                          % if max-min of output column is greater than one then use a "sum" operator
                findall([bool([neg(Negative), op([sum]), nb_terms(Range,Range), conds(Conds), use_mods(UseMods)])],
                        (member([bool([neg(Negative), op(L), nb_terms(Low,Up), conds(Conds), use_mods(UseMods)])], OptionsBool),
                         memberchk(sum, L),
                         Low =< Range,
                         Up  >= Range
                        ),
                        OptionsFormula)                % get the sum Boolean operator if it occurs in the option list
            )
        ;
         CondBool = cond_ex ->                         % if generate options for conditional formulae
            OptionsBoolCond = OptionsBool-OptionsCond, % get set of potential Boolean and Conditional formulae that was generated once and for all
            option_cond_min_cst(KindCombi, Min),
            option_cond_max_cst(KindCombi, Max),
            findall(OptionCond,
                    (member(OptionCond,OptionsCond),
                     OptionCond = [_,then(Then),else(Else)],
                     member([Then, Else],
                            ClusterTypes)
                    ),
                    OptionsFormulaCond),
            findall([cond_ex([then(Then), else(Else), bool([neg(Negative), op(Op), nb_terms(Low,Up), conds(Conds), use_mods(UseMods)])])],
                    (member([bool([neg(Negative), op(Op), nb_terms(Low,Up), conds(Conds), use_mods(UseMods)])], OptionsBool),
                     Negative = 0,             % Extremely restrictive settings - 2-3 terms, no negation, no mods, only AND and OR
                     Op = [Op1],
                     memberchk(Op1,[and,or]),
                     Low >= 2,
                     UseMods = 0,
                     (LimitBool = 1 ->  Up = 2 ; true),
                     member([Then, Else],
                            ClusterTypes)
                    ),
                    OptionsFormulaCondEx)      % get all Boolean operators of the options list that are different from sum
        ;
            OptionsBoolCond = _-OptionsFormulaCond, % get set of potential conditional formulae that were generated once and for all
            findall(OptionCond,
                    (member(OptionCond,OptionsFormulaCond),
                     OptionCond = [_,then(Then),else(Else)],
                     nonmember([Then, Else],
                               ClusterTypes)
                    ),
                    OptionsFormula)
        ),
        (CondBool = cond_ex ->
            (foreach(InputSet,FilteredFd), foreach(InputSet-OptionsFormulaCond,   ListsOptionsCond),   param(OptionsFormulaCond)   do true),
            (foreach(InputSet,FilteredFd), foreach(InputSet-OptionsFormulaCondEx, ListsOptionsCondEx), param(OptionsFormulaCondEx) do true),
            ListsOptions = [ListsOptionsCond, ListsOptionsCondEx]
        ;
            (foreach(InputSet,FilteredFd), foreach(InputSet-OptionsFormula, ListsOptions), param(OptionsFormula) do true)
        )
    ).


gen_options_lists_by_type(decomp, KindCombi, col(Table,OutputCol), Kind, Mode, _OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd,
                  ListsOption) :-
                                                            % record relevant Fds for the layer to be later processed in formula_generator
    KindCombi = model_seeker,
    !,
    (Mode = preprocessed(_, _, _, _Layer) ->
        ListsOptionInputs = [mandatory_attr(InputParams)],
        ListsOption = [diff_splits]-ListsOptionInputs
        %ListsOption = []-ListsOptionInputs % for test purposes, disable otherwise
    ;
     ForceToUseOnlySmallFd = 1 ->                           % if force to use an fd which is included in the original input parameters
        ListsOptionInputs = [mandatory_attr(InputParams)],         % (see predicate search_secondaries1_small_fd in main.pl for that)
        ListsOption = [diff_splits, cond_splits]-ListsOptionInputs
        %ListsOption = [diff_splits, unary_splits]-ListsOptionInputs % for test purposes, disable otherwise
    ;                                                       % then force to use it (in this case InputParams corresponds to the small fd)
        gen_filtered_fd(KindCombi, col(Table,OutputCol), Kind, PrevInputParams, InputParams, ListsOptionInputs),
        ListsOption = [diff_splits, cond_splits]-ListsOptionInputs
        %ListsOption = [diff_splits, unary_splits]-ListsOptionInputs % for test purposes, disable otherwise
    ).
gen_options_lists_by_type(decomp, KindCombi, col(Table,OutputCol), Kind, Mode, _OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd,
                  ListsOption) :-
                                                            % record relevant Fds for the layer to be later processed in formula_generator
    !,
    (Mode = preprocessed(_, _, _, _Layer) ->
        ListsOptionInputs = [mandatory_attr(InputParams)],
        ListsOption = [diff_splits, unary_splits]-ListsOptionInputs
        %ListsOption = []-ListsOptionInputs % for test purposes, disable otherwise
    ;
     ForceToUseOnlySmallFd = 1 ->                           % if force to use an fd which is included in the original input parameters
        ListsOptionInputs = [mandatory_attr(InputParams)],         % (see predicate search_secondaries1_small_fd in main.pl for that)
        ListsOption = [diff_splits, unary_splits, cond_splits]-ListsOptionInputs
        %ListsOption = [diff_splits, unary_splits]-ListsOptionInputs % for test purposes, disable otherwise
    ;                                                       % then force to use it (in this case InputParams corresponds to the small fd)
        gen_filtered_fd(KindCombi, col(Table,OutputCol), Kind, PrevInputParams, InputParams, ListsOptionInputs),
        ListsOption = [diff_splits, unary_splits, cond_splits]-ListsOptionInputs
        %ListsOption = [diff_splits, unary_splits]-ListsOptionInputs % for test purposes, disable otherwise
    ).

gen_options_lists_by_type(cluster, KindCombi, col(Table,OutputCol), _Kind, Mode, OptionsBoolCond, PrevInputParams, InputParams, _ForceToUseOnlySmallFd,
                  ClusterOption) :-
                                                            % record the different prefix trees (each level of a tree gives the functional
                                                            % dependency used for determining a level of the case statement) and the
                                                            % types of Boolean formulae (no sum) that will be tried out at the different
    !,                                                      % levels of a tree
    tab_get_vals_fd(col(Table,OutputCol), ValsFd),          % get list of flat functional dependencies of the different possible cases
                                                            % filter flat functional dependencies so that:
                                                            %  . all columns used by an fd are not included in PrevInputParams
                                                            %  . all columns used by an fd are included in InputParams
    filter_vals_fd(ValsFd, PrevInputParams, InputParams, FilteredValsFd),
                                                            % from the filtered flat functional dependencies build a set of trees that
                                                            % merge the shared functional dependencies of the different prefixes: done
                                                            % to decrease the number of subproblems for which we try to find a Boolean formula
    build_tree_wrt_common_prefix(FilteredValsFd, PrefixTreeValsFd),
    (Mode = main(ParamsOutput) ->  % the formula search must be called from the main level
         true
    ;
         write(gen_options_lists_by_type(cluster)),nl,halt
    ),
    get_table_entries(InputParams, col(Table,OutputCol), ParamsOutput, TableEntries),
    gen_value_sets_wrt_boolean_operators_for_prefix_tree(PrefixTreeValsFd, KindCombi, TableEntries, Mode, col(Table,OutputCol)),
    OptionsBoolCond = OptionsBool-_,                        % get set of potential Boolean formulae that was generated once and for all
    findall([bool([neg(Negative), op(LFiltered), nb_terms(Low,Up), conds(Conds), use_mods(UseMods)])],
            (member([bool([neg(Negative), op(L), nb_terms(Low,Up), conds(Conds), use_mods(UseMods)])], OptionsBool),
             delete(L, sum, LFiltered),                     % can only use "and", "or", "allequal", "xor", "voting", "card1" (but not "sum")
             LFiltered = [_|_]                              % should have at least one operator in the filtered list
            ),
            OptionsFormula),                                % get all Boolean operators of the options list that are different from sum
    ClusterOption = [[cluster([tree(PrefixTreeValsFd), options(OptionsFormula)])]].

gen_options_lists_by_type(formula(MinMonomes,MaxMonomes), KindCombi, col(Table,OutputCol), Kind, Mode, _OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, ListsOptions) :-
    length(InputParams, NInputParams),

    % BUILD THE DIFFERENT OPTIONS REGARDING THE TYPE OF CANDIDATE FORMULA
    (KindCombi \== model_seeker -> % options used for finding the conjecture of a combinatorial object
        tab_get_bound_type(KindCombi, Table, BoundType),     % extract bound type
        ((Kind = secondary, BoundType = low) ->
            BinaryFunctions1 = binary_function([min,max,ceil,floor,prod,mod,cmod,dmod]),  % use ceil rather than floor to get simpler formulae
            BinaryFunctions2 = binary_function([ceil,floor,mod,cmod,dmod])
        ;
         (Kind = secondary, BoundType = up) ->
            BinaryFunctions1 = binary_function([min,max,floor,ceil,prod,mod,cmod,dmod]),  % use floor rather than ceil to get simpler formulae
            BinaryFunctions2 = binary_function([floor,ceil,mod,cmod,dmod])
        ;
         Kind = low ->
            BinaryFunctions1 = binary_function([min,max,ceil,prod,mod,cmod,dmod]),  % use ceil rather than floor to get simpler formulae
            BinaryFunctions2 = binary_function([ceil,mod,cmod,dmod])
        ;
         Kind = up ->
            BinaryFunctions1 = binary_function([min,max,floor,prod,mod,cmod,dmod]), % use floor rather than ceil to get simpler formulae
            BinaryFunctions2 = binary_function([floor,mod,cmod,dmod])
        ;
            write(gen_options_lists_by_type(formula(MinMonomes,MaxMonomes), Kind, BoundType)), nl, halt
        ),
        option_poly_min_cst(KindCombi,  MinCst),  % get minimum value of a constant on a formula using polynomials
        option_poly_max_cst(KindCombi,  MaxCst),  % get maximum value of a constant on a formula using polynomials
        option_poly_min_coef(KindCombi, MinCoef), % get minimum value of a coefficient on a formula using polynomials
        option_poly_max_coef(KindCombi, MaxCoef), % get maximum value of a coefficient on a formula using polynomials

        % we institute tight control of which option for which MaxMonomes is assigned to. The reasons are:
        % - we want to check simpler formula first (for performance and interpretablity reasons)
        % - during the decomposition we don't have an opportunity to backtrack, so if we can acquire a simpler polynomial formula
        %   before a more complex conditional formula, then we must do so.
        %   e.g. we want to ensure that formula "(v fdiv minc)" placed in the ListOptionsPoly2, otherwise
        %   the conditional formula "(v mod minc=0 ? v cdiv minc : (minc+v-3) fdiv minc)" will be called first.
        OptionQuadratic               = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                         main_formula([1-t([degree(2,2),non_zero(1,2,MaxMonomes,MaxMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
        OptionQuadraticUnaryF2        = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                         unary_function([min(-2,2),max(-2,2),floor(2,2),mod(2,2),geq0,in,power(2),sum_consec]),
                                         main_formula([2-t([degree(2,2),non_zero(1,2,MaxMonomes,MaxMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
        (MaxMonomes =< 4 ->
            OptionLinear              = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                         main_formula([1-t([degree(1,1),non_zero(1,1,MaxMonomes,MaxMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])]
        ;   % if the number of monomes is greater than 4 then skip this optioh
            OptionLinear              = none
        ),
        MaxMonomesM1 is max(MaxMonomes - 1, 0),
        ((MaxMonomes >= 2, MaxMonomes =< 4) ->
            OptionLinearBinary        = [nb_polynom([1]),nb_unary([0]),nb_binary([1]),
                                         binary([min,max,floor,mfloor,prod,ceil,mod,fmod,cmod,dmod]),
                                         main_formula([1-t([degree(1,1),non_zero(1,1,MaxMonomesM1,MaxMonomesM1),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])]
        ;   % if the number of monomes is greater than 4 then skip this optioh
            OptionLinearBinary        = none
        ),
        
        MaxMonomesM2 is max(MaxMonomes - 2, 0),
        (MaxMonomesM2 > 0 ->
             OptionQuadraticUnary      = [nb_polynom([1]),nb_unary([1]),nb_binary([0]),
                                          unary([min(-2,2),max(-2,2),floor(2,2),ceil(2,2),mod(2,2),in,geq(0,2),power(2),sum_consec]),
                                          main_formula([1-t([degree(2,2),non_zero(1,2,MaxMonomesM2,MaxMonomesM2),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionQuadraticBinary     = [nb_polynom([1]),nb_unary([0]),nb_binary([1]),
                                          binary([min,max,floor,mfloor,prod,ceil,mod,fmod,cmod,dmod]),
                                          main_formula([1-t([degree(2,2),non_zero(1,2,MaxMonomesM2,MaxMonomesM2),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])]
        ; % if the number of monomes is less or equal to 0 then skip these options
             OptionQuadraticUnary      = none,
             OptionQuadraticBinary     = none
        ),
        
        MaxMonomesM3 is max(MaxMonomes - 3, 0),
        MaxMonomesM2Binary is MaxMonomesM2 * 3,
        MinMonomesM2Binary is max(MaxMonomesM2Binary - 2, 0),
        (MaxMonomesM3 > 0 ->
             OptionLinearUnaryF        = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                          unary_function([min(-2,2),max(-2,2),floor(2,2),mod(2,2),geq0,in,power(2),sum_consec]),
                                          main_formula([2-t([degree(1,1),non_zero(1,1,MaxMonomesM3,MaxMonomesM3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionLinearUnary         = [nb_polynom([1]),nb_unary([1]),nb_binary([0]),
                                          unary([min(-2,2),max(-2,2),floor(2,2),ceil(2,2),mod(2,2),in,geq(0,2),power(2),sum_consec]),
                                          main_formula([1-t([degree(1,1),non_zero(1,1,MaxMonomesM3,MaxMonomesM3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionQuadraticUnaryF1    = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                          unary_function([min(-2,2),max(-2,2),floor(2,2),mod(2,2),geq0,in,power(2),sum_consec]),
                                          main_formula([2-t([degree(2,2),non_zero(1,2,MaxMonomesM3,MaxMonomesM3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionLinearUnaryFUnary   = [nb_polynom([1]),nb_unary([1]),nb_binary([0]),
                                          unary([min(-2,2),max(-2,2),floor(2,2),ceil(2,2),mod(2,2),in,geq(0,2),power(2),sum_consec]),
                                          unary_function([min(-2,2),max(-2,2),floor(2,2),mod(2,2),geq0,in,power(2),sum_consec]),
                                          main_formula([2-t([degree(1,1),non_zero(1,1,MaxMonomesM3,MaxMonomesM3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionLinearBinaryF       = [nb_polynom([2]),nb_unary([0]),nb_binary([0]), non_zero_coeffs_in_all_polynomials(MinMonomesM2Binary,MaxMonomesM2Binary),
                                          BinaryFunctions1,
                                          main_formula([3-t([degree(1,1),non_zero(1,1,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)],
                                                            [degree(1,1),non_zero(1,1,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionLinearBinaryFUnary  = [nb_polynom([2]),nb_unary([1]),nb_binary([0]), non_zero_coeffs_in_all_polynomials(MinMonomesM2Binary,MaxMonomesM2Binary),
                                          unary([min(-2,2),max(-2,2),floor(2,2),ceil(2,2),mod(2,2),in,geq(0,2),power(2),sum_consec]),
                                          BinaryFunctions1,
                                          main_formula([3-t([degree(1,1),non_zero(1,1,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)],
                                                            [degree(1,1),non_zero(1,1,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionQuadraticBinaryF1   = [nb_polynom([2]),nb_unary([0]),nb_binary([0]), non_zero_coeffs_in_all_polynomials(MinMonomesM2Binary,MaxMonomesM2Binary),
                                          BinaryFunctions1,
                                          main_formula([3-t([degree(1,2),non_zero(1,2,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)],
                                                            [degree(1,1),non_zero(1,1,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
             OptionQuadraticBinaryF2   = [nb_polynom([2]),nb_unary([0]),nb_binary([0]), non_zero_coeffs_in_all_polynomials(MinMonomesM2Binary,MaxMonomesM2Binary),
                                          BinaryFunctions2,
                                          main_formula([3-t([degree(1,1),non_zero(1,1,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)],
                                                            [degree(1,2),non_zero(1,2,0,4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])]
        ; % if the number of monomes is less or equal to 0 then skip these options
             OptionLinearUnaryF        = none,
             OptionLinearUnary         = none,
             OptionQuadraticUnaryF1    = none,
             OptionLinearUnaryFUnary   = none,
             OptionLinearBinaryF       = none,
             OptionLinearBinaryFUnary  = none,
             OptionQuadraticBinaryF1   = none,
             OptionQuadraticBinaryF2   = none
        ),

        MaxMonomesM4 is max(MaxMonomes - 4, 0),
        (MaxMonomesM4 > 0 ->
             OptionCubicUnaryF         = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                         unary_function([min(-2,2),max(-2,2),floor(2,2),mod(2,2),geq0,in,power(2),sum_consec]),
                                         main_formula([2-t([degree(3,3),non_zero(1,3,MaxMonomesM4,MaxMonomesM4),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])]
        ; % if the number of monomes is less or equal to 0 then skip this option
             OptionCubicUnaryF         = none
        ),

        OptionFormulaVeryLight1 = [OptionLinear,            OptionQuadratic],
        OptionFormulaLight1     = [OptionLinearUnaryF,      OptionLinearUnary],
        OptionFormulaStandard1  = [OptionQuadraticUnaryF1,  OptionQuadraticUnaryF2,
                                  OptionQuadraticUnary,    OptionCubicUnaryF],
        OptionFormulaHeavy1     = [OptionLinearUnaryFUnary,
                                  OptionLinearBinary,      OptionQuadraticBinary,    % binary terms are put near the end as more costly
                                  OptionLinearBinaryF,     OptionLinearBinaryFUnary, % binary functions are put at the end as more costly
                                  OptionQuadraticBinaryF1, OptionQuadraticBinaryF2],
        % remove option "none" from all lists
        remove_none_poly_option(OptionFormulaVeryLight1, OptionFormulaVeryLight ),
        remove_none_poly_option(OptionFormulaLight1,     OptionFormulaLight     ),
        remove_none_poly_option(OptionFormulaStandard1,  OptionFormulaStandard  ),
        remove_none_poly_option(OptionFormulaHeavy1,     OptionFormulaHeavy     )
    ;                                    % options used when not on a combinatorial object, i.e on the model seeker
       option_poly_min_cst(KindCombi,  MinCst),   % get minimum value of a constant on a formula using polynomials
       option_poly_max_cst(KindCombi,  MaxCst),   % get maximum value of a constant on a formula using polynomials
       option_poly_min_coef(KindCombi, MinCoef),  % get minimum value of a coefficient on a formula using polynomials
       option_poly_max_coef(KindCombi, MaxCoef),  % get maximum value of a coefficient on a formula using polynomials
       (for(I,MinMonomes,MaxMonomes),
        foreach(OptionFormulaA,OptionFormulaAAll), % valid for all output columns
        foreach(OptionFormulaQ,OptionFormulaQAll), % valid for output columns with limited range (to avoid overflow)
        param([MinCst, MaxCst, MinCoef, MaxCoef])
       do
        build_poly_options(MinCst, MaxCst, MinCoef, MaxCoef, I, I, OptionFormulaQ, OptionFormulaA)
       ),
       append(OptionFormulaAAll,OptionFormulaA1),
       append(OptionFormulaQAll,OptionFormulaQ1)
    ),
    
    % BUILD THE DIFFERENT OPTIONS REGARDING THE WAY ATTRIBUTES OCCUR WITHIN THE CANDIDATE FORMULA
    (KindCombi \== model_seeker ->                           % if we are on combinatorial objects
        tab_get_primaries(Table, Primaries),
        length(Primaries, NParam),
        NParam_1 is NParam-1,
        (Mode = preprocessed(_, _, _, _) ->
            OptionAttributesMain = [mandatory_attr(InputParams)]
        ;
         (Kind = secondary, NParam_1 > 0)  ->
             (ForceToUseOnlySmallFd = 1 ->                   % if force to use an fd which is included in the original input parameters
                FilteredFd = [mandatory_attr(InputParams)]   % (see predicate search_secondaries1_small_fd in main.pl for that)
             ;                                               % then force to use it (in this case InputParams corresponds to the small fd)
                gen_filtered_fd(KindCombi, col(Table,OutputCol), Kind, PrevInputParams, InputParams, FilteredFd)
             ),
             listify(FilteredFd, OptionAttributesMain)
        ;
         Kind = secondary ->
            OptionAttributesMain = [[mandatory_attr(Primaries), min_attr(NParam), max_attr(NParam)]]
        ;
            tab_get_primaries(Table, Primaries),
            length(Primaries, LenA),
            OPrimaries = [mandatory_attr(Primaries), min_attr(LenA), max_attr(LenA)],
            filter_functional_dependencies([OPrimaries], PrevInputParams, NInputParams, InputParams, OptionPrimaries),
            tab_get_small_fd(Table, SmallFd),
            findall(Option, (member(Fd, SmallFd), length(Fd, LenFd),
                             Option = [mandatory_attr(Fd), min_attr(LenFd), max_attr(LenFd)]), OptionAttributesSmallFd),
            delete(OptionAttributesSmallFd, OPrimaries, OptionAttributesM),
            filter_functional_dependencies(OptionAttributesM, PrevInputParams, NInputParams, InputParams, OptionAttributesMain),
            tab_get_secondaries(Table, Secondaries),
            NParam1 is NParam+1,
            findall(Option, (member(Char, Secondaries), append(Primaries,  [Char], Chars),
                             Option = [mandatory_attr(Chars), min_attr(NParam1), max_attr(NParam1)]), OptionsSecond),
            (SmallFd = [SmallestFd|_] ->
                length(SmallestFd, SmallestFdLen),
                (SmallestFdLen = NParam_1 ->
%                   write('COMPLETE SMALLEST FUNCTIONAL DEPENDENCY WITH A PRIMARY CHARACTERISTICS'), nl,
                    findall(Option, (member(CharP, Primaries),
                                     \+ memberchk(CharP,SmallestFd),
                                     append(SmallestFd, [CharP], Chars),
                                     Option = [mandatory_attr(Chars), min_attr(NParam), max_attr(NParam)]), OptionsThird)
                ;
                    OptionsThird = []
                )
            ;
                OptionsThird = []
            ),
            append([OptionsSecond, OptionsThird], OptionAttributesST),
%           write(option_attributes_st(OptionAttributesST)), nl,
            filter_functional_dependencies(OptionAttributesST, PrevInputParams, NInputParams, InputParams, OptionAttributesSecond)
%           write(option_attributes_second(OptionAttributesSecond)), nl
        )
    ;                                          % if we are in model seeker then get the attached ranked fd of column OutputCol
        (Mode = preprocessed(_, _, _, _) ->
            RankedFds = [[1,-1,1]-InputParams]
        ;
            tab_get_ranked_fd(col(Table, OutputCol), RankedFds)
        ),
        (RankedFds = [] ->                     % abort if no functional dependency for column OutputCol
            % write(gen_options_lists(empty_ranked_fd)), nl, halt
            InputsSubsetsA = [],
            InputsSubsetsQ = []
        ;

            % select ranked fd that will be used in non-quadratic formulae
            %.............................................................
                                               % filter out fd wrt significance of correlation coefficient
                                               % (note that we record minus abs. value of corr.coef. because of sorting in ascending order)
            findall(Attrs, (member([_,Corr,_]-Attrs, RankedFds), Corr =< -0.5), FilteredRankedFds),
            length(FilteredRankedFds, LenFilteredRankedFds),
            NTop is 10,
                                               % keep up to NTop best ranked functional dependencies
            TopLimit is min(LenFilteredRankedFds, NTop),
            prefix_length(FilteredRankedFds, PrefixFilteredRankedFds, TopLimit),
                                               % create the list of options wrt to the best ranked fd
            findall([mandatory_attr(Attrs), min_attr(NAttrs), max_attr(NAttrs)],
                    (member(Attrs, PrefixFilteredRankedFds),
                     length(Attrs, NAttrs)),
                    InputsSubsetsA),
            % write(a(InputsSubsetsA)), nl, nl,

            % select ranked fd that will be used in all formulae
            %...................................................
            tab_get_inputs(Table, Inputs),     % get the list of all inputs
            findall(col(Table,Col),            % select input columns that can be used in quadratic formulae (i.e. avoid potential overflow)
                    (member(col(Table,Col), Inputs),
                     tab_get_min_max(col(Table,Col), Min, Max),
                     abs(Min) =< 100000000, abs(Max) =< 100000000
                    ),
                    InputsQuadratic),
            findall(Input,                     % go through all fd and keep only those fd for which all input column are in InputsQuadratic
                    (member(Input, FilteredRankedFds),(foreach(X,Input) do member(X, InputsQuadratic))),
                    InputsSubsetsQ1),          % keep up to 10 best ranked functional dependencies
            length(InputsSubsetsQ1, LenInputsSubsetsQ1),
            TopLimitQ is min(LenInputsSubsetsQ1, NTop),
            prefix_length(InputsSubsetsQ1, InputsSubsetsQ2, TopLimitQ),
                                               % create the list of options wrt to the best ranked fd
            findall([mandatory_attr(Attrs), min_attr(NAttrs), max_attr(NAttrs)],
                    (member(Attrs, InputsSubsetsQ2),
                     length(Attrs, NAttrs)),
                    InputsSubsetsQ)
        )
        % , write(q(InputsSubsetsQ)), nl, nl
    ),
    % BUILD ALL COMBINATIONS OF OPTIONS RESULTING FROM THE TYPE OF FORMULA AND THE WAY ATTRIBUTES OCCUR IN THE FORMULA
    (KindCombi \== model_seeker ->             % if combinatorial object construct formulae list by increasing formula complexity
        append([OptionFormulaVeryLight,OptionFormulaLight,OptionFormulaStandard,OptionFormulaHeavy], OptionFormulaAll),
        (Kind = secondary ->
            % compute secondary char. by increasing formulae complexity
            (foreach(InputSet,OptionAttributesMain), foreach(InputSet-OptionFormulaAll, ListsOptions), param(OptionFormulaAll) do true)
        ;
            % first try to directly compute bound from primary parameters using all formulae (i.e., first do not use at all secondary characteristics)
            (foreach(InputSet,OptionPrimaries), foreach(InputSet-OptionFormulaAll, ListsOptionsPrimaries), param(OptionFormulaAll) do true),
            % first try to directly compute bound from small fd using all formulae
            (foreach(InputSet,OptionAttributesMain), foreach(InputSet-OptionFormulaAll, ListsOptionsMain), param(OptionFormulaAll) do true),
            % then try to compute bound using also secondary char. in the increasing order of complexity
            (foreach(InputSet,OptionAttributesSecond), foreach(InputSet-OptionFormulaAll, ListsOptionsSecondAll), param(OptionFormulaAll) do true),
            append([ListsOptionsPrimaries,
                    ListsOptionsMain,
                    ListsOptionsSecondAll], ListsOptions)
        )
    ;
        (foreach(InputSet,InputsSubsetsA), foreach(InputSet-OptionFormulaA1, ListsOptionsA1), param(OptionFormulaA1) do true),
        (foreach(InputSet,InputsSubsetsQ), foreach(InputSet-OptionFormulaQ1, ListsOptionsQ1), param(OptionFormulaQ1) do true),
        append([ListsOptionsA1, ListsOptionsQ1], ListsOptions)
    ).

get_augmented_fd(col(Table,OutputCol), Fd, AugmentedFd) :-
    tab_get_arity(col(Table,_), Arity),
    findall(col(Table,I), (I in 1..Arity,indomain(I),I=\=OutputCol,tab_get_inout(col(Table,I),input)), Cols),
    cartesian_product_with_sort(Fd, Cols, FdCols),
    append(Fd, FdCols, Fd1),
    remove_dups(Fd1, Fd2),
    sort_wrt_list_length(Fd2, SortedFd2), % as want to check smallest functional dependencies first
    findall(mandatory_attr(F), (member(F, SortedFd2), length(F, LenF), LenF =< 6), AugmentedFd). % as a condition uses at most 6 attributes

% used when on combinatorial objects
gen_filtered_fd(KindCombi, col(Table,OutputCol), Kind, PrevInputParams, InputParams, FilteredFd) :-
   KindCombi \== model_seeker,
   !,
   length(InputParams, NInputParams),
   tab_get_fd_eq(col(Table,OutputCol), Fd),
   (Fd = [] ->
       FilteredFd = []
   ;
       get_augmented_fd(col(Table,OutputCol), Fd, AugmentedFd),
       split_list_wrt_smallest_length(AugmentedFd, AugmentedFdMinLen, _),                 % consider only fd with smallest length
       (Kind = secondary ->
           tab_get_primaries(Table, Primaries),                                           % sometimes (rarely) a secondary char. can have
           (memberchk(mandatory_attr(Primaries), AugmentedFdMinLen) ->                    % a fd involving fewer parameters than the number
               AugmentedFdMinLenPrimaries = AugmentedFdMinLen                             % of input parameters: therefore add the primaries
           ;
               add_primaries_just_before_first_non_primary(AugmentedFdMinLen,             % add the primaries just before the first fd for
                                                           Primaries,                     % which the set of parameters is not included
                                                           AugmentedFdMinLenPrimaries)    % within the primaries parameters: this give the
                                                                                          % opportunity to find simpler formulae, i.e.
                                                                                          % formulae which only depend on a subset of the
                                                                                          % primaries parameters; note that a secondary char. 
                                                                                          % may (rarely) be determined by a fraction of the
                                                                                          % primary char.
           ),
           length(InputParams, NInputParams),                                             % keep only fd for which char. are in InputParams
           filter_functional_dependencies(AugmentedFdMinLenPrimaries,                     % as compute formulae by layers in order to avoid
                                          PrevInputParams,                                % loops: e.g. x = f1(y,z) and y = f2(x,z)
                                          NInputParams, InputParams, FilteredFd)          % remove fd for which char.are in PrevInputParams
                                                                                          % in order to avoid searching formula with an
                                                                                          % already used fd
       ;
        filter_functional_dependencies(AugmentedFdMinLen,                                 % if on a bound
                                       PrevInputParams,
                                       NInputParams, InputParams, FilteredFd)
       )
   ).
% used when on the model seeker
gen_filtered_fd(model_seeker, col(Table,OutputCol), _Kind, _PrevInputParams, _InputParams, FilteredFd) :-
   tab_get_ranked_fd(col(Table,OutputCol), RankedFds), % used the ranked functional dependencies (they do not mention the primary keys)
   findall(mandatory_attr(Fd), member(_-Fd, RankedFds), FilteredFd1),
   length(FilteredFd1, LenFilteredFd1),
   TopLimit is min(LenFilteredFd1, 2),                 % consider only the first 2 ranked functional dependencies
   prefix_length(FilteredFd1, FilteredFd, TopLimit).

add_primaries_just_before_first_non_primary([], Primaries, [mandatory_attr(Primaries)]) :- !.
add_primaries_just_before_first_non_primary([Fd|R], Primaries, [Fd|S]) :-
    \+ list_included(Fd, Primaries),
    !,
    add_primaries_just_before_first_non_primary(R, Primaries, S).
add_primaries_just_before_first_non_primary(List, Primaries, [mandatory_attr(Primaries)|List]).

filter_functional_dependencies([], _, _, _, []) :- !.
filter_functional_dependencies([List|R], PrevInputParams, NInputParams, InputParams, Res) :-
    is_list(List),
    !,                                                                 % if we have a list then filter this list
    filter_functional_dependencies(List, PrevInputParams, NInputParams, InputParams, FilteredList),
    (FilteredList = []                                                                             -> Res = S               ;
     (\+ memberchk(mandatory_attr(_), FilteredList), \+ memberchk(optional_attr(_), FilteredList)) -> Res = S               ;
                                                                                                      Res = [FilteredList|S]),
    filter_functional_dependencies(R, PrevInputParams, NInputParams, InputParams, S).
filter_functional_dependencies([table_attr(Tuples)|R], PrevInputParams, NInputParams, InputParams, Res) :-
    !,                                                                 % if we have a table listing tuples of attributes
    filter_tuples(Tuples, PrevInputParams, NInputParams, InputParams, FilteredTuples),  % remove tuples that are not included in input parameters
    (FilteredTuples = [] -> Res = S ; Res = [table_attr(FilteredTuples)|S]),
    filter_functional_dependencies(R, PrevInputParams, NInputParams, InputParams, S).
filter_functional_dependencies([Compound|R], PrevInputParams, NInputParams, InputParams, [Compound|S]) :-
    compound(Compound),
    functor(Compound, Functor, 1),
    memberchk(Functor, [mandatory_attr,optional_attr]),
    arg(1, Compound, ListAttr),
    length(ListAttr, Len),
    Len =< NInputParams,                                               % assumes distint parameters
    included_in_input_params_and_not_included_in_prev_input_params(ListAttr, PrevInputParams, InputParams),
    !,                                                                 % copy mandatory/optional attributes if included in input parameters
                                                                       % and not included in previous input parameters
    filter_functional_dependencies(R, PrevInputParams, NInputParams, InputParams, S).
filter_functional_dependencies([Option|R], PrevInputParams, NInputParams, InputParams, [Option|S]) :-
    \+ memberchk(Option, [mandatory_attr(_),optional_attr(_)]),
    !,                                                                 % copy options like min_attr(Min) or max_attr(Max)
    filter_functional_dependencies(R, PrevInputParams, NInputParams, InputParams, S).
filter_functional_dependencies([_|R], PrevInputParams, NInputParams, InputParams, S) :-
                                                                       % skip current functional dependency as not included in input parameters,
                                                                       % or as included in previous input parameters
    filter_functional_dependencies(R, PrevInputParams, NInputParams, InputParams, S).

filter_tuples([], _, _, _, []) :- !.
filter_tuples([Tuple|R], PrevInputParams, NInputParams, InputParams, [Tuple|S]) :-
    length(Tuple, Len),
    Len =< NInputParams,                                               % assumes distint parameters
    included_in_input_params_and_not_included_in_prev_input_params(Tuple, PrevInputParams, InputParams),
    !,                                                                 % copy tuple if included in input parameters and not included in
    filter_tuples(R, PrevInputParams, NInputParams, InputParams, S).   % previous input parameters
filter_tuples([_|R], PrevInputParams, NInputParams, InputParams, S) :- % skip current tuple as not included in input parameters, or as
    filter_tuples(R, PrevInputParams, NInputParams, InputParams, S).   % included in previous input parameters

included_in_input_params_and_not_included_in_prev_input_params(List, PrevInputParams, InputParams) :-
    list_included(List, InputParams),
    \+ list_included(List, PrevInputParams).

% Given the list LOp of all Boolean operators that occur in the list of Boolean options
% and an output column OutputCol (where F is the difference between the maximum value
% and the minimum value in this column) for which we will try to acquire a Boolean formula,
% compute different sets of values that will be used to restrict the value of the constant
% in a Boolean condition of the form attr = cst, attr =< cst, attr >= cst.
% The parameter Mode is used is to determine whether or not the pre-calculation is done for decomposed formulae or not
gen_value_sets_wrt_boolean_operators(KindCombi, TableEntries, Mode, InputParams, OutputCol, ClusterOne, ClusterZero, ColSetsBoolAll) :-
    (KindCombi = model_seeker ->
         tab_get_nb_rows(OutputCol, NbRows),
         (NbRows < 1000 ->
             option_cond_max_cst(KindCombi, MaxCst)  % get max. value of a constant in a Boolean condition
         ;
             MaxCst = 2
         ),
         BinaryTermList = [plus,abs,min,max,prod,minus,floor,ceil] %TO RESTORE after metadata is completed
         %BinaryTermList = [] %TO REMOVE
    ;
         option_cond_max_cst(KindCombi, MaxCst), % get max. value of a constant in a Boolean condition
         BinaryTermList = [plus,abs,min,max,prod,minus,floor,ceil,mfloor,mod,cmod,dmod,fmod]
    ),
    (Mode = main(ParamsOutput) ->
         tab_get_min_max(OutputCol, TargetMin, TargetMax),
         remove_last_elem(ParamsOutput, Params, _), % prevent backtrack as last element may be in the list (problem with model seeker)
         Mode1 = precalculated_entries(TableEntries, Params, TargetMin, TargetMax)
    ;
     Mode = preprocessed(TargetMin, TargetMax, _, _) ->
         Mode1 = precalculated_entries(TableEntries, InputParams, TargetMin, TargetMax)
    ;
         write(gen_value_sets_wrt_boolean_operators),nl,halt
    ),
    % get all set of coefficient/constant values attached to the different type of conditions (attr_cmp_coef, unary function, unary term, binary functions)
    % and record them in ColSetsBoolAll so that they can be used in gen_bool_formula to shrink the domain of these variables
    %.......................................................................................................................................
    tab_get_attribute_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, MaxCst, ListOfAttrValues),
    tab_get_unaryf_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, prod, MaxCst, UnaryFSet),
    (KindCombi = model_seeker ->
         UnaryTermSet = [],
         TernarySet1  = [],
         TernarySet2  = [],
         TernarySet3  = [],
         TernarySet4  = [],
         TernarySet5  = []
    ;
         tab_get_unary_term_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, mod,  MaxCst, UnaryTermSet),
         tab_get_ternary_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, sum_leq_attr,TernarySet1),
         tab_get_ternary_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, minus_mod_eq0,TernarySet2),
         tab_get_ternary_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, ceil_leq_floor,TernarySet3),
         tab_get_ternary_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, attr_geq_fmod,TernarySet4),
         tab_get_ternary_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, mod_gt_mod,TernarySet5)
    ),
    findall(BinaryTermSet, (member(BinaryTerm,BinaryTermList),
                            tab_get_binary_term_values(InputParams, OutputCol, Mode1, ClusterOne, ClusterZero, BinaryTerm, MaxCst, BinaryTermSet)),
            BinaryTermSets),
    append([ListOfAttrValues,UnaryFSet,UnaryTermSet,TernarySet1,TernarySet2,TernarySet3,TernarySet4,TernarySet5|BinaryTermSets],ColSetsBoolAll).

gen_value_sets_wrt_boolean_operators_for_prefix_tree([], _, _, _, _) :- !.
gen_value_sets_wrt_boolean_operators_for_prefix_tree([Node|R], KindCombi, TableEntries, Mode, col(Table,OutputCol)) :-
    Node = t(Fd,Val1,BoolSetsCluster,Childs),
    collect_cluster_zero(Childs, ClusterZero),
    %write(node(Table,OutputCol,Fd,[Val1],ClusterZero)),nl,
    (ClusterZero = [] ->
        BoolSetsCluster = []
    ;
        gen_value_sets_wrt_boolean_operators(KindCombi, TableEntries, Mode, Fd, col(Table,OutputCol), [Val1], ClusterZero, BoolSetsCluster)
    ),
    gen_value_sets_wrt_boolean_operators_for_prefix_tree(Childs, KindCombi, TableEntries, Mode, col(Table,OutputCol)),
    gen_value_sets_wrt_boolean_operators_for_prefix_tree(     R, KindCombi, TableEntries, Mode, col(Table,OutputCol)).

% in the context of cluster where we acquire a case formula (each branch has the form "if Boolean formula then col=cluster value")
% filters out the list of flat functional dependencies that can be used in such case formula; this is done by:
%  . removing each branch such that the set of functional dependencies of that branch are not included in InputParams,
%  . removing each branch such that the set of functional dependencies of that branch is included in PrevInputParams.
% this is done for two reasons:
%  . compute a case formula that only uses columns from InputParams (i.e. "already known" columns)
%  . compute a case formula that was not already obtained on a previous layer (in the case of combinatorial objects)
filter_vals_fd([], _, _, []) :- !.
filter_vals_fd([ValFd|R], PrevInputParams, InputParams, [ValFd|FilteredValsFd]) :-
    compound_included_in_list(ValFd, InputParams),
    \+ compound_included_in_list(ValFd, PrevInputParams),
    !,
    filter_vals_fd(R, PrevInputParams, InputParams, FilteredValsFd).
filter_vals_fd([_|R], PrevInputParams, InputParams, FilteredValsFd) :-
    filter_vals_fd(R, PrevInputParams, InputParams, FilteredValsFd).

compound_included_in_list([], _) :-
    !.
compound_included_in_list(Term, _) :-
    integer(Term),
    !.
compound_included_in_list(Term, ListOfCompounds) :-
    compound(Term),
    memberchk(Term, ListOfCompounds),
    !.
compound_included_in_list(Term, ListOfCompounds) :-
    compound(Term),
    functor(Term, _, Arity),
    compound_included_in_list(1, Arity, Term, ListOfCompounds).

compound_included_in_list(I, N, _, _) :-
    I > N, !.
compound_included_in_list(I, N, Term, ListOfCompounds) :-
    I =< N,
    arg(I, Term, ArgI),
    compound_included_in_list(ArgI, ListOfCompounds),
    I1 is I+1,
    compound_included_in_list(I1, N, Term, ListOfCompounds).

remove_none_poly_option(LOptionsUnfiltered, LOptionsFiltered) :-
    findall(Option,
            (member(Option, LOptionsUnfiltered),
             Option \== none),
            LOptionsFiltered).

build_poly_options(MinCst, MaxCst, MinCoef, MaxCoef,
                   MinNumberOfMonomes, MaxNumberOfMonomes,
                   OptionFormulaQ, OptionFormulaAll):-
    MaxNumberOfMonomes1 is MaxNumberOfMonomes - 2,
    MinNumberOfMonomes1 is max(MinNumberOfMonomes - 2, 0),
    MinNumberOfMonomesD is max(MaxNumberOfMonomes - 1, 0),
    OptionLinear               = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                  main_formula([1-t([degree(1,1),non_zero(1,1,MinNumberOfMonomes,MaxNumberOfMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionLinearUnaryF        = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                 unary_function([min(-3,3),max(-3,3),floor(2,3),geq0,in,power(2)]),
                                 main_formula([2-t([degree(1,1),non_zero(1,1,MinNumberOfMonomes,MaxNumberOfMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionLinearBinaryF       = [nb_polynom([2]),nb_unary([0]),nb_binary([0]), non_zero_coeffs_in_all_polynomials(MinNumberOfMonomesD,MaxNumberOfMonomes),
                                 binary_function([min,max,floor,prod]),
                                 main_formula([3-t([degree(1,1),non_zero(1,1,0,3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)],
                                                   [degree(1,1),non_zero(1,1,0,3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    OptionQuadratic            = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                  main_formula([1-t([degree(1,2),non_zero(1,2,MinNumberOfMonomes,MaxNumberOfMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionLinearUnary         = [nb_polynom([1]),nb_unary([1]),nb_binary([0]),
                                 unary([min(-3,3),max(-3,3),floor(2,3),ceil(2,3),in,geq(0,3),power(2)]),
                                 main_formula([1-t([degree(1,1),non_zero(1,1,MinNumberOfMonomes,MaxNumberOfMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionLinearBinary        = [nb_polynom([1]),nb_unary([0]),nb_binary([1]),
                                 binary([min,max,floor,prod,ceil]),
                                 main_formula([1-t([degree(1,1),non_zero(1,1,MinNumberOfMonomes,MaxNumberOfMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionLinearUnaryFUnary   = [nb_polynom([1]),nb_unary([1]),nb_binary([0]),
                                 unary([min(-3,3),max(-3,3),floor(2,3),ceil(2,3),in,geq(0,3),power(2)]),
                                 unary_function([min(-3,3),max(-3,3),floor(2,3),geq0,in,power(2)]),
                                 main_formula([2-t([degree(1,1),non_zero(1,1,MinNumberOfMonomes,MaxNumberOfMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionLinearBinaryFUnary  = [nb_polynom([2]),nb_unary([1]),nb_binary([0]), non_zero_coeffs_in_all_polynomials(MinNumberOfMonomesD,MaxNumberOfMonomes),
                                 unary([min(-3,3),max(-3,3),floor(2,3),ceil(2,3),in,geq(0,3),power(2)]),
                                 binary_function([min,max,floor,prod]),
                                 main_formula([3-t([degree(1,1),non_zero(1,1,0,3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)],
                                                   [degree(1,1),non_zero(1,1,0,3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionQuadraticUnaryF     = [nb_polynom([1]),nb_unary([0]),nb_binary([0]),
                                 unary_function([min(-3,3),max(-3,3),floor(2,3),geq0,in,power(2)]),
                                 main_formula([2-t([degree(2,2),non_zero(1,2,MinNumberOfMonomes,MaxNumberOfMonomes),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionQuadraticBinaryF    = [nb_polynom([2]),nb_unary([0]),nb_binary([0]),
                                 binary_function([min,max,floor,prod]), non_zero_coeffs_in_all_polynomials(MinNumberOfMonomesD,MaxNumberOfMonomes),
                                 main_formula([3-t([degree(1,2),non_zero(1,2,0,3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)],
                                                   [degree(1,2),non_zero(1,2,0,3),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionQuadraticUnary      = [nb_polynom([1]),nb_unary([1]),nb_binary([0]),
                                 unary([min(-3,3),max(-3,3),floor(2,3),ceil(2,3),in,geq(0,3),power(2)]),
                                 main_formula([1-t([degree(2,2),non_zero(1,2,MinNumberOfMonomes1,MaxNumberOfMonomes1),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    _OptionQuadraticBinary     = [nb_polynom([1]),nb_unary([0]),nb_binary([1]),
                                 binary([min,max,floor,prod,ceil]),
                                 main_formula([1-t([degree(2,2),non_zero(1,2,MinNumberOfMonomes1,MaxNumberOfMonomes1),coef(MinCoef,MaxCoef),cst(MinCst,MaxCst)])])],
    OptionFormulaQ           = [OptionQuadratic],
    OptionFormulaAll         = [OptionLinear].
