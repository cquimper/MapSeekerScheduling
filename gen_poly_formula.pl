% Purpose: GENERATE A PARAMETERISED CANDIDATE POLYNOMIAL FORMULA WRT SOME GIVEN OPTIONS,
%          AND ON BACTRACKING PRODUCE THE NEXT CANDIDATE FORMULA
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_poly_formula,[gen_possible_combinations_of_attr/3,
                            extend_functor_unary/7,
                            extend_functor_binaryt/3,
                            get_break_sym_in_unaryt_binaryt/7,
                            get_break_sym_in_unaryf/3,
                            get_break_sym_in_binaryf/3,
                            formula_gen_attr_ctrs/10,
                            formula_gen_polynoms/10,
                            restrict_attr_wrt_binary_functions/4,
                            gen_source_target_templates/4,
                            formula_pattern_gen_additional_ctr/2]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(gen_cond_formula).
:- use_module(utility).
:- use_module(table_access).
:- use_module(instrument_code).

% generates the list of possible combinations of attributes that can occur in the formula,
% where each combination is a list of LenA 0/1 value (a 1 at position i indicates that the i-th attribute should occur in the formula)
%  [Tuple|R]: list of tuples where each tuple gives a set of table columns that should occur
%  AttrNames: list of attributes names: mandatory + optional attributes
%  [Entry|S]: produced list of feasible attributes
gen_possible_combinations_of_attr([], _, []) :- !.
gen_possible_combinations_of_attr([Tuple|R], AttrNames, [Entry|S]) :-
    gen_possible_combination_of_attr(AttrNames, Tuple, Entry),
    gen_possible_combinations_of_attr(R, AttrNames, S).

gen_possible_combination_of_attr([], _, []) :- !.
gen_possible_combination_of_attr([Attr|R], Tuple, [B|S]) :-
    (memberchk(Attr, Tuple) -> B = 1 ; B = 0),
    gen_possible_combination_of_attr(R, Tuple, S).

% if needed add to the function of a unary term/function a constant or a set of values, return the list of created not yet fixed constants
extend_functor_unary([], [], _, _, _, [], []) :- !.
extend_functor_unary([F|Q], [G|R], InUnary, ListFunction1, ListFunction2, Csts, LowUps) :-
    functor(F, U, A),
    ((memberchk(U, ListFunction1), A = 2) ->
        arg(1, F, MinCst),                                  % get minimum value of the constant in the unary term/function
        arg(2, F, MaxCst),                                  % get maximum value of the constant in the unary term/function
        MinCst =< MaxCst,                                   % check that not an empty inverval of values
        functor(G, U, 1),                                   % create a term with the used unary term/function
        arg(1, G, Cst),                                     % and the constant used in this unary term/function
        (InUnary = 1 -> Cst in MinCst..MaxCst ;             % as the constant is not yet known declare a variable with its potential range
         U = floor   -> Cst in 2..MaxCst      ;             % if binary function and floor then restrict denominator
                        Cst in MinCst..MaxCst ),
        Csts = [Cst|S],                                     % create a term with the in function and a set of values
        LowUps = T
    ;                                                       % set variables are not passed to the labeling (use only propagation to restrict their domains)
                                                            % WARNING: if change 0..2, 0..3 then change also constants in get_break_sym_in_unaryf
     F = in                           -> functor(G, in, 2), arg(1, G, Low), arg(2, G, Up),
                                         Low in 0..2, Up in 0..3, Low #=< Up, Csts = S, LowUps = [Low,Up|T] ;
    (F = power(D), integer(D), D > 1) -> G = F, Csts = S, LowUps = T                                        ;
    memberchk(F, ListFunction2)       -> G = F, Csts = S, LowUps = T                                        ;
                                         write(extend_functor_unary), nl, halt                              ),
    extend_functor_unary(Q, R, InUnary, ListFunction1, ListFunction2, S, T).

% add to the function of a binary term the order variable between the two attributes, return the list of created not yet fixed ordering variables
extend_functor_binaryt([], [], []) :- !.
extend_functor_binaryt([F|R], [G|S], Orders) :-
    (memberchk(F, [min,max,prod]) ->
        functor(G, F, 1),                                   % create a term with the used binary function
        arg(1, G, 0),                                       % and create the order variable between the two attributes: fix it to 0 as function is commutative
        Orders = T                                          % no new order variable for commutative binary functions
    ;
    memberchk(F, [floor,mfloor,ceil,mod,cmod,dmod,fmod]) ->
        functor(G, F, 1),                                   % create a term with the used binary function
        arg(1, G, Order),                                   % and create the order variable between the two attributes
        Order in 0..1,
        Orders = [Order|T]
    ;
        write(extend_functor_binaryt), nl, halt
    ),
    extend_functor_binaryt(R, S, T).

% if formula uses one single unary term corresponding to "power" or a binary term corresponding to "prod"
get_break_sym_in_unaryt_binaryt(1, IndexUnaryT, [power(_)], _, _, _, power_prod(IndexUnaryT)) :- !.
get_break_sym_in_unaryt_binaryt(_, _, _, 1, IndexBinaryT, [prod], power_prod(IndexBinaryT)) :- !.
get_break_sym_in_unaryt_binaryt(_, _, _, _, _, _, none).

get_break_sym_in_unaryf(2, [UnaryFunction], BreakSymInUnaryF) :-    % if the formula uses a unary function (i.e. F=2)
    !,                                                              % if the formula uses "in(polynom,low..up)"
    (functor(UnaryFunction, in, 2) ->                               % then create a symmetry breaking constraint that will be posted when creating
        arg(1, UnaryFunction, Low),                                 % the constant coefficient of the polynom to avoid symmetrical formulae like:
        arg(2, UnaryFunction, Up),                                  %  [-y + x - 1 in 2..3], [-y + x in 3..4] (where the cst -1 can be pushed
                                                                    %                                          in low..up)
        BreakSymInUnaryF = in(Low, 0, 2, Up, 0, 3)                  % WARNING: modify constants 0, 2, 0, 3 in extend_functor_unary if
                                                                    %          change these values
    ;
     functor(UnaryFunction, mod, 1) ->                              % if the formula uses "mod Cst"
        arg(1, UnaryFunction, Cst),                                 % then create a symmetry breaking constraint to avoid symmetrical formulae like:
        BreakSymInUnaryF = mod(Cst)                                 % (y - 2.x) mod 3, (y + x) mod 3 (each monome coef. is >= 0 and < Cst)
   ;
     functor(UnaryFunction, power, 1) ->                            % if the formula uses "power(polynom,degree)"
        BreakSymInUnaryF = power                                    % then create a symmetry breaking constraint that will be posted after creating
    ;                                                               % all the coefficients of the polynom to avoid symmetrical solutions like
        BreakSymInUnaryF = none                                     % (-y - x)^2, (y + x)^2 (enforce the leftmost non zero coefficient to be positive)
    ).
get_break_sym_in_unaryf(_, _, none).                                % return none as the formula we generate does not use a unary function

get_break_sym_in_binaryf(3, [prod], prod) :- !. % if formula is a binary function using "prod"
get_break_sym_in_binaryf(_, _, none).           % return none as the formula we generate is not a binary function using prod

% FOR A CANDIDATE FORMULA GENERATES THE CONSTRAINTS ON THE ATTRIBUTES IT WILL/SHOULD MENTION
%-------------------------------------------------------------------------------------------
%  . FormulaPattern  : formula pattern for which we generate the constraints on the attributes
%  . MinA            : minimum number of attributes which should be mentionned in the formula
%  . MaxA            : maximum number of attributes which should be mentionned in the formula
%  . LenA            : total number of attributes
%  . AttrNames       : mandatory and optional attributes
%  . LenMandatoryAttr: number of mandatory attributes
%  . ListsP          : indicate for each polynom which parameter (attribute and unary/binary term) is used or not (ListsP is a computed result)
%  . ListA           : list of 0/1 values indicating which variables are used in a polynom/unary term/binary term
%  . AllAttrVars     : output all variables attached to the way we select attributes
%  . TuplesAttr      : empty if did not use the table_attr option, otherwise provide the possible combinaisons of attributes that should be used
formula_gen_attr_ctrs(FormulaPattern, MinA, MaxA, LenA, AttrNames, LenMandatoryAttr, ListsP, ListA, AllAttrVars, TuplesAttr) :-
    formula_pattern(np(NP),         FormulaPattern),      % get the number of polynoms
    formula_pattern(nu(NU),         FormulaPattern),      % get the number of unary terms
    formula_pattern(nb(NB),         FormulaPattern),      % get the number of binary terms
    formula_pattern(unary(Unary),   FormulaPattern),      % get unary terms
    formula_pattern(binary(Binary), FormulaPattern),      % get binary terms
    NUB is NU+NB,                                         % number of terms (unary and binary)
    Len is LenA+NUB,                                      % maximum number of parameters of a polynom
    gen_lists_dvars(NU, LenA, 0, 1, ListsU),              % U_i,j = 1 iff ith unary  term mentions j-th attribute
    gen_lists_dvars(NB, LenA, 0, 1, ListsB),              % B_i,j = 1 iff ith binary term mentions j-th attribute
    gen_lists_dvars(NP, Len,  0, 1, ListsP),              % P_i,j = 1 iff ith polynom mentions j         th attribute   (j in [1,        LenA      ])
                                                          % P_i,j = 1 iff ith polynom mentions j-LenA    th unary  term (j in [LenA+1,   LenA+NU   ])
                                                          % P_i,j = 1 iff ith polynom mentions j-LenA-NU th binary term (j in [LenA+NU+1,LenA+NU+NB])
    gen_lists_dvars(1,  LenA, 0, 1, [ListA]),             % A_j   = 1 iff jth attribute is used in a polynom/unary term/binary term
    remove_equal_col_attributes(AttrNames, ListA),        % set A_j to 0 if jth attribute corresponds to a column which is equal to a previous column
    (TuplesAttr = [_|_] ->                                % when table_attr option is used then restrict ListA to one of the feasible combinaision of
        call(table([ListA], TuplesAttr))                  % attributes which was given in the option
    ;
        true
    ),
    length(PrefixOfOnes, LenMandatoryAttr),               % create a prefix list of ListA whose elements are all set to 1 (for the mandatory attributes)
    domain(PrefixOfOnes, 1, 1),                           % the A_j corresponding to the mandatory attributes
    prefix_length(ListA, PrefixOfOnes, LenMandatoryAttr), % are all set to 1 to force to use all the mandatory attributes
    gen_count_ctrs(ListsU, 1, eq(1)),                     % each unary  term should use exactly one attribute
    gen_count_ctrs(ListsB, 1, eq(2)),                     % each binary term should use exactly two attributes
    gen_count_ctrs(ListsP, 1, geq(1)),                    % each polynom     should use at least one attribute or one unary/binary term
    append([ListsP,ListsU,ListsB], ListsPUB),             % append the three matrices P_i,j, U_i,j, B_i,j, and
    prefixes_length(ListsPUB, PListsPUB, LenA),           % remove the suffix part corresponding to unary and binary terms (as keep only attributes)
    transpose(PListsPUB, TPListsPUB),                     % transpose the resulting matrix to also post constraints on its columns
    gen_or_ctrs(TPListsPUB, var, ListA),                  % link the variables associated with a same attribute j of U_i,j, P_i,j and B_i,j to Aj
    gen_count_ctrs([ListA], 1, in(MinA,MaxA)),            % impose the number of ones in ListA to be inside [MinA,MaxA]
    (NUB > 0 ->                                           % if use at least one unary term or one binary term
        suffixes_length(ListsP, SListsP, NUB),            % get the suffix part corresponding to unary and binary terms
        transpose(SListsP, TSListsP),                     % transpose the resulting matrix to also post constraints on its columns
        gen_count_ctrs(TSListsP, 1, geq(1))               % each unary/binary term should be used by at least one polynom
    ;
        true
    ),
    flatten(ListsPUB, AllAttrVars),                       % group variables associated with the way attributes are used in the polynoms & unary/binary terms
    formula_pattern(attributes_use([p(ListsP),u(ListsU),  % record how the attributes and unary/binary terms are used by the polynoms
                                    b(ListsB),a(ListA)]),
                                    FormulaPattern),
    restrict_attr_wrt_unary_terms(ListsU, Unary,          % post some constraints to restrict attributes when formula uses some unary terms
                                  AttrNames),
    restrict_attr_wrt_binary_terms(ListsB, Binary, LenA,  % post some constraints to restrict attributes when formula uses some binary terms
                                   AttrNames).

remove_equal_col_attributes([], []) :- !.                 % scan all potential attributes of a formula and set to 0 the corresponding 0/1 variable
remove_equal_col_attributes([AttrName|R], [A|S]) :-       % if that attribute corresponds to a column of the table that is equal to some previous
    tab_get_equal(AttrName, Eq),                          % column: this will prevent that attribute to be used in the formula
    (Eq > 0 -> A = 0 ; true),
    remove_equal_col_attributes(R, S).

% USE THE OPTIONS ASSOCIATED WITH THE MAIN FORMULA TYPE TO GENERATE ALL THE POLYNOMS USED IN THE MAIN FORMULA
%------------------------------------------------------------------------------------------------------------
formula_gen_polynoms(I, NP, _, _, _, _, _, [], [], []) :- I > NP, !.
formula_gen_polynoms(I, NP, FOptions, Params, BreakSymInUnaryBinaryT, BreakSymInUnaryF, BreakSymInBinaryF, [ListP|R], [Polynom|S], [Coefs|T]) :-
    I =< NP,
    arg(I, FOptions, OptionsI),                                                                       % get option of i-th polynom
    (memberchk(degree(MinD,MaxD),     OptionsI) -> true ; write(formula_gen_polynoms1), nl, halt),    % check that minimum/maximum degree are specified
    (memberchk(coef(MinCoef,MaxCoef), OptionsI) -> true ; write(formula_gen_polynoms2), nl, halt),    % check that range of coefficient is specified
    (memberchk(cst(MinCst,MaxCst),    OptionsI) -> true ; write(formula_gen_polynoms3), nl, halt),    % check that range of constant is specified
    (MinD >  0                                  -> true ; write(formula_gen_polynoms4), nl, halt),    % check that at least linear
    (MinD =< MaxD                               -> true ; write(formula_gen_polynoms5), nl, halt),    % check that non-empty degree interval
    gen_polynom(Params, BreakSymInUnaryBinaryT, BreakSymInUnaryF, BreakSymInBinaryF,                  % generate i-th polynom
                t(MinD,MaxD,MinCoef,MaxCoef,MinCst,MaxCst), OptionsI, ListP, Polynom, Coefs),
    I1 is I+1,
    formula_gen_polynoms(I1, NP, FOptions, Params, BreakSymInUnaryBinaryT, BreakSymInUnaryF, BreakSymInBinaryF, R, S, T).

% generate a polynom of maximum degree MaxD: the different monomes where each monome is
% a coefficient (to be found) and the degrees of the parameters in that monome
gen_polynom(Params, BreakSymInUnaryBinaryT, BreakSymInUnaryF, BreakSymInBinaryF, MinMax, OptionsI, ListP, Monomes, Coefs) :-
    MinMax = t(MinD,MaxD,_MinCoef,_MaxCoef,_MinCst,_MaxCst),
    length(Params, NParams),                                                % number of parameters of the polynom (attributes + unary/binary terms)
    all_options(unary_boolean, UnaryBooleanFunctions),                      % get Boolean unary functions (as can stop to generate up to degree 1)
    get_max_degree_params(Params, UnaryBooleanFunctions, MaxD, ParamsMaxD), % get the maximum possible degree of each of the parameters of the polynom
    length(MonomesDegree, NParams),                                         % create a list of degree variables for a monome
    gen_list_dvars_min_maxs(MonomesDegree, 0, ParamsMaxD),                  % set up minimum and maximum values of degree variables
    gen_sum_term(MonomesDegree, var, SumDegrees),                           % SumExposents is the sum of the degrees of the parameters of a monome
    SumExposents in 0..MaxD,                                                % use findall to generate all feasible combination of degrees inside the
    call(SumDegrees #= SumExposents),                                       % different monomes
    findall(t(SumExposents,MonomesDegree,_),                                % the monomes are created by decreasing degree: the last coefficient
            (labeling([down],[SumExposents]),                               % corresponds to the monome of degree 0
             labeling([],MonomesDegree)), Monomes),
    gen_polynom_set_domain_coefs(Monomes, BreakSymInUnaryF, MinMax, Coefs), % declare the domains of the coefficients of the polynom
    gen_polynom_min_max_degree_ctr(Monomes, MinD, MaxD),                    % enforce that at least one monome whose sum of exponents is between
    gen_polynom_non_zeros_ctr(Monomes, OptionsI),                           % MinD and MaxD has a non zero coefficient (always enforced as MinD>0)
    gen_polynom_param_use_ctr(Monomes, OptionsI, NParams, ListP),
    (BreakSymInUnaryBinaryT = power_prod(IndexUnaryBinaryT) ->              % if the formula uses one single term corresponding to "power" or "prod"
        enforce_single_monome_coefs_to_be_null(Monomes, IndexUnaryBinaryT)  % then force to 0 all monomes coefficients mentionning only power or prod
    ;
        true
    ),
    (BreakSymInUnaryF = power ->                                            % if the formula is a unary function that uses "power(polynom,degree)"
        enforce_first_non_zero_monome_coef_to_be_positive(Coefs)            % then create a symmetry breaking constraint
    ;
        true
    ),
    (BreakSymInBinaryF = prod ->                                            % if the formula is a binary function that use "prod"
        gen_polynom_count_non_zero_monomes(Monomes, NbNonZeroCoefs),        % then enforce the polynom to have more than one non-zero coefficient
        NbNonZeroCoefs #> 1
    ;
        true
    ).

enforce_single_monome_coefs_to_be_null([], _) :- !.
enforce_single_monome_coefs_to_be_null([t(_,MonomesDegree,Coef)|R], IndexUnaryBinaryT) :-
    (all_zero_except_ith_position(MonomesDegree, IndexUnaryBinaryT) -> Coef = 0 ; true),
    enforce_single_monome_coefs_to_be_null(R, IndexUnaryBinaryT).

all_zero_except_ith_position([], _) :- !.
all_zero_except_ith_position([D|R], I) :-
    (I = 1 -> true ; D = 0),
    I1 is I-1,
    all_zero_except_ith_position(R, I1).

enforce_first_non_zero_monome_coef_to_be_positive(Coefs) :-                 % when formula is a unary function using power, we break symmetry in the
    gen_signature(Coefs, Signature),                                        % polynom which is inside the power function by enforcing that the first
    automaton(Signature, _, Signature,                                      % non zero coefficient of the polynom is positive
              [source(s),sink(r)],                                          % (as we forbid that all coefficients are set to 0, only r is accepting)
              [arc(s, 1, s),  % if coefficient is 0 then stay on the initial state
               arc(s, 2, r),  % if coefficient is positive then go on the accepting state r
               arc(r, 0, r),  % on state r we can have a negative,
               arc(r, 1, r),  % a zero, or
               arc(r, 2, r)], % a positive coefficient and we stay on the accepting state r
               [], [], []).

gen_polynom_set_domain_coefs([], _, _, []) :- !.
gen_polynom_set_domain_coefs([t(SumExposents,_,Coef)|R], BreakSymInUnaryF, MinMax, [Coef|S]) :-
    MinMax = t(_,_,MinCoef,MaxCoef,                      % set the range of the cst of each monome wrt information
                    MinCst,MaxCst),                      % passed in the option attached to each polynom
    (SumExposents = 0 ->                                 % if we are on the constant monome of the polynom
        Coef in MinCst..MaxCst,                          % declare domain of the constant term
        (BreakSymInUnaryF = in(Low,MinLow,MaxLow,        % if formula corresponds to a unary function that uses the "in" function
                               Up,MinUp,MaxUp   ) ->     % then break symmetry wrt interval [Low,Up] of the "in" function:
            Coef #> 0 #=> Low #= MinLow #\/ Up #= MinUp, %  . condition stating that we cannot decrease Coef toward 0 anymore, as we
                                                         %    are blocked either by the minimum value of Low or by the minimum value of Up
            Coef #< 0 #=> Low #= MaxLow #\/ Up #= MaxUp  %  . condition stating that we cannot increase Coef toward 0 anymore, as we
        ;                                                %    are blocked either by the maximum value of Low or by the maximum value of Up
            true
        )
    ;
        Coef in MinCoef..MaxCoef
    ),
    (BreakSymInUnaryF = mod(Cst) ->                      % if formula is a unary function that uses the "mod" function
        Coef #>= 0,                                      % then break symmetry wrt the constant of the modulo by enforcing that each
        Coef #< Cst                                      % coefficient of the monome is between 0 and Cst-1
    ;
        true
    ),
    gen_polynom_set_domain_coefs(R, BreakSymInUnaryF, MinMax, S).

gen_polynom_count_non_zero_monomes(Monomes, CountNonZero) :- % return in CountNonZero then number of coefficients which will be different from 0
    get_monomes_coefs(Monomes, Coefs),
    gen_sum_term(Coefs, var_neq(0), SumNonZero),
    call(CountNonZero #= SumNonZero).

get_monomes_coefs([], []) :- !.
get_monomes_coefs([t(_,_,Coef)|R], [Coef|S]) :-
    get_monomes_coefs(R, S).

gen_polynom_min_max_degree_ctr(Monomes, MinD, MaxD) :-
    gen_polynom_min_max_degree_ctr1(Monomes, MinD, MaxD, Coefs), % extract all coefficients for which the sum of the exponents of the
    (Coefs = [] ->                                               % terms in the corresponding monome are between the required min.and max. degrees
        false                                                    % and impose at least one of these coefficients to be different from 0
    ;                                                            % note that this list may be empty, e.g. when just using Boolean attributes and 
        gen_or_term(Coefs, var_neq(0), Term),                    % and MinD > 1, this is why we fail when Coefs = []
        call(Term)
    ).

gen_polynom_min_max_degree_ctr1([], _, _, []) :- !.
gen_polynom_min_max_degree_ctr1([t(SumExponents,_,Coef)|R], MinD, MaxD, [Coef|S]) :-
    MinD =< SumExponents,
    MaxD >= SumExponents,
    !,
    gen_polynom_min_max_degree_ctr1(R, MinD, MaxD, S).
gen_polynom_min_max_degree_ctr1([_|R], MinD, MaxD, S) :-
    gen_polynom_min_max_degree_ctr1(R, MinD, MaxD, S).

gen_polynom_non_zeros_ctr(Monomes, OptionsI) :-
    findall(non_zero(Dmin,Dmax,NZmin,NZmax),                   % extract all non_zero options from the list of option of i-th polynom
            member(non_zero(Dmin,Dmax,NZmin,NZmax), OptionsI),
            NonZerosCtr),
    gen_polynom_non_zeros_ctr1(NonZerosCtr, Monomes).    % handle the list (eventually empty) of non zero options

gen_polynom_non_zeros_ctr1([], _) :- !.
gen_polynom_non_zeros_ctr1([non_zero(Dmin,Dmax,NZmin,NZmax)|R], Monomes) :- % handle the current non zero options
    gen_polynom_non_zeros_ctr2(Monomes, Dmin, Dmax, Coefs),                 % extract all coefficients for which the sum of the exponents of the
    gen_sum_term(Coefs, var_neq(0), Term),                                  % terms in the corresponding monome are between Dmin and Dmax
    %write([Term, NZmin, NZmax]),nl,
    (NZmin = NZmax ->
         call(Term #= NZmin)
    ;
         call(Term #>= NZmin),                                                   % and impose that at least NZmin of these coefficients are different from 0
         call(Term #=< NZmax)                                                    % and impose that at most  NZmax of these coefficients are different from 0
    ),
    %write(passed),nl,
    gen_polynom_non_zeros_ctr1(R, Monomes).

gen_polynom_non_zeros_ctr2([], _, _, []) :- !.
gen_polynom_non_zeros_ctr2([t(SumExposents,_,Coef)|R], Dmin, Dmax, [Coef|S]) :-
    SumExposents >= Dmin, SumExposents =< Dmax, !,
    gen_polynom_non_zeros_ctr2(R, Dmin, Dmax, S).
gen_polynom_non_zeros_ctr2([_|R], Dmin, Dmax, S) :-
    gen_polynom_non_zeros_ctr2(R, Dmin, Dmax, S).

gen_polynom_param_use_ctr(Monomes, OptionsI, NParams, ListP) :-
    gen_polynom_param_use_ctr1(1, NParams, Monomes, ListP, ParamUsed), % build the sum of Boolean variables telling wheter a parameter is used or not
    (memberchk(param(Pmax), OptionsI) ->                               % if the option restricting the maximum number of parameters is used
        call(ParamUsed #=< Pmax)                         % then restrict this sum to be less than or equal to Pmax
    ;
        true
    ).

gen_polynom_param_use_ctr1(J, N, _, [], 0) :-
    J > N, !.                                                               
gen_polynom_param_use_ctr1(J, N, Monomes, [Pij|R], Pij+S) :-    % compute for the j-th parameter of the i-th polynom if it will occur in a monome or not
    J =< N,                                                     % get the monomes of the i-th polynom, extract the exponent of the j-th parameter,
    gen_polynom_param_use_ctr2(Monomes, J, Coefs),              % and keep the coefficient if the exponent is greater than 0
    gen_or_term(Coefs, var_neq(0), Term),                       % if one of the keept coefficients is different from 0
    call(Term #= Pij),                            % this means that the j-th parameter will be used by the polynom
    J1 is J+1,                                                  % index of next parameter of the i-th polynom
    gen_polynom_param_use_ctr1(J1, N, Monomes, R, S).

gen_polynom_param_use_ctr2([], _, []) :- !.
gen_polynom_param_use_ctr2([t(_,Exposents,Coef)|R], J, [Coef|S]) :-
    nth1(J,Exposents,ExposentJ),
    ExposentJ > 0,
    !,
    gen_polynom_param_use_ctr2(R, J, S).
gen_polynom_param_use_ctr2([_|R], J, S) :-
    gen_polynom_param_use_ctr2(R, J, S).

% compute for each parameter of a polynom its maximum degree in a monome:
%  for an attribute which take its value in  0..1   we stop at degree 1,
%  for an attribute which take its value in -1..1   we stop at degree 2,
%  for a unary term u which uses a Boolean function we stop at degree 1,
%  if none of the previous conditions apply we stop at the maximum degree MaxD
get_max_degree_params([], _, _, []) :- !.
get_max_degree_params([Param|R], UnaryBooleanFunctions, MaxD, [MaxDP|S]) :-
    functor(Param, Functor, _),
    (Param = col(_,_)                          -> tab_get_min_max(Param, Min, Max) ;
     memberchk(Functor, UnaryBooleanFunctions) -> Min = 0, Max = 1                 ;
                                                  Min = 2                          ),
    ((Min =  0, Max = 1) -> MaxDP = 1   ;
     (Min = -1, Max = 1) -> MaxDP = 2   ;
                            MaxDP = MaxD),
    get_max_degree_params(R, UnaryBooleanFunctions, MaxD, S).

% RESTRICT THE ATTRIBUTES THAT CAN BE MENTIONNED WITHIN A UNARY TERM
%-------------------------------------------------------------------
restrict_attr_wrt_unary_terms([], _, _) :- !.               % do nothing if no unary term
restrict_attr_wrt_unary_terms(ListsU, Unary, AttrNames) :-
    tab_get_mins_attr_names(AttrNames, AttrMins),
    tab_get_maxs_attr_names(AttrNames, AttrMaxs),
    tab_get_indices_in_range_attr_names(AttrNames,  0,       1, 1, Attrs01),
    tab_get_indices_in_range_attr_names(AttrNames, -1,       1, 1, Attrs11),
    tab_get_indices_neg_min_attr_names(AttrNames,               1, AttrMinNeg),
    select_unary_or_binary_terms_wrt_function_list(Unary, ListsU, 0, [min,max],    ListsBMinMax),
    select_unary_or_binary_terms_wrt_function_list(Unary, ListsU, 0, [floor,ceil], ListsBFloorCeil),
    select_unary_or_binary_terms_wrt_function_list(Unary, ListsU, 0, [mod],        ListsBMod),
    select_unary_or_binary_terms_wrt_function_list(Unary, ListsU, 0, [geq],        ListsBGeq),
    select_unary_or_binary_terms_wrt_function_list(Unary, ListsU, 0, [power],      ListsBPower),
    select_unary_or_binary_terms_wrt_function_list(Unary, ListsU, 1, [sum_consec], ListsBSumConsec),
    restrict_min_max_wrt_unary_term(ListsBMinMax, AttrMins, AttrMaxs),
    restrict_floor_ceil_wrt_unary_term(ListsBFloorCeil, AttrMins, AttrMaxs),
    restrict_mod_wrt_unary_term(ListsBMod, AttrMaxs),
    restrict_geq_wrt_unary_term(ListsBGeq, AttrMins, AttrMaxs),
    restrict_power_wrt_unary_term(ListsBPower, Attrs01, Attrs11),
    restrict_sum_consec_wrt_unary_term(ListsBSumConsec, AttrMinNeg).

restrict_min_max_wrt_unary_term([], _, _) :- !.
restrict_min_max_wrt_unary_term([[Cst|ListU]|R], AttrMins, AttrMaxs) :-
    get_value_selected_attribute(ListU, AttrMins, Min),
    get_value_selected_attribute(ListU, AttrMaxs, Max),
    Cst #> Min,
    Cst #< Max,
    restrict_min_max_wrt_unary_term(R, AttrMins, AttrMaxs).

restrict_floor_ceil_wrt_unary_term([], _, _) :- !.
restrict_floor_ceil_wrt_unary_term(ListsBMinMax, AttrMins, AttrMaxs) :-
    compute_max_abs(AttrMins, AttrMaxs, AttrMaxAbs),
    restrict_floor_ceil_wrt_unary_term(ListsBMinMax, AttrMaxAbs).

restrict_floor_ceil_wrt_unary_term([], _) :- !.
restrict_floor_ceil_wrt_unary_term([[Cst|ListU]|R], AttrMaxAbs) :-
    get_value_selected_attribute(ListU, AttrMaxAbs, MaxAbs),
    abs(Cst) #=< MaxAbs,
    Cst #\= 0,
    restrict_floor_ceil_wrt_unary_term(R, AttrMaxAbs).

restrict_mod_wrt_unary_term([], _) :- !.
restrict_mod_wrt_unary_term([[Cst|ListU]|R], AttrMaxs) :-
    get_value_selected_attribute(ListU, AttrMaxs, Max),
    Cst #> 1,
    Cst #=< Max,
    restrict_mod_wrt_unary_term(R, AttrMaxs).

restrict_geq_wrt_unary_term([], _, _) :- !.
restrict_geq_wrt_unary_term([[Cst|ListU]|R], AttrMins, AttrMaxs) :-
    get_value_selected_attribute(ListU, AttrMins, Min),
    get_value_selected_attribute(ListU, AttrMaxs, Max),
    Cst #>= Min,                                            % BUG: SHOULD BE STRICTLY GREATER, AS OTHERWISE WE GET THE CONSTANT 1
    Cst #=< Max,                                            % BUG: SHOULD BE STRICTLY LESS,    AS OTHERWISE WE GET THE CONSTANT 1
    restrict_geq_wrt_unary_term(R, AttrMins, AttrMaxs).

restrict_power_wrt_unary_term([], _, _) :- !.
restrict_power_wrt_unary_term([[Degree|ListU]|R],
                              Attrs01,                  % list of indices of the 0/1 attributes
                              Attrs11) :-               % list of indices of the -1/0/1 attributes
    nths1(Attrs01, ListU, 0),                           % forbid using a 0/1 attribute (set its Boolean variable to 0)
    (Degree > 2 -> nths1(Attrs11, ListU, 0) ; true),    % if degree of power >2 then forbid using a -1/0/1 attribute (set its Boolean variable to 0)
    restrict_power_wrt_unary_term(R, Attrs01, Attrs11).

restrict_sum_consec_wrt_unary_term([], _) :- !.
restrict_sum_consec_wrt_unary_term([ListU|R], AttrMinNeg) :-
    nths1(AttrMinNeg, ListU, 0),
    restrict_sum_consec_wrt_unary_term(R, AttrMinNeg).

get_value_selected_attribute(Vars, Values, ValAttribute) :-
    automaton(Values, Val, Vars, 
              [source(s),sink(r)],
              [arc(s, 0, s       ),
               arc(s, 1, r, [Val]),
               arc(r, 0, r       )],
               [_], [1000000], [ValAttribute]).

% RESTRICT THE ATTRIBUTES THAT CAN BE MENTIONNED WITHIN A BINARY TERM
%--------------------------------------------------------------------

% post the following constraints to restrict binary terms attributes:
%   min(A1,A2),   max(A1,A2)                                       : avoid A1 gt A2, A1 geq A2, A2 gt A1, A2 geq A1
%   floor(A1,A2), ceil(A1,A2), mod(A1,A2), dmod(A1,A2), fmod(A1,A2): avoid A2 gt A1, A2 geq A1
%   cmod(A1,A2)                                                    : avoid A1 gt A2, A1 geq A2
%   mod(A1,A2), dmod(A1,A2), fmod(A1,A2)                           : enforce A2 gt 0
%   cmod(A1,A2)                                                    : enforce A1 gt 0
%
%   ListsB   : list which gives for each binary term a list of LenA 0/1 variables telling whether the binary term uses or not a given attribute
%   Binary   : list of binary terms (includes the Order of the two attributes for non commutative binary functions)
%   LenA     : total number of attributes (mandatory and optional attributes)
%   AttrNames: mandatory and optional attributes
restrict_attr_wrt_binary_terms([], _, _, _) :- !.                                       % do nothing if no binary term
restrict_attr_wrt_binary_terms(ListsB, Binary, LenA, AttrNames) :-
    select_unary_or_binary_terms_wrt_function_list(Binary, ListsB, 1, [min,max],         ListsBMinMax),
    select_unary_or_binary_terms_wrt_function_list(Binary, ListsB, 0, [floor,ceil],      ListsBFloorCeil), 
    select_unary_or_binary_terms_wrt_function_list(Binary, ListsB, 0, [cmod],            ListsBCmod),
    select_unary_or_binary_terms_wrt_function_list(Binary, ListsB, 0, [mod,dmod,fmod],   ListsBModDmodFmod),
    append(ListsBFloorCeil, ListsBModDmodFmod, ListsBFloorCeilModDmodFmod),
    ((ListsBMinMax = [], ListsBFloorCeilModDmodFmod = [], ListsBCmod = []) -> true ;
        build_table_attr_wrt_cmp(LenA, AttrNames, geq_gt, TableGeqGt),                  % get all pairs of attributes [I,J] such that we always have 
        (TableGeqGt = [_|_] ->                                                          % I geq J, or I gt J
            restrict_attr_wrt_binary_term(ListsBMinMax,               TableGeqGt, LenA, 0), % min,max  : avoid A1 gt A2, A1 geq A2, A2 gt A1, A2 geq A1
            restrict_attr_wrt_binary_term(ListsBFloorCeilModDmodFmod, TableGeqGt, LenA, 1), % floor,...: avoid A2 gt A1, A2 geq A1
            restrict_attr_wrt_binary_term(ListsBCmod,                 TableGeqGt, LenA, 2)  % cmod     : avoid A1 gt A2, A1 geq A2
        ;
            true
        )
    ),
    ((ListsBModDmodFmod = [], ListsBCmod = []) -> true ;                                                           % mod,dmod,fmod : enforce A2 gt 0
        build_table_attr_wrt_cmp(LenA, AttrNames, gt0, TableGt0),                                                  % cmod          : enforce A1 gt 0
        ((TableGt0 = [_|_], ListsBModDmodFmod = [_|_]) -> restrict_attr_wrt_table(ListsBModDmodFmod, 1, TableGt0) ; true),
        ((TableGt0 = [_|_], ListsBCmod        = [_|_]) -> restrict_attr_wrt_table(ListsBCmod,    0, TableGt0) ; true)
    ).

% Select the list of 0/1 variables of those binary terms for which the binary function is in Functions;
% in addition add in front of such lists the corresponding order variable when NoOrder=0 
%  [Bin|R]  : list of binary functions used in a binary term, where each function has the form FunctionName(OrderVar)
%  [ListB|S]: list of 0/1 variables associated with each used binary term (the 0/1 var. tell which attributes the binary function uses)
%  NoOrder  : 1 for a commutative function (no order between the attributes), 0 for a non commutative function (an 0/1 ordering variable is used)
%  Functions: list of functions names we are looking for
%  Result   : list of selected ListB that will be returned
select_unary_or_binary_terms_wrt_function_list([], [], _, _, []) :- !.
select_unary_or_binary_terms_wrt_function_list([Bin|R], [ListB|S], NoOrder, Functions, Result) :-
    functor(Bin, Name, _),
    (memberchk(Name, Functions) ->
        (NoOrder = 1 -> Result = [ListB|T] ; arg(1, Bin, F), Result = [[F|ListB]|T])
    ;
        Result = T
    ),
    select_unary_or_binary_terms_wrt_function_list(R, S, NoOrder, Functions, T).

% build the table of tuples of indices of the attributes which can occur in the formula such that a given comparison Cmp holds
%  LenA     : number of elements of AttrNames
%  AttrNames: attributes names that can be potentially mentionned by the formula we build
%  Cmp      : provide the comparaison used to select attributes
%  Table    : constructed table of tuples verifying the comparison Cmp
build_table_attr_wrt_cmp(LenA, AttrNames, Cmp, Table) :-
    (Cmp = geq_gt ->                                                                            % build the table of (I,J) of indices of the attributes such
        findall([I,J],                                                                          % I gt J or I geq J (find this information in the metadata)
                (I in 1..LenA,
                 indomain(I),                                                                   % go through the attributes we can have in the formula
                 nth1(I, AttrNames, AttrNameI),                                                 % get the different attributes names
                 tab_get_cmp(AttrNameI, ListCmpI),                                              % extract all comparaison relations found for AttrNameI
                 findall(AttrNameJ1, member(geq(AttrNameI,AttrNameJ1), ListCmpI), AttrNamesJ1), % extract all attributes J for which geq comparaison with I
                 findall(AttrNameJ2, member(gt(AttrNameI, AttrNameJ2), ListCmpI), AttrNamesJ2), % extract all attributes J for which gt  comparaison with I
                 lists_member([AttrNamesJ1,AttrNamesJ2], AttrNameJ),                            % go through all possible AttrNameJ
                 nth1(J, AttrNames, AttrNameJ)),                                                % and get index J associated with AttrNameJ
                Table)
    ;
     Cmp = gt0 ->                                                                               % build the table of (I) of index of the attributes which
        findall([I],                                                                            % are always strictly greater than 0 (find this in the metadata)
                (I in 1..LenA,
                indomain(I),                                                                    % go through the attributes we can have in the formula
                nth1(I, AttrNames, AttrNameI),                                                  % get the different attributes names
                tab_get_min(AttrNameI, MinI),                                                   % extract minimum value for AttrNameI
                MinI > 0),                                                                      % and check that is strictly positive
               Table)
    ;
        write(build_table_attr_wrt_cmp), nl, halt
    ).

restrict_attr_wrt_binary_term([], _, _, _) :- !.
restrict_attr_wrt_binary_term(Lists, TableGeqGt, LenA, Flag) :-
    build_geq_gt_table_wrt_lena(TableGeqGt, Flag, LenA, Table), % build forbidden tuples: put two 1 at positions i and j, put 0 at other positions
    (Lists = [List] ->
        call(#\table([List], Table))
    ;
        write(build_geq_gt_table_wrt_lena), halt
    ).

% for each pair [I,J] of indices of the attributes which can occur in the formula such that there is the comparaison I geq J,
% or the comparison I gt J, build a sublist of length LenA of 0/1 values with two 1 at positions I and J (indices start at 1)
% and 0 at the other positions. Such sublists correspond to forbidden tuples of values used to avoid using an attribute that
% is for sure greater than or equal to another attribute in the context of binary terms
%  [[I,J]|R]     : list of pairs of attributes indexes for which I geq J or I gt J
%  Arg2LeqArg1   : 0 if build forbidden tuples to avoid A1 gt A2, A1 geq A2, A2 gt A1, A2 geq A1 (min,max)
%                  1 if build forbidden tuples to avoid A2 gt A1, A2 geq A1                      (floor,ceil,mod,dmod)
%                  2 if build forbidden tuples to avoid A1 gt A2, A1 geq A2                      (cmod)
%  LenA          : number of attributes which can be potentially used in the formula
%  [TableEntry|S]: list of forbidden tuples that will be returned
build_geq_gt_table_wrt_lena([], _, _, []) :- !.
build_geq_gt_table_wrt_lena([[I,J]|R], Arg2LeqArg1, LenA, [TableEntry|S]) :-
    length(Entry, LenA),
    build_geq_gt_table_wrt_lena1(Entry, 1, I, J), % build the forbidden tuples of 0/1 values for the pair of attributes I, J
    ( Arg2LeqArg1 = 0         -> TableEntry = Entry                          ; % if min or max (i.e. Arg2LeqArg1=0) no order variable
                                                                               % if floor, ceil, mod, dmod (i.e. Arg2LeqArg1=1) then restrict order variable
     (Arg2LeqArg1 = 1, I < J) -> TableEntry = [1|Entry]                      ; %  1: forbid bin(J,I) with J geq I
     (Arg2LeqArg1 = 1, I > J) -> TableEntry = [0|Entry]                      ; %  0: forbid bin(I,J) with I geq J
                                                                               % if cmod (i.e. Arg2LeqArg1=2) then restrict order variable
     (Arg2LeqArg1 = 2, I < J) -> TableEntry = [0|Entry]                      ; %  0: forbid bin(I,J) with J geq I
     (Arg2LeqArg1 = 2, I > J) -> TableEntry = [1|Entry]                      ; %  1: forbid bin(J,I) with I geq J
                                 write(build_geq_gt_table_wrt_lena), nl, halt),
    build_geq_gt_table_wrt_lena(R, Arg2LeqArg1, LenA, S).

build_geq_gt_table_wrt_lena1([], _, _, _) :- !.
build_geq_gt_table_wrt_lena1([V|R], Cur, I, J) :-
    (Cur = I -> V = 1 ; Cur = J -> V = 1 ; V = 0),
    Next is Cur+1,
    build_geq_gt_table_wrt_lena1(R, Next, I, J).

restrict_attr_wrt_table([], _, _) :- !.
restrict_attr_wrt_table([[Order|ListB]|R], SecondAttr, Table) :-
    get_attribute_index_binary_term(Order, ListB, SecondAttr, IndAttribute),
    call(table([[IndAttribute]], Table)),
    restrict_attr_wrt_table(R, SecondAttr, Table).

% for the sequence of 0/1 variables Vars force two variables to be assigned to value 1,
% and in addition impose the following conditions:
%   if SecondAttr = 1 then:
%     if Order=0 then set IndAttribute to the position of the second occurrence of 1 (i.e. pos. of second used attribute of the binary function)
%     if Order=1 then set IndAttribute to the position of the first  occurrence of 1 (i.e. pos. of second used attribute of the binary function)
%   if SecondAttr = 0 then:
%     if Order=1 then set IndAttribute to the position of the second occurrence of 1 (i.e. pos. of first used attribute of the binary function)
%     if Order=0 then set IndAttribute to the position of the first  occurrence of 1 (i.e. pos. of first used attribute of the binary function)
get_attribute_index_binary_term(Order, Vars, SecondAttr, IndAttribute) :-
    gen_signature_attribute_index_binary_term(Vars, Order, SecondAttr, 1, Positions, Signature),
    automaton(Positions, I, Signature, 
              [source(s),sink(f)],
              [arc(s,0,s    ), arc(s,1,s    ), arc(s,2,r    ), arc(s,3,r,[I]),
               arc(r,0,r    ), arc(r,1,r    ), arc(r,2,f,[I]), arc(r,3,f    ),
               arc(f,0,f    ), arc(f,1,f    )],
               [_], [1], [IndAttribute]).

gen_signature_attribute_index_binary_term([], _, _, _, [], []) :- !.
gen_signature_attribute_index_binary_term([Vi|R], Order, SecondAttr, I, [I|S], [VOi|T]) :-
    Vi #= 0 #/\ Order #= 0 #<=> VOi #= 0,
    Vi #= 0 #/\ Order #= 1 #<=> VOi #= 1,
    (SecondAttr = 1 ->
        Vi #= 1 #/\ Order #= 0 #<=> VOi #= 2,
        Vi #= 1 #/\ Order #= 1 #<=> VOi #= 3
    ;
        Vi #= 1 #/\ Order #= 1 #<=> VOi #= 2,
        Vi #= 1 #/\ Order #= 0 #<=> VOi #= 3
    ),
    I1 is I+1,
    gen_signature_attribute_index_binary_term(R, Order, SecondAttr, I1, S, T).

% RESTRICT THE ATTRIBUTES THAT CAN BE MENTIONNED WITHIN A BINARY FUNCTION
%------------------------------------------------------------------------
% post some constraints to restrict attributes when formula is a binary function
% ListBinaryFunction: a list with one single binary function
% Coefs             : the coefficient of the two polynoms where the last coefficient corresponds to the monome of degree 0
% ListA             : list of 0/1 variables set to 1 iff i-th attribute is used in the formula
restrict_attr_wrt_binary_functions(3, ListBinaryFunction, Coefs, ListA) :- !,
    ListBinaryFunction = [BinaryFunction],                                 % know that we have only one binary function, we get it
    (memberchk(BinaryFunction, [min,max]) ->                               % break symmetry if on min/max binary functions, but not on
        Coefs = [Coefs1,Coefs2],                                           % prod as it interact badly with the fact that the first
        length(Coefs1, Len1),                                              % argument is >= 0 on prod (loose solution !)
        length(Coefs2, Len2),
        (Len1 = Len2 ->                                                    % can only break symmetry if the two polynomials have the
            LexOpt = [op(#<)],                                             % same maximum degree (otherwise can loose some solution !)
            lex_chain(Coefs, LexOpt)
        ;
            true
        )
    ;
     memberchk(BinaryFunction,[ceil,floor]) ->
        append(Coefs, FlatCoefs),                                          % put coefficients of both polynom in a single list
        get_max_abs_dvars(FlatCoefs, MaxAbs),
        Primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97],
        findall(Prime, (member(Prime, Primes), Prime =< MaxAbs), LPrimes),
        gcd_eq_one_ctr(LPrimes, FlatCoefs)                                 % gcd of all non zero coefs of both polynoms should be 1
                                                                           % note that, as we impose each denominator to be > 0,
                                                                           % see (gen_binaryf_template), we do not enforce the first
                                                                           % non-zero coefficient of the numerator to be positive !!!
    ;
        true
    ),
    count(1, ListA, #>, 1).                                                % impose that the binary function uses at least two distinct attributes
restrict_attr_wrt_binary_functions(_, _, _, _).

% GENERATE AND RECORD THE SOURCE LOGICAL VARIABLES, THE SOURCE TEMPLATE, THE REAL VARIABLES IN ORDER TO STATE A CONSTRAINT ON A TABLE ENTRY
%------------------------------------------------------------------------------------------------------------------------------------------
gen_source_target_templates(formula, FormulaPattern, AttrNames, LenA) :-                % used only on formulae involving polynomials
    length(LogAttributes, LenA),                                                        % create a list of logical variables for the actual values of the attributes
    formula_pattern(tattributes(LogAttributes), FormulaPattern),                        % record logical variables
    gen_unary_templates(FormulaPattern, LenA, LogAttributes, LogResUnary),              % generate templates associated with unary terms
    gen_binary_templates(FormulaPattern, AttrNames, LenA, LogAttributes, LogResBinary), % generate templates associated with binary terms
    append([LogAttributes, LogResUnary, LogResBinary], LogAttributesUnaryBinary),       % put together all logical result vars: attr. + unary terms + binary terms
    gen_polynom_templates(FormulaPattern, LogAttributesUnaryBinary, LogResPolynom),     % generate templates associated with the polynoms
    gen_unaryf_template(FormulaPattern, LogResPolynom),                                 % generate template associated with unary function (when used)
    gen_binaryf_template(FormulaPattern, LogResPolynom).                                % generate template associated with binary function (when used)

gen_unary_templates(FormulaPattern, LenA, LogAttributes, LogResUnary) :-
    formula_pattern(attributes_use(AttributesUse), FormulaPattern),                     % get the used attributes
    formula_pattern(unary(UnaryTerms), FormulaPattern),                                 % extract unary functions and variables corresponding to the constant
    formula_pattern(mandatory_optional_attributes_names(AttrNames), FormulaPattern),    % get all names (col(Table,Col)) involved in current formula
    memberchk(u(ListsU), AttributesUse),                                                % extract 0/1 variables telling which attribute a unary term used
    gen_unary_templates1(UnaryTerms, ListsU, AttrNames, LenA, LogAttributes,            % create the different unary templates
                         UnaryTemplates, LogResUnary),
    formula_pattern(tunary(UnaryTemplates), FormulaPattern).                            % record the unary templates that were built

gen_unary_templates1([], [], _, _, _, [], []) :- !.
gen_unary_templates1([UnaryTerm|Q], [ListU|R], AttrNames, LenA, LogAttributes, [UnaryTemplate|S], [LogRes|T]) :-
    gen_unary_template(UnaryTerm, ListU, AttrNames, LenA, LogAttributes, UnaryTemplate, LogRes),
    gen_unary_templates1(Q, R, AttrNames, LenA, LogAttributes, S, T).

gen_unary_template(UnaryTerm, ListU, AttrNames, LenA, LogAttributes, UnaryTemplate, LogRes) :-
    UnaryTemplate = t(SourceVars,                                    % logical var.giving the attr. values, the 0/1 var. telling which attr. is used, the cst
                      SourceTerm,                                    % unary term that uses the logical variables
                      TargetVars,                                    % the real 0/1 var,the cst used(IGNORE input values as they depend of each table entry)
                      BoolCtrTargetParams),                          % information (target vars. and csts for generating constraints while generating the monomes of
                                                                     % a polynomial to prevent generating term of the form min(V,1)^d where the minimum of V is 1 or
                                                                     % term of the form max(V,0)^d where the maximum of V is 1 with d>1)
    length(LogListU, LenA),                                          % create the logical var. associated with the 0/1 var.
    gen_linear_sum(LogAttributes, LogListU, LogSum),                 % generate cartesian product of attributes values and Boolean variables
    functor(UnaryTerm, U, _),                                        % get the unary function of the unary term (Arity=1 or 0 depending on the unary function)
    (memberchk(U, [min,max,floor,ceil,mod]) ->                       % if the unary function uses a constant
                                                                     % put together all logical variables: LogRes will be used in the template
                                                                     % associated with a polynom which uses this binary term
        append([[LogRes], LogAttributes, LogListU, [LogCst]], SourceVars),
        arg(1, UnaryTerm, Cst),                                      % then get that constant (a variable as constant not yet known) and
        append(ListU, [Cst], TargetVars),                            % record the Boolean variables and the constant variable in the target variables
        functor(SourceTerm, #=, 2),                                  % create the source term: a constraint of the form LogRes #= U(LogSum,LogCst)
        arg(1, SourceTerm, LogRes),
        arg(2, SourceTerm, UTerm),
        (memberchk(U, [floor,ceil]) ->                               % create the right hand side of the equality constraint: the unary function u, where
            functor(UTerm, div, 2),                                  %  . the first  argument is the cartesian product of logical variables
            (U = floor ->                                            %  . the second argument is the logical variable representing the constant
                arg(1, UTerm, LogSum),
                arg(2, UTerm, LogCst)
            ;
                arg(1, UTerm, (LogSum+LogCst-1)),                    % convert the ceil operation to a floor operation
                arg(2, UTerm, LogCst)
            )
        ;                                                            % U corresponds either min or max
            element(I, ListU, 1),                                    % get index of attribute that will be used in the min (a variable)
            (U = min ->
                tab_get_mins_attr_names(AttrNames, MinOrMaxAttrs),   % get list of minimum values of the different attributes
                element(I, MinOrMaxAttrs, MinOrMaxAttr),             % get minimum value of the attribute that will be used in the min (a variable)
                BoolCtrTargetParams = bool_ctr_params(MinOrMaxAttr-0,% record information to later generate some constraint of the form
                                                      Cst-1)         % MinOrMaxAttr=0 and Cst=1 => Coef=0 for monomes of degree > 1
            ;
                tab_get_maxs_attr_names(AttrNames, MinOrMaxAttrs),   % get list of maximum values of the different attributes
                element(I, MinOrMaxAttrs, MinOrMaxAttr),             % get maximum value of the attribute that will be used in the max (a variable)
                BoolCtrTargetParams = bool_ctr_params(MinOrMaxAttr-1,% record information to later generate some constraint of the form
                                                      Cst-0)         % MinOrMaxAttr=1 and Cst=0 #=> Coef=0 for monomes of degree > 1
            ),
            functor(UTerm, U, 2),                                    % create the unary term min(Attr,Cst) or max(Attr,Cst)
            arg(1, UTerm, LogSum),
            arg(2, UTerm, LogCst)
        )                                                          ;
     U = geq ->                                                      % FIRST RESTRICT THE MIN/MAX VALUE OF THE CONSTANT (independant from the rows of the table)
        element(I, ListU, 1),                                        % I is the index of the attribute used in the unary term
        tab_get_mins_attr_names(AttrNames, MinAttrs),                % get the minimum values of the different attributes used in the formula
        tab_get_maxs_attr_names(AttrNames, MaxAttrs),                % get the maximum values of the different attributes used in the formula
        element(I, MinAttrs, MinLow),                                % MinLow corresponds to the minimum value of the attribute used in the unary term
        element(I, MaxAttrs, MaxUp),                                 % MaxUp corresponds to the maximum value of the attribute used in the unary term
        arg(1, UnaryTerm, Cst),                                      % get that constant (a variable as constant not yet known)
        Cst #> MinLow,                                               % start of interval should be at least equal to MinLow
        Cst #< MaxUp,                                                % end of interval should be at most equal to MaxUp
                                                                     % SECOND: CREATE A PROTOTYPE CONSTRAINT THAT WILL BE USED ON EACH ROW OF THE TABLE
                                                                     % (TO SORT OUT: BUG REDUNDANT WITH OTHER BUG: KEEP ONLY THE BEST)
                                                                     % put together all logical variables: LogRes will be used in the template
                                                                     % associated with a polynom which uses this binary term
        append([[LogRes], LogAttributes, LogListU, [LogCst]], SourceVars),
        append(ListU, [Cst], TargetVars),                            % record the Boolean variables and the constant variable in the target variables
% -> modif start
        functor(SourceTerm, ',', 2),                                 % create the source term: LogRes in 0..1, LogRes #<=> (LogSum #>= LogCst)
        arg(1, SourceTerm, LogRes in 0..1),                          % first  constraint of the term
        arg(2, SourceTerm, ReifTerm),                                % second constraint of the term
        functor(ReifTerm, #<=>, 2),                                  % create constraint LogRes #= U(LogSum,LogCst)
        arg(1, ReifTerm, LogRes),
        arg(2, ReifTerm, UTerm),
% <- modif end
        functor(UTerm, #>=, 2),
        arg(1, UTerm, LogSum),
        arg(2, UTerm, LogCst)                                      ;

     U = in ->                                                       % FIRST RESTRICT THE MIN/MAX VALUE OF THE INTERVAL (independant from the rows of the table)
        element(I, ListU, 1),                                        % I is the index of the attribute used in the unary term
        tab_get_mins_attr_names(AttrNames, MinAttrs),                % get the minimum values of the different attributes used in the formula
        tab_get_maxs_attr_names(AttrNames, MaxAttrs),                % get the maximum values of the different attributes used in the formula
        element(I, MinAttrs, MinLow),                                % MinLow corresponds to the minimum value of the attribute used in the unary term
        element(I, MaxAttrs, MaxUp),                                 % MaxUp corresponds to the maximum value of the attribute used in the unary term
        Low #>= MinLow,                                              % start of interval should be at least equal to MinLow
        Up  #=< MaxUp,                                               % end of interval should be at most equal to MaxUp
        Low #> MinLow #\/ Up #< MaxUp,                               % interval should not include the full range of values
        append([[LogRes], LogAttributes, LogListU, [LogLow,LogUp]],  % SECOND: CREATE A PROTOTYPE CONSTRAINT THAT WILL BE USED ON EACH ROW OF THE TABLE
               SourceVars),                                          % put together all logical variables
        arg(1, UnaryTerm, Low),                                      % get the Low variable (i.e. start of the interval of values) and
        arg(2, UnaryTerm, Up),                                       % get the Up  variable (i.e. end   of the interval of values)
        append(ListU, [Low,Up], TargetVars),                         % record the Boolean variables and the values variable in the target variables
% -> modif start
        functor(SourceTerm, ',', 2),                                 % create the source term: LogRes in 0..1, LogRes #<=> (LogLow#=<LogSum #/\ LogSum#=<LogUp)
        arg(1, SourceTerm, LogRes in 0..1),                          % first  constraint of the term
        arg(2, SourceTerm, ReifTerm),                                % second constraint of the term
        functor(ReifTerm, #<=>, 2),                                  % create the a constraint of the form
        arg(1, ReifTerm, LogRes),                                    % LogRes #<=> (Low #=< LogSum #/\ LogSum #=< Up)
        arg(2, ReifTerm, (LogLow#=<LogSum #/\ LogSum#=<LogUp))     ;
% <- modif end

     U = power ->
        append([[LogRes], LogAttributes, LogListU], SourceVars),     % put together all logical variables
        TargetVars = ListU,                                          % record the Boolean variables
        arg(1, UnaryTerm, D),                                        % get the exponent of the power
        functor(SourceTerm, #=, 2),                                  % create the source term: a constraint of the form LogRes #= LogSum^D
        arg(1, SourceTerm, LogRes),
        arg(2, SourceTerm, UTerm),
        gen_power_template(D, LogSum, UTerm)                       ; % create the term UTerm = LogSum^d
     U = sum_consec ->
        append([[LogRes], LogAttributes, LogListU], SourceVars),     % put together all logical variables
        TargetVars = ListU,                                          % record the Boolean variables
        functor(SourceTerm, #=, 2),                                  % create the source term: a constraint of the form LogRes #= 1+2+...+LogSum
        arg(1, SourceTerm, LogRes),
        arg(2, SourceTerm, UTerm),
        UTerm = ((LogSum * (LogSum + 1)) div 2)                    ; % create the term UTerm = 1+2+...+LogSum
        write(gen_unary_template), nl, halt
    ),
    (var(BoolCtrTargetParams) ->                                     % if BoolCtrTargetParams remains uninitialized then fix it to none
        BoolCtrTargetParams = none
    ;
        true
    ).

gen_binary_templates(FormulaPattern, AttrNames, LenA, LogAttributes, LogResBinary) :-
    formula_pattern(attributes_use(AttributesUse), FormulaPattern), % get the used attributes
    formula_pattern(binary(BinaryTerms), FormulaPattern),           % extract binary functions
    memberchk(b(ListsB), AttributesUse),                            % extract 0/1 variables telling which attribute a binary term use
    gen_binary_templates1(BinaryTerms, ListsB, AttrNames, LenA, LogAttributes, % create the different binary templates
                          BinaryTemplates, LogResBinary),
    formula_pattern(tbinary(BinaryTemplates), FormulaPattern).      % record the binary templates that were built

gen_binary_templates1([], [], _, _, _, [], []) :- !.
gen_binary_templates1([BinaryTerm|Q], [ListB|R], AttrNames, LenA, LogAttributes, [BinaryTemplate|S], [LogRes|T]) :-
    gen_binary_template(BinaryTerm, ListB, AttrNames, LenA, LogAttributes, BinaryTemplate, LogRes),
    gen_binary_templates1(Q, R, AttrNames, LenA, LogAttributes, S, T).

gen_binary_template(BinaryTerm, ListB, AttrNames, LenA, LogAttributes, BinaryTemplate, LogRes) :-
    arg(1, BinaryTerm, Order),                                               % get real order variable
    gen_integer_list_from_low_to_up(1, LenA, Positions),
    automaton(Positions, Pi, ListB,                                          % automaton constraint for getting the two indexes corresponding
              [source(s),sink(f)],                                           % to the positions of the two "1" (giving the two attributes used
              [arc(s,0,s), arc(s,1,r, [Pi,I2]),                              % in the binary term)
               arc(r,0,r), arc(r,1,f, [I1,Pi]),
               arc(f,0,f)],
               [I1,I2], [0,0], [Ind1,Ind2]),
    Ind1 #< Ind2,                                                            % get stronger propagation as automaton not Berge-acyclic (use registers)
    BinaryTemplate = t(SourceVars,                                           % logical var.giving attr.values,the 0/1 var.telling which attr.is used
                       SourceTerm,                                           % binary term that uses the logical variables
                       TargetVars),                                          % the real 0/1 var, (IGNORE input values as well as result of element ctrs
                                                                             % as they depend of each table entry)
    functor(BinaryTerm, B, 1),                                               % get binary function of the binary term
    Ctr1 = element(LogInd1, LogAttributes, LogAttr1),                        % first  element ctr. linking first  index and value of first attribute
    Ctr2 = element(LogInd2, LogAttributes, LogAttr2),                        % second element ctr. linking second index and value of second attribute
    (B = mfloor ->                                                           % if mfloor then need to get the minimum value of the second attribute
        tab_get_mins_attr_names(AttrNames, MinAttrs),
        element(Ind1, MinAttrs, MinAttr1),
        element(Ind2, MinAttrs, MinAttr2),
        Order #= 0 #=> MinAttr2 #> 0,                                        % min.value of the attribute corresponding to the denominator of mfloor
        Order #= 1 #=> MinAttr1 #> 0                                         % should be greater than 0
    ;
        true
    ),
    (memberchk(B, [min,max,prod]) ->                                         % no ordering alternative as commutative functions (no LogOrder variable!)
                                                                             % put together all logical variables: LogRes will be used in the template
                                                                             % associated with a polynom which uses this binary term
        append([[LogRes],LogAttributes,[LogAttr1,LogAttr2,LogInd1,LogInd2]], SourceVars),
        TargetVars = [Ind1,Ind2],                                            % put together all corresponding real variables (no Order variable!)
        functor(Ctr3, #=, 2),                                                % create an equality constraint
        arg(1, Ctr3, LogRes),                                                % left hand side of the equality constraint Ctr3
        (B = prod ->
            arg(2, Ctr3, (LogAttr1 * LogAttr2))                              % create a constraint of the form LogRes = Attr1*Attr2
        ;
            functor(Term1, B, 2),                                            % create a constraint of the form LogRes = min(Attr1,Attr2), or
            arg(1, Term1, LogAttr1),                                         %                                 LogRes = max(Attr1,Attr2)
            arg(2, Term1, LogAttr2),
            arg(2, Ctr3, Term1)                                              % right hand side of the equality constraint Ctr3
        )
    ;                                                                        % non commutative func.: have an ordering alternative (use LogOrder var!)
                                                                             % put together all logical variables: LogRes will be used in the template
                                                                             % associated with a polynom which uses this binary term
        (B = cmod ->                                                         % convert cmod(Attr1,Attr2) to Attr1 - Attr2 mod Attr1
            append([[LogRes],LogAttributes,[LogAttr1,LogAttr2,LogInd1,LogInd2,LogOrder]], SourceVars),
            TargetVars = [Ind1,Ind2,Order],                                  % put together all corresponding real variables (including the Order var.)
            
            ((integer(Order), Order=1) ->                                    % if order variable already fixed to 1 then no need for creating Term1
                true                                                         % as the result will be just Term2
            ;
                Term1 = (LogAttr1 - (LogAttr2 mod LogAttr1))                 % first  alternative wrt the Order variable
            ),
            ((integer(Order), Order=0) ->                                    % if order variable already fixed to 0 then no need for creating Term2
                true                                                         % as the result will be just Term1
            ;
                Term2 = (LogAttr2 - (LogAttr1 mod LogAttr2))                 % second alternative wrt the Order variable
            )
        ;
         B = dmod ->                                                         % convert dmod(Attr1,Attr2) to Attr1 - Attr1 mod Attr2
            append([[LogRes],LogAttributes,[LogAttr1,LogAttr2,LogInd1,LogInd2,LogOrder]], SourceVars),
            TargetVars = [Ind1,Ind2,Order],                                  % put together all corresponding real variables (including the Order var.)
            ((integer(Order), Order=1) ->                                    % if order variable already fixed to 1 then no need for creating Term1
                true                                                         % as the result will be just Term2
            ;
                Term1 = (LogAttr1 - (LogAttr1 mod LogAttr2))                 % first  alternative wrt the Order variable
            ),
            ((integer(Order), Order=0) ->                                    % if order variable already fixed to 0 then no need for creating Term2
                true                                                         % as the result will be just Term1
            ;
                Term2 = (LogAttr2 - (LogAttr2 mod LogAttr1))                 % second alternative wrt the Order variable
            )
        ;
         B = fmod ->                                                         % convert fmod(Attr1,Attr2) to
            append([[LogRes],LogAttributes,[LogAttr1,LogAttr2,LogInd1,LogInd2,LogOrder]], SourceVars),
            TargetVars = [Ind1,Ind2,Order],                                  % put together all corresponding real variables (including the Order var.)
                                                                             % ([Attr1 mod Attr2=0])*Attr2 + ([Attr1 mod Attr2>0])*(Attr1 mod Attr2)
            ((integer(Order), Order=1) ->                                    % if order variable already fixed to 1 then no need for creating Term1
                true                                                         % as the result will be just Term2
            ;                                                                % first  alternative wrt the Order variable
                Term1 = (LogAttr1 mod LogAttr2 #= 0)*LogAttr2 + (LogAttr1 mod LogAttr2 #> 0)*(LogAttr1 mod LogAttr2)
            ),
            ((integer(Order), Order=0) ->                                    % if order variable already fixed to 0 then no need for creating Term2
                true                                                         % as the result will be just Term1
            ;                                                                % second alternative wrt the Order variable
                Term2 = (LogAttr2 mod LogAttr1 #= 0)*LogAttr1 + (LogAttr2 mod LogAttr1 #> 0)*(LogAttr2 mod LogAttr1)
            )
        ;
            (memberchk(B, [floor,ceil,mfloor]) -> BB = div ; BB = B),        % convert floor/mfloor/ceil to div
            (B = mfloor ->
                append([[LogRes],LogAttributes,[LogAttr1,LogAttr2,LogInd1,LogInd2,LogMinAttr1,LogMinAttr2,LogOrder]], SourceVars),
                TargetVars = [Ind1,Ind2,MinAttr1,MinAttr2,Order]             % put together all corresponding real variables (including the Order var.)
            ;
                append([[LogRes],LogAttributes,[LogAttr1,LogAttr2,LogInd1,LogInd2,LogOrder]], SourceVars),
                TargetVars = [Ind1,Ind2,Order]                               % put together all corresponding real variables (including the Order var.)
           ),
            ((integer(Order), Order=1) ->                                    % if order variable already fixed to 1 then no need for creating Term1
                true                                                         % as the result will be just Term2
            ;
                functor(Term1, BB, 2),                                       % Term1 = binary function(Attr1,Attr2) (alternative 1 wrt the Order var.)
                (B = ceil   -> arg(1, Term1, (LogAttr1+LogAttr2-1))       ;  % convert ceil to floor if necessary
                               arg(1, Term1,  LogAttr1)                   ), % the numerator   of the fraction (same numerator for floor and mfloor)
                (B = mfloor -> arg(2, Term1, max(LogAttr2-LogMinAttr2,1)) ;  % the denominator of the fraction (same denominator for floor and ceil)
                               arg(2, Term1, LogAttr2)                    )
            ),
            ((integer(Order), Order=0) ->                                    % if order variable already fixed to 0 then no need for creating Term2
                true                                                         % as the result will be just Term1
            ;
                functor(Term2, BB, 2),                                       % Term2 = binary function(Attr2,Attr1) (alternative 2 wrt the Order var.)
                (B = ceil   -> arg(1, Term2, (LogAttr2+LogAttr1-1))       ;  % convert ceil to floor if necessary
                               arg(1, Term2,  LogAttr2)                   ), % the numerator   of the fraction (same numerator for floor and mfloor)
                (B = mfloor -> arg(2, Term2, max(LogAttr1-LogMinAttr1,1)) ;  % the denominator of the fraction (same denominator for floor and ceil)
                               arg(2, Term2, LogAttr1)                    )
            )
        ),
        functor(Ctr3, #=, 2),                                                % create an equality constraint for returning the result of the function
        arg(1, Ctr3, LogRes),                                                % left-hand side of the equality constraint
        ((integer(Order), Order=1) ->                                        % when Order already fixed to 1 then right-hand side is just Term2
            arg(1, Ctr3, Term2)
        ;
         (integer(Order), Order=0) ->                                        % when Order already fixed to 0 then right-hand side is just Term1
            arg(1, Ctr3, Term1)
        ;                                                                    % when Order not already fixed then the right-hand side is:
                                                                             %  . if LogOrder=0 then LogRes=Term1 (returns function(Attr1,Attr2))
                                                                             %  . if LogOrder=1 then LogRes=Term2 (returns function(Attr2,Attr1))
            arg(2, Ctr3, (((1-LogOrder) * Term1) + (LogOrder * Term2)))
        )
    ),
    SourceTerm = [Ctr1,Ctr2,Ctr3].                                           % put together the three constraints that were created

gen_polynom_templates(FormulaPattern, LogAttributesUnaryBinary, LogResPolynom) :-
    formula_pattern(polynoms(Polynoms),     FormulaPattern),                                                     % extract polynoms
    formula_pattern(tunary(UnaryTemplates), FormulaPattern),                                                     % extract unary templates
    gen_polynom_templates1(Polynoms, UnaryTemplates, LogAttributesUnaryBinary, PolynomTemplates, LogResPolynom), % generate a template for each polynom
    formula_pattern(tpolynom(PolynomTemplates), FormulaPattern).                                                 % record the polynoms templates that were built

gen_polynom_templates1([], _, _, [], []) :- !.
gen_polynom_templates1([Polynom|R], UnaryTemplates, LogAttributesUnaryBinary, [PolynomTemplate|S], [LogRes|T]) :-
    gen_polynom_template(Polynom, UnaryTemplates, LogAttributesUnaryBinary, PolynomTemplate, LogRes),            % create the template for the polynom Polynom
    gen_polynom_templates1(R, UnaryTemplates, LogAttributesUnaryBinary, S, T).

gen_polynom_template(Polynom, UnaryTemplates, LogAttributesUnaryBinary, PolynomTemplate, LogRes) :-
    PolynomTemplate = t(SourceVars,
                        SourceTerm,
                        TargetVars),
    length(Polynom, NbMonomes),                                                                    % get number of monomes of the polynome
    length(LogCoefMonomes, NbMonomes),                                                             % create logical coef. variable for each monome
    append([[LogRes], LogAttributesUnaryBinary, LogCoefMonomes], SourceVars),                      % put together all logical variables of the polynom
    gen_sum_monomes_source_term(Polynom, UnaryTemplates, LogCoefMonomes, LogAttributesUnaryBinary, % create the source sum of the monomes and the target variables
                                SourceSumMonomes, TargetVars),
    functor(SourceTerm, #=, 2),                                                                    % create a source term of the form LogRes #= SourceSumMonomes
    arg(1, SourceTerm, LogRes),
    arg(2, SourceTerm, SourceSumMonomes).

gen_sum_monomes_source_term([], _, [], _, 0, []) :- !.
gen_sum_monomes_source_term([t(_,Degrees,Coef)|Q],                         % t(_,Degrees,Coef):current monome of the polynom for which construct source template
                             UnaryTemplates,                               % if unary template<>none contains information for stating a constraint on Coef
                             [LogCoef|R],                                  % LogCoef: logical coefficient of current monome
                             LogAttributesUnaryBinary,                     % logical vars.of the polynom (access to attributes, unary terms, binary terms values)
                             (LogCoef*LogDegrees)+S,                       % (LogCoef*LogDegrees): logical term associated with current monome
                             [Coef|T]) :-                                  % Coef: real coefficient of current monome that is used in the target variables
    gen_monome_source_term(Degrees, UnaryTemplates, Coef,                  % generate the logical monome (pass Coef as may have to use it to set up a constraint for
                           LogAttributesUnaryBinary, LogDegrees),          % preventing generating term of the form min(V,1)^d or max(V,0)^d with d>1
    gen_sum_monomes_source_term(Q, UnaryTemplates, R,                      % handle the rest of the monomes of the polynom
                                LogAttributesUnaryBinary, S, T).

gen_monome_source_term([], _, _, [], 1) :- !.                              % multiply by 1 the monome
gen_monome_source_term([0|R], UnaryTemplates, Coef, [_|S], T) :- !,        % if current degree is 0 we skip the corresponding attribute/unary term/binary term
    gen_monome_source_term(R, UnaryTemplates, Coef, S, T).                 % and continue for the next attributes/unary terms/binary terms
gen_monome_source_term([D|R], UnaryTemplates, Coef, [LogAttrUnaryBinary|S], LogAttrPowerD*T) :-
    D > 0,                                                                 % if current degree is >0 then
    ((D > 1,                                                               % if degree of current monome is greater than one
                                                                           % and we have a unary term corresponds to min(_,1) or max(_,0)
                                                                           % then post a constraint that avoid having a Boolen unary term with a degree > 1 by
      UnaryTemplates = [t(_,_,_,bool_ctr_params(MinOrMaxAttr-ValMinOrMaxAttr,Cst-ValCst))]) ->
        MinOrMaxAttr #= ValMinOrMaxAttr #/\ Cst #= ValCst #=> Coef #= 0    % forcing the monome coefficient to be equal to 0 if the situation occurs
    ;
        true
    ),
    gen_power_template(D, LogAttrUnaryBinary, LogAttrPowerD),              % generate the current attribute/unary term/binary term to the power D
    gen_monome_source_term(R, UnaryTemplates, Coef, S, T).                 % and continue for the next attributes/unary terms/binary terms

gen_unaryf_template(FormulaPattern, LogResPolynom) :-
    formula_pattern(f(F), FormulaPattern),
    (F = 2 ->                                                        % we have a unary function
        formula_pattern(unaryf([UnaryFunction]), FormulaPattern),    % get the unary function, and
        functor(UnaryFunction, UFunction, _),
        UnaryfTemplate = [t(SourceVars,
                            SourceTerm,
                            TargetVars)],
        LogResPolynom = [LogResPoly],                                % logical result variables of the polynom
        (memberchk(UFunction, [min,max,floor,mod]) ->
            arg(1, UnaryFunction, Cst),                              % get variable corresponding to the real constant
            functor(Ctr1, #=, 2),                                    % create the template for the main constraint: 
            arg(1, Ctr1, LogRes),                                    %  LogRes #= function(LogResPoly,LogCst)
            arg(2, Ctr1, UTerm),
            (UFunction = mod ->
                UTerm = (LogResPoly mod LogCst)
            ;
                (UFunction = floor -> UFunc = div ; UFunc = UFunction),
                functor(UTerm, UFunc, 2),                            % create the term function(LogResPoly,LogCst)
                arg(1, UTerm, LogResPoly),
                arg(2, UTerm, LogCst)                                   
            ),                                                       % logical vars.of the template: 
            append([[LogRes], LogResPolynom, [LogCst]], SourceVars), % result of unary func., logical result var. of the polynom, and constant variable
            TargetVars = [Cst],                                      % only Cst as the other variables depends of the table entries
            SourceTerm = [Ctr1]                                    ;

         UFunction = geq0 ->
% -> modif start
            functor(Ctr1, ',' , 2),                                  % create the source term: LogRes in 0..1, LogRes #<=> (LogResPoly #>= 0)
            arg(1, Ctr1, LogRes in 0..1),                            % first  constraint of the term
            arg(2, Ctr1, ReifTerm),                                  % second constraint of the term
            functor(ReifTerm, #<=>, 2),                              % create constraint LogRes #<=> (LogResPoly #>= 0)
            arg(1, ReifTerm, LogRes),
            arg(2, ReifTerm, UTerm),
% <- modif end

            functor(UTerm, #>=, 2),
            arg(1, UTerm, LogResPoly),                               % create the term (LogResPoly #>= 0)
            arg(2, UTerm, 0),
            append([LogRes], LogResPolynom, SourceVars),             % logical vars.of the template: result of unary func.and logical result var. of the polynom
            TargetVars = [],
            SourceTerm = [Ctr1]                                    ;

         UFunction = in ->
            arg(1, UnaryFunction, Low),                              % get real start of interval of values
            arg(2, UnaryFunction, Up),                               % get real end   of interval of values

% -> modif start
            functor(Ctr1, ',' , 2),                                  % create the source term: LogRes in 0..1,LogRes#<=>(LogLow#=<LogResPoly #/\ LogResPoly#=<LogUp)
            arg(1, Ctr1, LogRes in 0..1),                            % first  constraint of the term
            arg(2, Ctr1, ReifTerm),                                  % second constraint of the term
            functor(ReifTerm, #<=>, 2),                              % create constraint LogRes #<=> (LogLow #=< LogResPoly #/\ LogResPoly #=< LogUp)
            arg(1, ReifTerm, LogRes),
            arg(2, ReifTerm, (LogLow#=<LogResPoly #/\ LogResPoly#=<LogUp)),
% <- modif end
                                                                     % result of unary func., logical result var. of the polynom, and low/up variables
            append([[LogRes],LogResPolynom,[LogLow,LogUp]], SourceVars),
            TargetVars = [Low,Up],                                   % only Values as the other variables depends of the table entries
            SourceTerm = [Ctr1]                                    ;





         UFunction = power ->
            arg(1, UnaryFunction, Degree),                           % get degree of the polynom
            gen_power_template(Degree, LogResPoly, PowerTerm),
            functor(Ctr1, #=, 2),                                    % create the template for the main constraint: 
            arg(1, Ctr1, LogRes),                                    %  LogRes #= LogResPoly^Degree
            arg(2, Ctr1, PowerTerm),
            append([[LogRes],LogResPolynom],SourceVars),             % result of unary func., logical result var. of the polynom
            TargetVars = [],
            SourceTerm = [Ctr1]                                    ;
         UFunction = sum_consec ->
            functor(Ctr1, #=, 2),                                    % create the template for the main constraint:
            arg(1, Ctr1, LogRes),                                    %  LogRes #= ((LogResPoly * (LogResPoly + 1)) div 2)
            arg(2, Ctr1, UTerm),
            UTerm = ((LogResPoly * (LogResPoly + 1)) div 2),         % create the term ((LogResPoly * (LogResPoly + 1)) div 2)
            functor(Ctr2, #>=, 2),                                   % force the polynom to be greater than or equal to 0
            arg(1, Ctr2, LogResPoly),
            arg(2, Ctr2, 0),
            append([LogRes], LogResPolynom, SourceVars),             % logical vars.of the template: result of unary func.and logical result var. of the polynom
            TargetVars = [],
            SourceTerm = [Ctr1,Ctr2]                               ;
            write(gen_unaryf_template), nl, halt                   )
    ;
        UnaryfTemplate = []
    ),
    formula_pattern(tunaryf(UnaryfTemplate), FormulaPattern).        % record unary function templates that was built (or empty list if nothing was built)

gen_power_template(1, LogVar, LogVar) :- !.
gen_power_template(D, LogVar, LogVar*R) :-
    D > 1,
    D1 is D-1,
    gen_power_template(D1, LogVar, R).

gen_binaryf_template(FormulaPattern, LogResPolynom) :-
    formula_pattern(f(F), FormulaPattern),
    (F = 3 ->                                                       % we have a binary function between two polynoms
        formula_pattern(binaryf([BinaryFunction]), FormulaPattern), % get the binary function
        functor(BinaryFunction, BFunction, _),
        BinaryfTemplate = [t(SourceVars,
                             SourceTerm,
                             TargetVars)],
        append([LogRes], LogResPolynom, SourceVars),                % logical vars.of the template:result of bin.func.and logical results vars. of the polynoms
        TargetVars = [],                                            % no target variables as the variables depends of the table entries
        LogResPolynom = [LogResPoly1,LogResPoly2],                  % logical result variables of the two polynoms of the binary function
        functor(Ctr1, #=, 2),                                       % create the template for the main constraint:
        arg(1, Ctr1, LogRes),                                       %  LogRes #= function(LogResPoly1,LogResPoly2)
        arg(2, Ctr1, BTerm),
        (memberchk(BFunction,[ceil,floor]) -> BFunc = div      ;
         memberchk(BFunction, [cmod,dmod]) -> BFunc = mod      ;
                                              BFunc = BFunction),   % create the term function(LogResPoly1,LogResPoly2)
        (BFunction = ceil -> BTerm = ((LogResPoly1 + LogResPoly2 - 1) div LogResPoly2) ;
         BFunction = cmod -> BTerm = (LogResPoly1 - (LogResPoly2 mod LogResPoly1))     ;
         BFunction = dmod -> BTerm = (LogResPoly1 - (LogResPoly1 mod LogResPoly2))     ;
         BFunction = prod -> BTerm = (LogResPoly1*LogResPoly2)                         ;
                             functor(BTerm, BFunc, 2),              
                             arg(1, BTerm, LogResPoly1),
                             arg(2, BTerm, LogResPoly2)                                ),
        (memberchk(BFunction,[ceil,floor]) ->                       % if use ceil or floor then create a second constraint enforcing
            functor(Ctr2, #>, 2),                                   % the denominator (i.e. LogResPoly2) to be strictly greater than 0
            arg(1, Ctr2, LogResPoly2),
            arg(2, Ctr2, 0),
            SourceTerm = [Ctr1,Ctr2]            ;                   % the source term consists of two constraints when we use floor
         BFunction = prod ->                                        % if use prod then create a second constraint enforcing
           functor(Ctr2, #>=, 2),                                   % the first polynom to be greater than or equal to 0
           arg(1, Ctr2, LogResPoly1),
           arg(2, Ctr2, 0),
           SourceTerm = [Ctr1,Ctr2]             ;                   % the source term consists of two constraints when we use prod
         memberchk(BFunction, [mod,dmod]) ->                        % if use mod or dmod then create:
            functor(Ctr2, #>, 2),                                   %  . a second constraint enforcing LogResPoly2 to be strictly greater than 1
            arg(1, Ctr2, LogResPoly2),
            arg(2, Ctr2, 1),
            functor(Ctr3, #>=, 2),                                  %  . a third constraint enforcing LogResPoly1 to be greater than or equal to 0
            arg(1, Ctr3, LogResPoly1),
            arg(2, Ctr3, 0),
            SourceTerm = [Ctr1,Ctr2,Ctr3]        ;                  % the source term consists of three constraints when we use mod or dmod
         BFunction = cmod ->                                        % if use mod or cmod then create:
            functor(Ctr2, #>, 2),                                   %  . a second constraint enforcing LogResPoly1 to be strictly greater than 1
            arg(1, Ctr2, LogResPoly1),
            arg(2, Ctr2, 1),
            functor(Ctr3, #>=, 2),                                  %  . a third constraint enforcing LogResPoly2 to be greater than or equal to 0
            arg(1, Ctr3, LogResPoly2),
            arg(2, Ctr3, 0),
            SourceTerm = [Ctr1,Ctr2,Ctr3]        ;                  % the source term consists of three constraints when we use cmod
         memberchk(BFunction, [min,max]) ->                         % if use min, max or prod then
            SourceTerm = [Ctr1]                  ;                  % the source term consists of one single constraint
            write(gen_binaryf_template), nl, halt)
    ;
        BinaryfTemplate = []
    ),
    formula_pattern(tbinaryf(BinaryfTemplate), FormulaPattern).     % record binary function templates that was built (or empty list if nothing was built)

% GENERATE ADDITIONAL CONSTRAINTS USING THE METADATA AS WELL AS THE TARGET COLUMN (i.e. the column for which we compute a formula)
%---------------------------------------------------------------------------------------------------------------------------------
formula_pattern_gen_additional_ctr(FormulaPattern, TargetTabCol) :-
    formula_pattern(f(1),  FormulaPattern),                                            % the main formula is just a polynom
    formula_pattern(nu(0), FormulaPattern),                                            % the number of unary  terms is 0
    formula_pattern(nb(0), FormulaPattern),                                            % the number of binary terms is 0
    !,
    formula_pattern(polynoms([Polynom]), FormulaPattern),                              % extract the unique polynom (as F=1)
    tab_get_gcd(TargetTabCol, GcdTarget),
    get_polynom_coefs(Polynom, Coefs),
    (GcdTarget = 0 ->
        get_max_abs_dvars(Coefs, MaxAbs),
        Primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97],
        findall(Prime, (member(Prime, Primes), Prime =< MaxAbs), LPrimes),
        gcd_eq_one_ctr(LPrimes, Coefs),
        set_first_coef_diff_from_zero_to_positive(Coefs)
    ;
        true
    ).
formula_pattern_gen_additional_ctr(_, _).

get_polynom_coefs([], []) :- !.
get_polynom_coefs([t(_,_,Coef)|R], [Coef|S]) :-
    get_polynom_coefs(R, S).

gcd_eq_one_ctr([], _) :- !.
gcd_eq_one_ctr([P|R], L) :- gcd_eq_one_ctr(L, P, Disj), call(Disj), gcd_eq_one_ctr(R, L).

gcd_eq_one_ctr([], _, 0) :- !.
gcd_eq_one_ctr([V|R], P, V mod P #> 0 #\/ S) :- gcd_eq_one_ctr(R, P, S).

set_first_coef_diff_from_zero_to_positive(Coefs) :-                     % as coefficients are in reverse order: first coefficient
    reverse(Coefs, RevCoefs),                                           % corresponds to the last parameter, and last coefficient
    RevCoefs = [CstTerm|RestRevCoefs],                                  % to the constant term !!! E.g., if the parameters are
    append(RestRevCoefs, [CstTerm], Coefficients),                      % v, maxc, mins, and we have a linear term, then Coefs is the
    set_first_coef_diff_from_zero_to_positive1(Coefficients, Term),     % list of coefficients [Coef_mins,Coef_maxc,Coef_v,CstTerm];
    call(Term).                                           % but post the ctr. on [Coef_v,Coef_maxc,Coef_mins,CstTerm]

set_first_coef_diff_from_zero_to_positive1([C], C #>= 0) :- !.
set_first_coef_diff_from_zero_to_positive1([C1,C2|R], (C1 #> 0 #\/ (C1 #= 0 #/\ S))) :-
    set_first_coef_diff_from_zero_to_positive1([C2|R], S).
