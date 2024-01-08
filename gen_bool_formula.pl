% Purpose: UTILITIES USED TO GENERATE A PARAMETERISED BOOLEAN CANDIDATE FORMULA WRT SOME GIVEN OPTIONS,
%          AND ON BACTRACKING ON THE MAIN OPERATOR OF THE FORMULA PRODUCE THE NEXT CANDIDATE FORMULA
% Authors: Nicolas Beldiceanu, Ramiz Gindullin, IMT Atlantique

:- module(gen_bool_formula,[formula_generator_bool_conds/5,            % generate Boolean candidate formula where coefficient are not yet fixed
                            pos_bool_cond_entry/2,                     % position of an entry of a Boolean condition
                            get_bool_cost_vars/2,                      % get all cost vars.of all the conditions of a Boolean candidate formula
                            get_entries_of_all_boolean_conditions/3,
                            avoid_simplifiable_conditional/9]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(utility).
:- use_module(table_access).
:- use_module(bool_cond_utility).
%:- use_module(forbid_specific_combinations_of_conds).                 % manual constraints rules preventing simplifiable expressions
:- use_module(forbid_specific_combinations_of_conds_generated).        % synthesised constraints rules preventing simplifiable expressions

all_bool_options(binary, [plus,minus,abs,min,max,                      % all binary functions which can be used in condition of a boolean expression
                          prod,floor,mfloor,ceil,mod,
                          cmod,dmod,fmod]).

all_mod_conds([minus_mod_eq0,
               attr_geq_fmod,
               unary_term_eq_coef(mod),
               unary_term_geq_coef(mod),
               unary_term_leq_coef(mod),
               binary_term_eq_coef(mod),
               binary_term_eq_coef(cmod),
               binary_term_eq_coef(dmod),
               binary_term_eq_coef(fmod),
               binary_term_leq_coef(mod),
               binary_term_leq_coef(cmod),
               binary_term_leq_coef(dmod),
               binary_term_leq_coef(fmod),
               binary_term_geq_coef(mod),
               binary_term_geq_coef(cmod),
               binary_term_geq_coef(dmod),
               binary_term_geq_coef(fmod)]).                            % fact listing all boolean terms containing mod

% position in term corresponding to a Boolean condition of an attribute describing the Boolean condition
% (see CreatedCond in the predicate formula_generator_bool_cond1)
% used to access to term elements in a generic way
pos_bool_cond_entry(condb,          1) :- !.
pos_bool_cond_entry(newcondtype,    2) :- !.
pos_bool_cond_entry(lcondattr,      3) :- !.
pos_bool_cond_entry(lcondextravars, 4) :- !.
pos_bool_cond_entry(condcostvars  , 5) :- !.
pos_bool_cond_entry(lcondcst,       9) :- !.
pos_bool_cond_entry(lcondcoef,      10):- !.

% From the options describing a Boolean formula, generates the corresponding Boolean formula (its constraints, source variables).
%
% INPUT PARAMETER
%  BoolParams      : list of terms representing the options of the Boolean formula, that is a list of the form
%                    [mandatory_attr(_),neg(_),op(_),nb_terms(_),conds(_)]
%  ColSetsBool     : for each input parameter give the set of possible values for the constant when it is used in a Boolean comparaison
%                    between that input parameter and a constant that need to be found, used in CondAttr1 = CondCoef, CondAttr1 =< CondCoef,
%                    CondAttr1 >= CondCoef for restricting CondCoef
%  ShiftBool       : constant from which shift the Boolean formula (minimum value of the output column)
%
% OUTPUT PARAMETER
%  NbUnary              : number of selected unary   conditions (a variable usually)
%  NbBinary             : number of selected binary  conditions (a variable usually)
%  NbTernary            : number of selected ternary conditions (a variable usually)
%  AdjustMaxOccAttrToOne: set to 1 if each attribute should be used exactly once in the Boolean condition
%  BoolConds            : list of the following form [mandatory_attr(_), neg(_), op(_), nb_terms(_), conds([_|_])]
%                         where each condition of conds has the following form:
%
%  CondB           : -1 if condition unused, 0 if negation of condition used, 1 if condition used
%  NewCondType     : condition type used (keep eventual unary/binary term, but remove the constants used to declare a domain)
%  LCondAttr       : target attribute vars.indicating which attribute the condition uses (indexes of the used attribute within MandatoryAttr)
%  LCondExtraVars  : extra target variables used by the condition (label first on the attribute variables)
%  LCondCostVars   : cost variables for computing the cost of the cond part (absolute value of constant used in unary term of the condition)
%  CondAttr1       : index of the first attribute used by the condition (between 1 and LenA)
%  CondAttr2       : index of the eventual second attribute used by the condition (0 if unused, between 1 and LenA otherwise)
%  CondAttr3       : index of the eventual third  attribute used by the condition (0 if unused, between 1 and LenA otherwise)
%  CondCst         : constant associated with the condition (0 if unused)
%  CondCoef        : coefficient associated with the condition (0 if unused)
%  SourceVarsLocal : source variables that are specific for each table entry (we use the convention that the first source var. corresponds
%                    to the result of the condition and the last sources vars.correspond to the entries of the table, i.e. LogAttributes)
%  SourceVarsGlobal: source variables that are the same for each table entry
%  TargetVarsGlobal: target variables that are the same for each table entry
%  SourceCtrs      : list of constraints that will be posted on each table entry
%  CondCheckOrder  : set to the position of the LogCondOrder var.in the list SourceVarsLocal if will have to check that both arguments of
%                    a min or max unary function contribute after posting all the ctrs on all table entries, set to 0 if no check to do
%
formula_generator_bool_conds(BoolParams, ColSetsBool, ShiftBool, t(NbUnary,NbBinary,NbTernary,AdjustMaxOccAttrToOne), BoolConds) :-
    (memberchk(mandatory_attr(MandatoryAttr), BoolParams) -> true ; write(formula_generator_bool_conds1), nl, halt),
    (memberchk(neg(Negated),                  BoolParams) -> true ; write(formula_generator_bool_conds2), nl, halt),
    (memberchk(op(LOplus),                    BoolParams) -> true ; write(formula_generator_bool_conds3), nl, halt),
    (memberchk(nb_terms(MinTerm,MaxTerm),     BoolParams) -> true ; write(formula_generator_bool_conds4), nl, halt),
    (memberchk(conds(LConds),                 BoolParams) -> true ; write(formula_generator_bool_conds5), nl, halt),
    (foreach(OO,LOplus)
        do (memberchk(OO,[and,or,sum,allequal,xor,voting,card1]) -> true ; write(formula_generator_bool_conds6(OO)), nl, halt)
    ),                                                % access to the element of a condition (indices in the term)
    (memberchk(use_mods(UseMods),             BoolParams) -> true ; write(formula_generator_bool_conds6a), nl, halt),
    member(Oplus, LOplus),                            % select the operator of the Boolean formula
    pos_bool_cond_entry(condb,       PosCondB),
    pos_bool_cond_entry(newcondtype, PosNewCondType),
    pos_bool_cond_entry(lcondattr,   PosLCondAttr),
    ((integer(MinTerm), integer(MaxTerm), MinTerm > 0, MinTerm =< MaxTerm) -> true ; write(formula_generator_bool_conds7), nl, halt),
    NbTerms in MinTerm..MaxTerm,
    indomain(NbTerms),                                % select number of terms of the formula by increasing number of terms
    length(MandatoryAttr, NbAttrs),                   % get number of mandatory attributes
    gen_compatible_unary_and_binary_and_ternary(      % generate list of triples of compatible unary, binary, and ternary conditions in order
                                    NbTerms, NbAttrs, % to generate enough terms wrt number of mandatory attributes
                                    UnariesBinariesTernaries),
    (UnariesBinariesTernaries = [] -> false ; true),  % generate a failure if NbTerms not compatible with NbAttrs
    % get all Boolean mandatory attributes as they can only occur in conditions of type attr_eq_attr or attr_eq_coef
    % (MinAttrs and MaxAttrs will also used for posting some constraint for specific Boolean conditions)
    %.....................................................................................................................................
    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of min. and max. values of the mandatory attr.
    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % as well as list of index+1 of Boolean attributes
    get_shifted_bool_attributes(MinAttrs, MaxAttrs, 2, ShiftedBoolAttrs),
    (length(ShiftedBoolAttrs, NbAttrs) ->             % if all attributes are Boolean then use the following list of conditions/nber of occ.
        (NbTerms = 1 -> Negated = 0 ; true),          % if only one term allow just to negate the inner condition (but not the full condition)
        BoolOnly = 1,
        MinNbTermsNbAttrs is min(NbTerms,NbAttrs),
        LCondsBool = [t(cond(attr_eq_coef(coef(0,1))), no_negation,    MinNbTermsNbAttrs),
                      t(cond(attr_eq_attr),            no_restriction, MinNbTermsNbAttrs)],
        duplicate_bool_conds(LCondsBool, NbTerms, NewLConds)
    ;                                                 % use standard option: duplicate each condition wrt it maximum number of occurrences
        BoolOnly = 0,
        duplicate_bool_conds(LConds, NbTerms, NewLConds)
    ),

    BoolConds = [                                     % create the descriptor of the Boolean formula:
                 mandatory_attr(MandatoryAttr),       %  . record mandatory attrs.used over all terms of the Boolean formula (no optional attr.)
                 neg(Negated),                        %  . record whether the Boolean formula is negated or not
                 shift(ShiftBool),                    %  . record shift that is added to the Boolean formula (min of output column)
                 op(Oplus),                           %  . record selected operator
                 nb_terms(NbTerms),                   %  . record selected number of conditions of the Boolean formula
                 conds(Conds),                        %  . record conditions of the Boolean formula (for now on, a free variable)
                 combined_row_ctrs(SourceVarsLocal,   %  . record source/target variables and list of source constraints that
                                   SourceVarsGlobal,  %    are common to the whole Boolean formula
                                   TargetVarsGlobal,
                                   SourceCtrs)],

                                                      % create the descriptors of the different conditions of the Boolean formula
                                                      % including constraints that only depend of one single condition
    formula_generator_bool_conds1(NewLConds, Oplus, Negated, NbTerms, MandatoryAttr, BoolOnly, UseMods, MinAttrs, MaxAttrs, ShiftedBoolAttrs,
                                  ColSetsBool, LogRes, Conds, LCondB, LLogCondB, LLogCondBool),

    restrict_nb_unary_nb_binary_nb_ternary(Conds,     % retrict valid combination of number of unary, of binary, and ternary conditions
                                           UnariesBinariesTernaries,
                                           NbTerms, NbAttrs,
                                           t(NbUnary,NbBinary,NbTernary,AdjustMaxOccAttrToOne)),

    break_symmetry_between_duplicated_conds(Conds,    % break symmetry between adjacent duplicated conditions by using an automaton ctr
                                            PosCondB, PosNewCondType, PosLCondAttr),

    (NbTerms > 1 ->                                   % post ctrs.to avoid combinations of Boolean conditions that could be simplified
        forbid_specific_combinations_of_conds(Conds, Oplus, NbTerms, MandatoryAttr, LConds, MinAttrs, MaxAttrs,
                                              ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr),
        FamilyA  = [attr_eq_attr,attr_leq_attr],
        _FamilyB = [attr_eq_coef,attr_leq_coef,attr_geq_coef],
        _FamilyC = [attr_in_interval],
        FamilyD1 = [attr_eq_unary,attr_leq_unary],
        FamilyD2 = [attr_eq_unary,unary_leq_attr],
        FamilyE  = [binary_term_eq_coef(plus),     binary_term_leq_coef(plus),     binary_term_geq_coef(plus)],
        FamilyF  = [binary_term_eq_coef(minus),    binary_term_leq_coef(minus),    binary_term_geq_coef(minus)],
        Family1  = [unary_term_eq_coef(mod),       unary_term_leq_coef(mod),       unary_term_geq_coef(mod)],
        Family2  = [binary_term_eq_coef(abs),      binary_term_leq_coef(abs),      binary_term_geq_coef(abs)],
        Family3  = [binary_term_eq_coef(min),      binary_term_leq_coef(min),      binary_term_geq_coef(min)],
        Family4  = [binary_term_eq_coef(max),      binary_term_leq_coef(max),      binary_term_geq_coef(max)],
        Family5  = [binary_term_eq_coef(prod),     binary_term_leq_coef(prod),     binary_term_geq_coef(prod)],
        Family6  = [binary_term_eq_coef(floor),    binary_term_leq_coef(floor),    binary_term_geq_coef(floor)],
        Family7  = [binary_term_eq_coef(mfloor,_), binary_term_leq_coef(mfloor,_), binary_term_geq_coef(mfloor,_)],
        Family8  = [binary_term_eq_coef(ceil),     binary_term_leq_coef(ceil),     binary_term_geq_coef(ceil)],
        Family9  = [binary_term_eq_coef(mod),      binary_term_leq_coef(mod),      binary_term_geq_coef(mod)],
        Family10 = [binary_term_eq_coef(cmod),     binary_term_leq_coef(cmod),     binary_term_geq_coef(cmod)],
        Family11 = [binary_term_eq_coef(dmod),     binary_term_leq_coef(dmod),     binary_term_geq_coef(dmod)],
        Family12 = [binary_term_eq_coef(fmod),     binary_term_leq_coef(fmod),     binary_term_geq_coef(fmod)],
        Family13 = [sum_leq_attr],    % will not restrict specific combinations as only one member in this family
        Family14 = [minus_mod_eq0],   % will not restrict specific combinations as only one member in this family
        Family15 = [ceil_leq_floor],   % will not restrict specific combinations as only one member in this family
        Family16 = [attr_geq_fmod],
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, FamilyA,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, FamilyD1, PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, FamilyD2, PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, FamilyE,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, FamilyF,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family1,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family2,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family3,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family4,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family5,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family6,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family7,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family8,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family9,  PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family10, PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family11, PosCondB, PosNewCondType),
        restrict_specific_combinations_of_conds(Conds, NbTerms, 2, Family12, PosCondB, PosNewCondType),
        incompatible_families(Conds, [Family1,Family2,Family3, Family4, Family5, Family6, Family7,
                                      Family8,Family9,Family10,Family11,Family12,
                                      Family13,Family14,Family15,Family16],
                              PosCondB, PosNewCondType)
    ;
        true
    ),
    (UseMods = 1 ->                                   % if forced to use mod
        all_mod_conds(ListModConds),                  % collect CondB variables of conditions using mod
        collect_condb_for_mods(Conds, ListModConds, PosCondB, PosNewCondType, LCondBMod),
        count(0, LCondBMod, #=, NbTermsModZero),      % get number of negated terms
        count(1, LCondBMod, #=, NbTermsModOne ),      % get number of non-negated terms
        NbTermsModOne + NbTermsModZero #>= 1          % for using at least one term with mod
    ;
        true
    ),
    length(Conds, M),                                 % number of conditions of the Boolean formula
    M_NbTerms is M - NbTerms,                         % number of conditions that should be discarded
    count(-1, LCondB, #=, M_NbTerms),                 % enforce the Boolean formula to contain NbTerms by discarding M-NbTerms conditions
                                                      % (for each condition CondB will be fixed to -1 if the condition is not used)
                                                      % generate some extra constraints (used on every row) that USES ALL Boolean conditions
    formula_generator_bool_extra_ctr(Oplus, NbTerms, LogRes, LCondB, LLogCondB, LLogCondBool,
                                     SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs).

collect_condb_for_mods([], _, _, _, []) :- !.         % extract CondB variables corresponding to terms containing a mod
collect_condb_for_mods([Cond|R], ListModConds, PosCondB, PosNewCondType, [CondBMod|S]) :- 
    arg(PosNewCondType, Cond, Type),
    memberchk(Type, ListModConds), !,
    arg(PosCondB, Cond, CondBMod),
    collect_condb_for_mods(R, ListModConds, PosCondB, PosNewCondType, S).
collect_condb_for_mods([_|R], ListModConds, PosCondB, PosNewCondType, LCondBMod) :-
    collect_condb_for_mods(R, ListModConds, PosCondB, PosNewCondType, LCondBMod).

% duplicate each Boolean condition with respect to its maximum possible number of occurrences (which can not exceed NbTerms);
% the maximum possible number of occurrences of each duplicated condition is set to 1 
duplicate_bool_conds([], _, []) :- !.
duplicate_bool_conds([t(cond(Cond),CondRestriction,CondMaxOcc)|R], NbTerms, [t(cond(Cond),CondRestriction,1)|S]) :-
    (CondMaxOcc = 1 ->                             % if maximum possible number of occurrences was set to 1
        duplicate_bool_conds(R, NbTerms, S)        % then copy condition and handle all next conditions
    ;                                              % if maximum possible number of occurrences greater than 1
        CondMaxOcc1 is min(NbTerms,CondMaxOcc)-1,  % create a condition (with one occurrence) and decrease max number of occ. of current condition
        (CondMaxOcc1  = 0 ->
            duplicate_bool_conds(R, NbTerms, S)
        ;
            duplicate_bool_conds([t(cond(Cond),CondRestriction,CondMaxOcc1)|R], NbTerms, S)
        )
    ).

get_shifted_bool_attributes([], [], _, []) :- !.
get_shifted_bool_attributes([Min|MinAttrs], [Max|MaxAttrs], ShiftedIndex, [ShiftedIndex|BoolAttrs]) :-
    Min = 0, % it is not enough to test that Max-Min=1
    Max = 1,
    !,
    ShiftedIndex1 is ShiftedIndex+1,
    get_shifted_bool_attributes(MinAttrs, MaxAttrs, ShiftedIndex1, BoolAttrs).
get_shifted_bool_attributes([_|MinAttrs], [_|MaxAttrs], ShiftedIndex, BoolAttrs) :-
    ShiftedIndex1 is ShiftedIndex+1,
    get_shifted_bool_attributes(MinAttrs, MaxAttrs, ShiftedIndex1, BoolAttrs).

get_bool_cost_vars(FormulaPattern, CostVars) :-
    pos_bool_cond_entry(condcostvars, PosCondCostVars),
    memberchk(conds(Conds), FormulaPattern),
    get_bool_cost_vars(Conds, PosCondCostVars, CondsCostVars),
    flatten(CondsCostVars, FlatCostVars),
    memberchk(op(Oplus), FormulaPattern),    % extract Boolean function that is used
    (memberchk(Oplus,[card1,xor,voting]) ->  % if it is an "exotic" Boolean function
        CostVars = [2|FlatCostVars]          % then add a cost penalty of +2
    ;
        CostVars = FlatCostVars
    ).

get_bool_cost_vars([], _, []) :- !.
get_bool_cost_vars([Cond|R], PosCondCostVars, [CondCostVars|S]) :-
    arg(PosCondCostVars, Cond, CondCostVars),
    get_bool_cost_vars(R, PosCondCostVars, S).

get_entries_of_all_boolean_conditions([], _, []) :- !.
get_entries_of_all_boolean_conditions([Cond|R], PosEntry, [Entry|S]) :-
    arg(PosEntry, Cond, Entry),
    get_entries_of_all_boolean_conditions(R, PosEntry, S).

% given the list of candidate conditions, creates a descriptor for each candidate condition (as conditions where duplicated, CondMaxOcc=1)
formula_generator_bool_conds1([], _, _, _, _, _, _, _, _, _, _, _, [], [], [], []) :- !.
formula_generator_bool_conds1([t(cond(Cond),CondRestriction,1)|R], Oplus, Negated, NbTerms, MandatoryAttr, BoolOnly, UseMods, MinAttrs, MaxAttrs, ShiftedBoolAttrs,
                              ColSetsBool, LogRes, LCreatedCond, LCondB, LLogCondB, LLogCondBool) :-
    (formula_generator_bool_cond1(Oplus, Negated, NbTerms, MandatoryAttr, BoolOnly, UseMods, MinAttrs, MaxAttrs, ShiftedBoolAttrs,
                                  ColSetsBool, Cond, CondRestriction, LogRes, CreatedCond, CondB, LogCondB, LogCondBool) ->
        LCreatedCond = [CreatedCond|S],
        LCondB       = [CondB|T],
        LLogCondB    = [LogCondB|U],
        LLogCondBool = [LogCondBool|V]
    ;
        LCreatedCond = S,
        LCondB       = T,
        LLogCondB    = U,
        LLogCondBool = V
    ),
    formula_generator_bool_conds1(R, Oplus, Negated, NbTerms, MandatoryAttr, BoolOnly, UseMods, MinAttrs, MaxAttrs, ShiftedBoolAttrs, ColSetsBool, LogRes, S, T, U, V).

formula_generator_bool_cond1(Oplus, Negated, NbTerms, MandatoryAttr, BoolOnly, UseMods, MinAttrs, MaxAttrs, ShiftedBoolAttrs,
                             ColSetsBool, Cond, CondRestriction, LogRes, CreatedCond, CondB, LogCondB, LogCondBool) :-
    % CondB=-1 if condition unused, CondB=0 if condition used negatively, CondB=1 if condition used positively
    ((NbTerms = 1, Negated = 1)       -> CondB in -1..1, CondB #\= 0                                    ; % avoid not not Cond if one single term
     CondRestriction = no_negation    -> CondB in -1..1, CondB #\= 0                                    ;
     CondRestriction = no_restriction -> CondB in -1..1                                                 ;
                                         write(formula_generator_bool_cond1a(CondRestriction)), nl, halt),
    CreatedCond = cond(CondB,                            % tell how the condition will be used (see above)
                       NewCondType,                      % record type of condition (without the eventual parameters)
                       LCondAttr,                        % list of shifted indexes of attributes variables that are used in the condition
                       LCondExtraVars,                   % list of additional vars (passed to collect all vars in order to do the labeling)
                       LCondCostVars,                    % list of cost variables used for computing the cost of the condition
                       CondAttr1, CondAttr2, CondAttr3,  % index of the 1st, 2nd, 3rd attributes used by the condition (0 if unused)
                       CondCst, CondCoef,                % eventual constant and coefficient used by the condition (0 if unused)
                       SourceVarsLocal,                  % source variables that are specific for each table entry
                       SourceVarsGlobal,                 % source variables that are the same for each table entry
                       TargetVarsGlobal,                 % target variables that are the same for each table entry
                       SourceCtrs,                       % list of constraints that will be posted on each table entry
                       CondCheckOrder),                  % set to the position of the LogCondOrder variable in the list SourceVarsLocal
                                                         % if will have to check that both args of a min or max unary function contribute
                                                         % after posting all the constraints on all table entries, set to 0 if no check to do
    
    (compound(Cond) -> true ;
     atom(Cond)     -> true ; write(formula_generator_bool_cond1b), nl, halt),               % fail if Cond is not an atom or a term
    functor(Cond, CondType, CondArity),                                                      % get condition type and number of parameters
    length(MandatoryAttr, LenA),                                                             % get number of mandatory attributes
    length(LogAttributes, LenA),                                                             % create a list of logical variables for the table entry
    LenA1 is LenA+1,
    append([0], LogAttributes, LogAttributes0),                                              % first column for the case where condition is not used

    % retrict the arity of the conditions we generate (from the number of terms NbTerms and the number of mandatory attributes LenA)
    %.....................................................................................................................................
    MaxLenAU is (NbTerms-1)*3+1,                                                             % max.nb.of att.we can cover using at least a unary cond
    MaxLenAB is MaxLenAU+1,                                                                  % max.nb.of att.we can cover using at least a binary cond
    (MaxLenAU < LenA -> DoNotGenerateUnaryCond   = 1 ;                                       % if using a unary cond does not allow to cover all
                        DoNotGenerateUnaryCond   = 0 ),                                      % mandatory att.then do not consider unary cond
    (MaxLenAB < LenA -> DoNotGenerateBinaryCond  = 1 ;                                       % if using a binary cond does not allow to cover
     LenA     < 2    -> DoNotGenerateBinaryCond  = 1 ;                                       % all mandatory att. or if it cover too many
                        DoNotGenerateBinaryCond  = 0 ),                                      % att.then do not consider binary cond
    (LenA     < 3    -> DoNotGenerateTernaryCond = 1 ;                                       % if using a ternary cond cover too many att.then
                        DoNotGenerateTernaryCond = 0 ),                                      % do not consider ternary cond

    % cond( attr_eq_attr): CondAttr1 =  CondAttr2
    % cond(attr_leq_attr): CondAttr1 =< CondAttr2
    %.....................................................................................................................................
    ((memberchk(CondType, [attr_eq_attr,attr_leq_attr]), CondArity = 0, LenA = 1) ->
        false
    ;
     (memberchk(CondType, [attr_eq_attr,attr_leq_attr]), DoNotGenerateBinaryCond = 1) ->
        false
    ;
     (memberchk(CondType, [attr_eq_attr,attr_leq_attr]), CondArity = 0, LenA > 1) ->         % COMPARING TWO ATTRIBUTES
        LCondAttr      = [CondAttr1,CondAttr2],                                              % list of shifted index of attribute variables used
        CondAttr3      = 0,                                                                  % no third attribute (as just use two attributes)
        LCondExtraVars = [condb(CondB)],                                                     % list of additional variables
        domain(LCondAttr, 1, LenA1),                                                         % variables of the first and second attributes
        CondB #= -1 #<=> CondAttr1 #= 1,                                                     % set shifted index to 1 if condition not used
        CondB #= -1 #<=> CondAttr2 #= 1,                                                     % set shifted index to 1 if condition not used
        CondCoef = 0,                                                                        % no coefficient (as just use two attributes)
        CondCst  = 0,                                                                        % no constant    (as just use two attributes)

        % REMARK: as soon as CondAttr1 will be fixed LogAttr1 will be fixed (LogAttributes correspond to fix values of a row of a table)
        %         as soon as CondAttr2 will be fixed LogAttr2 will be fixed for the same reason
        %.................................................................................................................................
        Ctr1 = element(LogCondAttr1, LogAttributes0, LogAttr1),                              % constraint for getting value of Attr1
        Ctr2 = element(LogCondAttr2, LogAttributes0, LogAttr2),                              % constraint for getting value of Attr2
        (CondType = attr_eq_attr ->                                                          % break symmetry as can permutte
            CondAttr1 #=< CondAttr2,
            CondAttr1 #= 1 #\/ CondAttr1 #< CondAttr2,                                       % variables in an equality
            ForbiddenCmps = [eq,lt,gt,neq],                                                  % if use '=' then should NOT ALLWAYS BE
            Ctr3 = (LogCondBool #<=> (LogAttr1 #= LogAttr2)),                                % true ('=') or false ('<','>','<>')
            (BoolOnly = 1 ->                                                                 % if used in Boolean only formulae:
                Cost in 0..2,
                CondB #= -1 #=> Cost #= 0,
                CondB #>= 0 #=> Cost #= 2-CondB,
                LCondCostVars = [Cost]
            ;                                                                                % otherwise no cost for the condition
                LCondCostVars = []
            )
        ;                                                                                    % when use two attributes in a condition
            CondAttr1 #= 1 #\/ CondAttr1 #\= CondAttr2,                                      % these attributes should be distinct
            ForbiddenCmps = [leq,eq,lt,gt],                                                  % if use '=<' then should NOT ALLWAYS BE
            Ctr3 = (LogCondBool #<=> (LogAttr1 #=< LogAttr2)),                               % true ('=<','=','<') or false ('>')
            Cost in 0..1, CondB #= -1 #=> Cost #= 0, CondB #> -1 #=> Cost #= 1,              % Never used in Boolean only formulae
            LCondCostVars = [Cost]
        ),
        gen_table_constraint_for_a_condition(Oplus, NbTerms, LogRes, LogCondB, LogCondBool, LCtr),
        append([Ctr1,Ctr2,Ctr3], LCtr, SourceCtrs),                                          % list of constraints to post on each table entry
        gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, ForbiddenCmps, 1, LCondAttr),   % 1 as shift index by 1
        append([[LogCondBool,LogAttr1,LogAttr2], LogAttributes, [LogRes]], SourceVarsLocal),
        SourceVarsGlobal = [LogCondB,LogCondAttr1,LogCondAttr2],
        TargetVarsGlobal = [CondB,CondAttr1,CondAttr2],
        NewCondType      = CondType,
        CondCheckOrder   = 0
    ;

    % cond( attr_eq_coef(coef(CondCoefMin, CondCoefMax))): CondAttr1 =  CondCoef
    % cond(attr_leq_coef(coef(CondCoefMin, CondCoefMax))): CondAttr1 =< CondCoef
    % cond(attr_geq_coef(coef(CondCoefMin, CondCoefMax))): CondAttr1 >= CondCoef
    %.....................................................................................................................................
     (memberchk(CondType, [attr_eq_coef,attr_leq_coef,attr_geq_coef]), DoNotGenerateUnaryCond = 1) ->
        false
    ;
     (memberchk(CondType, [attr_eq_coef,attr_leq_coef,attr_geq_coef]), CondArity = 1) ->     % COMPARING AN ATTRIBUTE WITH A COEFFICIENT
        ((arg(1, Cond, coef(CondCoefMin,CondCoefMax)),                                       % get range of coefficient
          integer(CondCoefMin), integer(CondCoefMax), CondCoefMin =< CondCoefMax) ->         % check that we have a proper interval
            CondCoef in CondCoefMin..CondCoefMax                                             % create coefficient variable
        ;
            write(formula_generator_bool_cond1c), nl, halt
        ),
        (CondType = attr_eq_coef  -> LCondExtraVars = [condb(CondB),coef(eq, CondCoef)] ;    % list of additional variables
         CondType = attr_leq_coef -> LCondExtraVars = [condb(CondB),coef(leq,CondCoef)] ;
                                     LCondExtraVars = [condb(CondB),coef(geq,CondCoef)] ),
        LCondAttr      = [CondAttr1],                                                        % list of index of attribute variables used
        CondAttr1 in 1..LenA1,                                                               % variable of the first attribute (shifted)
        CondB #= -1 #<=> CondAttr1 #= 1,                                                     % set shifted index to 1 if condition not used
        CondB #= -1 #=> CondCoef #= CondCoefMin,                                             % set coef to min value if condition not used
        CondAttr2 = 0,                                                                       % no second attribute (as just use one attribute)
        CondAttr3 = 0,                                                                       % no third  attribute (as just use one attribute)
        CondCst   = 0,                                                                       % no constant
                                                                                             % extract from ColSetsBool the sets of possible values
                                                                                             % for CondCoef wrt each potential mandatory attribute
                                                                                             % and, if necessary, restrict CondAttr1

        get_set_of_values_for_each_mandatory_attribute(MandatoryAttr, 1, Oplus, Negated, CondType, ColSetsBool,
                                                       CondCoefMin, CondCoefMax, CoefValuesList),
        CoefValuesList = [_,_|_],                                                            % want to have at least 2 pairs of values (not just default values)
        table([[CondAttr1,CondCoef]], CoefValuesList),                                       % restrict to compatible pairs
                                                                                             % other table entries correspond to variables whose
                                                                                             % domain are the possible values of CondCoef
        (BoolOnly = 1 ->                                                                     % if used in Boolean only formulae:
            Cost in 0..1,
            CondB #= -1 #=> Cost #= 0,
            CondB #> -1 #=> Cost #= 1 - CondCoef,
            LCondCostVars = [Cost]                                                           % no cost is for the condition
        ;                                                                                    % otherwise cost of attr_leq_coef,attr_geq_coef,
            MinCost is min(0,CondCoefMin),                                                   % attr_eq_coef is calculated
            MaxCost is max(0,CondCoefMax)+1,
            Cost in MinCost..MaxCost,
            CondB #= -1 #=> Cost #= 0,
            (CondType = attr_eq_coef -> CondB #> -1 #=> Cost #= CondCoef ; CondB #> -1 #=> Cost #= CondCoef + 1),
            LCondCostVars = [Cost]
        ),
        Ctr1 = element(LogCondAttr1, LogAttributes0, LogAttr1),                              % constraint for getting value of Attr1
        (CondType = attr_eq_coef ->                                                          % if use '='
            Ctr2 = (LogCondBool #<=> (LogAttr1 #= LogCondCoef))
        ;
         CondType = attr_leq_coef ->                                                         % if use '=<'
            Ctr2 = (LogCondBool #<=> (LogAttr1 #=< LogCondCoef))
        ;
            Ctr2 = (LogCondBool #<=> (LogAttr1 #>= LogCondCoef))
        ),
        append([[LogCondBool,LogAttr1], LogAttributes, [LogRes]], SourceVarsLocal),
        gen_table_constraint_for_a_condition(Oplus, NbTerms, LogRes, LogCondB, LogCondBool, LCtr),
        append([Ctr1,Ctr2], LCtr, SourceCtrs),                                               % list of constraints to post on each table entry
        SourceVarsGlobal = [LogCondB,LogCondAttr1,LogCondCoef],
        TargetVarsGlobal = [CondB,CondAttr1,CondCoef],
        NewCondType      = CondType,
        CondCheckOrder   = 0
    ;

    % cond(attr_in_interval(MinInterval,MaxInterval)): CondAttr1 in [CondCst,CondCoef]
    %.....................................................................................................................................
    (CondType = attr_in_interval, DoNotGenerateUnaryCond = 1) ->
        false
    ;
    (CondType = attr_in_interval, CondArity = 2) ->                                          % COMPARING AN ATTRIBUTE WITH AN INTERVAL
        arg(1, Cond, MinInterval),                                                           % get minimum value for the start of the interval
        arg(2, Cond, MaxInterval),                                                           % get maximum value for the end of the interval
        ((integer(MinInterval),
          integer(MaxInterval),
          MinInterval < MaxInterval) ->
            CondCst  in MinInterval..MaxInterval,                                            % create variable corresponding to interval start
            CondCoef in MinInterval..MaxInterval,                                            % create variable corresponding to interval end
            LCondAttr      = [CondAttr1],                                                    % list of index of attribute variables used
            LCondExtraVars = [condb(CondB),coef(geq,CondCst),coef(leq,CondCoef)],            % list of additional variables
            CondAttr1 in 1..LenA1,                                                           % variable of the first attribute (shifted)
            CondAttr2 = 0,                                                                   % no second attribute
            CondAttr3 = 0,                                                                   % no third  attribute
            CondB #= -1 #<=> CondAttr1 #= 1,                                                 % set shifted index to 1 if condition not used
            CondB #= -1 #=>  CondCst   #= MinInterval,                                       % set coef to min value if condition not used
            CondB #= -1 #=>  CondCoef  #= MinInterval,                                       % set coef to min value if condition not used
            CondB #> -1 #<=> CondCst   #< CondCoef,                                          % interval start strictly before interval end
            get_set_of_values_for_each_mandatory_attribute(MandatoryAttr, 1, Oplus, Negated, attr_leq_coef, ColSetsBool,
                                                           MinInterval, MaxInterval, CoefValuesList),
            CoefValuesList = [_,_|_],                                                        % want to have at least 2 pairs of values (not just default values)
            table([[CondAttr1,CondCst]], CoefValuesList),                                    % restrict to compatible pairs
            table([[CondAttr1,CondCoef]], CoefValuesList),                                   % restrict to compatible pairs
            element(CondAttr1, [MinInterval|MinAttrs], MinAttr1),                            % get smallest value of attribute CondAttr1
            element(CondAttr1, [MinInterval|MaxAttrs], MaxAttr1),                            % get largest  value of attribute CondAttr1
            CondB #> -1 #=> CondCst  #> MinAttr1,                                            % if use condition avoid to rewrite "in" as leq CondCoef
            CondB #> -1 #=> CondCoef #< MaxAttr1,                                            % if use condition avoid to rewrite "in" as geq CondCst
            MinCost is min(0,MinInterval),                                                   % (strictly as otherwise this would be an equality)
            MaxCost is max(0,MaxInterval+2),                                                 % +2 for catching the term + 1 + (CondB #= 0) !!!
            Cost in MinCost..MaxCost,                                                        % cost of attr_in_interval
            CondB #= -1 #=> Cost #= 0,                                                       % no cost if condition is not used
            CondB #> -1 #=> Cost #= abs(CondCoef) + 1 + (CondB #= 0),                        % otherwise cost is the upper bound of the interval
                                                                                             % + 1 for the in + 1 for a negation
            Ctr1 = element(LogCondAttr1, LogAttributes0, LogAttr1),                          % constraint for getting value of Attr1
            Ctr2 = (LogCondBool #<=> (LogCondCst #=< LogAttr1 #/\ LogAttr1 #=< LogCondCoef)),% the main constraint
            append([[LogCondBool,                                                            % put together all local source variables
                     LogAttr1], LogAttributes, [LogRes]], SourceVarsLocal),
            gen_table_constraint_for_a_condition(Oplus, NbTerms, LogRes, LogCondB, LogCondBool, LCtr),
            append([Ctr1,Ctr2], LCtr, SourceCtrs),                                           % list of constraints to post on each table entry
            SourceVarsGlobal = [LogCondB,LogCondAttr1,LogCondCst,LogCondCoef],               % put together all global source variables
            TargetVarsGlobal = [CondB,CondAttr1,CondCst,CondCoef],                           % put together all global target variables
            NewCondType      = CondType,                                                     % type is just attr_in_interval
            LCondCostVars    = [Cost],
            CondCheckOrder   = 0
        ;
            write(formula_generator_bool_cond1d), nl, halt
        )
    ;

    % cond(attr_eq_unary([min(CondCstMin,CondCstMax),...])) : CondAttr1 = CondU(CondAttr2,CondCst)
    % cond(attr_leq_unary([min(CondCstMin,CondCstMax),...])): CondAttr1 =< CondU(CondAttr2,CondCst)
    % cond(unary_leq_attr([min(CondCstMin,CondCstMax),...])): CondU(CondAttr1,CondCst) =< CondAttr2
    %.....................................................................................................................................
    (memberchk(CondType,[attr_eq_unary,attr_leq_unary,unary_leq_attr]), CondArity = 1, LenA = 1) ->
        false
    ;
    (memberchk(CondType,[attr_eq_unary,attr_leq_unary,unary_leq_attr]), DoNotGenerateBinaryCond = 1) ->
        false
    ;
    (memberchk(CondType,[attr_eq_unary,                                                      % COMPARING AN ATTRIBUTE AND A UNARY TERM, OR
                        attr_leq_unary,unary_leq_attr]), CondArity = 1, LenA > 1) ->         %           A UNARY TERM AND AN ATTRIBUTE
        arg(1, Cond, LCondUnary),                                                            % get list of possible unary terms
        LCondAttr      = [CondAttr1,CondAttr2],                                              % list of index of attribute variables used
        CondAttr3      = 0,                                                                  % no third attribute (as just use two attributes)
        LCondExtraVars = [condb(CondB),CondCst],                                             % list of additional variables
        domain(LCondAttr, 1, LenA1),                                                         % variables of the first and second attributes
        CondB #= -1 #<=> CondAttr1 #= 1,                                                     % set shifted index to 1 if condition not used
        CondB #= -1 #<=> CondAttr2 #= 1,                                                     % set shifted index to 1 if condition not used
        CondB #> -1 #<=> CondAttr1 #\= CondAttr2,                                            % attributes should be distinct if condition used
        CondCoef = 0,                                                                        % no coefficient (as compare two attributes)
        (memberchk(CondType, [attr_eq_unary,attr_leq_unary]) ->
            ForbiddenCmps = [leq,eq,lt]                                                      % should NOT ALLWAYS BE true ('=<','=','<')
        ;
            ForbiddenCmps = [geq,eq,gt]                                                      % should NOT ALLWAYS BE true ('>=','=','>')
        ),
        gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, ForbiddenCmps, 1, LCondAttr),   % 1 as shift index by 1
        (LCondUnary = [CondUnary]  ->                                                        % check that we have a list with one single function
            ((compound(CondUnary),                                                           % we should have a term
              functor(CondUnary, CondU, 2),                                                  % get unary term
              memberchk(CondU, [prod]),                                                      % we just use the prod unary term
              arg(1, CondUnary, CondCstMin),                                                 % get min value of constant used in unary term
              arg(2, CondUnary, CondCstMax),                                                 % get max value of constant used in unary term
              integer(CondCstMin), integer(CondCstMax), CondCstMin =< CondCstMax) ->         % check that we have a proper interval
                MinCondCst is max(2,CondCstMin),                                             % as 2 is the minimum value of the constant
                CondCst in MinCondCst..CondCstMax,                                           % create the constant
                CondB #= -1 #=> CondCst #= MinCondCst,                                       % fix constant if condition is not used
                CondB #> -1 #=> CondCst #\= 1,                                               % prevent y=1.x even if normaly covered by gen_options
                MinCost is min(0,MinCondCst),                                                % (strictly as otherwise this would be an equality)
                MaxCost is max(0,CondCstMax+1),                                              % + for catching the term + 1 !!!
                Cost in MinCost..MaxCost,                                                    % cost of attr_in_interval
                CondB #= -1 #=> Cost #= 0,                                                   % no cost if condition is not used
                (ConType = attr_eq_unary ->
                    CondB #> -1 #=> Cost #= CondCst                                          % cost is CondCst if use equality
                ;
                    CondB #> -1 #=> Cost #= CondCst + 1                                      % cost is CondCst+1 if use inequality
                ),
                Ctr1 = element(LogCondAttr1, LogAttributes0, LogAttr1),                      % constraint for getting value of Attr1
                Ctr2 = element(LogCondAttr2, LogAttributes0, LogAttr2),                      % constraint for getting value of Attr2
                get_set_of_unaryf_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, CondType, prod, ColSetsBool,
                                                                               CondCstMin, CondCstMax, CstValuesList),
                CstValuesList = [_,_|_],
                table([[CondAttr1,CondAttr2,CondCst]], CstValuesList),                       % restrict to compatible triples
                (CondType = attr_leq_unary ->
                    Ctr3 = (LogCondBool #<=> (LogAttr1 #=< LogAttr2 * LogCondCst)),
                    NewCondType = attr_leq_unary(prod)
                ;
                 CondType = unary_leq_attr ->
                    Ctr3 = (LogCondBool #<=> (LogAttr1 * LogCondCst #=< LogAttr2)),
                    NewCondType = unary_leq_attr(prod)
                ;
                 ConType = attr_eq_unary ->
                    Ctr3 = (LogCondBool #<=> (LogAttr1 #= LogAttr2 * LogCondCst)),
                    NewCondType = attr_eq_unary(prod)
                ;
                    write(formula_generator_bool_cond1e), nl, halt
                ),
                gen_table_constraint_for_a_condition(Oplus, NbTerms, LogRes, LogCondB, LogCondBool, LCtr),
                append([[LogCondBool,LogAttr1,LogAttr2], LogAttributes, [LogRes]], SourceVarsLocal),
                append([Ctr1,Ctr2,Ctr3], LCtr, SourceCtrs),                                  % list of constraints to post on each table entry
                SourceVarsGlobal = [LogCondB, LogCondAttr1, LogCondAttr2, LogCondCst],       % global source variables
                TargetVarsGlobal = [   CondB,    CondAttr1,    CondAttr2,    CondCst],       % target source variables
                LCondCostVars    = [Cost],
                CondCheckOrder   = 0
            ;
                write(formula_generator_bool_cond1f), nl, halt
            )
        ;
            write(formula_generator_bool_cond1g), nl, halt
        )
    ;

    % cond( unary_term_eq_coef([min(CondCstMin,CondCstMax),...], coef(CondCoefMin,CondCoefMax))): CondU(Attr1,CondCst) =  CondCoef
    % cond(unary_term_leq_coef([min(CondCstMin,CondCstMax),...], coef(CondCoefMin,CondCoefMax))): CondU(Attr1,CondCst) =< CondCoef
    % cond(unary_term_geq_coef([min(CondCstMin,CondCstMax),...], coef(CondCoefMin,CondCoefMax))): CondU(Attr1,CondCst) >= CondCoef
    %.....................................................................................................................................
    (memberchk(CondType, [unary_term_eq_coef,unary_term_leq_coef,unary_term_geq_coef]), DoNotGenerateUnaryCond = 1) ->
        false
    ;
    (memberchk(CondType,[unary_term_eq_coef,unary_term_leq_coef,unary_term_geq_coef]),
     CondArity = 2                                                                   ) ->    % COMPARING A UNARY TERM WITH A COEFFICIENT
        arg(1, Cond, LCondUnary),                                                            % get list of possible unary terms
        ((arg(2, Cond, coef(CondCoefMin,CondCoefMax)),                                       % get range of coefficient
          integer(CondCoefMin), integer(CondCoefMax), CondCoefMin =< CondCoefMax) ->         % check that we have a proper interval
            CondCoef in CondCoefMin..CondCoefMax                                             % create coefficient variable
        ;
            write(formula_generator_bool_cond1h), nl, halt
        ),                                                                                   % list of additional variables
        (CondType = unary_term_eq_coef  -> LCondExtraVars = [condb(CondB),coef(eq, CondCoef),CondCst] ;
         CondType = unary_term_leq_coef -> LCondExtraVars = [condb(CondB),coef(leq,CondCoef),CondCst] ;
                                           LCondExtraVars = [condb(CondB),coef(geq,CondCoef),CondCst] ),
        LCondAttr      = [CondAttr1],                                                        % list of index of attribute variables used
        CondAttr1 in 1..LenA1,                                                               % variable of the first attribute (shifted)
        CondAttr2 = 0,                                                                       % no second attribute (as just use one attribute)
        CondAttr3 = 0,                                                                       % no third  attribute (as just use one attribute)
        CondB #= -1 #<=> CondAttr1 #= 1,                                                     % set shifted index to 1 if condition not used
        (LCondUnary = [CondUnary] ->                                                         % check that we have a list with one single function
            ((compound(CondUnary),                                                           % we should have a term
              functor(CondUnary, CondU, 2),                                                  % get unary term
              (CondU = mod ->                                                                % we only have mod
                true
              ;
                write(formula_generator_bool_cond1i), nl, halt
              ),
              CondB #= -1 #=> CondCoef #= CondCoefMin,                                       % fix constant if condition is not used
              arg(1, CondUnary, CondCstMin),                                                 % get min value of constant used in unary term
              arg(2, CondUnary, CondCstMax),                                                 % get max value of constant used in unary term
              integer(CondCstMin), integer(CondCstMax), CondCstMin =< CondCstMax) ->         % check that we have a proper interval
                CondCst in CondCstMin..CondCstMax,                                           % create constant
                CondB #= -1 #=> CondCst #= CondCstMin,                                       % fix constant if condition is not used
                MinCost is min(0,CondCstMin),
                (CondType = unary_term_eq_coef ->                                            % + for catching the term for the cost !!!
                    MaxCost is max(0,CondCstMax+CondCoefMax+1)
                ;
                    MaxCost is max(0,CondCstMax+CondCoefMax+2)
                ),                                                                           % cost of unary_term_eq_coef, unary_term_leq_coef
                Cost in MinCost..MaxCost,                                                    % and unary_term_geq_coef
                CondB #= -1 #=> Cost #= 0,                                                   % no cost if condition is not used
                (CondType = unary_term_eq_coef ->
                    CondB #> -1 #=> Cost #= CondCst+CondCoef+1                               % otherwise cost is equal to CondCst+CondCoef+1
                ;
                    CondB #> -1 #=> Cost #= CondCst+CondCoef+2                               % otherwise cost is equal to CondCst+CondCoef+2
                ),
                element(CondAttr1, [CondCstMin|MinAttrs], MinAttr1),                         % get smallest value of attribute CondAttr1
                element(CondAttr1, [CondCstMin|MaxAttrs], MaxAttr1),                         % get largest value of attribute CondAttr1
                CondB #> -1 #=> CondCst #> 1,
                CondB #> -1 #=> CondCst #=< (MaxAttr1 - MinAttr1),                           % force that at least one value occurs twice in
                                                                                             % the result of LogAttr1 mod LogCondCst (as otherwise
                                                                                             % se could use attr eq/leq/geq coef or in/not in)
                                                                                             % the goal is to restrict situations like [x mod 9]
                                                                                             % when x in [3,7] and eq/leq/geq could be substituted
                                                                                             % with eq/leq/geq coef, in, not in
                get_set_of_unary_term_values_for_each_mandatory_attribute(MandatoryAttr, 1, Oplus, Negated, CondType, mod, ColSetsBool,    %NEW
                                                          CondCstMin, CondCstMax, CondCoefMin, CondCoefMax, CoefValuesList),
                table([[CondAttr1,CondCst,CondCoef]], CoefValuesList),                       % restrict compatible triples
                gen_table_constraint_for_a_condition(Oplus, NbTerms, LogRes, LogCondB, LogCondBool, LCtr),
                Ctr1 = element(LogCondAttr1, LogAttributes0, LogAttr1),                      % constraint for getting value of Attr1
                (CondType = unary_term_eq_coef ->
                    CondB #> -1 #=> CondCoef #<  CondCst,
                    Ctr2 = (LogCondBool #<=> ((LogAttr1 mod LogCondCst) #=  LogCondCoef)),
                    NewCondType = unary_term_eq_coef(mod)
                ;
                 CondType = unary_term_leq_coef ->
                    CondB #> -1 #=> CondCoef #<  CondCst - 1,                                % prevent situation like x mod 3 <= 2 (always true)
                    Ctr2 = (LogCondBool #<=> ((LogAttr1 mod LogCondCst) #=< LogCondCoef)),
                    NewCondType = unary_term_leq_coef(mod)
                ;
                    CondB #> -1 #=> CondCoef #<  CondCst - 1,                                % prevent situation like x mod 3 >= 2 (as could be replaced by x mod 3 = 2)
                    Ctr2 = (LogCondBool #<=> ((LogAttr1 mod LogCondCst) #>= LogCondCoef)),
                    NewCondType = unary_term_geq_coef(mod)
                ),
                CondCheckOrder = 0,
                append([[LogCondBool,LogAttr1], LogAttributes, [LogRes]], SourceVarsLocal),
                append([Ctr1,Ctr2], LCtr, SourceCtrs),                                       % list of constraints to post on each table entry
                SourceVarsGlobal = [LogCondB, LogCondAttr1, LogCondCst, LogCondCoef],
                TargetVarsGlobal = [   CondB,    CondAttr1,    CondCst,    CondCoef],
                LCondCostVars    = [Cost]
            ;
                write(formula_generator_bool_cond1j), nl, halt
            )
        ;
            write(formula_generator_bool_cond1k), nl, halt
        )
    ;

    % cond( binary_term_eq_coef([min,...], coef(CondCoefMin,CondCoefMax)): CondB(CondAttr1,CondAttr2) =  CondCoef
    % cond(binary_term_leq_coef([min,...], coef(CondCoefMin,CondCoefMax)): CondB(CondAttr1,CondAttr2) =< CondCoef
    % cond(binary_term_geq_coef([min,...], coef(CondCoefMin,CondCoefMax)): CondB(CondAttr1,CondAttr2) >= CondCoef
    %.....................................................................................................................................
    (memberchk(CondType,[binary_term_eq_coef,binary_term_leq_coef,binary_term_geq_coef]), CondArity = 2, LenA = 1) ->
        false
    ;
    (memberchk(CondType,[binary_term_eq_coef,binary_term_leq_coef,binary_term_geq_coef]), DoNotGenerateBinaryCond = 1) ->
        false
    ;                                                                                        % COMPARING A BINARY TERM WITH A COEFFICIENT
    (memberchk(CondType,[binary_term_eq_coef,binary_term_leq_coef,binary_term_geq_coef]), CondArity = 2, LenA > 1) ->
        arg(1, Cond, LCondBinary),                                                           % get list of possible binary terms
        (CondType = binary_term_eq_coef  -> LCondExtraVars = [condb(CondB),coef(eq, CondCoef)] ; % list of additional variables
         CondType = binary_term_leq_coef -> LCondExtraVars = [condb(CondB),coef(leq,CondCoef)] ;
                                            LCondExtraVars = [condb(CondB),coef(geq,CondCoef)] ),
        LCondAttr      = [CondAttr1, CondAttr2],                                             % list of index of attribute variables used
        CondAttr1 in 1..LenA1,                                                               % variable of the first attribute (shifted)
        CondAttr2 in 1..LenA1,                                                               % variable of the second attribute (shifted)
        CondAttr3 = 0,                                                                       % no third  attribute (as just use one attribute)
        CondCst = 0,
        CondB #= -1 #<=> CondAttr1 #= 1,                                                     % set shifted index to 1 if condition not used
        CondB #= -1 #<=> CondAttr2 #= 1,                                                     % set shifted index to 1 if condition not used
        ((arg(2, Cond, coef(CondCoefMin,CondCoefMax)),                                       % get range of coefficient
          integer(CondCoefMin), integer(CondCoefMax), CondCoefMin =< CondCoefMax) ->         % check that we have a proper interval
            CondCoef in CondCoefMin..CondCoefMax                                             % create coefficient variable
        ;
            write(formula_generator_bool_cond1l), nl, halt
        ),         
        (LCondBinary = [CondBinary] ->                                                       % check that we have a list with one single function
            ((all_bool_options(binary, BinaryFunctions),                                     % get the set of possible binary functions
              memberchk(CondBinary, BinaryFunctions)) ->                                     % we should have a binary term
                (memberchk(CondBinary, [plus,abs,min,max,prod]) ->                           % if use a commutative binary term
                    CondB #> -1 #=> CondAttr1 #<  CondAttr2                                  % break symmetry as can permutte attributes
                ;                                                                            % if does not use a commutative binary term
                    CondB #> -1 #=> CondAttr1 #\= CondAttr2                                  % attributes should be distinct
                ),
                (memberchk(CondBinary, [floor,mfloor,ceil,mod,dmod,fmod]) ->                 % min value of Attr2 should be > 0
                    forbid_cols_which_can_be_leq0(LenA, MandatoryAttr, 1, CondAttr2)
                ;
                 CondBinary = cmod ->                                                        % min value of Attr1 should be > 0
                    forbid_cols_which_can_be_leq0(LenA, MandatoryAttr, 1, CondAttr1)
                ;
                    true                                                                     % otherwise no restriction
                ),
                CondB #= -1 #=> CondCoef #= CondCoefMin,                                     % fix constant if condition is not used
                MinCost is min(0,CondCoefMin),
                (CondBinary = plus   -> CostBinFunc = 0 ;
                 CondBinary = minus  -> CostBinFunc = 1 ;
                 CondBinary = abs    -> CostBinFunc = 5 ;
                 CondBinary = min    -> CostBinFunc = 5 ;
                 CondBinary = max    -> CostBinFunc = 5 ;
                 CondBinary = prod   -> CostBinFunc = 6 ;
                 CondBinary = floor  -> CostBinFunc = 7 ;
                 CondBinary = mfloor -> CostBinFunc = 8 ;
                 CondBinary = ceil   -> CostBinFunc = 7 ;
                 CondBinary = mod    -> CostBinFunc = 5 ;
                 CondBinary = cmod   -> CostBinFunc = 6 ;
                 CondBinary = dmod   -> CostBinFunc = 6 ;
                 CondBinary = fmod   -> CostBinFunc = 6 ;
                                        true            ),
                (CondType = binary_term_eq_coef ->                                           % + for catching the term for the cost !!!
                    MaxCost is max(0,CondCoefMax+2+CostBinFunc)
                ;
                    MaxCost is max(0,CondCoefMax+3+CostBinFunc)
                ),
                Cost in MinCost..MaxCost,                                                    % cost of unary_term_eq_coef, unary_term_leq_coef
                CondB #= -1 #=> Cost #= 0,                                                   % no cost if condition is not used
                (CondType = binary_term_eq_coef ->
                    CondB #> -1 #=> Cost #= abs(CondCoef) + 2 + CostBinFunc                  % otherwise cost is equal to CondCoef+2
                ;
                    CondB #> -1 #=> Cost #= abs(CondCoef) + 3 + CostBinFunc                  % otherwise cost is equal to CondCoef+3
                ),
                gen_table_constraint_for_a_condition(Oplus, NbTerms, LogRes, LogCondB, LogCondBool, LCtr),
                Ctr1 = element(LogCondAttr1, LogAttributes0, LogAttr1),
                Ctr2 = element(LogCondAttr2, LogAttributes0, LogAttr2),
                (CondBinary = plus ->
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                            plus, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],                                               % want to have at least 2 pairs of values (not just default values)
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> (LogAttr1+LogAttr2 #= LogCondCoef)),
                        NewCondType = binary_term_eq_coef(plus)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> (LogAttr1+LogAttr2 #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(plus)
                    ;
                        Ctr3 = (LogCondBool #<=> (LogAttr1+LogAttr2 #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(plus)
                    )
                ;
                 CondBinary = minus ->
                    CondB #> -1 #=> CondCoef #> 0,                                                  % only apply if condition is used
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                            minus, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->                                              % as want a positive result
                        Ctr3 = (LogCondBool #<=> (LogAttr1-LogAttr2 #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(minus)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> (LogAttr1-LogAttr2 #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(minus)
                    ;
                        Ctr3 = (LogCondBool #<=> (LogAttr1-LogAttr2 #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(minus)
                    )
                ;
                 CondBinary = abs ->                                                                % no order between the two attributes
                    CondB #> -1 #=> CondCoef #> 0,                                                  % |x1-x2| = 0 is equivalent to x1=x2
                                                                                                    % |x1-x2| <= 0 is equivalent to x1=x2
                                                                                                    % |x1-x2| >= 0 always true
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                            abs, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],                                                      % want to have at least 2 pairs of values (not just default values)
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt,eq,geq,gt], 1, LCondAttr),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> (abs(LogAttr1-LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(abs)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> (abs(LogAttr1-LogAttr2) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(abs)
                    ;
                        CondB #> -1 #=> CondCoef #> 1,
                        Ctr3 = (LogCondBool #<=> (abs(LogAttr1-LogAttr2) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(abs)
                    )
                ;
                 CondBinary = min ->                                                                 % no order between the two attributes
                    element(CondAttr1, [0|MinAttrs], MinAttr1),
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    element(CondAttr1, [0|MaxAttrs], MaxAttr1),
                    element(CondAttr2, [0|MaxAttrs], MaxAttr2),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt,eq,geq,gt], 1, LCondAttr),
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                            min, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        CondB #> -1 #=> CondCoef #>= max(MinAttr1,MinAttr2),                      % avoid case like min(x,y)=0 (with x,y in [2,4]) as always false
                        CondB #> -1 #=> CondCoef #=< min(MaxAttr1,MaxAttr2),                      % avoid case like min(x,y)=5 (with x,y in [2,4]) as always false
                        Ctr3 = (LogCondBool #<=> (min(LogAttr1,LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(min)
                    ;
                     CondType = binary_term_leq_coef ->
                        CondB #> -1 #=> CondCoef #>  max(MinAttr1,MinAttr2),                      % avoid case like min(x,y)>=2 (with x,y in [2,4]) as always true
                        CondB #> -1 #=> CondCoef #<  min(MaxAttr1,MaxAttr2),                      % avoid case like min(x,y)>=4 (with x,y in [2,4]) as equivalent to min(x,y)=4
                        Ctr3 = (LogCondBool #<=> (min(LogAttr1,LogAttr2) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(min)
                    ;
                        CondB #> -1 #=> CondCoef #>  max(MinAttr1,MinAttr2),                      % avoid case like min(x,y)=<2 (with x,y in [2,4]) as equivalent to min(x,y)=2
                        CondB #> -1 #=> CondCoef #<  min(MaxAttr1,MaxAttr2),                      % avoid case like min(x,y)=<4 (with x,y in [2,4]) as always true
                        Ctr3 = (LogCondBool #<=> (min(LogAttr1,LogAttr2) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(min)
                    )
                ;
                 CondBinary = max ->                                                                  % no order between the two attributes
                    element(CondAttr1, [0|MinAttrs], MinAttr1),
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    element(CondAttr1, [0|MaxAttrs], MaxAttr1),
                    element(CondAttr2, [0|MaxAttrs], MaxAttr2),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt,eq,geq,gt], 1, LCondAttr),
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                            max, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        CondB #> -1 #=> CondCoef #>= max(MinAttr1,MinAttr2),                      % avoid case like max(x,y)=0 (with x,y in [2,4]) as always false
                        CondB #> -1 #=> CondCoef #=< min(MaxAttr1,MaxAttr2),                      % avoid case like max(x,y)=5 (with x,y in [2,4]) as always false
                        Ctr3 = (LogCondBool #<=> (max(LogAttr1,LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(max)
                    ;
                     CondType = binary_term_leq_coef ->
                        CondB #> -1 #=> CondCoef #>  max(MinAttr1,MinAttr2),                      % avoid case like max(x,y)>=2 (with x,y in [2,4]) as always true
                        CondB #> -1 #=> CondCoef #<  min(MaxAttr1,MaxAttr2),                      % avoid case like max(x,y)>=4 (with x,y in [2,4]) as equivalent to max(x,y)=4
                        Ctr3 = (LogCondBool #<=> (max(LogAttr1,LogAttr2) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(max)
                    ;
                        CondB #> -1 #=> CondCoef #>  max(MinAttr1,MinAttr2),                      % avoid case like max(x,y)=<2 (with x,y in [2,4]) as equivalent to max(x,y)=2
                        CondB #> -1 #=> CondCoef #<  min(MaxAttr1,MaxAttr2),                      % avoid case like max(x,y)=<4 (with x,y in [2,4]) as always true
                        Ctr3 = (LogCondBool #<=> (max(LogAttr1,LogAttr2) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(max)
                    )
                ;
                 CondBinary = floor ->                                                            % num should not always be leq,lt than den
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    element(CondAttr2, [0|MaxAttrs], MaxAttr2),
                    (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0) #=> CondB #= -1,                          % do not select an attribute with can take value 0
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                            floor, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 1, LCondAttr),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(floor)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div LogAttr2) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(floor)
                    ;
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div LogAttr2) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(floor)
                    )
                ;
                 CondBinary = mfloor ->
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 1, LCondAttr), % num should not always be leq,lt,eq than den
                                                                                                  % (in theory should not be forbidden)
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                                        mfloor, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div max(LogAttr2-LogMinAttr2,1)) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(mfloor,MinAttr2)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div max(LogAttr2-LogMinAttr2,1)) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(mfloor,MinAttr2)
                    ;
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div max(LogAttr2-LogMinAttr2,1)) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(mfloor,MinAttr2)
                    )
                ;
                 CondBinary = mod ->                                                              % arg1 should not always be leq,lt than arg2
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    element(CondAttr2, [0|MaxAttrs], MaxAttr2),
                    (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0) #=> CondB #= -1,                          % no division by zero
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                            mod, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 1, LCondAttr),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 mod LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(mod)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 mod LogAttr2) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(mod)
                    ;
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 mod LogAttr2) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(mod)
                    )
                ;
                 CondBinary = prod ->
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                                        prod, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 * LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(prod)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 * LogAttr2) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(prod)
                    ;
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 * LogAttr2) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(prod)
                    )
                ;
                 CondBinary = ceil ->                                                             % num should not always be leq,lt than den
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    element(CondAttr2, [0|MaxAttrs], MaxAttr2),
                    (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0) #=> CondB #= -1,                          % no division by zero
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                                        ceil, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 1, LCondAttr),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> (((LogAttr1+LogAttr2-1) div LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(ceil)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> (((LogAttr1+LogAttr2-1) div LogAttr2) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(ceil)
                    ;
                        Ctr3 = (LogCondBool #<=> (((LogAttr1+LogAttr2-1) div LogAttr2) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(ceil)
                    )
                ;
                 CondBinary = cmod ->                                                             % arg1 should not allways be gt,geq than arg2
                    element(CondAttr1, [0|MinAttrs], MinAttr1),
                    element(CondAttr1, [0|MaxAttrs], MaxAttr1),
                    (MinAttr1 #=< 0 #/\ MaxAttr1 #>= 0) #=> CondB #= -1,                          % no division by zero
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                                        cmod, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [geq,gt], 1, LCondAttr),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr2 mod LogAttr1)) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(cmod)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr2 mod LogAttr1)) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(cmod)
                    ;
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr2 mod LogAttr1)) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(cmod)
                    )
                ;
                 CondBinary = dmod ->                                                             % arg1 should not always be leq,lt than arg2
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    element(CondAttr2, [0|MaxAttrs], MaxAttr2),
                    (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0) #=> CondB #= -1,                          % no division by zero
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                                        dmod, ColSetsBool, CondCoefMin, CondCoefMax, ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 1, LCondAttr),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr1 mod LogAttr2)) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(dmod)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr1 mod LogAttr2)) #=< LogCondCoef)),
                        NewCondType = binary_term_leq_coef(dmod)
                    ;
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr1 mod LogAttr2)) #>= LogCondCoef)),
                        NewCondType = binary_term_geq_coef(dmod)
                    )
                ;
                 CondBinary = fmod ->                                                               % arg1 should not always be leq,lt than arg2
                    element(CondAttr2, [0|MinAttrs], MinAttr2),
                    element(CondAttr2, [0|MaxAttrs], MaxAttr2),
                    (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0) #=> CondB #= -1,                            % no division by zero
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 1, Oplus, Negated, CondType,
                                                                                        fmod, ColSetsBool, CondCoefMin, CondCoefMax,
                                                                                        ValuesAttrPairs),
                    ValuesAttrPairs = [_,_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]], ValuesAttrPairs),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 1, LCondAttr),   % avoid reified constraint for Ctr3 as when
                    Ctr3 = (LogMod #= (LogAttr1 mod (LogAttr2 + (LogCondB #= -1)))),                % LogCondB=-1 (LogAttr2=0) and no mod 0 op.
                    Ctr4 = (LogModZ #<=> (LogMod #= 0)),
                    (CondType = binary_term_eq_coef ->
                        Ctr5 = (LogCondBool #<=> ((LogModZ * LogAttr2 + LogMod #=  LogCondCoef))),
                        NewCondType = binary_term_eq_coef(fmod)
                    ;
                     CondType = binary_term_leq_coef ->
                        Ctr5 = (LogCondBool #<=> ((LogModZ * LogAttr2 + LogMod #=< LogCondCoef))),
                        NewCondType = binary_term_leq_coef(fmod)
                    ;
                        Ctr5 = (LogCondBool #<=> ((LogModZ * LogAttr2 + LogMod #>= LogCondCoef))),
                        NewCondType = binary_term_geq_coef(fmod)
                    )
                ;
                    true
                ),
                (CondBinary = mfloor ->
                    SourceVarsGlobal = [LogCondB, LogCondAttr1,LogCondAttr2,LogCondCoef,LogMinAttr2],
                    TargetVarsGlobal = [   CondB,    CondAttr1,   CondAttr2,   CondCoef,   MinAttr2]
                ;
                    SourceVarsGlobal = [LogCondB, LogCondAttr1,LogCondAttr2,LogCondCoef],
                    TargetVarsGlobal = [   CondB,    CondAttr1,   CondAttr2,   CondCoef]
                ),
                append([[LogCondBool,LogAttr1,LogAttr2], LogAttributes, [LogRes]], SourceVarsLocal),
                (CondBinary = fmod ->
                    append([Ctr1,Ctr2,Ctr3,Ctr4,Ctr5], LCtr, SourceCtrs)                         % list of constraints to post on each table entry
                ;
                    append([Ctr1,Ctr2,Ctr3], LCtr, SourceCtrs)                                   % list of constraints to post on each table entry
                ),
                LCondCostVars = [Cost],
                CondCheckOrder = 0
            ;
                write(formula_generator_bool_cond1m), nl, halt
            )
        ;
            write(formula_generator_bool_cond1n), nl, halt
        )
    ;

    % cond(sum_leq_attr)  :      CondAttr1+CondAttr2                =< CondAttr3
    % cond(minus_mod_eq0) :     (CondAttr3-CondAttr1) mod CondAttr2 =  0
    % cond(ceil_leq_floor): ceil(CondAttr1-CondAttr3,CondAttr2)     =< floor(CondAttr1-CondAttr2,CondAttr3)
    % cond(attr_geq_fmod):                                CondAttr1 >= fmod(CondAttr2,CondAttr3)
    % cond(mod_gt_mod) :                     (LogAttr2 mod LogAttr3 >  LogAttr1 mod LogAttr3)
    %.....................................................................................................................................
    (memberchk(CondType,[sum_leq_attr,minus_mod_eq0,ceil_leq_floor,attr_geq_fmod,mod_gt_mod]),
     CondArity = 0, LenA < 3) ->
        false
    ;
    (memberchk(CondType,[sum_leq_attr,minus_mod_eq0,ceil_leq_floor,attr_geq_fmod,mod_gt_mod]),
     DoNotGenerateTernaryCond = 1) ->
        false
    ;
    (memberchk(CondType,[sum_leq_attr,minus_mod_eq0,ceil_leq_floor,attr_geq_fmod,mod_gt_mod]),
     CondArity = 0, LenA >= 3) ->
        LCondAttr      = [CondAttr1,CondAttr2,CondAttr3],                                    % list of shifted index of attribute variables used
        LCondExtraVars = [condb(CondB)],                                                     % list of additional variables
        domain(LCondAttr, 1, LenA1),                                                         % variables of the first, second, and third attributes
                                                                                             % the table constraint posted by
                                                                                             % gen_table_ctr_wrt_authorised_cmp forces
                                                                                             % all three attributes to be distinct
        (memberchk(CondType, [ceil_leq_floor, mod_gt_mod]) ->
             gen_table_ctr_wrt_authorised_cmp(LenA, 3,                                       % condition not used (arg1=arg2=arg3=1) or
                                              MandatoryAttr,                                 % if condition is used then:
                                              [geq,gt],                                      %  . arg1 should always be geq or gt than arg2
                                              1,                                             %  . arg2 should always be geq or gt than arg3
                                              LCondAttr)                                     % 1 as shift index by 1
        ;
         CondType = attr_geq_fmod ->
             true
        ;
             gen_table_ctr_wrt_authorised_cmp(LenA, 3,                                       % condition not used (arg1=arg2=arg3=1) or
                                              MandatoryAttr,                                 % if condition is used then:
                                              [leq,lt],                                      %  . arg1 should always be leq or lt than arg2
                                              1,                                             %  . arg2 should always be leq or lt than arg3
                                              LCondAttr)                                     % 1 as shift index by 1
        ),
        CondB #= -1 #<=> CondAttr1 #= 1,                                                     % set shifted index to 1 if condition not used
        CondB #= -1 #<=> CondAttr2 #= 1,                                                     % set shifted index to 1 if condition not used
        CondB #= -1 #<=> CondAttr3 #= 1,                                                     % set shifted index to 1 if condition not used
        CondCoef = 0,                                                                        % no coefficient (as just use two attributes)
        CondCst  = 0,                                                                        % no constant    (as just use two attributes)
        Cost in 0..2,                                                                        % cost will be 2 if use this type of condition
        CondB #= -1 #<=> Cost #= 0,
        CondB #> -1 #<=> Cost #= 2,
        get_set_of_ternary_triplets_of_mandatory_attributes(MandatoryAttr, 1, CondType, ColSetsBool, TripletsList),
        TripletsList = [_,_|_],
        table([LCondAttr], TripletsList),
        Ctr1 = element(LogCondAttr1, LogAttributes0, LogAttr1),
        Ctr2 = element(LogCondAttr2, LogAttributes0, LogAttr2),
        Ctr3 = element(LogCondAttr3, LogAttributes0, LogAttr3),
        (CondType = sum_leq_attr ->
            Ctr4 = (LogCondBool #<=> (LogAttr1+LogAttr2 #=< LogAttr3))
        ;
         CondType = minus_mod_eq0 ->
            Ctr4 = (LogCondBool #<=> ((LogAttr3-LogAttr1) mod LogAttr2 #= 0))
        ;
         CondType = attr_geq_fmod ->
            element(CondAttr3, [0|MinAttrs], MinAttr3),
            element(CondAttr3, [0|MaxAttrs], MaxAttr3),
            (MinAttr3 #=< 0 #/\ MaxAttr3 #>= 0) #=> CondB #= -1,                             % no division by zero
            Ctr4 = (LogMod #= (LogAttr2 mod (LogAttr3 + (LogCondB #= -1)))),                % LogCondB=-1 (LogAttr2=0) and no mod 0 op.
            Ctr5 = (LogModZ #<=> (LogMod #= 0)),
            Ctr6 = (LogCondBool #<=> (LogAttr1 #>= (LogModZ * LogAttr3 + LogMod)))
        ;
         CondType = mod_gt_mod ->
            Ctr4 = (LogCondBool #<=> (LogAttr2 mod LogAttr3 #> LogAttr1 mod LogAttr3))
        ;
            element(CondAttr2, [0|MinAttrs], MinAttr2),
            element(CondAttr2, [0|MaxAttrs], MaxAttr2),
            (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0) #=> CondB #= -1,                             % no division by zero
            element(CondAttr3, [0|MinAttrs], MinAttr3),
            element(CondAttr3, [0|MaxAttrs], MaxAttr3),
            (MinAttr3 #=< 0 #/\ MaxAttr3 #>= 0) #=> CondB #= -1,                             % no division by zero
            Ctr4 = (LogCondBool #<=> (((LogAttr1 - LogAttr3 + LogAttr2 - 1) / LogAttr2) #=< ((LogAttr1 - LogAttr2) / LogAttr3)))
        ),
        gen_table_constraint_for_a_condition(Oplus, NbTerms, LogRes, LogCondB, LogCondBool, LCtr),
        (CondType = attr_geq_fmod ->
            append([Ctr1,Ctr2,Ctr3,Ctr4,Ctr5,Ctr6], LCtr, SourceCtrs)                        % list of constraints to post on each table entrytrue
        ;
            append([Ctr1,Ctr2,Ctr3,Ctr4], LCtr, SourceCtrs)                                  % list of constraints to post on each table entry
        ),
        append([[LogCondBool,LogAttr1,LogAttr2,LogAttr3], LogAttributes, [LogRes]], SourceVarsLocal),
        SourceVarsGlobal = [LogCondB,LogCondAttr1,LogCondAttr2,LogCondAttr3],
        TargetVarsGlobal = [CondB,CondAttr1,CondAttr2,CondAttr3],
        NewCondType      = CondType,
        LCondCostVars    = [Cost],
        CondCheckOrder   = 0
    ;
        write(formula_generator_bool_cond1o(CondType,CondArity)), nl, halt
    ),
    (UseMods = 0 ->                                                                          % if should no use mod
        all_mod_conds(ListModConds),                                                         % then fail if we just generate a
        nonmember(NewCondType, ListModConds)                                                 % condition using mod
    ;
        true
    ),
    (is_list(LCondAttr) ->
        (memberchk(CondType,[attr_eq_attr,attr_eq_coef]) ->                                  % can only use Boolean attributes
            true                                                                             % in attr_eq_attr or attr_eq_coef
        ;                                                                                    % otherwise filter Boolean attributes
            filter_boolean_attrs(LCondAttr, ShiftedBoolAttrs)                                % from a condition mentionning
        )                                                                                    % arithmetic operators
    ;
        write(formula_generator_bool_cond1p(CondType,CondArity)), nl, halt
    ).

filter_boolean_attrs([], _) :- !.
filter_boolean_attrs([CondAttr|R], ShiftedBoolAttrs) :-
    filter_boolean_attr(ShiftedBoolAttrs, CondAttr),
    filter_boolean_attrs(R, ShiftedBoolAttrs).

filter_boolean_attr([], _) :- !.
filter_boolean_attr([Val|R], CondAttr) :-
    CondAttr #\= Val,
    filter_boolean_attr(R, CondAttr).

% for a condition generate a table constraint that will be posted on each table entry for linking Res, CondB and CondBool
% use call_imply_cond_ctr rather than reified constraint as LogRes is fixed when we post such constraint (save a lot of memory, increase performance)
gen_table_constraint_for_a_condition(and,    _NbTerms, LogRes, LogCondB, LogCondBool,
                                            [call_imply_cond_ctr(LogRes, 1, table([[LogCondB,LogCondBool]], [[-1,0],[-1,1],[0,0],[1,1]]))]) :- !.
gen_table_constraint_for_a_condition(or,     _NbTerms, LogRes, LogCondB, LogCondBool,
                                            [call_imply_cond_ctr(LogRes, 0, table([[LogCondB,LogCondBool]], [[-1,0],[-1,1],[0,1],[1,0]]))]) :- !.
gen_table_constraint_for_a_condition(sum,    NbTerms, LogRes, LogCondB, LogCondBool,
                                            [call_imply_cond_ctr(LogRes, NbTerms, table([[LogCondB,LogCondBool]], [[-1,0],[-1,1],[0,0],[1,1]]))]) :- !.
gen_table_constraint_for_a_condition(Oplus, _NbTerms, _LogRes, _LogCondB, _LogCondBool, []) :-
    memberchk(Oplus, [allequal, xor, voting, card1]), !.
gen_table_constraint_for_a_condition(Oplus, _, _, _, _, _) :- write(gen_table_constraint_for_a_condition(Oplus)), nl, halt.

% generate some extra constraints (used on every row) that USES ALL Boolean conditions
formula_generator_bool_extra_ctr(Oplus,
                                 NbTerms,
                                 LogRes,
                                 LCondB,
                                 LLogCondB,
                                 LLogCondBool,
                                 SourceVarsLocal,
                                 SourceVarsGlobal,
                                 TargetVarsGlobal,
                                 CSourceCtrs) :-
    (Oplus = and  ->
        build_additional_term_for_and(LLogCondB, LLogCondBool, AndTerm),
        CSourceCtrs = [call_imply_cond_ctr(LogRes, 0, AndTerm)]
    ;
     Oplus = or ->
        build_additional_term_for_or(LLogCondB, LLogCondBool, OrTerm),
        CSourceCtrs = [call_imply_cond_ctr(LogRes, 1, OrTerm)]
    ;
     Oplus = sum ->
        build_additional_term_for_sum(LLogCondB, LLogCondBool, SumTerm),
        CSourceCtrs = [(LogRes #= SumTerm)]
    ;
     Oplus = allequal ->
        build_additional_terms_for_allequal(LLogCondB, LLogCondBool, SumPosTerm, SumNegTerm),
        Ctr1 = call_imply_cond_ctr(LogRes, 0,  SumPosTerm #< NbTerms),
        Ctr2 = call_imply_cond_ctr(LogRes, 0,  SumNegTerm #< NbTerms),
        Ctr3 = call_imply_cond_ctr(LogRes, 1, (SumPosTerm #= NbTerms) #\/ (SumNegTerm #= NbTerms)),
        CSourceCtrs = [Ctr1,Ctr2,Ctr3]
    ;
     Oplus = xor ->
        build_additional_term_for_sum(LLogCondB, LLogCondBool, SumTerm),
        CSourceCtrs = [(LogRes #= SumTerm mod 2)]
    ;
     Oplus = voting ->
        build_additional_term_for_sum(LLogCondB, LLogCondBool, SumTerm),
        MinVote is (NbTerms+1) // 2,
        CSourceCtrs = [(LogRes #= (SumTerm #>= MinVote))]
    ;
     Oplus = card1 ->
        build_additional_term_for_sum(LLogCondB, LLogCondBool, SumTerm),
        CSourceCtrs = [(LogRes #= (SumTerm #= 1))]
    ;
        write(formula_generator_bool_extra_ctr(Oplus)), nl, halt
    ),
    append([LogRes], LLogCondBool, SourceVarsLocal),
    SourceVarsGlobal = LLogCondB,
    TargetVarsGlobal = LCondB.

build_additional_term_for_and([LogCondB],   [LogCondBool],   (LogCondB #= (#\ LogCondBool))) :- !.
build_additional_term_for_and([LogCondB|R], [LogCondBool|S], (LogCondB #= (#\ LogCondBool)) #\/ T) :-
    build_additional_term_for_and(R, S, T).

build_additional_term_for_or([LogCondB],   [LogCondBool],   (LogCondB #= LogCondBool)) :- !.
build_additional_term_for_or([LogCondB|R], [LogCondBool|S], (LogCondB #= LogCondBool) #\/ T) :-
    build_additional_term_for_or(R, S, T).

build_additional_term_for_sum([LogCondB],   [LogCondBool],   (LogCondB #= LogCondBool)) :- !.
build_additional_term_for_sum([LogCondB|R], [LogCondBool|S], (LogCondB #= LogCondBool) + T) :-
    build_additional_term_for_sum(R, S, T).

build_additional_terms_for_allequal([LogCondB],   [LogCondBool],   (LogCondB #= LogCondBool),   (LogCondB #= (#\ LogCondBool))) :- !.
build_additional_terms_for_allequal([LogCondB|R], [LogCondBool|S], (LogCondB #= LogCondBool)+T, (LogCondB #= (#\ LogCondBool))+U) :-
    build_additional_terms_for_allequal(R, S, T, U).

% retrict valid combination of number of unary conditions, number of binary conditions, and number of ternary conditions
% (set a flag AdjustMaxOccAttrToOne that will be used for posting some constraint when set to 1)
restrict_nb_unary_nb_binary_nb_ternary(Conds, UnariesBinariesTernaries, NbTerms, NbAttrs, t(NbUnary,NbBinary,NbTernary,AdjustMaxOccAttrToOne)) :-
    pos_bool_cond_entry(condb,     CondBPos),
    pos_bool_cond_entry(lcondattr, LCondAttrPos),
    get_nb_unary_nb_binary_nb_ternary(Conds, CondBPos, LCondAttrPos, TermUnary, TermBinary, TermTernary),
    NbUnary   in 0..NbTerms,
    NbBinary  in 0..NbTerms,
    NbTernary in 0..NbTerms,
    call(NbUnary   #= TermUnary),                      % number of conditions in the Boolean term mentionning one single attribute
    call(NbBinary  #= TermBinary),                     % number of conditions in the Boolean term mentionning two attributes
    call(NbTernary #= TermTernary),                    % number of conditions in the Boolean term mentionning three attributes
    table([[NbUnary,NbBinary,NbTernary]], UnariesBinariesTernaries),
    ((NbAttrs > 1, integer(NbUnary), integer(NbBinary), integer(NbTernary)) ->
        MaxAttr is NbUnary + 2*NbBinary + 3*NbTernary, % max.number of attributes we can generate
        (MaxAttr = NbAttrs ->                          % from NbUnary unary cond., NbBinary binary cond., NbTernary ternary cond.
            AdjustMaxOccAttrToOne = 1                  % set to one as all attributes will have to be used exactly once
        ;
            AdjustMaxOccAttrToOne = 0                  % set to zero as not sure that all attributes will have to be used exactly once
        )
    ;
        AdjustMaxOccAttrToOne = 0                      % set to zero as NbUnary, NbBinary and NbTernary not all yet fixed
    ).

% build three terms which respectively give the number of unary, binary, and ternary conditions used in a Boolean term
get_nb_unary_nb_binary_nb_ternary([], _, _, 0, 0, 0) :- !.
get_nb_unary_nb_binary_nb_ternary([Cond|R], CondBPos, LCondAttrPos, (CondB #>= 0)+S, T, U) :-
    arg(LCondAttrPos, Cond, [_]),                                                              % unary condition as exactly one attribute
    !,
    arg(CondBPos, Cond, CondB),
    get_nb_unary_nb_binary_nb_ternary(R, CondBPos, LCondAttrPos, S, T, U).
get_nb_unary_nb_binary_nb_ternary([Cond|R], CondBPos, LCondAttrPos, S, (CondB #>= 0)+T, U) :-
    arg(LCondAttrPos, Cond, [_,_]),                                                            % binary condition as exactly two attributes
    !,
    arg(CondBPos, Cond, CondB),
    get_nb_unary_nb_binary_nb_ternary(R, CondBPos, LCondAttrPos, S, T, U).
get_nb_unary_nb_binary_nb_ternary([Cond|R], CondBPos, LCondAttrPos, S, T, (CondB #>= 0)+U) :-
    arg(LCondAttrPos, Cond, [_,_,_]),                                                          % ternary condition as exactly three attributes
    !,
    arg(CondBPos, Cond, CondB),
    get_nb_unary_nb_binary_nb_ternary(R, CondBPos, LCondAttrPos, S, T, U).
get_nb_unary_nb_binary_nb_ternary([Cond|_], _, _, _, _, _) :-
    write(get_nb_unary_nb_binary_nb_ternary(Cond)), nl, halt.

% as duplicated conditions both have the same type and are located consecutively in the list of duplicated conditions,
% go through the list of conditions and
% post a symmetry breaking constraint on pair of consecutive conditions that share the same type
break_symmetry_between_duplicated_conds([], _, _, _) :- !.
break_symmetry_between_duplicated_conds([_], _, _, _) :- !.
break_symmetry_between_duplicated_conds([Cond1,Cond2|R], PosCondB, PosNewCondType, PosLCondAttr) :-
    arg(PosCondB,       Cond1, CondB1),     % get variable telling whether we use or not the condition, its type and its shifted indices
    arg(PosNewCondType, Cond1, Type1),
    arg(PosLCondAttr,   Cond1, LCondAttr1),
    arg(PosCondB,       Cond2, CondB2),     % get variable telling whether we use or not the condition, its type and its shifted indices
    arg(PosNewCondType, Cond2, Type2),
    arg(PosLCondAttr,   Cond2, LCondAttr2),
    (Type1 = Type2 ->                       % if the two conditions have the same type they were duplicated, so break symmetry
        Vector1 = [CondB1|LCondAttr1],      % put together the variable telling whether we use or not the condition and its shifted indices
        Vector2 = [CondB2|LCondAttr2],      % put together the variable telling whether we use or not the condition and its shifted indices
        gen_bool_cond_symmetry_breaking_automaton(Vector1, Vector2)
    ;
        true
    ),
    break_symmetry_between_duplicated_conds([Cond2|R], PosCondB, PosNewCondType, PosLCondAttr).

% INPUT:
% Vector1 = [CondB1|LCondAttr1] where
%  . CondB1=-1 if the condition is not used,
%  . CondB1= 0 if the negation of the condition is used
%  . CondB1= 1 if the condition is used
%  . LCondAttr1 is the list of shifted column indices used as arguments of the condition.
%
% Vector2 = [CondB2|LCondAttr2] with a similar meaning.
%
% We break symmetries by imposing:
%  (1) if   Condition1 is unused      (i.e. CondB1=-1, all shifted indices in LCondAttr1 are set to 1)
%      then Condition2 is also unused (i.e. CondB2=-1, all shifted indices in LCondAttr2 are set to 1)
%  (2) if   Condition1 is used        (i.e. CondB1>-1, all shifted indices in LCondAttr1 are greater than 1)
%      then either
%       (2.1) Condition2 is unused,
%       (2.2) Condition2 is used with a vector of arguments that is lexicographically smaller than
%             the vector of arguments of Condition1
gen_bool_cond_symmetry_breaking_automaton(Vector1, Vector2) :-
    gen_signature_bool_cond_symmetry_breaking_automaton(Vector1, Vector2, Signature),
    automaton(Signature, _, Signature, 
              [source(r),sink(s),sink(t),sink(x)],
              [arc(r,0,s),
               arc(r,1,t),
               arc(r,2,v),
               arc(r,3,w),
               arc(r,4,w),
               arc(s,4,s),
               arc(t,5,t),
               arc(v,6,v),
               arc(w,6,w),
               arc(v,7,x),
               arc(v,8,x),
               arc(w,7,x),
               arc(x,6,x),
               arc(x,7,x),
               arc(x,8,x)],
               [], [], []).

gen_signature_bool_cond_symmetry_breaking_automaton([], [], []) :- !.
gen_signature_bool_cond_symmetry_breaking_automaton([C1|R1], [C2|R2], [L|R]) :-
    L in 0..7,
    C1 #= -1 #/\ C2 #= -1                 #<=> L #= 0, % both conditions are unused
   (C1 #=  0 #\/ C1 #=  1) #/\ C2 #= -1   #<=> L #= 1, % only one condition is used
    C1 #=  1 #/\ C2 #=  0                 #<=> L #= 2, % the first condition is used, negated or not, and the negation of the second condition is used
    C1 #=  0 #/\ C2 #=  0                 #<=> L #= 3, % the first condition is used, negated or not, and the negation of the second condition is used
    C1 #=  1 #/\ C2 #=  1                 #<=> L #= 4, % as the first component of vectors it means that both conditions are used in a positive way;
                                                       % otherwise means that the attributes of both vectors are unused.
    C1 #>  1 #/\ C2 #=  1                 #<=> L #= 5, % C1 is an attribute of the 1st condition, and C2 an unused attribute of the 2nd condition
                                                       % (as the 2nd condition is unused)
    C1 #>  1 #/\ C2 #>  1  #/\ C1 #= C2   #<=> L #= 6, % C1 and C2 are attributes of the two used conditions, such that C1 = C2
    C1 #>  1 #/\ C2 #>  1  #/\ C1 #> C2   #<=> L #= 7, % C1 and C2 are attributes of the two used conditions, such that C1 > C2
    C1 #>  1 #/\ C2 #>  1  #/\ C1 #< C2   #<=> L #= 8, % C1 and C2 are attributes of the two used conditions, such that C1 < C2
    gen_signature_bool_cond_symmetry_breaking_automaton(R1, R2, R).

forbid_specific_combinations_of_conds([], _, _, _, _, _, _, _, _, _, _) :- !.
forbid_specific_combinations_of_conds([Cond|R], Oplus, NbTerms, MandatoryAttr, LConds,  MinAttrs, MaxAttrs, ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr) :-
    arg(PosNewCondType, Cond, Type),
    pos_bool_cond_entry(lcondcst,       PosCondCst),
    pos_bool_cond_entry(lcondcoef,      PosCondCoef),
    forbid_specific_combinations_of_conds(R, Cond-Type, Oplus, NbTerms, MandatoryAttr, LConds,
                                          MinAttrs, MaxAttrs, ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr, PosCondCoef, PosCondCst),
    forbid_specific_combinations_of_conds(R, Oplus, NbTerms, MandatoryAttr, LConds,  MinAttrs, MaxAttrs,ColSetsBool, PosCondB, PosNewCondType, PosLCondAttr).

% for cond_ex
avoid_simplifiable_conditional([],_,_,_,_,_,_,_,_) :- !.
avoid_simplifiable_conditional([Cond|R], MandatoryAttr,  Oplus,
                               NewThenType, NewElseType,
                               LThenAttr,   LElseAttr,
                               ThenCoef,    ElseCoef) :-
    pos_bool_cond_entry(condb,             PosCondB),
    pos_bool_cond_entry(lcondattr,     PosLCondAttr),
    pos_bool_cond_entry(lcondcoef,      PosCondCoef),
    pos_bool_cond_entry(newcondtype, PosNewCondType),
    arg(PosCondB,       Cond, CondB),
    arg(PosLCondAttr,   Cond, LCondAttr),
    arg(PosCondCoef,    Cond, CondCoef),
    arg(PosNewCondType, Cond, NewCondType),
    ((Oplus = and, NewCondType = attr_eq_attr) -> % no need to use one of the attributes for THEN
        LCondAttr = [_,CondAttr],
        (foreach(ThenAttr,LThenAttr), param([CondAttr,CondB])
        do
         (CondB #= 1) #=> (CondAttr #\= ThenAttr)
        )
    ;
     (Oplus = and, NewCondType = attr_eq_coef) -> % no need to use the attribute for THEN
        LCondAttr = [CondAttr],
        (foreach(ThenAttr,LThenAttr), param([CondAttr,CondB])
        do
         (CondB #= 1) #=> (CondAttr #\= ThenAttr)
        )
    ;
     (Oplus = and, NewCondType = attr_leq_coef) -> % no need to use the attribute for ELSE
        LCondAttr = [CondAttr],
        tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs),                           % get list of maximum values of the mandatory attributes
        element(CondAttr, MaxAttrs, MaxAttr1),                                      % get largest value of attribute CondAttr1
        (foreach(ElseAttr,LElseAttr), param([CondB,CondAttr,MaxAttr1,CondCoef])
        do
         ((CondB #= 1)#/\(MaxAttr1 #= CondCoef + 1)) #=> (CondAttr #\= ElseAttr)
        )
    ;
     (Oplus = and, NewCondType = attr_geq_coef) -> % no need to use the attribute for ELSE
        LCondAttr = [CondAttr],
        tab_get_mins_attr_names(MandatoryAttr, MinAttrs),                           % get list of maximum values of the mandatory attributes
        element(CondAttr, MinAttrs, MinAttr1),                                      % get largest value of attribute CondAttr1
        (foreach(ElseAttr,LElseAttr), param([CondB,CondAttr,MinAttr1,CondCoef])
        do
         ((CondB #= 1)#/\(MinAttr1 #= (CondCoef - 1))) #=> (CondAttr #\= ElseAttr)
        )
    ;
        true
    ),
    avoid_simplifiable_conditional(R, MandatoryAttr, Oplus,
                                   NewThenType, NewElseType,
                                   LThenAttr,   LElseAttr,
                                   ThenCoef,    ElseCoef).

restrict_specific_combinations_of_conds(Conds, NbTerms, MinNbTerms, Types, PosCondB, PosNewCondType) :-
    (NbTerms > MinNbTerms ->
        get_all_condb_in_a_given_set_of_types(Conds, Types, PosCondB, PosNewCondType, LCondB),
        (LCondB = [_|_] ->
            MinUnused is NbTerms-MinNbTerms,
            count(-1, LCondB, #>=, MinUnused)
        ;
            true
        )
    ;
        true
    ).

get_all_condb_in_a_given_set_of_types([], _, _, _, []) :- !.
get_all_condb_in_a_given_set_of_types([Cond|R], Types, PosCondB, PosNewCondType, [CondB|S]) :-
    arg(PosNewCondType, Cond, TypeParam),
    memberchk(TypeParam, Types),
    !,
    arg(PosCondB, Cond, CondB),
    get_all_condb_in_a_given_set_of_types(R, Types, PosCondB, PosNewCondType, S).
get_all_condb_in_a_given_set_of_types([_|R], Types, PosCondB, PosNewCondType, S) :-
    get_all_condb_in_a_given_set_of_types(R, Types, PosCondB, PosNewCondType, S).

incompatible_families(Conds, LFamilies, PosCondB, PosNewCondType) :-
    length(LFamilies, Len),
    length(LAnchors,  Len),
    incompatible_families(Conds, LFamilies, PosCondB, PosNewCondType, LAnchors),
    unused_families(LAnchors, LUnused),
    MinUnused is Len-1,
    count(-1, LUnused, #>=, MinUnused).

unused_families([], []) :- !.
unused_families([L|R], [U|S]) :-
    U in -1..1,
    (L = [_|_] ->
        maximum(U, L)
    ;
        U = -1
    ),
    unused_families(R, S).

incompatible_families([], _, _, _, LAnchors) :- !,
    set_to_empty_list(LAnchors).
incompatible_families([Cond|R], LFamilies, PosCondB, PosNewCondType, LAnchors) :-
    find_cond_family(LFamilies, Cond, PosCondB, PosNewCondType, LAnchors, LNewAnchors),
    incompatible_families(R, LFamilies, PosCondB, PosNewCondType, LNewAnchors).

find_cond_family([], _, _, _, L, L) :- !.
find_cond_family([Types|_], Cond, PosCondB, PosNewCondType, [Anchor|S], [NewAnchor|S]) :-
    arg(PosNewCondType, Cond, TypeParam),
    memberchk(TypeParam, Types),
    !,
    arg(PosCondB, Cond, CondB),
    Anchor = [CondB|NewAnchor].
find_cond_family([_|R], Cond, PosCondB, PosNewCondType, [Anchor|S], [Anchor|T]) :-
    find_cond_family(R, Cond, PosCondB, PosNewCondType, S, T).

set_to_empty_list([]) :- !.
set_to_empty_list([[]|R]) :-
    set_to_empty_list(R).

% TODO: check why did not use forbid_cols_that_cant_intersect_min_max, and check restriction wrt function used
%       (most likely we may need to extract metadata from entries related to 0 or 1 and wrt a Boolean target column)
