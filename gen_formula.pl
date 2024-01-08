% Purpose: GENERATE A PARAMETERISED CANDIDATE FORMULA WRT SOME GIVEN OPTIONS,
%          AND ON BACTRACKING PRODUCE THE NEXT CANDIDATE FORMULA
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_formula,[is_boolean_formula/1,                              % return true if we have an Boolean formula, false otherwise
                       is_unconditional_formula/1,                        % return true if we have an polynomial formula, false otherwise
                       get_cond_cost_vars/2,                              % get list of variables used to compute the cost of the cond part
                       get_then_cost_vars/2,                              % get list of variables used to compute the cost of the then part
                       get_else_cost_vars/2,                              % get list of variables used to compute the cost of the else part
                       get_nber_of_used_unary_binary_functions/2,         % correction added to penalize the cost of a conditional formula
                       formula_generator/17,                              % generate a candidate formula where coefficient are not yet fixed
                       clear_type_found_formula/0,                        % remove all previous formulae we found as on a new table
                       record_type_found_formula/3,                       % remove type of previous formula we found and record current one
                       keep_conditional_formula/2]).                      % true if found already a Boolean formula with a small cost for a given col

:- use_module(library(timeout)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(gen_poly_formula).
:- use_module(gen_bool_formula).
:- use_module(gen_cond_formula).
:- use_module(gen_options_for_formulae).
:- use_module(utility).
:- use_module(gen_clusters_val_fds).
:- use_module(table_access).
:- use_module(instrument_code).

% OPTIONS EXAMPLE
%----------------
% [nb_polynom([1]),                               one polynom in the formula
%  nb_unary([1]),                                 one unary term in the formula
%  nb_binary([0]),                                no binary term in the formula
%  main_formula([2-t([                            the formula is a unary function applied to a polynom
%                     degree(3,3),                all monomes of the polynom have degree 3
%                     non_zero(1,2,0,0),          no monome of degree between 1 and 2
%                     non_zero(3,3,0,2),          at most two monomes of degree 3
%                     coef(0,1),                  coefficients of all monomes are between 0 and 1
%                     cst(1,1)])]),               constant of the polynom is equal to 1
%  unary([max(3,4)]),                             the unary term is a max between an attribute and a constant in 3..4
%  unary_function([power(2)]),                    the unary function is the square function
%  mandatory_attr([col(v_c_minc_maxc_s_maxa,1)]), the first  column of table v_c_minc_maxc_s_maxa is a mandatory attribute
%  optional_attr([col(v_c_minc_maxc_s_maxa,2),    the second column of table v_c_minc_maxc_s_maxa is an optional attribute
%                 col(v_c_minc_maxc_s_maxa,3),    the third  column of table v_c_minc_maxc_s_maxa is an optional attribute
%                 col(v_c_minc_maxc_s_maxa,4),    the fourth column of table v_c_minc_maxc_s_maxa is an optional attribute
%                 col(v_c_minc_maxc_s_maxa,5)]),  the fifth  column of table v_c_minc_maxc_s_maxa is an optional attribute
%                 min_attr(3),                    the formula involves at least 3 attributes
%                 max_attr(3),                    the formula involves at most  3 attributes
%  table_attr([[col(v_c_minc_maxc_s_maxa,1),      the formula should involve one of the two following combination of attributes:
%               col(v_c_minc_maxc_s_maxa,2),      (1,2,5) or (1,3,4)
%               col(v_c_minc_maxc_s_maxa,5)],
%              [col(v_c_minc_maxc_s_maxa,1),
%               col(v_c_minc_maxc_s_maxa,3),
%               col(v_c_minc_maxc_s_maxa,4)]])],

% FORMULAE GENERATED FROM THE PREVIOUS OPTION EXAMPLE
% (minc^2.max(v,3) + minc^2.maxc + 1)^2
% (minc.maxc^2 + minc^2.max(v,3) + 1)^2
% (minc.maxc.max(v,3) + 1)^2
% (minc.maxc.max(v,3) + minc^3 + 1)^2
% .....................................
% (s.max(v,3)^2 + v.c^2 + 1)^2
% (s.max(v,3)^2 + v.c.s + 1)^2
% (s.max(v,3)^2 + v.c.max(v,3) + 1)^2
% (max(v,3)^3 + v.c.s + 1)^2

% OPTIONS USED FOR PARAMETRISING A FORMULA (AND DEFAULT VALUES OF THESE OPTIONS)
%-------------------------------------------------------------------------------
% for each polynom of a main formula we may specify a list of options in the following list (option no_zero may occur more than once):
%  . degree(dmin,dmax)        : restrict the minimum/maximum degree of a polynom to be in dmin,dmax (note that this option is required)
%  . param(pmax)              : restrict the maximum number of parameters of a polynom, i.e. number of attributes + number of unary/binary terms
%  . non_zero(dmin,dmax,nzmax): restrict the maximum number of non zeros coefficients among the coefficient associated with monomes whose
%                               degree is in dmin,dmax
%  . coef(min,max)            : restrict the minimum/maximum value of the coefficients attached to a monome that mention at least one attribute
%  . cst(min,max)             : restrict the minimum/maximum value of the coefficients attached to a monome that does not mention any attribute

max_main_formula(3). % maximum id of main formula

                                                                                          % DEFAULT OPTIONS FOR A CONDITIONAL FORMULA
                                                                                          %   use at least one cond or one then or one else,
                                                                                          %   otherwise assume we have a main formula
default_option(cond,            [attr_eq_coef,attr_eq_attr,attr_leq_coef,attr_leq_attr]). % default condition of a conditional formula
default_option(then,            [cst,attr]).                                              % default then part of a conditional formula
default_option(else,            [cst,attr]).                                              % default else part of a conditional formula

                                                                          % DEFAULT OPTIONS FOR A MAIN FORMULA
default_option(mandatory_attr,  []).                                      % mandatory attributes: those which should occur in the formula
default_option(optional_attr,   []).                                      % optional  attributes: those which may be skipped
default_option(min_attr,        [0]).                                     % minimum number of attributes
default_option(max_attr,        [9]).                                     % maximum number of attributes
default_option(table_attr,      []).                                      % []: option not used by default
default_option(nb_polynom,      [0,1,2]).                                 % 0: no polynom,     1: one polynom,    2: two polynoms
default_option(nb_unary,        [0,1]).                                   % 0: no unary term,  1: one unary term
default_option(nb_binary,       [0,1]).                                   % 0: no binary term, 1: one binary term
default_option(main_formula,    [0-t,                                     % 0: constant        (no polynom)
                                 1-t([degree(1,1)]),                      % 1: polynom         (min degree = max degree = 1)
                                 2-t([degree(1,1)]),                      % 2: unary function  (min degree = max degree = 1)
                                 3-t([degree(1,1)],[degree(1,1)])]).      % 3: binary function (min degree = max degree = 1 for both polynoms)
default_option(unary,           [min,max,floor,ceil,mod,                  % default unary  functions which can be used for a unary  term
                                 in,geq,power(2),sum_consec]).
default_option(binary,          [min,max,floor,mfloor,mod,prod,ceil,      % default binary functions which can be used for a binary term
                                 cmod,dmod,fmod]).
default_option(unary_function,  [in,geq0,power(2),sum_consec]).           % default unary  functions which can be used for a unary  function
default_option(binary_function, [min,max,floor,mod,prod,cmod,dmod]).      % default binary functions which can be used for a binary function

get_option(Option, Options, Choices) :-                                   % get option Option from the list of options Options,
    functor(O, Option, 1),                                                % and return the default value if Option is not
    arg(1, O, Choices),                                                   % mentionned in the list of options
    (memberchk(O, Options) -> true ; default_option(Option, Choices)).

is_boolean_formula(FormulaPattern) :-
    formula_pattern(op(_), FormulaPattern).                               % op(_) only used on Boolean formulae

is_unconditional_formula(FormulaPattern) :-
    formula_pattern(f(_), FormulaPattern).                                % f(_) only used on unconditional formulae

is_case_formula(FormulaPattern) :-
    formula_pattern(option(_,_,_), FormulaPattern).                       % option(_,_,_) only used on case formulae

is_decomposed_formula(FormulaPattern) :-
    FormulaPattern = [decomp(_, _, _)].                                   % decomposed formulae have a set structure

get_cond_cost_vars(FormulaPattern, CostVars) :-
    formula_pattern(cond(_, _, _, _, _, _, CostVars, _, _, _, _, _), FormulaPattern).

get_then_cost_vars(FormulaPattern, CostVars) :-
    formula_pattern(then(_, _, _, _, _,    CostVars, _, _, _, _, _), FormulaPattern).

get_else_cost_vars(FormulaPattern, CostVars) :-
    formula_pattern(else(_, _, _, _, _,    CostVars, _, _, _, _, _), FormulaPattern).

get_nber_of_used_unary_binary_functions(FormulaPattern, NbUnaryBinaryFunctions) :-
    formula_pattern(then(TType, _, _, _, _, _, _, _, _, _, _), FormulaPattern),
    functor(TType, ThenFunctor, _),
    (memberchk(ThenFunctor, [unary_term,binary_term]) ->
        ThenUsed = 1
    ;
        ThenUsed = 0
    ),
    formula_pattern(else(EType, _, _, _, _, _, _, _, _, _, _), FormulaPattern),
    functor(EType, ElseFunctor, _),
    (memberchk(ElseFunctor, [unary_term,binary_term]) ->
        ElseUsed = 1
    ;
        ElseUsed = 0
    ),
    NbUnaryBinaryFunctions is ThenUsed+ElseUsed.

% CREATING A FORMULA PATTERN AND SETTING/GETTING ITS PARAMETERS
%--------------------------------------------------------------
formula_pattern_create(bool, [mandatory_attr(_),       % attributes used in the Boolean formula
                              neg(_),                  % 1 if Boolean formula negated, 0 otherwise
                              shift(_),                % shift of the Boolean formula (min of output col) % NEWBOOL
                              op(_),                   % operator (and, or, allequal, sum, xor, voting, card1) used in the Boolean formula
                              nb_terms(_),             % number of Boolean conditions used in the Boolean formula
                              conds(_),                % a list of conditions where each condition is described below
                              combined_row_ctrs(_SourceVarsLocal,   
                                                _SourceVarsGlobal,
                                                _TargetVarsGlobal,
                                                _SourceCtrs)]).

%                             cond(_CB,                  B=-1: the condition is unused, B=0: the negation is used, B=1: the condition is used
%                                  _CType,               attr_eq_coef     | attr_eq_attr        | unary_term_eq_coef   | binary_term_eq_coef  |
%                                                        attr_leq_coef    | attr_leq_attr       | unary_term_leq_coef  | binary_term_leq_coef |
%                                                                                                 unary_term_geq_coef  |
%                                                        attr_in_interval | linear_eq_coef      |
%                                                        attr_eq_unary    | attr_leq_unary      | unary_leq_attr       | 
%                                                        attr_geq_coef    | unary_term_geq_coef | binary_term_geq_coef |
%                                                        sum_leq_attr     | minus_mod_eq0
%                                  _CAttr1,              first  attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
%                                  _CAttr2,              second attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
%                                  _CAttr3,              third  attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
%                                  _CCst,                unary term constant  : 0 if none
%                                  _CCoef,               coefficient used     : 0 if none
%                                  _CCostVars,           variables used to compute the cost of the cond part
%                                  _CSourceVarsLocal,    local source vars    : depend of each table entry (first var. is the condition result)
%                                  _CSourceVarsGlobal,   global source vars   : independant from each table entry
%                                  _CTargetVarsGlobal,   global target vars   : already created
%                                  _CSourceCtrs,         list of constraints to post on each table entry
%                                  _CCheckOrder)        >0 if will have to check that both arguments of a min or max unary function contribute

formula_pattern_create(cond, [mandatory_attr(_),       % attr_eq_coef     | attr_eq_attr   | unary_term_eq_coef  | binary_term_eq_coef  |
                              cond(_CType,             % attr_leq_coef    | attr_leq_attr  | unary_term_leq_coef | binary_term_leq_coef |
                                                       % attr_in_interval | attr_eq_unary  | attr_leq_unary      | unary_leq_attr       |
                                                       % linear_eq_coef   | sum_leq_attr   | minus_mod_eq0
                                   _CAttr1,            % first  attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
                                   _CAttr2,            % second attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
                                   _CAttr3,            % third  attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
                                   _CCst,              % unary term constant  : 0 if none
                                   _CCoef,             % coefficient used     : 0 if none
                                   _CCostVars,         % variables used to compute the cost of the cond part
                                   _CSourceVarsLocal,  % local source vars    : depend of each table entry (first var. is the condition result)
                                   _CSourceVarsGlobal, % global source vars   : independant from each table entry
                                   _CTargetVarsGlobal, % global target vars   : already created
                                   _CSourceCtrs,       % list of constraints to post on each table entry
                                   _CCheckOrder),      % >0 if will have to check that both arguments of a min or max unary function contribute
                              then(_TType,             % cst | attr | unary_term | binary_term
                                   _TAttr1,            % first  attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
                                   _TAttr2,            % second attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
                                   _TCst,              % unary term constant  : 0 if none
                                   _TCoef,             % coefficient used     : 0 if none
                                   _TCostVars,         % variables used to compute the cost of the then part
                                   _TSourceVarsLocal,  % local source vars    : depend of each table entry (first var. is the then result)
                                   _TSourceVarsGlobal, % global source vars   : independant from each table entry
                                   _TTargetVarsGlobal, % global target vars   : already created
                                   _TSourceCtrs,       % list of constraints to post on each table entry
                                   _TCheckOrder),      % >0 if will have to check that both arguments of a min or max unary function contribute
                              else(_EType,             % cst | attr | unary_term | binary_term
                                   _EAttr1,            % first  attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names)
                                   _EAttr2,            % second attribute used: 0 if none, >0 otherwise (index in mandatory_attributes_names) 
                                   _ECst,              % unary term constant  : 0 if none
                                   _ECoef,             % coefficient used     : 0 if none
                                   _ECostVars,         % variables used to compute the cost of the else part
                                   _ESourceVarsLocal,  % local source vars    : depend of each table entry (first var. is the else result)
                                   _ESourceVarsGlobal, % global source vars   : independant from each table entry
                                   _ETargetVarsGlobal, % global target vars   : already created
                                   _ESourceCtrs,       % list of constraints to post on each table entry
                                   _ECheckOrder)]).    % >0 if will have to check that both arguments of a min or max unary function contribute

formula_pattern_create(formula, [np(_),                                    % number of polynoms in the formula
                                 nu(_),                                    % number of unary terms in the formula
                                 nb(_),                                    % number of binary terms in the formula
                                 f(_),                                     % type of formula
                                 cst(_),                                   % constant term when the formula is just a constant
                                 unary(_),                                 % unary  functions used in each unary term
                                 binary(_),                                % binary functions used in each binary term
                                 unaryf(_),                                % unary  function  used in the unary function
                                 binaryf(_),                               % binary function  used in the binary function
                                 attributes_use(_),                        % how the attribute and unary/binary terms are used by the polynoms
                                 polynoms(_),                              % for each polynom all its monoms and their coefficients
                                 tattributes(_),                           % a list of logical vars.(one var.for each attribute) used in
                                                                           % the source templates
                                 tunary(_),                                % template used for each unary term for posting a constraint
                                                                           % on a table entry
                                 tbinary(_),                               % template used for each binary term for posting a constraint
                                                                           % on a table entry
                                 tpolynom(_),                              % template used for each polynom for posting a constraint
                                                                           % on a table entry
                                 tunaryf(_),                               % template used for each unary function for posting a constraint
                                                                           % on a table entry
                                 tbinaryf(_),                              % template used for each binary function for posting a constraint
                                                                           % on a table entry
                                 mandatory_optional_attributes_names(_)]). % for each mandatory and optional attribute its name wrt
                                                                           % the original tables (does not mention unary/binary terms)

% GENERATE CANDIDATE FORMULAE (and on bactracking produce the next candidate formula)
%------------------------------------------------------------------------------------
formula_generator(CallMap, KindCombi, OptionsBoolCond, LOptions, TableEntriesPassed, Mode, ColSetsBool,
                  ShiftBool, Kind, Table, Col, CurMaxCost, FormulaPattern,
                  FormulaFamily, OptionFConjecture, Cost, Result) :-
    %statistics(total_runtime,[Start|_]),
    %write(Name-mode(Start, Mode)),nl,
    %tab_get_name(col(Table, Col), Name),
    %tab_get_name(col(Table, Col), mina),
    %(Mode = main(_) -> FormulaFamily = decomp, Type = cond_splits ; true),
    %(Mode = main(_) -> FormulaFamily = decomp, Type = diff_splits ; true),
    member(Options, LOptions),                 % go through the different list of options
    % First, determine the family of the formula and apply, if necessary, restrictions on maximum cost of the formula
    (Options = bool(OptionsA) ->
        FormulaFamily = bool
    ;
     Options = cond_ex(OptionsA) ->
        FormulaFamily = cond_ex
    ;
     Options = cond(OptionsA) ->
        keep_conditional_formula(Kind, Col),   % avoid a conditional formula if found already a Boolean formula with a small cost
        FormulaFamily = cond
    ;
     Options = formula(OptionsA) ->
        keep_unconditional_formula(Kind, Col), % avoid an unconditional formula if found already a Boolean formula or
        FormulaFamily = formula                % if found a conditional formula with a small cost
    ;
     Options = cluster(OptionsA) ->
        keep_cluster_formula(Kind, Col),       % avoid a cluster formula if found already a conditional formula with a small cost
        FormulaFamily = cluster
    ;
     Options = decomp(OptionsA) ->
        keep_unconditional_formula(Kind, Col), % avoid an unconditional formula if found already a Boolean formula or
        FormulaFamily = decomp                 % if found a conditional formula with a small cost
    ;
        write(formula_generator(Options)),nl,halt
    ),

    %statistics(total_runtime,[Start|_]),
    %write(Start-FormulaFamily),
    %(Mode = main(_) -> true ; write('. nested')),
    %nl,
    (FormulaFamily = decomp ->
        % next, for decompositioned formulae, select an Fd for the decomposition
        TableEntriesWithoutDuplicates = TableEntriesPassed,
        OptionsA = TypeList-InputsList,
        member(OptionF, InputsList),
        member(Type, TypeList),
        (Mode = main(ParamsList) ->
             tab_get_min_max(col(Table,Col), OutputMin, OutputMax),
             ParamsList1 = ParamsList
        ;
         Mode = preprocessed(OutputMin, OutputMax, _, _) ->
             OptionF = mandatory_attr(AttrNames),
             append(AttrNames, [col(Table,Col)], ParamsList1)
        ),
        Option = [OptionF,
                  type(Type),
                  all_params(ParamsList1),
                  output_min_max(OutputMin-OutputMax),
                  mode(Mode)],
        OptionFConjecture = [decomp(OptionFConjecture1)]
    ; % next, for cluster formula, produce the prefix tree
     (FormulaFamily = cluster, Mode = main(_)) ->
        TableEntriesWithoutDuplicates = [],
        tab_get_nval(col(Table,Col), NVal),
        OptionFConjecture = [nb_clusters(NVal)],
        member(Option,OptionsA)
    ;
     (FormulaFamily = cluster, Mode = preprocessed(_, _, _, _)) ->
        false
    ; % next, for all non-cluster and non-decompositioned formulae, produce the list of input table entries and select the specific candidate formula
        tab_get_arity(col(Table,_), Arity),                                    % get arity of the table
        (FormulaFamily = formula ->
             member(MandatoryAttrs-OptionsF, OptionsA),
             (MandatoryAttrs = [mandatory_attr(AttrNames)|_] -> MandatoryAttrs1 = MandatoryAttrs            ;
                                                                MandatoryAttrs1 = [MandatoryAttrs]          ,
                                                                MandatoryAttrs  = mandatory_attr(AttrNames) )
        ;
             member(mandatory_attr(AttrNames)-OptionsF, OptionsA)
        ),
        % if no table entries are passed then
        (Mode = main(ParamsOutput) ->
             append(AttrNames, [col(Table, Col)], AttrNamesOutput),
             % firstly - collect column indices within AttrNamesOutput
             match_list_indices_sublist(ParamsOutput, AttrPos, AttrNamesOutput), % get positions AttrPos of input columns
             % secondly - use collected column indices AttrPos to extract relevant table entries
             (foreach(TableEntryPassed, TableEntriesPassed), foreach(TableEntry, TableEntries), param(AttrPos)
             do
              match_list_indices_sublist(TableEntryPassed, AttrPos, TableEntry)
             ),
             % thirdly - remove, if any, duplicates from table entries
             remove_duplicates_from_projected_table(KindCombi, Table, Arity, AttrNames, TableEntries,
                                                    TableEntriesWithoutDuplicates)  % remove duplicates to save time (avoid duplicated ctrs)
        ;
             TableEntriesWithoutDuplicates = TableEntriesPassed
        ),
        member(OptionF, OptionsF),
        OptionFConjecture = OptionF,
        (FormulaFamily = formula ->
            append(MandatoryAttrs1, OptionF, Option)
        ;
            Option = [mandatory_attr(AttrNames)|OptionF]
        )
    ),
    %statistics(total_runtime,[End|_]),
    %write(Name-mode(End,FormulaFamily-Mode)),nl,
    /*(FormulaFamily = formula ->
         write(option(FormulaFamily, MandatoryAttrs, Option)), nl
    ;
         true
    ),*/
    formula_generator1(FormulaFamily, KindCombi, CallMap, OptionsBoolCond, TableEntriesWithoutDuplicates, Option, ColSetsBool, ShiftBool, Table, Col, CurMaxCost,
                       FormulaPattern, OptionFConjecture1, Cost, Result).

% avoid generating candidate conditional formulae for which allready find a simple formula
keep_conditional_formula(Kind, Col) :-
    ((Kind = secondary, type_found_formula(Col, boolean, Cost), Cost =< 3) -> false ; true).

% avoid generating candidate unconditional formulae for which allready find a simple formula
keep_cluster_formula(Kind, Col) :-
    ((Kind = secondary, type_found_formula(Col, conditional, Cost), Cost =< 3) -> false ;
                                                                                  true  ).

% avoid generating candidate unconditional formulae for which allready find a simple formula
keep_unconditional_formula(Kind, Col) :-
    ((Kind = secondary, type_found_formula(Col, boolean, _))                   -> false ;
     (Kind = secondary, type_found_formula(Col, decomposed, _))                -> false ; % to test later
     (Kind = secondary, type_found_formula(Col, conditional, Cost), Cost =< 3) -> false ;
     (Kind = secondary, type_found_formula(Col, case,        Cost), Cost =< 5) -> false ;
                                                                                  true  ).

% remove all previous formulae we found as on a new table (used in main)
clear_type_found_formula :-
    retractall(type_found_formula(_,_,_)).

% remove type of previous formula we found and record current one (used in main)
record_type_found_formula(FormulaPattern, Col, Cost) :-
    retractall(type_found_formula(Col,_,_)),
    (is_boolean_formula(FormulaPattern)       -> assert(type_found_formula(Col,boolean,Cost))       ;
     is_unconditional_formula(FormulaPattern) -> assert(type_found_formula(Col,unconditional,Cost)) ;
     is_case_formula(FormulaPattern)          -> assert(type_found_formula(Col,case,Cost))          ;
     is_decomposed_formula(FormulaPattern)    -> assert(type_found_formula(Col,decomposed,Cost))    ;
                                                 assert(type_found_formula(Col,conditional,Cost))   ).

% Generate a candidate Boolean formula
%  . KindCombi             : kind of combinatorial object we deal with (e.g. graph, forest, ...) or model seeker
%  . CallMap               : tell in which type of call we are (primary or secondary) and
%                            the kind of map we currently generate: use to instrument the code
%  . TableEntries          : list of all projected table entries wrt the selected input columns
%  . Options               : lists of options from which we grab the different options that parametrise the candidate formulae generator
%  . ColSetsBool           : for each input parameter give the set of possible values for the constant when it is used in a Boolean comparaison
%                            between that input param.and a constant that need to be found, see CondAttr1 = CondCoef, CondAttr1 =< CondCoef,
%                            CondAttr1 >= CondCoef in module gen_bool_formula.pl
%  . ShiftBool             : constant from which shift the Boolean formula (minimum value of the output column) % NEWBOOL
%  . Table                 : table for which a formula is learned
%  . Col                   : column of Table for which a formula is learned
%  . CurMaxCost            : maximum allowed cost (used when user:max_cost(_) not present)
%  . FormulaPattern        : generated formula pattern
%  . Cost                  : cost of the formula that will be found (should not exceed max(Cost))
%  . Result                : time_out if got a time out during the labeling
formula_generator1(bool, KindCombi, CallMap, _OptionsBoolCond, TableEntries, Options, ColSetsBool, ShiftBool, Table, Col, CurMaxCost,
                   FormulaPattern, _OptionFConjecture, Cost, Result) :- !,
    AllAttrVars            = [],                                        % set to empty as not relevant
    AllCstOrdInUnaryBinary = [],                                        % set to empty as not relevant
    AllLowUps              = [],                                        % set to empty as not relevant
    get_option(mandatory_attr, Options, MandatoryAttr),                 % get mandatory attributes: those which should occur in the formula
    get_option(bool,           Options, OpNbTermConds),                 % get parameters of the Boolean formula: [op(_),nb_terms(_),conds(_),neg,nb_occ]
    BoolParams = [mandatory_attr(MandatoryAttr)|OpNbTermConds],         % put together mandatory attributes with Boolean formula parameters
    formula_generator_bool_conds(BoolParams, ColSetsBool,               % from the options describing a Boolean formula,
                                 ShiftBool,                             % generates the corresponding Boolean formula
                                 t(NbUnary,NbBinary,NbTernary,AdjustMaxOccAttrToOne),
                                 FormulaPattern1),
    formula_pattern_create(bool, FormulaPattern1),
    (get_option(cluster,           Options, [[ValFD],Vals0]) ->         % if on a cluster formula then transfer the cluster values to
        FormulaPattern = [cluster([ValFD],Vals0)|FormulaPattern1]       % FormulaPatter as this information will be used when posting
    ;                                                                   % the constraints on a cluster
     get_option(then_else,         Options, ThenElse) ->                % if it's conditional with a Boolean condition
        FormulaPattern = [then_else(ThenElse)|FormulaPattern1]          % retrieve the information about then and else terms
    ;
        FormulaPattern = FormulaPattern1
    ),
    memberchk(conds(Conds), FormulaPattern),
    pos_bool_cond_entry(lcondattr,      PosLCondAttr),
    pos_bool_cond_entry(lcondextravars, PosLCondExtraVars),
    get_entries_of_all_boolean_conditions(Conds, PosLCondAttr,      LCondAttr),
    get_entries_of_all_boolean_conditions(Conds, PosLCondExtraVars, LCondExtraVars),
    flatten(LCondAttr, FlatLCondAttr),                                  % attribute variables
    flatten(LCondExtraVars, FlatLCondExtraVars),                        % extra variables of the Boolean condition (coefficients)
    append(FlatLCondAttr, FlatLCondExtraVars, AllCoefs),                % AllCoefs: all used variables corresponding to the attr. and coef.
    length(MandatoryAttr, LenA),
    LenA1 is LenA+1,
    length(Conds, M),                                                   % get total number of Boolean conditions
    (AdjustMaxOccAttrToOne = 1 ->                                       % if formula_generator_bool_conds did find out that each attribute
        MaxOccOfAnAttr = 1                                              % should occur exactly one time
    ;
        (get_option(cluster, Options, _) ->                             % if case formulae:
            MaxOccOfAnAttr is min(M,2)                                  % maximum number of occurrence of each attribute in the conditions is 2
        ;                                                               % if Boolean formulae:
            MaxOccOfAnAttr is min(M,3)                                  % maximum number of occurrence of each attribute in the conditions is 3
        )
    ),
    gen_occ_vars(2, LenA1, MaxOccOfAnAttr, OccVars, OccVarsAlone),      % generate occ. vars for gcc constraint
    length(FlatLCondAttr, LenFlatLCondAttr),
    MaxOcc1 is LenFlatLCondAttr-LenA,
    Occ1 in 0..MaxOcc1,                                                 % add value 1 (0+1) corresponding to an unused attribute
    global_cardinality(FlatLCondAttr, [1-Occ1|OccVars]),                % all mandatory attributes should be used
    gen_sum_term(OccVarsAlone, var, SumOccVars),                        % link together sum of occurrence variables of each used attribute
    call(SumOccVars #= NbUnary + 2*NbBinary + 3*NbTernary),             % with number of unary and binary condition effectively used

%   write('--------------------------------------------------------------------------------------------------------------------'), nl,
%   write('options: '), write(MandatoryAttr), nl,
%   write(' formula pattern = '),  write(FormulaPattern), nl,

    instrument_code(instrument_in_formula_generator, CallMap),
    TimeOut = 120000,                                                   % set up timeout for computing a Boolean formula
    (get_option(then_else,         Options, ThenElse) ->                % if its generalised conditional use the information about then and else terms
        ThenElse = [then(NewThenType, ThenCoef, LThenAttr, LThenExtraVars, _LThenCostVars,
                    _ThenSourceVarsLocal, _ThenSourceVarsGlobal, _ThenTargetVarsGlobal, _ThenSourceCtrs, _ThenCheckOrder),
                    else(NewElseType, ElseCoef, LElseAttr, LElseExtraVars, _LElseCostVars,
                    _ElseSourceVarsLocal, _ElseSourceVarsGlobal, _ElseTargetVarsGlobal, _ElseSourceCtrs, _ElseCheckOrder)],
        formula_pattern(op(Oplus),                 FormulaPattern),
        avoid_simplifiable_conditional(Conds, MandatoryAttr, Oplus,
                                        NewThenType, NewElseType,
                                        LThenAttr,   LElseAttr,
                                        ThenCoef,    ElseCoef),
        append([AllAttrVars,LThenAttr,LElseAttr],       AllAttrVars1),
        append([AllCoefs,LThenExtraVars,LElseExtraVars],AllCoefs1),
        (user:max_cost(Max) -> Cost in 0..Max ; Max = CurMaxCost, Cost in 0..Max),
        get_bool_cost_vars(FormulaPattern, BoolCostVars),                   % used to compute the cost of the Boolean formula
        get_option(cost_then_else, Options, CostThenElse),
        BoolCost in 0..Max,
        gen_cost_sum_abs(BoolCostVars, BoolCost),
        Cost #= CostThenElse + BoolCost,
        search_a_single_formula_for_all_table_entries(KindCombi, CallMap, TableEntries, FormulaPattern, col(Table,Col)),
        time_out(minimize(label(AllAttrVars1, AllCstOrdInUnaryBinary,
                                AllCoefs1, AllLowUps), Cost, [all]), TimeOut, Result)
    ;
        (user:max_cost(Max) -> Cost in 0..Max ; Max = CurMaxCost, Cost in 0..Max),
        get_bool_cost_vars(FormulaPattern, BoolCostVars),                   % used to compute the cost of the Boolean formula
        gen_cost_sum_abs(BoolCostVars, Cost),
        search_a_single_formula_for_all_table_entries(KindCombi, CallMap, TableEntries, FormulaPattern, col(Table,Col)),
        %statistics(total_runtime,[Start|_]),
        time_out(minimize(label(AllAttrVars, AllCstOrdInUnaryBinary, AllCoefs, AllLowUps), Cost, [all]), TimeOut, Result)%,
        %statistics(total_runtime,[Stop|_]),
        %Runtime is Stop - Start,
        %write(Runtime), write('ms'), nl
    ).

% Generate a candidate conditional formula
%  . KindCombi             : kind of combinatorial object we deal with (e.g. graph, forest, ...) or model seeker
%  . CallMap               : tell in which type of call we are (primary or secondary) and
%                            the kind of map we currently generate: use to instrument the code
%  . Options               : lists of options from which we grab the different options that parametrise the candidate formulae generator
%  . ColSetsBool           : for each input parameter give the set of possible values for the constant when it is used in a Boolean comparaison
%                            between that input param.and a constant that need to be found, see CondAttr1 = CondCoef, CondAttr1 =< CondCoef,
%  . Table                 : table for which a formula is learned
%  . Col                   : column of Table for which a formula is learned
%  . CurMaxCost            : maximum allowed cost (used when user:max_cost(_) not present)
%  . FormulaPattern        : generated formula pattern
%  . Cost                  : cost of the formula that will be found (should not exceed max(Cost))
%  . Result                : time_out if got a time out during the labeling
formula_generator1(cond, KindCombi, CallMap, _OptionsBoolCond, TableEntries, Options, ColSetsBool, _ShiftBool, Table, Col, CurMaxCost,
                   FormulaPattern, _OptionFConjecture, Cost, Result) :- !,
    AllAttrVars            = [],                                        % set to empty as not relevant
    AllCstOrdInUnaryBinary = [],                                        % set to empty as not relevant
    AllLowUps              = [],                                        % set to empty as not relevant
    get_option(mandatory_attr, Options, MandatoryAttr),                 % get mandatory attributes: those which should occur in the formula
    get_option(cond,           Options, Cond),                          % get condition type
    get_option(then,           Options, Then),                          % get type of formula of then part
    get_option(else,           Options, Else),                          % get type of formula of else part
    length(MandatoryAttr, LenA),                                        % number of mandatory attributes (uses only mandatory attributes)
    formula_generator_condition(LenA, MandatoryAttr, ColSetsBool, Cond, NewCondType, LCondAttr, LCondExtraVars, LCondCostVars, CondAttr1, CondAttr2, CondAttr3,
                                CondCst, CondCoef, CondSourceVarsLocal, CondSourceVarsGlobal, CondTargetVarsGlobal, CondSourceCtrs, CondCheckOrder),
    formula_generator_then_else(LenA, MandatoryAttr, Then, NewThenType, LThenAttr, LThenExtraVars, LThenCostVars, ThenAttr1, ThenAttr2,
                                ThenCst, ThenCoef, ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder),
    formula_generator_then_else(LenA, MandatoryAttr, Else, NewElseType, LElseAttr, LElseExtraVars, LElseCostVars, ElseAttr1, ElseAttr2,
                                ElseCst, ElseCoef, ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder),
    avoid_same_condattr_in_then(MandatoryAttr, NewCondType, LCondAttr, LThenAttr, CondCoef),
    avoid_same_condattr_in_else(MandatoryAttr, NewCondType, LCondAttr, LElseAttr, CondCoef),
    avoid_same_func_in_cond_and_then(NewCondType, CondAttr1, CondAttr2, % avoid to use same unary/binary function with same attributes
                                     NewThenType, ThenAttr1, ThenAttr2),% in condition and in the then part
    avoid_same_func_in_cond_and_then(NewCondType, CondAttr1, CondAttr2, % avoid to use same unary/binary function with same attributes
                                     NewElseType, ElseAttr1, ElseAttr2),% in condition and in the else part
    avoid_simplifiable_conditional(MandatoryAttr,
                                   NewCondType, NewThenType, NewElseType,
                                   LCondAttr,   LThenAttr,   LElseAttr,
                                   CondCoef,    ThenCoef,    ElseCoef),
    append([LCondAttr,LThenAttr,LElseAttr], LAttr),                     % put together all used attribute variables
    append([LCondExtraVars,LThenExtraVars,LElseExtraVars], LExtraVars), % put together all used extra variables
    append(LAttr, LExtraVars, AllCoefs),                                % AllCoefs: variables corresponding to the coef.used in the conditional formula
    length(LAttr, NAttr),                                               % number of created attributed variables
    gen_occ_vars(1, LenA, NAttr, OccVars),                              % generate occ. vars for gcc constraint
    global_cardinality(LAttr, OccVars),                                 % all mandatory attributes should be used
    FormulaPattern = [mandatory_attr(MandatoryAttr),                    % create the formula pattern
                      cond(NewCondType, CondAttr1, CondAttr2, CondAttr3, CondCst, CondCoef, LCondCostVars,
                           CondSourceVarsLocal, CondSourceVarsGlobal, CondTargetVarsGlobal, CondSourceCtrs, CondCheckOrder),
                      then(NewThenType, ThenAttr1, ThenAttr2,            ThenCst, ThenCoef, LThenCostVars,
                           ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder),
                      else(NewElseType, ElseAttr1, ElseAttr2,            ElseCst, ElseCoef, LElseCostVars,
                           ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder)],
    formula_pattern_create(cond, FormulaPattern),

%   write('--------------------------------------------------------------------------------------------------------------------'), nl,
%   write('options: '), write(MandatoryAttr), nl,
%   write(' cond = '), write('Type: '), write(NewCondType), nl,
%   write(' then = '), write('Then: '), write(NewThenType), nl,
%   write(' else = '), write('Else: '), write(NewElseType), nl,

    instrument_code(instrument_in_formula_generator, CallMap),
    (user:max_cost(Max) -> Cost in 0..Max ; Max = CurMaxCost, Cost in 0..Max),
    get_nber_of_used_unary_binary_functions(FormulaPattern, NbUnaryBinaryFunctions),
    (NbUnaryBinaryFunctions = 2 -> Penalty = 3 ; Penalty = NbUnaryBinaryFunctions),
    CondCost in 0..Max,
    ThenCost in 0..Max,
    ElseCost in 0..Max,
    gen_cost_sum_abs(LCondCostVars,      CondCost),                     % compute sum of absolute values of variables of the cond part
    gen_cost_sum_abs_max1(LThenCostVars, ThenCost),                     % sum of max of 1 and absolute values of variables of the then part
    gen_cost_sum_abs_max1(LElseCostVars, ElseCost),                     % sum of max of 1 and absolute values of variables of the else part
    Cost #= CondCost + max(ThenCost,ElseCost) + Penalty + 1,            % +1 as penalise conditional formula
    Cost #>= 2,                                                         % minimum cost is 2 for a conditional formula
    search_a_single_formula_for_all_table_entries(KindCombi, CallMap, TableEntries, FormulaPattern, col(Table,Col)),
    TimeOut = 1000,                                                     % set up timeout for computing a conditional formula
    time_out(minimize(label(AllAttrVars, AllCstOrdInUnaryBinary, AllCoefs, AllLowUps), Cost, [all]), TimeOut, Result).

formula_generator1(cond_ex, KindCombi, CallMap, OptionsBoolCond, TableEntries, Options, ColSetsBool, _ShiftBool, Table, Col, CurMaxCost,
                   FormulaPattern, _OptionFConjecture, Cost, Result) :- !,
    get_option(mandatory_attr, Options, MandatoryAttr),                 % get mandatory attributes: those which should occur in the formula
    get_option(cond_ex, Options, [then(Then), else(Else) | BoolParams]),
% TODO: as of now, Bool condition is forced to use all mandatory attributes in it. Later, if it's possible, this restriction could be relaxed.    
    length(MandatoryAttr, LenA),
    formula_generator_then_else(LenA, MandatoryAttr, Then, NewThenType, LThenAttr, LThenExtraVars, LThenCostVars, ThenAttr1, ThenAttr2,
                                ThenCst, ThenCoef, ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder),
    formula_generator_then_else(LenA, MandatoryAttr, Else, NewElseType, LElseAttr, LElseExtraVars, LElseCostVars, ElseAttr1, ElseAttr2,
                                ElseCst, ElseCoef, ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder),
    ThenElse = [then(NewThenType, ThenCoef, LThenAttr, LThenExtraVars, LThenCostVars,                 % collect information from 
                ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder),     % Then and Else terms
                else(NewElseType, ElseCoef, LElseAttr, LElseExtraVars, LElseCostVars,
                ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder)],
    (user:max_cost(Max) -> Cost in 0..Max ; Max = CurMaxCost, Cost in 0..Max),
    ThenCost in 0..Max,
    ElseCost in 0..Max,
    get_nber_of_used_unary_binary_functions([then(NewThenType, _, _, _, _, _, _, _, _, _, _),
                                             else(NewElseType, _, _, _, _, _, _, _, _, _, _)], NbUnaryBinaryFunctions),
    (NbUnaryBinaryFunctions = 2 -> Penalty = 3 ; Penalty = NbUnaryBinaryFunctions),
    gen_cost_sum_abs_max1(LThenCostVars, ThenCost),                      % sum of max of 1 and absolute values of variables of the then part
    gen_cost_sum_abs_max1(LElseCostVars, ElseCost),                      % sum of max of 1 and absolute values of variables of the else part
    CostThenElse #= max(ThenCost,ElseCost) + Penalty + 1,               % +1 as penalise conditional formula
    formula_generator1(bool,                                                                                    % and pass it to the Boolean solver
                       KindCombi,
                       CallMap,
                       OptionsBoolCond,
                       TableEntries,
                       [then_else(ThenElse),cost_then_else(CostThenElse),mandatory_attr(MandatoryAttr)|BoolParams],
                       ColSetsBool,
                       0,
                       Table,
                       Col,
                       CurMaxCost,
                       [_|FormulaPattern1], _, Cost, Result),
    FormulaPattern = [cond_ex,
                      then(NewThenType, ThenAttr1, ThenAttr2,            ThenCst, ThenCoef, LThenCostVars,
                           ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder),
                      else(NewElseType, ElseAttr1, ElseAttr2,            ElseCst, ElseCoef, LElseCostVars,
                           ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder)|FormulaPattern1].

formula_generator1(cluster, KindCombi, CallMap, _OptionsBoolCond, [], Options, _ColSetsBool, _ShiftBool, Table, Col, CurMaxCost,
                   FormulaPattern, _OptionFConjecture, Cost, Result) :- !,
    %write(formula_generator1(Table,Col)), nl,
    get_option(cluster, Options, ClusterOptions),
    get_option(tree,    ClusterOptions, PrefixTreeValsFd),
    get_option(options, ClusterOptions, OptionsFormula),
                                                                         % test the two alternative to build the boolean formulae
    TimeOut = 600000,                                                    % global timeout
    %statistics(total_runtime,[StartF|_]),
    time_out(gen_cluster_formula(KindCombi, PrefixTreeValsFd,
                                 t(CallMap,Table,Col,CurMaxCost),
                                 OptionsFormula, FormulaPattern),
             TimeOut, Result),
    %statistics(total_runtime,[StopF|_]),
    %RuntimeF is StopF - StartF,
    %write(RuntimeF), write('ms'), nl,
    (Result = time_out ->
        true
    ;
        %nl,
        (foreach(Term,FormulaPattern), foreach(CostI, CostList)
         do
         (Term = option(_) ->
             CostI is 0
         ;
             Term = option(CostI,_,_)
         )
        )
    ),
    max_member(Cost, CostList),
    true.

% Generate a candidate unconditional formula
%  . KindCombi             : kind of combinatorial object we deal with (e.g. graph, forest, ...) or model seeker
%  . CallMap               : tell in which type of call we are (primary or secondary) and
%                            the kind of map we currently generate: use to instrument the code
%  . Options               : lists of options from which we grab the different options that parametrise the candidate formulae generator
%  . _ColSetsBool          : not used for conditional formula
%  . Table                 : table for which a formula is learned
%  . Col                   : column of Table for which a formula is learned
%  . CurMaxCost            : maximum allowed cost (used when user:max_cost(_) not present)
%  . FormulaPattern        : generated formula pattern
%  . Cost                  : cost of the formula that will be found (should not exceed max(Cost))
%  . Result                : time_out if got a time out during the labeling
formula_generator1(formula, KindCombi, CallMap, _OptionsBoolCond, TableEntries, Options, _ColSetsBool, _ShiftBool, Table, Col, CurMaxCost,
                   FormulaPattern, _OptionFConjecture, Cost, Result) :- !,

%  
%  . AllAttrVars           : list of variables reflecting how attributes are shared by polynoms and unary/binary terms
%  . AllCstOrdInUnaryBinary: list of variables corresponding to constants in unary functions and order in binary functions
%  . AllCoefs              : list of variables corresponding to the coefficients of the polynom
%  . AllLowUps             : list of bound of intervals in the predicates that have to be fixed at the very end to a single value (no backtrack)

    get_option(mandatory_attr,  Options, MandatoryAttr),                % get mandatory attributes: those which should occur in the formula
    get_option(optional_attr,   Options, OptionalAttr),                 % get optional  attributes: those which may be skipped
    get_option(min_attr,        Options, MinAttr),                      % get minimum number of attributes
    get_option(max_attr,        Options, MaxAttr),                      % get maximum number of attributes
    get_option(table_attr,      Options, TableAttr),                    % get tuples of possible attributes (if empty list this option is not used)
    get_option(nb_polynom,      Options, ChoicesNP),                    % get choices for the number of polynoms
    get_option(nb_unary,        Options, ChoicesNU),                    % get choices for the number of unary terms
    get_option(nb_binary,       Options, ChoicesNB),                    % get choices for the number of binary terms
    get_option(main_formula,    Options, ChoicesF),                     % get choices for the type of main formula (and the polynoms used)
    get_option(unary,           Options, ChoicesU),                     % get choices for the unary terms
    get_option(binary,          Options, ChoicesB),                     % get choices for the binary terms
    get_option(unary_function,  Options, ChoicesUF),                    % get choices for functions used in unary functions
    get_option(binary_function, Options, ChoicesBF),                    % get choices for functions used in binary functions
    length(MandatoryAttr, LenMandatoryAttr),                            % number of mandatory attributes
    length(OptionalAttr,  LenOptionalAttr),                             % number of optional attributes
    LenA is LenMandatoryAttr + LenOptionalAttr,                         % total number of attributes
    MinA is max(MinAttr,LenMandatoryAttr),                              % actual minimum number of attributes
    MaxA is min(MaxAttr,LenA),                                          % actual maximum number of attributes
    (MinA > MaxA -> write(error(formula_generator1)), nl, halt ;        % abort if contradicting information on the attributes
     LenA > 20   -> write(error(formula_generator2)), nl, halt ;        % abort if number of attributes exceeds 20
                    true                                       ),
    append(MandatoryAttr, OptionalAttr, AttrNames),                     % put together the mandatory and the optional attributes (will record them later)
    gen_possible_combinations_of_attr(TableAttr, AttrNames,             % if TableAttr not empty generate all combinations of attributes which may be used
                                      TuplesAttr),                      % (usually these combinations will be selected from functional dependencies)
    max_member(MaxNP, ChoicesNP),                                       % get maximum number of polynoms
    max_member(MaxNU, ChoicesNU),                                       % get maximum number of unary  terms
    max_member(MaxNB, ChoicesNB),                                       % get maximum number of binary terms
    max_main_formula(MaxF),                                             % get id of last potential main formula
    NP in 0..MaxNP,                                                     % number of polynoms in the formula
    NU in 0..MaxNU,                                                     % number of unary terms in the formula
    NB in 0..MaxNB,                                                     % number of binary terms in the formula
    F  in 0..MaxF,                                                      % type of the main formula
    (MaxAttr = 0 -> NP #= 0 ; true),                                    % if no attribute then formula is a constant
    (MinAttr > 1 -> NP #> 0 ; true),                                    % if at least one attribute then formula is not a constant
    NU+NB #=< 1,                                                        % do not use simultaneously a unary and binary term
    NP #= 0 #=> NU #= 0,                                                % if no polynom then no unary term
    NP #= 0 #=> NB #= 0,                                                % if no polynom then no binary term
    NP #= 0 #=> F #= 0,                                                 % if no polynom then formula is a constant
    NP #= 1 #=> F #= 1 #\/ F #= 2,                                      % if one polynom then formula is a polynom or unary function
    NP #= 2 #=> F #= 3,                                                 % if two polynoms then formula is binary function
    member(NP, ChoicesNP),                                              % select the number of polynoms
    member(NU, ChoicesNU),                                              % select the number of unary terms
    member(NB, ChoicesNB),                                              % select the number of binary terms
    member(FFOptions,  ChoicesF),                                       % select the type of formulas
    FFOptions = F-FOptions,
    (F = 2 -> NUF = 1 ; NUF = 0),                                       % number of unary  functions used (depending on F)
    (F = 3 -> NBF = 1 ; NBF = 0),                                       % number of binary functions used (depending on F)
    length(ListUnary,          NU),                                     % create a unary  function list (one for each unary term)
    length(ListBinary,         NB),                                     % create a binary function list (one for each binary term)
    length(ListUnaryFunction,  NUF),                                    % create a unary  function list (one for each unary function)
    length(ListBinaryFunction, NBF),                                    % create a binary function list (one for each binary function)
    members(ListUnary,          ChoicesU),                              % select the unary  functions of the unary  terms
    members(ListBinary,         ChoicesB),                              % select the binary functions of the binary terms
    members(ListUnaryFunction,  ChoicesUF),                             % select the unary  functions of the unary  functions
    members(ListBinaryFunction, ChoicesBF),                             % select the binary functions of the binary functions

%   write('--------------------------------------------------------------------------------------------------------------------'), nl,
%   write('options: F = '), write(F), write(' '), write(AttrNames), nl,
%   write('mandatory: '), write(MandatoryAttr), write('  optional: '), write(OptionalAttr), nl,
%   write(' NU = '), write(ListUnary), write(' NB = '), write(ListBinary),
%   write(' NUF = '), write(ListUnaryFunction), write(' NBF = '), write(ListBinaryFunction), nl,

    instrument_code(instrument_in_formula_generator, CallMap),
    extend_functor_unary(ListUnary, NewUnaryTerm, 1,                    % if necessary add constant or value (a free variable as not fixed initially)
                         [min,max,floor,ceil,mod,geq],
                         [sum_consec],CstsUT,LowUpsUT),
    extend_functor_unary(ListUnaryFunction, NewUnaryFunction, 0,        % if necessary add constant or value (a free variable as not fixed initially)
                         [min,max,floor,mod],
                         [geq0,sum_consec], CstsUF,LowUpsUF),
    append(LowUpsUT, LowUpsUF, AllLowUps),
    extend_functor_binaryt(ListBinary, NewBinaryTerm, OrdersBT),        % if necessary add order variable (a 0-1 free variable as not fixed initially)
    append([OrdersBT,CstsUT, CstsUF], AllCstOrdInUnaryBinary),
    formula_pattern_create(formula, FormulaPattern),                    % create a formula pattern to record the combination of params.we just create
    (F = 0 ->                                                           % if formula type is a constant
        Cst in inf..sup,                                                % then create a variable representing the constant to find
        formula_pattern(cst(Cst), FormulaPattern)                       % and record this variable
    ;                                                                   % if formula type is not a constant
        formula_pattern(cst(0), FormulaPattern)                         % then record a dummy constant
    ),
    formula_pattern(np(NP),                      FormulaPattern),       % record the number of polynoms
    formula_pattern(nu(NU),                      FormulaPattern),       % record the number of unary terms
    formula_pattern(nb(NB),                      FormulaPattern),       % record the number of binary terms
    formula_pattern(f(F),                        FormulaPattern),       % record the type of main formula
    formula_pattern(unary(NewUnaryTerm),         FormulaPattern),       % record the functions used in unary terms
    formula_pattern(binary(NewBinaryTerm),       FormulaPattern),       % record the functions (and the parameter order) used in binary terms
    formula_pattern(unaryf(NewUnaryFunction),    FormulaPattern),       % record the functions used in unary functions
    formula_pattern(binaryf(ListBinaryFunction), FormulaPattern),       % record the functions used in binary functions
    formula_gen_attr_ctrs(FormulaPattern, MinA, MaxA, LenA, AttrNames,  % generate the constraints on the attributes that should/may occur in the formula:
                          LenMandatoryAttr, ListsP, ListA, AllAttrVars, % force mandatory attributes to occur at most once, restrict min/max nb of attributes
                          TuplesAttr),
    append([MandatoryAttr,OptionalAttr,                                 % put together the mandatory attributes, the optional attributes, the unary
            ListUnary,                                                  % terms and the binary terms as this will be the parameters of the polynoms
            ListBinary], Params),                                       % of the formula
    IndexUnaryT  is LenA+1,                                             % index of the first unary  attribute
    IndexBinaryT is LenA+NU+1,                                          % index of the first binary attribute
    get_break_sym_in_unaryt_binaryt(NU, IndexUnaryT,  ListUnary,        % if formula uses one single unary term corresponding to "power", or
                                    NB, IndexBinaryT, ListBinary,       % if formula uses one single unary term corresponding to "prod"
                                    BreakSymInUnaryBinaryT),            % then will create a symmetry breaking constraint
    get_break_sym_in_unaryf(F, NewUnaryFunction,                        % if formula is a unary function using "mod", "in" or "power"
                            BreakSymInUnaryF),                          % then will create a symmetry breaking constraint
    get_break_sym_in_binaryf(F, ListBinaryFunction,                     % if formula is a binary function using "prod"
                             BreakSymInBinaryF),                        % then will create a symmetry breaking constraint
    formula_gen_polynoms(1, NP, FOptions, Params,                       % get coefficients of the polynoms that were created,
                         BreakSymInUnaryBinaryT, BreakSymInUnaryF,      % so that later on we can fix them
                         BreakSymInBinaryF, ListsP, Polynoms, Coefs),
    restrict_attr_wrt_binary_functions(F, ListBinaryFunction,           % post some constraints to restrict attributes when formula is a binary function
                                       Coefs, ListA),
    append(Coefs, AllCoefs),                                            % get a single list of variables
    (formula_pattern(non_zero_coeffs_in_all_polynomials(NZmin,NZmax),   % modif
                     FormulaPattern) ->                                 % if use restriction on nber of non zero coef. over all polynomials   
        gen_sum_term(AllCoefs, var_neq(0), TermAllCoefsNeq0),           % build term given the number of non zero coefficients of all polynomials
        call(TermAllCoefsNeq0 #>= NZmin),                               % impose that at least NZmin of these coefficients are different from 0
        call(TermAllCoefsNeq0 #=< NZmax)                                % impose that at most  NZmax of these coefficients are different from 0
    ;                                                                   % nothing to do if does not use option non_zero_coeffs_in_all_polynomials
        true
    ),
    formula_pattern(polynoms(Polynoms), FormulaPattern),                % record the generated polynoms
    formula_pattern(mandatory_optional_attributes_names(AttrNames),     % record the names of the mandatory and the optional attributes
                    FormulaPattern),                                    % so that when we print a formula we can refer to the table and column names
    gen_source_target_templates(formula, FormulaPattern,                % generate all templates that allow posting a constraint of a table entry
                                AttrNames, LenA),
    (user:max_cost(Max) -> Cost in 0..Max ; Max = CurMaxCost, Cost in 0..Max),
    append([AllCstOrdInUnaryBinary, AllCoefs], Vars),                   % ignore constants of Boolean terms, i.e. AllLowUps
    gen_cost_sum_abs(Vars, AllLowUps, Cost),
    search_a_single_formula_for_all_table_entries(KindCombi, CallMap, TableEntries, FormulaPattern, col(Table,Col)),
    TimeOut = 15000,                                                    % set up timeout for computing an unconditional formula
    time_out(minimize(label(AllAttrVars, AllCstOrdInUnaryBinary, AllCoefs, AllLowUps), Cost, [all]), TimeOut, Result).

label(AllAttrVars, AllCstOrdInUnaryBinary, AllCoefs, AllLowUps) :-
    split_list_vars(AllCoefs, AllCondB, AllCmpCoefs, AllRemainingVars),
    labeling([ffc],  AllAttrVars),
    labeling([ffc],  AllCstOrdInUnaryBinary),
    labeling([down], AllCondB),
    labelpos(AllRemainingVars),
    labelcoefs(AllCmpCoefs),
    once(labeling([], AllLowUps)).

split_list_vars([], [], [], []) :- !.
split_list_vars([V|R], S, T, U) :-
    integer(V), !,
    split_list_vars(R, S, T, U).
split_list_vars([Term|R], [V|S], T, U) :-
    nonvar(Term),
    functor(Term, condb, 1), !,
    arg(1, Term, V),
    split_list_vars(R, S, T, U).
split_list_vars([Term|R], S, [CmpV|T], U) :-
    nonvar(Term),
    functor(Term, coef, 2), !,
    arg(1, Term, Cmp),
    arg(2, Term, V),
    functor(CmpV, Cmp, 1),
    arg(1, CmpV, V),
    split_list_vars(R, S, T, U).
split_list_vars([V|R], S, T, [V|U]) :-
    split_list_vars(R, S, T, U).

labelpos([]) :- !.
labelpos([V|R]) :-
    integer(V),
    !,
    labelneg(R).
labelpos([V|R]) :-
    V #> 0,
    labeling([up,bisect], [V]),
    labelneg(R).
labelpos([0|R]) :-
    labelneg(R).
labelpos([V|R]) :-
    V #< 0,
    labeling([down,bisect], [V]),
    labelneg(R).

labelneg([]) :- !.
labelneg([V|R]) :-
    integer(V),
    !,
    labelpos(R).
labelneg([V|R]) :-
    V #< 0,
    labeling([down,bisect], [V]),
    labelpos(R).
labelneg([0|R]) :-
    labelpos(R).
labelneg([V|R]) :-
    V #> 0,
    labeling([up,bisect], [V]),
    labelpos(R).

labelcoefs([]) :- !.
labelcoefs([eq(V)|R]) :- !,
    fd_min(V, Min),
    V = Min,
    labelcoefs(R).
labelcoefs([leq(V)|R]) :- !,
    fd_min(V, Min),
    V = Min,
    labelcoefs(R).
labelcoefs([geq(V)|R]) :-
    fd_max(V, Max),
    V = Max,
    labelcoefs(R).

%----------------------------------------------------------------------------------------------------
% CODE FOR CLUSTER (A CASE FORMULA)

% we describe the frontier of the already explored tree corresponding to all the generated case statements
% as a list of subtrees we still have to develop; for each subtree we create a term t/5 with the following arguments:
%  (1) current cost vector:
%       . the length of this vector is the maximum between the maximum number of terms in a Boolean expression (we assume a maximum of 3)
%         and the maximum number of parameters of a functional dependency, where
%         the first component of the vector corresponds to the maximum defined above, and
%         the last component of the vector corresponds to a Boolean expression that uses a single term or
%         to a functional dependency using one single parameter.
%       . this vector is calculated as follows:
%         - for the part of the path already determined we just take the number of terms of the Boolean expressions already found,
%         - for the subtree remaining to be explored, the smallest cost (from a lexicographic point of view) associated with the
%           different paths of the subtree leading to a leaf of the subtree; the cost of a node of a path is the number of parameters
%           of the corresponding functional dependency,
%         - a penalty if there's no path in the subtree where each subsequent cluster contains more table entries than the previously selected cluster. 
%  (2) distance between the root of the subtree and its leaves
%  (3) the total number of initial options - the length of the list of options
%  (4) the number of parameters of the next functional dependency at the root of the subtree
%  (5) the list of options we still can try
%  (6) cost vector of the subtree:
%       . the length of this vector is the maximum described in (1).
%       . this vector is used to incrementally update the current cost vector.
%  (7) the list of already expanded path elements (in reverse order of expanded nodes),
%      where each path element corresponds to a pair of the form (functional dependency, value taken by the output attribute).
%  (8) unexplored subtree
%
% An example of frontier of an unexplored tree is given by:
%   Frontier = [t([1,1], 3, 20, 1, [O1,O2,...,O20], [1,1], [], t([col(low_minc_mins_s,1)], 0,
%                                                                    [t([col(low_minc_mins_s,1),col(low_minc_mins_s,2)], 1,
%                                                                         [t([],2,[])])])),
%               t([1,1], 3, 20, 2, [O1,O2,...,O20], [1,1], [], t([col(low_minc_mins_s,1),col(low_minc_mins_s,2)], 1,
%                                                                    [t([col(low_minc_mins_s,1)], 0,
%                                                                         [t([],2,[])]),
%                                                                     t([col(low_minc_mins_s,2)], 0,
%                                                                         [t([],2,[])])])),
%               t([1,1], 3, 20, 2, [O1,O2,...,O20], [1,1], [], t([col(low_minc_mins_s,1),col(low_minc_mins_s,2)], 2, 
%                                                                    [t([col(low_minc_mins_s,1)], 0,
%                                                                         [t([],1,[])])]))
%              ]
% this unexplored tree corresponds to the following squeleton where we indicate the value taken by the output attribute:
%      -------------
%     |      |      |
%     0      1      2
%           ---
%     |    |   |    |
%     1    0   0    0
%     |    |   |    |
%     2    2   2    1
%
% An example of frontier of a partially explored tree is given by:
%            |       
%            1 (assume evaluate to the second child of the root node and the Boolean formula has one term, but not the first and third children)
%           ---
%          |   |
%          0   0
%          |   |
%          2   2
% and corresponds to (the current cost [0,2] is the sum of [0,1] and [0,1],
% cost of the part of the path already determined, and the smallest lexicographical cost of the remaining subtree).
%   Frontier = [t([1,1], 3, 20, 1, [O1,O2,...,O20], [1,1], [], t([col(low_minc_mins_s,1)], 0,
%                                                                    [t([col(low_minc_mins_s,1),col(low_minc_mins_s,2)], 1, [t([],2,[])])])),
%               t([0,2], 2, 19, 1, [O2,O3,...,O20], [0,1], [[col(low_minc_mins_s,1),col(low_minc_mins_s,2)]-1],
%                 [t([col(low_minc_mins_s,1)], 0, [t([],2,[])]),
%                  t([col(low_minc_mins_s,2)], 0, [t([],2,[])])])
%                ),
%               t([1,1], 3, 20, 2, [O1,O2,...,O20], [1,1], [], t([col(low_minc_mins_s,1),col(low_minc_mins_s,2)], 2, 
%                                                                    [t([col(low_minc_mins_s,1)], 0, [t([],1,[])])]))
%              ]
%
% An example of path that reach a leave (assuming the Boolean formula at level 2 has also one term) is given by:
%            |       
%            1
%           --
%          |
%          0
%          |
%          2
% and corresponds to (the current cost [0,2] is the sum of [0,2] and [0,0],
% cost of the part of the path already determined, and the smallest lexicographical cost of the remaining subtree).
%   Frontier = [t([1,1], 3, 20, 1, [O1,O2,...,O20], [1,1], [], t([col(low_minc_mins_s,1)], 0,
%                                                                    [t([col(low_minc_mins_s,1),col(low_minc_mins_s,2)], 1, [t([],2,[])])])),
%               t([0,2], 1, 18, 0, [O3,O4,...,O20], [0,0], [[col(low_minc_mins_s,1)]-0,[col(low_minc_mins_s,1),col(low_minc_mins_s,2)]-1],
%                                                                                                                             [t([],2,[])]),
%               t([0,2], 2, 19, 1, [O2,O3,...,O20], [0,1], [[col(low_minc_mins_s,1),col(low_minc_mins_s,2)]-1],
%                                                          [t([col(low_minc_mins_s,2)], 0, [t([],2,[])])])
%                ),
%               t([1,1], 3, 20, 2, [O1,O2,...,O20], [1,1], [], t([col(low_minc_mins_s,1),col(low_minc_mins_s,2)], 2, 
%                                                                    [t([col(low_minc_mins_s,1)], 0, [t([],1,[])])]))
%              ]
gen_cluster_formula(KindCombi, Tree, GlobalParameters, Options, SolutionPathFrontier) :-
    find_max_fd_length(Tree, NbColFdMax),
    MaxLenCostVector is max(3, NbColFdMax),
    length(Cost0, MaxLenCostVector),
    (foreach(0, Cost0) do true),
    construct_initial_frontier(Tree, Cost0, GlobalParameters, Options, Frontier),
    sort(Frontier, SortedFrontier),
    length(Frontier, N1),
    length(SortedFrontier, N2),
    (N1 = N2 -> true ; write('identical nodes on frontier'), nl, halt),
    expand_frontier(SortedFrontier, KindCombi, GlobalParameters, Cost0, Options, SolutionPathFrontierR, 0, _CallsFrontier),
    %write('calls frontier: '), write(CallsFrontier), nl,
    reverse(SolutionPathFrontierR,SolutionPathFrontier).

expand_frontier([Node|R], KindCombi,
                t(CallMap,Table,Col,CurMaxCost),
                Cost0, Options, SolutionPath, CallsStart, CallsEnd) :-
    Node = t(TotalCost, Depth, CompLenOptions, LenFD, [Option|S], SubtreeCost, FoundFormulae, Subtree),
    !,
    Subtree = t(FD, ValFD, ColSetsBool, Childs),
    (length(FD, LenFD) -> true ; write(expand_frontier1), nl, halt),
%   write(try_option(Option)), nl,
    ShiftBool = 0,
    collect_cluster_zero(Childs,Vals0),
%write(node(TotalCost, FD, ValFD, Vals0, CompLenOptions)),nl,
    ((formula_generator1(bool,
                         KindCombi,
                         CallMap,
                         _OptionsBoolCond,
                         [],
                         [cluster([[ValFD],Vals0]),mandatory_attr(FD)|Option],
                         ColSetsBool,
                         ShiftBool,
                         Table,
                         Col,
                         CurMaxCost,
                         FormulaPattern, _, Cost, Result),
     Result \== time_out) ->
%       write(option(Option)), nl,
        ((FD = [], Childs = [_|_]) -> write(expand_frontier2), nl, halt ;
         (FD = [_|_], Childs = []) -> write(expand_frontier3), nl, halt ;
                                      true                              ),
        (Childs = [] ->
            SolutionPath = [option(ValFD)|FoundFormulae],
            CallsEnd is CallsStart
        ;
%           write('formula found'), nl,            
            formula_pattern(nb_terms(NbTerms), FormulaPattern),
            (Childs = [t([],ValFDLeaf,_,[])] ->
                CallsEnd is CallsStart + 1,
                SolutionPath = [option(ValFDLeaf),option(Cost,ValFD,FormulaPattern)|FoundFormulae]
                % the general format for all case formulae: option(Cost,ThenPattern,CondPattern)
            ;
                expand_node(Childs, option(Cost,ValFD,FormulaPattern), Node, R, NbTerms, Cost0, Options, col(Table,Col), UpdatedFrontier),
                sort(UpdatedFrontier, NewFrontier),
%               write('Pattern: '), write(option(Cost,ValFD,FormulaPattern)), nl,
                CallsMid is CallsStart + 1,
                expand_frontier(NewFrontier, KindCombi,
                                t(CallMap,Table,Col,CurMaxCost),
                                Cost0, Options, SolutionPath, CallsMid, CallsEnd)
            )
        )
    ;
%       write('formula not found'), nl,
        NewCompLenOptions is CompLenOptions+1,
        NewNode = t(TotalCost, Depth, NewCompLenOptions, LenFD, S, SubtreeCost, FoundFormulae, Subtree),
        sort([NewNode|R], NewFrontier),
        CallsMid is CallsStart + 1,
        expand_frontier(NewFrontier, KindCombi,
                        t(CallMap,Table,Col,CurMaxCost),
                        Cost0, Options, SolutionPath, CallsMid, CallsEnd)
    ).
expand_frontier([Node|R], KindCombi, % if all Boolean options for a node are exhausted, move to the next node in the frontier
                t(CallMap,Table,Col,CurMaxCost),
                Cost0, Options, SolutionPath, CallsStart, CallsEnd) :-
    Node = t(_TotalCost, _Depth, _CompLenOptions, _LenFD, [], _SubtreeCost, _FoundFormulae, _Subtree),
    !,
    expand_frontier(R, KindCombi,
                    t(CallMap,Table,Col,CurMaxCost),
                    Cost0, Options, SolutionPath, CallsStart, CallsEnd).
expand_frontier([Node|_], _KindCombi, % if a node has a mismatched format, halt the process
                t(_CallMap,_ColSetsBool,_Table,_Col,_CurMaxCost),
                _Cost0, _Options, _SolutionPath, _CallsStart, _CallsEnd):-
    write(expand_frontier(Node)), nl, halt.

expand_node([], _, _, Nodes, _, _, _, _, Nodes) :- !.
expand_node([Child|R], FoundFormula, Node, Nodes, NbTerms, Cost0, Options, Col, [NewNode|S]) :-
    Node  = t(TotalCost0, Depth0, _CompLenOptions0, _LenFD0, _Options0, SubtreeCost0, FoundFormulae0, _Subtree0),
    Child = t(FD1, _ValFD1, _BoolSets, _Childs1),
    NewNode = t(TotalCost1, Depth1, CompLenOptions1, LenFD1, Options1, SubtreeCost1, FoundFormulae1, Subtree1),
    substract_vectors(TotalCost0, SubtreeCost0, PathCost0),
    update_cost_vector(PathCost0, NbTerms, PathCost),
    length(FD1, LenFD1),
    get_cost_subtree(Child, Cost0, ChildCost),
    %add_vectors(PathCost, ChildCost, TotalCost1),
    add_vectors(PathCost, ChildCost, TotalCost1Unadjusted),
    (path_with_increasing_outputs([Child], Col) ->
        TotalCost1 = TotalCost1Unadjusted
    ;
        length(TotalCost1Unadjusted, N),
        update_cost_vector(TotalCost1Unadjusted, N, TotalCost1)             % a small penalty if there's no path with always increasing output values
    ),
    Depth1          is Depth0-1,
    CompLenOptions1 = 0,
    Options1        = Options,
    SubtreeCost1    = ChildCost,
    FoundFormulae1  = [FoundFormula|FoundFormulae0],
    Subtree1        = Child,
    expand_node(R, FoundFormula, Node, Nodes, NbTerms, Cost0, Options, Col, S).

construct_initial_frontier([], _, _, _, []) :- !.
construct_initial_frontier([SubTree|R], Cost0, GlobalParameters, Options, [t(Cost,Depth,0,LenFD,Options,Cost,[],SubTree)|S]) :-
    SubTree = t(Fd,_ValFd,_BoolSets,_),
    length(Fd,LenFD),
    GlobalParameters = t(_CallMap,Table,Col,_CurMaxCost),
    get_depth_subtree([SubTree], Depth),
    get_cost_subtree(SubTree, Cost0, CostUnadjusted),
    (path_with_increasing_outputs([SubTree],col(Table,Col)) ->
        Cost = CostUnadjusted
    ;
        length(CostUnadjusted, N),
        update_cost_vector(CostUnadjusted, N, Cost)             % a small penalty if there's no path with always increasing output values
    ),
    construct_initial_frontier(R, Cost0, GlobalParameters, Options, S). 

get_depth_subtree([t([],_,_,[])], Depth) :-
    !, Depth is 0.
get_depth_subtree(SubTree, Depth) :-
    SubTree = [t(_,_,_,Children)|_],
    get_depth_subtree(Children, Depth1),
    Depth is Depth1 + 1.

get_cost_subtree(t([],_,_,[]), Cost0, Cost0) :- !.
get_cost_subtree(Tree, Cost0, Cost) :-
    Tree = t(FdNode,_,_,Branches),
    length(FdNode, LenFdNode),
    (foreach(Branch, Branches),
     foreach(CostBranch, CostBranches),
     param(Cost0)
    do 
     get_cost_subtree(Branch, Cost0, CostBranch)),
    min_member(Cost1, CostBranches),
    update_cost_vector(Cost1, LenFdNode, Cost).

path_with_increasing_outputs(Tree, Col) :-
    member(Subtree, Tree),
    Subtree = t(_FD, ValFd, _ColSetsBool, Childs),
    path_with_increasing_outputs1(Childs, ValFd, Col), !.

path_with_increasing_outputs1([], _, _) :- !.
path_with_increasing_outputs1(Tree, ValFd, Col) :-
    tab_get_distinct_val_count(Col,ValCountsList),
    member(Subtree, Tree),
    Subtree = t(_FD, ValFd1, _ColSetsBool, Childs),
    memberchk(ValFd-CountValFd,ValCountsList),
    memberchk(ValFd1-CountValFd1,ValCountsList),
    CountValFd1 >= CountValFd,
    path_with_increasing_outputs1(Childs, ValFd1, Col), !.

update_cost_vector(CurCost, LenFD, NextCost) :-
    length(CurCost, Len),
    (Len < LenFD ->
        length(NextCost, LenFD),
        suffix(NextCost, CurCost),
        NextCost = [1|Rest],
        fill_zeros(Rest)
    ;
        I is Len-LenFD+1,
        inc_nth_pos_list(I, CurCost, NextCost)
    ).

fill_zeros([]) :- !.
fill_zeros([V|_]) :-
    integer(V), !.
fill_zeros([0|R]) :-
    fill_zeros(R).

inc_nth_pos_list(1, [X|R], [Y|R]) :- !,
    Y is X + 1.
inc_nth_pos_list(I, [X|R], [X|S]) :-
    I1 is I-1,
    inc_nth_pos_list(I1, R, S).

substract_vectors(Vector1, Vector2, Res) :-
    length(Vector1, L1),
    length(Vector2, L2),
    Delta is L1-L2,
    (Delta = 0 -> substract_vectors2(Vector1, Vector2, Res)        ;
     Delta > 0 -> substract_vectors1(Delta, Vector1, Vector2, Res) ;
                  write(substract_vectors), nl, halt               ).

substract_vectors1(Delta, [V|R], Vector2, [V|S]) :-
    Delta > 0,
    !,
    Delta1 is Delta-1,
    substract_vectors1(Delta1, R, Vector2, S).
substract_vectors1(0, Vector1, Vector2, Res) :-
    substract_vectors2(Vector1, Vector2, Res).

substract_vectors2([], [], []) :- !.
substract_vectors2([V1|R], [V2|S], [V12|T]) :-
    V12 is V1-V2,
    substract_vectors2(R, S, T).

add_vectors(Vector1, Vector2, Res) :-
    length(Vector1, L1),
    length(Vector2, L2),
    Delta is L1-L2,
    (Delta = 0 -> add_vectors2(Vector1, Vector2, Res)        ;
     Delta > 0 -> add_vectors1(Delta, Vector1, Vector2, Res) ;
                  Delta1 is -Delta,
                  add_vectors1(Delta1, Vector2, Vector1, Res)).

add_vectors1(Delta, [V|R], Vector2, [V|S]) :-
    Delta > 0,
    !,
    Delta1 is Delta-1,
    add_vectors1(Delta1, R, Vector2, S).
add_vectors1(0, Vector1, Vector2, Res) :-
    add_vectors2(Vector1, Vector2, Res).

add_vectors2([], [], []) :- !.
add_vectors2([V1|R], [V2|S], [V12|T]) :-
    V12 is V1+V2,
    add_vectors2(R, S, T).

% NbColFdMax - maximum number of columns in a FD belonging to the current branch
find_max_fd_length([], NbColFdMax) :-
    !,
    NbColFdMax is 0.
find_max_fd_length(Tree, NbColFdMax) :-
    (foreach(t(Fd,_,_,Branch), Tree),
     foreach(NbColFd, NbColFdList)
    do  % for each branch of an original tree a) create a parallel branch of a solution tree and
        %                                     b) find the maximum number of columns of FDs of this branch 
     find_max_fd_length(Branch, NbColFdBranch),
     length(Fd, NbColFd1),
     (NbColFdBranch > NbColFd1 -> NbColFd = NbColFdBranch ; NbColFd = NbColFd1)),
    max_member(NbColFdMax, NbColFdList).

% SEARCH A FORMULA THAT IS VALID FOR ALL TABLE ENTRIES THAT EXPLAIN A GIVEN OUPUT COLUMN OF A TABLE
%--------------------------------------------------------------------------------------------------
search_a_single_formula_for_all_table_entries(KindCombi, CallMap, TableEntries, FormulaPattern, Output) :-
    (formula_pattern(f(_), FormulaPattern) ->
        search_a_single_formula_for_all_table_entries_formula(KindCombi, CallMap, TableEntries, FormulaPattern, Output) % polynomial or unary function or binary function
    ;
     formula_pattern(then_else(_), FormulaPattern) ->
        search_a_single_formula_for_all_table_entries_cond_ex(KindCombi, CallMap, TableEntries, FormulaPattern, Output) % complex conditional formula
    ;
     formula_pattern(op(_), FormulaPattern) ->
        search_a_single_formula_for_all_table_entries_bool(KindCombi, CallMap, TableEntries, FormulaPattern, Output)    % Boolean formula
    ;
        search_a_single_formula_for_all_table_entries_cond(KindCombi, CallMap, TableEntries, FormulaPattern, Output)    % conditional formula
    ),
    !.
search_a_single_formula_for_all_table_entries(_, CallMap, _, _, _) :-
    instrument_code(instrument_fail_without_time_out, CallMap),
    false.

search_a_single_formula_for_all_table_entries_bool(KindCombi, _CallMap, TableEntries, FormulaPattern, Output) :-
    formula_pattern(mandatory_attr(AttrNames),          FormulaPattern),      % get parameters of Boolean formula
    formula_pattern(neg(Negated),                       FormulaPattern),      % get the fact whether the Boolean formula was negated or not
                                                                              % NEWBOOL
    formula_pattern(shift(ShiftBool),                   FormulaPattern),      % get shift of the Boolean formula
                                                                              % (min of output col, or 0 if Boolean formula called from a cluster)
    formula_pattern(op(Oplus),                          FormulaPattern),      % get operator (and,or,allequal,sum,xor,voting,card1) of Boolean formula
    formula_pattern(nb_terms(NbTerms),                  FormulaPattern),      % get number of used conditions of Boolean formula
    formula_pattern(conds(Conds),                       FormulaPattern),      % get all potential conditions of Boolean formla
    formula_pattern(combined_row_ctrs(SourceVarsLocal,                        % get constraint that is global to a Boolean formula
                                      SourceVarsGlobal,
                                      TargetVarsGlobal,
                                      SourceCtrs),      FormulaPattern),
    (formula_pattern(cluster(Vals1,Vals0), FormulaPattern) ->                 % if Boolean formula called from cluster
        Output = col(Table,_),                                                % get table name for the quantity computed by the formula
        tab_get_arity(col(Table,_), Arity),                                   % get arity of the table
        append(AttrNames, [Output], ParamsOutput),                            % add just after the parameters of the formula the formula output
        length(ParamsOutput, LenTarget),                                      % number of parameters + 1
        length(TargetTerm, LenTarget),                                        % create a target term and a source term in order to extract
        functor(SourceTerm, Table, Arity),                                    % the projection of the table wrt the input parameters and the output
        build_source_target_extraction_terms(ParamsOutput, SourceTerm, TargetTerm),
        findall(TargetTermUpdated, (call(user:SourceTerm),                    % get relevant columns of current table entry, and check whether the
                                    update_table_entry_cluster(TargetTerm,    % corresponding output is in Vals1 or Vals0:
                                                               Vals1,         %  . replace Vals1 by 1 to get TargetTermUpdated
                                                               Vals0,         %  . replace Vals0 by 0 to get TargetTermUpdated
                                                               TargetTermUpdated)),
                                   TableEntriesCluster),
        remove_duplicates_from_projected_table(KindCombi, Table, Arity, AttrNames, TableEntriesCluster,
                                           TableEntriesWithoutDuplicates)     % remove duplicates to save time (avoid duplicated ctrs)
    ;                                                                         % if on a normal Boolean formula, i.e. not called from cluster
        TableEntriesWithoutDuplicates = TableEntries
    ),
    post_bool_on_all_table_entries(TableEntriesWithoutDuplicates,             % post on sorted entries as avoid duplicated entries
                                   AttrNames, Negated, ShiftBool, Oplus, NbTerms, Conds,
                                   combined_row_ctrs(SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs)).

% post entry constraints for FormulaFamily = bool
post_bool_on_all_table_entries([], _, _, _, _, _, _, _) :- !.
post_bool_on_all_table_entries([TableEntries|R], AttrNames, Negated, ShiftBool, Oplus, NbTerms, Conds,
                               combined_row_ctrs(SourceVarsLocal,SourceVarsGlobal,TargetVarsGlobal,SourceCtrs)) :-
    update_table_entry(TableEntries, Negated, ShiftBool, Oplus, NbTerms, UpdatedRes, UpdatedTableEntries),
    post_bool_on_one_table_entry(Conds, AttrNames, Oplus, NbTerms, UpdatedTableEntries, LCondBool),
    append(SourceVarsLocal, SourceVarsGlobal,  SourceVars),
    append([[UpdatedRes],LCondBool,TargetVarsGlobal], TargetVars),
    copy_term(SourceVars-SourceCtrs, TargetVars-TargetCtrs),
    post_ctrs(TargetCtrs),
    post_bool_on_all_table_entries(R, AttrNames, Negated, ShiftBool, Oplus, NbTerms, Conds,
                                   combined_row_ctrs(SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs)).


% when we cluster the table entries in two clusters respectively corresponding to
%  (1) all table entries whose result value is in the set of values Vals1
%  (2) all table entries whose result value is in the set of values Vals0
% we replace accordingly the result Res by value 1 or 0 (Res is the last element of the list of table entries)
% fails is result of current table entry not in the union Vals1 U Vals0
update_table_entry_cluster([Res], Vals1, Vals0, [UpdatedRes]) :-
    !,
    (memberchk(Res, Vals1) -> UpdatedRes = 1 ;
     memberchk(Res, Vals0) -> UpdatedRes = 0 ;
                              false          ).
update_table_entry_cluster([TableEntry|R], Vals1, Vals0, [TableEntry|S]) :-
    update_table_entry_cluster(R, Vals1, Vals0, S).

update_table_entry(TableEntries, 0, 0, _Oplus, _NbTerms, UpdatedRes, TableEntries) :-
    !, last(TableEntries, UpdatedRes).
update_table_entry(TableEntries, Negated, ShiftBool, Oplus, NbTerms, UpdatedRes, UpdatedTableEntries) :-
    update_table_entry0(TableEntries, Negated, ShiftBool, Oplus, NbTerms, UpdatedRes, UpdatedTableEntries).

update_table_entry0([Res], Negated, ShiftBool, Oplus, NbTerms, UpdatedRes, UpdatedTableEntries) :-
    !,
    (Negated = 1 ->                                                            % if Boolean formula is negated then update result
        (Oplus = sum ->
            UpdatedRes is NbTerms-(Res-ShiftBool)
        ;
            UpdatedRes is 1-(Res-ShiftBool)
        )
    ;
     ShiftBool = 0 ->
        UpdatedRes is Res
    ;
        UpdatedRes is Res-ShiftBool
    ),
    UpdatedTableEntries = [UpdatedRes].
update_table_entry0([TableEntry|R], Negated, ShiftBool, Oplus, NbTerms, UpdatedRes, [TableEntry|S]) :-
    update_table_entry0(R, Negated, ShiftBool, Oplus, NbTerms, UpdatedRes, S).

post_bool_on_one_table_entry([], _, _, _, _, []) :- !.
post_bool_on_one_table_entry([Cond|R], AttrNames, Oplus, NbTerms, TableEntries, [CondBool|S]) :-
% write(g0(Cond)), nl,
% write(g1(TableEntries)), nl,
    Cond = cond(_CondB, _NewCondType, LCondAttr, _LCondExtraVars, _LCondCostVars, _CondAttr1, _CondAttr2, _CondAttr3, _CondCst, _CondCoef,
                SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs, _CondCheckOrder),
    CondBool in 0..1,
    append(SourceVarsLocal, SourceVarsGlobal, SourceVars),    
    length(LCondAttr, LenLCondAttr),
    length(TargetLCondAttr, LenLCondAttr),
    append([[CondBool],TargetLCondAttr,TableEntries,TargetVarsGlobal], TargetVars),
% write(g2(SourceVars-SourceCtrs)), nl,
% write(g3(TargetVars-TargetCtrs)), nl,
    copy_term(SourceVars-SourceCtrs, TargetVars-TargetCtrs),
% write(g4(TargetVars-TargetCtrs)), nl,
    post_ctrs(TargetCtrs),
% write(g5), nl,
    post_bool_on_one_table_entry(R, AttrNames, Oplus, NbTerms, TableEntries, S).

search_a_single_formula_for_all_table_entries_cond_ex(_KindCombi, _CallMap, TableEntries, FormulaPattern, _Output) :-
    formula_pattern(mandatory_attr(AttrNames),          FormulaPattern),      % get parameters of Boolean formula
    formula_pattern(neg(Negated),                       FormulaPattern),      % get the fact whether the Boolean formula was negated or not
                                                                              % NEWBOOL
    formula_pattern(shift(ShiftBool),                   FormulaPattern),      % get shift of the Boolean formula
                                                                              % (min of output col, or 0 if Boolean formula called from a cluster)
    formula_pattern(op(Oplus),                          FormulaPattern),      % get operator (and,or,allequal,sum,xor,voting,card1) of Boolean formula
    formula_pattern(nb_terms(NbTerms),                  FormulaPattern),      % get number of used conditions of Boolean formula
    formula_pattern(conds(Conds),                       FormulaPattern),      % get all potential conditions of Boolean formla
    formula_pattern(combined_row_ctrs(SourceVarsLocal,                        % get constraint that is global to a Boolean formula
                                      SourceVarsGlobal,
                                      TargetVarsGlobal,
                                      SourceCtrs),      FormulaPattern),
    formula_pattern(then_else(ThenElse), FormulaPattern),
    ThenElse = [then(NewThenType, _ThenCoef, _LThenAttr, _LThenExtraVars, _LThenCostVars,
                         _ThenSourceVarsLocal, _ThenSourceVarsGlobal, _ThenTargetVarsGlobal, _ThenSourceCtrs, ThenCheckOrder),
                    else(NewElseType, _ElseCoef, _LElseAttr, _LElseExtraVars, _LElseCostVars,
                         _ElseSourceVarsLocal, _ElseSourceVarsGlobal, _ElseTargetVarsGlobal, _ElseSourceCtrs, ElseCheckOrder)],
    functor(NewThenType, FThenType, _),                                          % extract type of then in order to enforce having
    (memberchk(FThenType, [unary_term,                                        % at least two distinct returned values in the then part
                           binary_term]) ->                                   % when the then part corresponds to a unary or to a binary term
        CheckNoUniqueReturnValueInThen = 1
    ;
        CheckNoUniqueReturnValueInThen = 0
    ),
    functor(NewElseType, FElseType, _),                                          % extract type of else in order to enforce having
    (memberchk(FElseType, [unary_term,                                        % at least two distinct returned values in the else part
                           binary_term]) ->                                   % when the else part corresponds to a unary or to a binary term
         CheckNoUniqueReturnValueInElse = 1
    ;
         CheckNoUniqueReturnValueInElse = 0
    ),
    post_cond_ex_on_all_table_entries(TableEntries,             % NEW post on sorted entries as avoid duplicated entries
                                      AttrNames, Negated, ShiftBool, Oplus, NbTerms, Conds,
                                      combined_row_ctrs(SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs),
                                      ThenElse, CheckNoUniqueReturnValueInThen, CheckNoUniqueReturnValueInElse,
                                      LBoolCondWithDistinctThenElse, LCondOrder, LThenOrder, LElseOrder, LResCond, LResThen, LResElse),
    CondCheckOrder = 0,
    additional_cond_ctrs_post(CondCheckOrder, ThenCheckOrder, ElseCheckOrder, CheckNoUniqueReturnValueInThen, CheckNoUniqueReturnValueInElse,
                              LBoolCondWithDistinctThenElse, LCondOrder, LThenOrder, LElseOrder,
                              LResCond, LResThen, LResElse).

% post entry constraints for FormulaFamily = cond_ex
post_cond_ex_on_all_table_entries([], _, _, _, _, _, _, _, _, _, _, [], [], [], [], [], [], []) :- !.
post_cond_ex_on_all_table_entries([TableEntries|R], AttrNames, Negated, ShiftBool, Oplus, NbTerms, Conds,
                               combined_row_ctrs(SourceVarsLocal,SourceVarsGlobal,TargetVarsGlobal,SourceCtrs),
                               ThenElse, CheckNoUniqueReturnValueInThen, CheckNoUniqueReturnValueInElse,
                               LBoolCondWithDistinctThenElse, LCondOrder, LThenOrder, LElseOrder, LResCond, LResThen, LResElse) :-
    ThenElse = [then(_NewThenType, _ThenCoef, _LThenAttr, _LThenExtraVars, _LThenCostVars,
                ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder),
                else(_NewElseType, _ElseCoef, _LElseAttr, _LElseExtraVars, _LElseCostVars,
                ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder)],
    remove_last_elem(TableEntries, TableEntriesWithoutLast, EntryRes),         % split table entries from which result is computed and result itself
    length(TableEntriesWithoutLast, LenA),
    length(ThenSourceVarsLocal, LenThenSourceVarsLocal),                       % get number of local source variables (include input parameters)
    DeltaThenSourceVarsLocal is LenThenSourceVarsLocal-LenA,                   % get number of local source variables that are not input parameters
    length(PThenTargetVarsLocal, DeltaThenSourceVarsLocal),                    % create fresh new target variables (as vary wrt table entries)
    append(ThenSourceVarsLocal, ThenSourceVarsGlobal, TSourceVars),            % put together all source variables
                                                                               % put together all target variables
    append([PThenTargetVarsLocal, TableEntriesWithoutLast, ThenTargetVarsGlobal], TTargetVars),
    ThenSourceVarsLocal  = [LogResThen|_],
    PThenTargetVarsLocal = [EntryResThen|_],
    copy_term(TSourceVars-ThenSourceCtrs, TTargetVars-ThenTargetCtrs),
    post_ctrs(ThenTargetCtrs),
    length(ElseSourceVarsLocal, LenElseSourceVarsLocal),                       % get number of local source variables (include input parameters)
    DeltaElseSourceVarsLocal is LenElseSourceVarsLocal-LenA,                   % get number of local source variables that are not input parameters
    length(PElseTargetVarsLocal, DeltaElseSourceVarsLocal),                    % create fresh new target variables (as vary wrt table entries)
    append(ElseSourceVarsLocal, ElseSourceVarsGlobal, ESourceVars),            % put together all source variables
    % put together all target variables
    append([PElseTargetVarsLocal, TableEntriesWithoutLast, ElseTargetVarsGlobal], ETargetVars),
    ElseSourceVarsLocal  = [LogResElse|_],
    PElseTargetVarsLocal = [EntryResElse|_],
    copy_term(ESourceVars-ElseSourceCtrs, ETargetVars-ElseTargetCtrs),
    post_ctrs(ElseTargetCtrs),
    SourceVarsLocal = [LogResBool|_],

    EntryResBool in 0..1,                                                      % tell whether on then or else part depending on the condition
    remove_last_elem(TableEntriesResBool,TableEntriesWithoutLast,EntryResBool),% switch the output column to the local target variable
    post_bool_on_one_table_entry(Conds, AttrNames, Oplus, NbTerms, TableEntriesResBool, LCondBool),
        
    append(SourceVarsLocal, SourceVarsGlobal,  SourceVars),
    append([[EntryResBool],LCondBool,TargetVarsGlobal], TargetVars),
    copy_term(SourceVars-SourceCtrs, TargetVars-TargetCtrs),
    post_ctrs(TargetCtrs),
        
    LSourceCtr = [  LogResBool #=> (LogRes #= LogResThen),
                  #\LogResBool #=> (LogRes #= LogResElse)],
    copy_term([LogRes,  LogResThen,  LogResElse,  LogResBool  ]-LSourceCtr,
              [EntryRes,EntryResThen,EntryResElse,EntryResBool]-LTargetCtr),
    post_ctrs(LTargetCtr),
    
    BoolCondWithDistinctThenElse in 0..2,
    EntryResThen #=  EntryResElse #=> BoolCondWithDistinctThenElse #= 2,       % ignore the Booleans for which ResThen=ResElse
    EntryResThen #\= EntryResElse #=> BoolCondWithDistinctThenElse #= EntryResBool,

    LBoolCondWithDistinctThenElse = [BoolCondWithDistinctThenElse|S],

    LCondOrder = U,
    (ThenCheckOrder > 0 ->                                                     % if need to return ThenOrder variable to check that both sides of
        nth1(ThenCheckOrder, PThenTargetVarsLocal, ThenOrder),                 % a min/max unary function are used on at least one table entry then
        LThenOrder = [EntryResBool-ThenOrder|V]                                    % extract ThenOrder variable using its position ThenCheckOrder
    ;                                                                          % (see formula_generator_then_else in gen_cond_formula.pl)
        LThenOrder = []
    ),
    (ElseCheckOrder > 0 ->                                                     % if need to return ElseOrder variable to check that both sides of
        nth1(ElseCheckOrder, PElseTargetVarsLocal, ElseOrder),                 % a min/max unary function are used on at least one table entry then
        LElseOrder = [EntryResBool-ElseOrder|W]                                    % extract ElseOrder variable using its position ElseCheckOrder
    ;                                                                          % (see formula_generator_then_else in gen_cond_formula.pl)
        LElseOrder = []
    ),
    LResCond = [EntryResBool|X],
    LResThen = [EntryResThen|Y],
    LResElse = [EntryResElse|Z],
    post_cond_ex_on_all_table_entries(R, AttrNames, Negated, ShiftBool, Oplus, NbTerms, Conds,
                                      combined_row_ctrs(SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs),
                                      ThenElse, CheckNoUniqueReturnValueInThen, CheckNoUniqueReturnValueInElse,
                                      S, U, V, W, X, Y, Z).


search_a_single_formula_for_all_table_entries_cond(_KindCombi, _CallMap, TableEntries, FormulaPattern, _Output) :-
                                                                              % get params, source vars, target vars, source ctrs of the formula
    formula_pattern(cond(_,_,_,_,_,_,_,CondSourceVarsLocal,CondSourceVarsGlobal,CondTargetVarsGlobal,
                         CondSourceCtrs,CondCheckOrder), FormulaPattern),
    formula_pattern(then(ThenType,_,_,_,_,_,  ThenSourceVarsLocal,ThenSourceVarsGlobal,ThenTargetVarsGlobal,
                         ThenSourceCtrs,ThenCheckOrder), FormulaPattern),
    formula_pattern(else(ElseType,_,_,_,_,_,  ElseSourceVarsLocal,ElseSourceVarsGlobal,ElseTargetVarsGlobal,
                         ElseSourceCtrs,ElseCheckOrder), FormulaPattern),
    functor(ThenType, FThenType, _),                                          % extract type of then in order to enforce having
    (memberchk(FThenType, [unary_term,                                        % at least two distinct returned values in the then part
                           binary_term]) ->                                   % when the then part corresponds to a unary or to a binary term
        CheckNoUniqueReturnValueInThen = 1
    ;
        CheckNoUniqueReturnValueInThen = 0
    ),
    functor(ElseType, FElseType, _),                                          % extract type of else in order to enforce having
    (memberchk(FElseType, [unary_term,                                        % at least two distinct returned values in the else part
                           binary_term]) ->                                   % when the else part corresponds to a unary or to a binary term
        CheckNoUniqueReturnValueInElse = 1
    ;
        CheckNoUniqueReturnValueInElse = 0
    ),
    post_cond_on_all_table_entries(TableEntries,                              % post on sorted entries as avoid duplicated entries
                                   CondSourceVarsLocal, CondSourceVarsGlobal, CondTargetVarsGlobal, CondSourceCtrs, CondCheckOrder,
                                   ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder,
                                   ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder,
                                   LBoolCondWithDistinctThenElse, LCondOrder, LThenOrder, LElseOrder,
                                   LResCond, LResThen, LResElse),
    additional_cond_ctrs_post(CondCheckOrder, ThenCheckOrder, ElseCheckOrder, CheckNoUniqueReturnValueInThen, CheckNoUniqueReturnValueInElse,
                              LBoolCondWithDistinctThenElse, LCondOrder, LThenOrder, LElseOrder,
                              LResCond, LResThen, LResElse).

post_cond_on_all_table_entries([], _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, [], [], [], [], [], [], []) :- !.
post_cond_on_all_table_entries([TableEntries|R], 
                               CondSourceVarsLocal, CondSourceVarsGlobal, CondTargetVarsGlobal, CondSourceCtrs, CondCheckOrder,
                               ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder,
                               ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder,
                               [BoolCondWithDistinctThenElse|S], LCondOrder, LThenOrder, LElseOrder,
                               [BoolCond|X], [ResThen|Y], [ResElse|Z]) :-
    remove_last_elem(TableEntries, TableEntriesWithoutLast, Result),           % split table entries from which result is computed and result itself
    length(TableEntriesWithoutLast, LenA),                                     % get number of input parameters
    length(CondSourceVarsLocal, LenCondSourceVarsLocal),                       % get number of local source variables (include input parameters)
    DeltaCondSourceVarsLocal is LenCondSourceVarsLocal-LenA,                   % get number of local source variables that are not input parameters
    length(PCondTargetVarsLocal, DeltaCondSourceVarsLocal),                    % create fresh new target variables (as vary wrt table entries)
    append(CondSourceVarsLocal, CondSourceVarsGlobal, CSourceVars),            % put together all source variables
                                                                               % put together all target variables
    append([PCondTargetVarsLocal, TableEntriesWithoutLast, CondTargetVarsGlobal], CTargetVars),
    copy_term(CSourceVars-CondSourceCtrs, CTargetVars-CondTargetCtrs),
    post_ctrs(CondTargetCtrs),
    length(ThenSourceVarsLocal, LenThenSourceVarsLocal),                       % get number of local source variables (include input parameters)
    DeltaThenSourceVarsLocal is LenThenSourceVarsLocal-LenA,                   % get number of local source variables that are not input parameters
    length(PThenTargetVarsLocal, DeltaThenSourceVarsLocal),                    % create fresh new target variables (as vary wrt table entries)
    append(ThenSourceVarsLocal, ThenSourceVarsGlobal, TSourceVars),            % put together all source variables
                                                                               % put together all target variables
    append([PThenTargetVarsLocal, TableEntriesWithoutLast, ThenTargetVarsGlobal], TTargetVars),
    copy_term(TSourceVars-ThenSourceCtrs, TTargetVars-ThenTargetCtrs),
    post_ctrs(ThenTargetCtrs),
    length(ElseSourceVarsLocal, LenElseSourceVarsLocal),                       % get number of local source variables (include input parameters)
    DeltaElseSourceVarsLocal is LenElseSourceVarsLocal-LenA,                   % get number of local source variables that are not input parameters
    length(PElseTargetVarsLocal, DeltaElseSourceVarsLocal),                    % create fresh new target variables (as vary wrt table entries)
    append(ElseSourceVarsLocal, ElseSourceVarsGlobal, ESourceVars),            % put together all source variables
                                                                               % put together all target variables
    append([PElseTargetVarsLocal, TableEntriesWithoutLast, ElseTargetVarsGlobal], ETargetVars),
    copy_term(ESourceVars-ElseSourceCtrs, ETargetVars-ElseTargetCtrs),
    post_ctrs(ElseTargetCtrs),
    PCondTargetVarsLocal = [BoolCond|_],                                       % extract Boolean condition variable (depend from table entries)
    PThenTargetVarsLocal = [ResThen|_],                                        % extract result of the then         (depend from table entries)
    PElseTargetVarsLocal = [ResElse|_],                                        % extract result of the else         (depend from table entries)
    Result #= BoolCond*ResThen + (1-BoolCond)*ResElse,
    ResThen #\= Result #=> BoolCond #= 0,
    ResElse #\= Result #=> BoolCond #= 1,
    BoolCondWithDistinctThenElse in 0..2,
    ResThen #=  ResElse #=> BoolCondWithDistinctThenElse #= 2,                 % ignore the Booleans for which ResThen=ResElse
    ResThen #\= ResElse #=> BoolCondWithDistinctThenElse #= BoolCond,
    (CondCheckOrder > 0 ->                                                     % if need to return CondOrder variable to check that both sides of
        nth1(CondCheckOrder, PCondTargetVarsLocal, CondOrder),                 % a min/max unary function are used on at least one table entry then
        LCondOrder = [CondOrder|U]                                             % extract CondOrder variable using its position CondCheckOrder
    ;                                                                          % (see formula_generator_condition in gen_cond_formula.pl)
        LCondOrder = []
    ),
    (ThenCheckOrder > 0 ->                                                     % if need to return ThenOrder variable to check that both sides of
        nth1(ThenCheckOrder, PThenTargetVarsLocal, ThenOrder),                 % a min/max unary function are used on at least one table entry then
        LThenOrder = [BoolCond-ThenOrder|V]                                    % extract ThenOrder variable using its position ThenCheckOrder
    ;                                                                          % (see formula_generator_then_else in gen_cond_formula.pl)
        LThenOrder = []
    ),
    (ElseCheckOrder > 0 ->                                                     % if need to return ElseOrder variable to check that both sides of
        nth1(ElseCheckOrder, PElseTargetVarsLocal, ElseOrder),                 % a min/max unary function are used on at least one table entry then
        LElseOrder = [BoolCond-ElseOrder|W]                                    % extract ElseOrder variable using its position ElseCheckOrder
    ;                                                                          % (see formula_generator_then_else in gen_cond_formula.pl)
        LElseOrder = []
    ),
    post_cond_on_all_table_entries(R,
                                   CondSourceVarsLocal, CondSourceVarsGlobal, CondTargetVarsGlobal, CondSourceCtrs, CondCheckOrder,
                                   ThenSourceVarsLocal, ThenSourceVarsGlobal, ThenTargetVarsGlobal, ThenSourceCtrs, ThenCheckOrder,
                                   ElseSourceVarsLocal, ElseSourceVarsGlobal, ElseTargetVarsGlobal, ElseSourceCtrs, ElseCheckOrder,
                                   S, U, V, W, X, Y, Z).

additional_cond_ctrs_post(CondCheckOrder, ThenCheckOrder, ElseCheckOrder, CheckNoUniqueReturnValueInThen, CheckNoUniqueReturnValueInElse,
                     LBoolCondWithDistinctThenElse, LCondOrder, LThenOrder, LElseOrder,
                     LResCond, LResThen, LResElse) :-
    minimum(0, LBoolCondWithDistinctThenElse),                                % the condition should be false for at least one table entry, and
    count(1,LBoolCondWithDistinctThenElse,#>,0),                              % true for at least one table entry (focus just on entries where
                                                                              % the value returned by the Then is different from the value returned
                                                                              % by the Else)
    (CondCheckOrder = 1 ->
        minimum(0, LCondOrder),
        maximum(2, LCondOrder)
    ;
        true
    ),
    (ThenCheckOrder > 0 ->                                                    % for a unary term min/max used in a then, check that both arguments
        gen_then_else_min_max_ctr(LThenOrder, 1, 0, ThenMinMaxCtr0),          % are used for at least one table entry such that the condition is true
        call(ThenMinMaxCtr0),
        gen_then_else_min_max_ctr(LThenOrder, 1, 2, ThenMinMaxCtr2),
        call(ThenMinMaxCtr2)
    ;
        true
    ),
    (ElseCheckOrder > 0 ->                                                    % for a unary term min/max used in a else, check that both arguments
        gen_then_else_min_max_ctr(LElseOrder, 0, 0, ElseMinMaxCtr0),          % are used for at least one table entry such that the condition is false
        call(ElseMinMaxCtr0),
        gen_then_else_min_max_ctr(LElseOrder, 0, 2, ElseMinMaxCtr2),
        call(ElseMinMaxCtr2)
    ;
        true
    ),
    (CheckNoUniqueReturnValueInThen = 1 ->
        no_unique_value_for_res_then_or_res_else(LResCond, LResThen, 1)
    ;
        true
    ),
    (CheckNoUniqueReturnValueInElse = 1 ->
        no_unique_value_for_res_then_or_res_else(LResCond, LResElse, 0)
    ;
        true
    ).

post_ctrs([]) :- !.
post_ctrs([Ctr|R]) :-
    %write(before_posting(Ctr)), nl,
    call(Ctr),
    %write(after__posting(Ctr)), nl,
    post_ctrs(R).

search_a_single_formula_for_all_table_entries_formula(_KindCombi, _CallMap, TableEntries, FormulaPattern, Output) :-
    formula_pattern(f(F),                       FormulaPattern),           % get type of the formula
    formula_pattern(tunary(UnaryTemplates),     FormulaPattern),           % get templates of unary terms
    formula_pattern(tbinary(BinaryTemplates),   FormulaPattern),           % get templates of binary terms
    formula_pattern(tpolynom(PolynomTemplates), FormulaPattern),           % get templates of polynoms
    formula_pattern(tunaryf(UnaryFTemplate),    FormulaPattern),           % get template of unary function
    formula_pattern(tbinaryf(BinaryFTemplate),  FormulaPattern),           % get template of binary function
    Output = col(Table,Col),                                               % get table name and column number for the quantity computed by the formula
    tab_get_type(col(Table,Col), Type),                                    % get type of output column (cst,bool,int)
    ((F = 2, Type \= bool) ->                                              % if the formula corresponds to a unary function and result not a Boolean
        formula_pattern(unaryf([UnaryFunction]), FormulaPattern),          % get the unary function
        functor(UnaryFunction, UFunction, _),                              % and fail if we have the "geq0" or the "in" function
        (memberchk(UFunction, [geq0,in]) -> false ; true)
    ;
        true
    ),
    formula_pattern_gen_additional_ctr(FormulaPattern, Output),            % generate an extra constraint wrt target column
    tab_get_inf_sup(col(Table,_), Inf, Sup),                               % get smallest and largest values in the table
    (F = 2 ->                                                              % we have a unary function involving one polynom
        formula_pattern(unaryf([UnaryFunction]), FormulaPattern),          % get the unary function
        functor(UnaryFunction, UFunction, _),
        (memberchk(UFunction, [min,max,floor,mod]) ->                      % for some binary functions set a flag to remember
            RecordUnaryFRes = 1                                            % that we will have to record the result of the polynom
        ;                                                                  % to set up a constraint after processing all table entries
            RecordUnaryFRes = 0
        )
    ;
        RecordUnaryFRes = 0
    ),
    (F = 3 ->                                                              % we have a binary function between two polynoms
        formula_pattern(binaryf([BinaryFunction]), FormulaPattern),        % get the binary function
        functor(BinaryFunction, BFunction, _),
        (memberchk(BFunction, [min,max,floor,mod,cmod,dmod]) ->            % for some binary functions set a flag to remember (no ceil binary function!)
            RecordBinaryFRes = 1                                           % that we will have to record the results of the two polynoms
        ;                                                                  % to set up a constraint after processing all table entries
            RecordBinaryFRes = 0
        )
    ;
        RecordBinaryFRes = 0
    ),
    post_on_all_table_entries(TableEntries, Inf, Sup, F,                   % post on sorted entries as avoid duplicated entries
                              t(PolynomTemplates, UnaryTemplates, BinaryTemplates, UnaryFTemplate, BinaryFTemplate),
                              RecordUnaryFRes, RecordBinaryFRes, UnaryFRes, BinaryFRes),
    (RecordUnaryFRes = 1 ->                                                % post a last constraint after processing all table entries
        arg(1, UnaryFunction, CstUnaryFunction),                           % get constant of the unary term
        set_lt_gt_ctr_for_at_least_one_table_entry_for_ufunction(UnaryFRes, CstUnaryFunction, UFunction)
    ;
        true
    ),
    (RecordBinaryFRes = 1 ->                                               % post a last constraint after processing all table entries
        set_lt_gt_ctr_for_at_least_one_table_entry_for_bfunction(BinaryFRes, BFunction)
    ;
        true
    ).

post_on_all_table_entries([], _, _, _, _, _, _, [], []) :- !.
post_on_all_table_entries([TableEntries|R], Inf, Sup, F, Templates, RecordUnaryFRes, RecordBinaryFRes, UnaryFRes, BinaryFRes) :-
    Templates = t(PolynomTemplates, UnaryTemplates, BinaryTemplates, UnaryFTemplate, BinaryFTemplate),
%   write(table(TableEntries)), nl,
    length(TableEntries, Len),
    NParam is Len-1,
    last(TableEntries, Result),                                         % get result variable
    prefix_length(TableEntries, Params, NParam),                        % get parameters
    post_unary(UnaryTemplates, Params, ResUnaryTemplates),              % post unary terms and get corresponding result vars.
    post_binary(BinaryTemplates, Inf, Sup, Params, ResBinaryTemplates), % post binary terms and get corresponding result vars.
    (F = 1 ->
        PolynomTemplates = [t(PolynomSourceVars,PolynomSourceTerm,PolynomTargetVars)],
        append([[Result], Params, ResUnaryTemplates, ResBinaryTemplates, PolynomTargetVars], NewPolynomTargetVars), % complete target variables
        copy_term(PolynomSourceVars-PolynomSourceTerm, NewPolynomTargetVars-PolynomTargetTerm),
        call(PolynomTargetTerm),
        UnaryFRes  = RestUnaryFRes,                           % no need to record result variable of unary functions
        BinaryFRes = RestBinaryFRes                           % no need to record results variables of two sides of binary functions
    ;
     F = 2 ->
        PolynomTemplates = [t(PolynomSourceVars,PolynomSourceTerm,PolynomTargetVars)],
        append([[Res], Params, ResUnaryTemplates, ResBinaryTemplates, PolynomTargetVars], NewPolynomTargetVars), % complete target variables
        % Sup1 is min(300,3*Sup)*Sup, Res in Inf..Sup1,
        Res in -10000..10000,
        copy_term(PolynomSourceVars-PolynomSourceTerm, NewPolynomTargetVars-PolynomTargetTerm),
        call(PolynomTargetTerm),
        % Result in Inf..Sup,
        Result in -10000..10000,
        post_unaryf(UnaryFTemplate, [Result,Res]),
        (RecordUnaryFRes = 1 ->                               % if need to record results variables of a unary function
            UnaryFRes = [Res|RestUnaryFRes]                   % to state a constraint later one, then record Res
        ;
            UnaryFRes = RestUnaryFRes
        ),
        BinaryFRes = RestBinaryFRes                            % no need to record results variables of two sides of binary functions

        % TODO:
        % . when we use a unary function of the form min, max, floor, ceil, mod, geq0, in, we want to check that for some condition wrt   
        %   the different entries of the table; because of that we need to return some pair of results for each entry of the table, and  
        %   post some condition just after as we did for the binary functions in the unary term in, we should both adjust 
        %   upper limit of an interval of values wrt the minimum and maximum if possible the lower and
        %   values of the attributes (the minimum and maximum value of an attribute should be determined by looking at the different
        %   metadata files
        % . for a same combination of characteristics when the size of the data vary, in order to avoid wrong minimum/maximum values).
        % . in a binary function the two polynoms should not both correspond to a polynom that consist of a single monome of degree
        %   one (otherwise the binary function corresponds to a binary term)
    ;
     F = 3 ->                                                 % binary function
        PolynomTemplates = [t(PolynomSourceVars1,PolynomSourceTerm1,PolynomTargetVars1),
                            t(PolynomSourceVars2,PolynomSourceTerm2,PolynomTargetVars2)],
        append([[Res1], Params, ResUnaryTemplates, ResBinaryTemplates, PolynomTargetVars1], NewPolynomTargetVars1),
        append([[Res2], Params, ResUnaryTemplates, ResBinaryTemplates, PolynomTargetVars2], NewPolynomTargetVars2),
        Res1 in -10000..10000,
        Res2 in -10000..10000,
        copy_term(PolynomSourceVars1-PolynomSourceTerm1, NewPolynomTargetVars1-PolynomTargetTerm1),
        copy_term(PolynomSourceVars2-PolynomSourceTerm2, NewPolynomTargetVars2-PolynomTargetTerm2),
        call(PolynomTargetTerm1),
        call(PolynomTargetTerm2),
        Result in -10000..10000,
        post_binaryf(BinaryFTemplate, [Result,Res1,Res2]),
        (RecordBinaryFRes = 1 ->                              % if need to record results variables of two sides of a binary function
            BinaryFRes = [Res1-Res2|RestBinaryFRes]           % to state a constraint later one, then record Res1 and Res2
        ;
            BinaryFRes = RestBinaryFRes
        ),
        UnaryFRes = RestUnaryFRes                             % no need to record result variable of unary functions
    ;
        write(post_on_all_table_entries), nl, halt
    ),
    post_on_all_table_entries(R, Inf, Sup, F, Templates, RecordUnaryFRes, RecordBinaryFRes, RestUnaryFRes, RestBinaryFRes).

set_lt_gt_ctr_for_at_least_one_table_entry_for_ufunction(UnaryFunctionResults, Cst, UFunction) :-
    gen_all_cmp_signatures(UnaryFunctionResults, Cst, UFunction, AllCmp),
    (memberchk(UFunction, [min,max]) -> minimum(0, AllCmp), maximum(2, AllCmp) ; % both arg1 < cst and arg1 > cst
     UFunction = floor               -> maximum(1, AllCmp)                     ; % arg1 >  cst
     UFunction = mod                 -> maximum(1, AllCmp)                     ; % arg1 >= cst
                                        write(set_lt_gt_ctr_for_at_least_one_table_entry_for_ufunction), nl, halt).

set_lt_gt_ctr_for_at_least_one_table_entry_for_bfunction(BinaryFunctionResults, BFunction) :-
    gen_all_cmp_signatures(BinaryFunctionResults, BFunction, AllCmp),
    (memberchk(BFunction, [min,max])       -> minimum(0, AllCmp), maximum(2, AllCmp) ; % both arg1 < arg2 and arg1 > arg2
     BFunction = floor                     -> maximum(1, AllCmp)                     ; % arg1 >  arg2
     memberchk(BFunction, [mod,cmod,dmod]) -> maximum(1, AllCmp)                     ; % mod,dmod:arg1 >= arg2, cmod:arg2 >= arg1
                                              write(set_lt_gt_ctr_for_at_least_one_table_entry_for_bfunction), nl, halt).

gen_all_cmp_signatures([], _, _, []) :- !.
gen_all_cmp_signatures([Res|R], Cst, UFunction, [Cmp|S]) :-
    (UFunction = floor ->
        Cmp in 0..1,
        call(Res #=< Cst #<=> Cmp #= 0),
        call(Res #>  Cst #<=> Cmp #= 1)  % will enforce that for at least one table entry, the numerator (Res) is greater than the denominator (Cst)
    ;
     UFunction = mod ->
        Cmp in 0..1,
        call(Res #<  Cst #<=> Cmp #= 0),
        call(Res #>= Cst #<=> Cmp #= 1)  % will enforce that for at least one table entry, Res is greater than or equal to Cst
    ;
        Cmp in 0..2,                     % we are on min or max
        call(Res #< Cst #<=> Cmp #= 0),  % will enforce that for at least one table entry, Res is smaller than Cst
        call(Res #= Cst #<=> Cmp #= 1),
        call(Res #> Cst #<=> Cmp #= 2)   % will enforce that for at least one table entry, Res is greater than Cst
    ),
    gen_all_cmp_signatures(R, Cst, UFunction, S).

gen_all_cmp_signatures([], _, []) :- !.
gen_all_cmp_signatures([Res1-Res2|R], BFunction, [Cmp|S]) :-
    (BFunction = floor ->
        Cmp in 0..1,
        call(Res1 #=< Res2 #<=> Cmp #= 0),
        call(Res1 #>  Res2 #<=> Cmp #= 1)  % will enforce that for at least one table entry, the numerator (Res1) is greater than the denominator (Res2)
    ;
     memberchk(BFunction, [mod,dmod]) ->
        Cmp in 0..1,
        call(Res1 #<  Res2 #<=> Cmp #= 0),
        call(Res1 #>= Res2 #<=> Cmp #= 1)  % will enforce that for at least one table entry, Res1 is greater than or equal to Res2
    ;
     BFunction = cmod ->
        Cmp in 0..1,
        call(Res2 #<  Res1 #<=> Cmp #= 0),
        call(Res2 #>= Res1 #<=> Cmp #= 1)  % will enforce that for at least one table entry, Res2 is greater than or equal to Res1
    ;
        Cmp in 0..2,                       % we are on min or max
        call(Res1 #< Res2 #<=> Cmp #= 0),  % will enforce that for at least one table entry, Res1 is smaller than Res2
        call(Res1 #= Res2 #<=> Cmp #= 1),
        call(Res1 #> Res2 #<=> Cmp #= 2)   % will enforce that for at least one table entry, Res1 is greater than Res2
    ),
    gen_all_cmp_signatures(R, BFunction, S).

post_unary([], _, []) :- !.
post_unary([UnaryTemplate|R], Params, [ResUnaryTemplate|S]) :-
    UnaryTemplate = t(UnarySourceVars,UnarySourceTerm,UnaryTargetVars,_),           % last argument is not used at this place (but on the monomes) % modif
    append([[ResUnaryTemplate], Params, UnaryTargetVars], NewUnaryTargetVars),
    copy_term(UnarySourceVars-UnarySourceTerm, NewUnaryTargetVars-UnaryTargetTerm),
    call(UnaryTargetTerm),
    post_unary(R, Params, S).

post_binary([], _, _, _, []) :- !.
post_binary([BinaryTemplate|R], Inf, Sup, Params, [ResBinaryTemplate|S]) :-
    BinaryTemplate = t(BinarySourceVars,BinarySourceTerm,BinaryTargetVars),
    % Sup1 is min(300,3*Sup)*Sup,
    % Attr1 in Inf..Sup1,
    % Attr2 in Inf..Sup1,
    Attr1 in -10000..10000,
    Attr2 in -10000..10000,
    append([[ResBinaryTemplate], Params, [Attr1,Attr2], BinaryTargetVars], NewBinaryTargetVars),
    copy_term(BinarySourceVars-BinarySourceTerm, NewBinaryTargetVars-BinaryTargetTerm),
    post_unaries_or_binaries(BinaryTargetTerm),
    post_binary(R, Inf, Sup, Params, S).

post_binaryf(BinaryFTemplate, Results) :-
    BinaryFTemplate = [t(BinaryFSourceVars,BinaryFSourceTerm,BinaryFTargetVars)],
    append(Results, BinaryFTargetVars, NewBinaryFTargetVars),
    copy_term(BinaryFSourceVars-BinaryFSourceTerm, NewBinaryFTargetVars-BinaryFTargetTerm),
    post_unaries_or_binaries(BinaryFTargetTerm).

post_unaryf(UnaryTemplate, [Result,Res]) :-
    UnaryTemplate = [t(UnarySourceVars,UnarySourceTerm,UnaryTargetVars)],
    append([[Result,Res], UnaryTargetVars], NewUnaryTargetVars),
    copy_term(UnarySourceVars-UnarySourceTerm, NewUnaryTargetVars-UnaryTargetTerm),
    post_unaries_or_binaries(UnaryTargetTerm).

post_unaries_or_binaries([]) :- !.
post_unaries_or_binaries([Ctr|R]) :-
    call(Ctr),
    post_unaries_or_binaries(R).

% remove all duplicated entries of a projected table in order to avoid posting again and again the same constraints
% on duplicated table entries to save time; assumes that all input params.of a table are located consecutively on the
% first columns of a table and are sorted lexicographically
remove_duplicates_from_projected_table(KindCombi, Table, Arity, AttrNames, TableEntries, TableEntriesWithoutDuplicates) :-
    (KindCombi = model_seeker ->                                              % if within the model seeker then
        sort(TableEntries, TableEntriesWithoutDuplicates)                     % have to sort to remove duplicates
    ;                                                                         % if on combinatorial objects
        user:signature(Table, _, ColumnsDescription),                         % read signature fact of table Table in order to
        findall(col(Table,I), (I in 1..Arity,                                 % extract primary input columns of the table
                               indomain(I),
                               arg(I, ColumnsDescription, t(_,primary,input))), ColPrimaries),
        (prefix(ColPrimaries, AttrNames) ->                                   % if all input parameters of the table belong to AttrNames
            TableEntriesWithoutDuplicates = TableEntries                      % then no need to sort to remove duplicates
        ;                                                                     % else need to remove duplicates:
         prefix(AttrNames, ColPrimaries) ->                                   %   if AttrNames prefix of input parameters of the table
            remove_consecutive_identical_entries(TableEntries,                %   then remove duplicates by removing identical adjacent elements
                                                 TableEntriesWithoutDuplicates)
        ;                                                                     %   else remove duplicates by sorting
            sort(TableEntries, TableEntriesWithoutDuplicates)        
        )
    ).
