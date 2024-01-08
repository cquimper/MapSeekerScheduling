% Purpose: UTILITIES USED TO GENERATE A PARAMETERISED CONDITIONAL CANDIDATE FORMULA WRT SOME GIVEN OPTIONS,
%          AND ON BACTRACKING PRODUCE THE NEXT CANDIDATE FORMULA
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_cond_formula,[all_options/2,                              % contain the different possible options
                            formula_generator_condition/18,             % generate conditional part of a candidate formula where coefficient are not yet fixed
                            formula_generator_then_else/16,             % generate then/else part of a candidate formula where coefficient are not yet fixed
                            avoid_same_func_in_cond_and_then/6,         % avoid using same unary/binary function with same attributes in cond and then parts
                            avoid_same_condattr_in_then/5,              % avoid using same same attribute in cond and then parts of cond of certain types
                            avoid_same_condattr_in_else/5,              % avoid using same same attribute in cond and else parts of cond of certain types
                            avoid_simplifiable_conditional/10,
                            no_unique_value_for_res_then_or_res_else/3,
                            gen_then_else_min_max_ctr/4               ]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(utility).
:- use_module(bool_cond_utility).
:- use_module(table_access).
                                                                          % OPTIONS FOR A CONDITIONAL FORMULA
% for a condional formula we can use the following
%  . unary : plus, minus, prod, min, max, max_min, floor, ceil, floor_min, mod
%  . binary: plus, minus, linear, abs, min, max, plus_min, prod, floor, mfloor, ceil, plus_floor, minus_floor, mod, cmod, dmod, fmod
all_options(cond,            [attr_eq_coef,   attr_eq_attr,   unary_term_eq_coef,  binary_term_eq_coef,
                              attr_leq_coef,  attr_leq_attr,  unary_term_leq_coef, binary_term_leq_coef,
                              attr_leq_unary, unary_leq_attr, attr_in_interval,
                              linear_eq_coef, sum_leq_attr,   minus_mod_eq0]).
all_options(then,            [cst, attr, unary_term, binary_term]).
all_options(else,            [cst, attr, unary_term, binary_term]).


% OPTIONS FOR A MAIN FORMULA
% since it's used in both gen_poly_formula and gen_cond_formula should it be moved to utilities so there's no need to
% import gen_cond_formula to gen_poly_formula
all_options(unary_boolean,   [in,geq]).                                   % all unary  functions which can be used for a unary  term which return 0/1
all_options(unary,           [plus,minus,prod,min,max,max_min,            % all unary  functions which can be used for a unary  term
                              floor,ceil,floor_min,mod,                   % (do not accept geq, in, power, sum_consec in conditional formulae)
                              in,geq,power,sum_consec]).                  % (do not accept plus,minus,prod,max_min,floor_min in unconditional formulae)
all_options(binary,          [plus,minus,linear,abs,min,max,plus_min,     % all binary functions which can be used for a binary term
                              prod,floor,mfloor,ceil,plus_floor,          % (plus,minus,linear,abs,plus_min,plus_floor only used
                              minus_floor,mod,cmod,dmod,fmod]).           %  on conditional formulae) % NEW
all_options(unary_function,  [in,geq0,power,sum_consec]).                 % all unary  functions which can be used for a unary  function
all_options(binary_function, [min,max,floor,mod,prod,cmod,dmod]).         % all binary functions which can be used for a binary function

% From the options describing a condition, generate the corresponding condition (its constraints, source variables).
%
% INPUT PARAMETERS
%  LenA            : number of mandatory attributes (note that we only have mandatory attributes)
%  MandatoryAttr   : list of mandatory attributes
%  Cond            : term representing the options of the condition
%
% OUTPUT PARAMETERS
%  NewCondType     : condition type used (keep eventual unary/binary term, but remove the constants used to declare a domain)
%                    when we use a condition of type attr_leq_coef, unary_term_leq_coef or binary_term_leq_coef we create the
%                    Boolean variable LogCondTightBool to allow one to check that the equality is reached for at least one
%                    entry of the table
%                    when we use a condition of type attr_in_interval we create four Boolean variables LogCondTightBool1
%                    LogCondTightBool2, LogCondTightBool3, LogCondTightBool4 to check that:
%                     . the interval start is reached          for  at least one entry
%                     . the interval end   is reached          for  at least one entry
%                     . the interval start is strictly less    than at least one entry
%                     . the interval end   is strictly greater than at least one entry
%  LCondAttr       : target attribute variables indicating which attribute the condition uses (indexes of the used attribute within MandatoryAttr)
%  LCondExtraVars  : extra target variables used by the condition (label first on the attribute variables)
%  LCondCostVars   : cost variables for computing the cost of the cond part (absolute value of constant used in unary term of the condition)
%  CondAttr1       : index of the first attribute used by the condition (between 1 and LenA)
%  CondAttr2       : index of the eventual second attribute used by the condition (0 if unused, between 1 and LenA otherwise)
%  CondAttr3       : index of the eventual third  attribute used by the condition (0 if unused, between 1 and LenA otherwise)
%  CondCst         : constant associated with the condition (0 if unused)
%  CondCoef        : coefficient associated with the condition (0 if unused)
%  SourceVarsLocal : source variables that are specific for each table entry (we use the convention that the first source variable corresponds
%                    to the result of the condition and the last sources variables correspond to the entries of the table, i.e. LogAttributes)
%  SourceVarsGlobal: source variables that are the same for each table entry
%  TargetVarsGlobal: target variables that are the same for each table entry
%  SourceCtrs      : list of constraints that will be posted on each table entry
%  CondCheckOrder  : set to the position of the LogCondOrder variable in the list SourceVarsLocal if will have to check that both arguments of
%                    a min or max unary function contribute after posting all the constraints on all table entries, set to 0 if no check to do
%
formula_generator_condition(LenA, MandatoryAttr, ColSetsBool, Cond, NewCondType, LCondAttr, LCondExtraVars, LCondCostVars, CondAttr1, CondAttr2, CondAttr3,
                            CondCst, CondCoef, SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs, CondCheckOrder) :-
    (compound(Cond) -> true ;
     atom(Cond)     -> true ; write(formula_generator_condition1), nl, halt),                % fail if Cond is not an atom or a term
    functor(Cond, CondType, CondArity),                                                      % get condition type and number of parameters
    length(LogAttributes, LenA),                                                             % create a list of logical variables for the table entry

    % cond( attr_eq_attr): CondAttr1 =  CondAttr2
    % cond(attr_leq_attr): CondAttr1 =< CondAttr2
    %.....................................................................................................................................
    ((memberchk(CondType, [attr_eq_attr,attr_leq_attr]), CondArity = 0) ->                   % COMPARING TWO ATTRIBUTES
        LCondAttr      = [CondAttr1,CondAttr2],                                              % list of index of attribute variables used
        CondAttr3      = 0,                                                                  % no third attribute (as just use two attributes)
        LCondExtraVars = [],                                                                 % list of additional variables
        domain(LCondAttr, 1, LenA),                                                          % variables of the first and second attributes
        CondCoef = 0,                                                                        % no coefficient (as just use two attributes)
        CondCst  = 0,                                                                        % no constant    (as just use two attributes)

        % REMARK: as soon as CondAttr1 will be fixed LogAttr1 will be fixed (LogAttributes correspond to fix values of a row of a table)
        %         as soon as CondAttr2 will be fixed LogAttr2 will be fixed for the same reason
        %.................................................................................................................................
        Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),                               % constraint for getting value of Attr1
        Ctr2 = element(LogCondAttr2, LogAttributes, LogAttr2),                               % constraint for getting value of Attr2
        (CondType = attr_eq_attr ->                                                          % break symmetry as can permutte
            CondAttr1 #< CondAttr2,                                                          % variables in an equality
            ForbiddenCmps = [eq,lt,gt,neq],                                                  % if use '=' then should NOT ALLWAYS BE
            Ctr3 = (LogCondBool #<=> (LogAttr1 #= LogAttr2))                                 % true ('=') or false ('<','>','<>')
        ;                                                                                    % when use two attributes in a condition
            CondAttr1 #\= CondAttr2,                                                         % these attributes should be distinct
            ForbiddenCmps = [leq,eq,lt,gt],                                                  % if use '=<' then should NOT ALLWAYS BE
            Ctr3 = (LogCondBool #<=> (LogAttr1 #=< LogAttr2))                                % true ('=<','=','<') or false ('>')
        ),
        gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, ForbiddenCmps, 0, LCondAttr),
        append([LogCondBool,LogAttr1,LogAttr2], LogAttributes, SourceVarsLocal),
        SourceVarsGlobal = [LogCondAttr1,LogCondAttr2],
        TargetVarsGlobal = [CondAttr1,CondAttr2],
        SourceCtrs       = [Ctr1,Ctr2,Ctr3],                                                 % list of constraints to post on each table entry
        NewCondType      = CondType,
        LCondCostVars    = [],
        CondCheckOrder   = 0
    ;

    % cond( attr_eq_coef(coef(CondCoefMin, CondCoefMax))): CondAttr1 =  CondCoef
    % cond(attr_leq_coef(coef(CondCoefMin, CondCoefMax))): CondAttr1 =< CondCoef
    %.....................................................................................................................................
     (memberchk(CondType, [attr_eq_coef,attr_leq_coef]), CondArity = 1) ->                   % COMPARING AN ATTRIBUTE WITH A COEFFICIENT
        ((arg(1, Cond, coef(CondCoefMin,CondCoefMax)),                                       % get range of coefficient
          integer(CondCoefMin), integer(CondCoefMax), CondCoefMin =< CondCoefMax) ->         % check that we have a proper interval
            CondCoef in CondCoefMin..CondCoefMax                                             % create coefficient variable
        ;
            write(formula_generator_condition2), nl, halt
        ),
        LCondAttr      = [CondAttr1],                                                        % list of index of attribute variables used
        LCondExtraVars = [CondCoef],                                                         % list of additional variables
        CondAttr1 in 1..LenA,                                                                % variable of the first attribute
        CondAttr2 = 0,                                                                       % no second attribute (as just use one attribute)
        CondAttr3 = 0,                                                                       % no third  attribute (as just use one attribute)
        CondCst   = 0,                                                                       % no constant
        get_set_of_values_for_each_mandatory_attribute(MandatoryAttr, 0, allequal, 0,
                                                       CondType, ColSetsBool,
                                                       CondCoefMin, CondCoefMax,
                                                       [_|CoefValuesList]),                  % first element of the output list is only useful
                                                                                             % for Boolean formulae search
        table([[CondAttr1,CondCoef]],CoefValuesList),                                        % restrict to compatible pairs
        Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),                               % constraint for getting value of Attr1
        (CondType = attr_eq_coef ->                                                          % if use '=' then eliminate attributes
            forbid_cols_that_cant_intersect_min_max(LenA, MandatoryAttr,                     % which cannot be in CondCoefMin..CondCoefMax
                                                    CondCoefMin, CondCoefMax, CondAttr1),    % (as condition would BE ALLWAYS FALSE)
            Ctr2 = (LogCondBool #<=> (LogAttr1 #= LogCondCoef)),
            SourceCtrs = [Ctr1,Ctr2],                                                        % list of constraints to post on each table entry
            append([LogCondBool,LogAttr1], LogAttributes, SourceVarsLocal)
        ;                                                                                    % if use '=<' then eliminate attributes
            forbid_cols_where_min_gt_up_or_max_leq_low(LenA, MandatoryAttr,                  % which are less than or equal to
                                                       CondCoefMin, CondCoefMax, CondAttr1), % CondCoefMin (as condition would BE ALLWAYS
            Ctr2 = (LogCondBool      #<=> (LogAttr1 #=< LogCondCoef)),                       % TRUE), or strictly greater than CondCoefMax
                                                                                             % (as condition would BE ALLWAYS FALSE)
            %Ctr3 = (LogCondTightBool #<=> (LogAttr1 #=  LogCondCoef)),                      % used to enforce, that among all entries for which
                                                                                             % the condition leq hold, we have an equality for
                                                                                             % at least on entry
            SourceCtrs = [Ctr1,Ctr2],%Ctr3],                                                   % list of constraints to post on each table entry
            %append([LogCondBool,LogCondTightBool,LogAttr1], LogAttributes, SourceVarsLocal)
            append([LogCondBool,LogAttr1], LogAttributes, SourceVarsLocal)
        ),
        SourceVarsGlobal = [LogCondAttr1,LogCondCoef],
        TargetVarsGlobal = [CondAttr1,CondCoef],
        NewCondType      = CondType,
        LCondCostVars    = [],
        CondCheckOrder   = 0
    ;

    % cond(attr_in_interval(MinInterval,MaxInterval)): CondAttr1 in [CondCst,CondCoef]
    %.....................................................................................................................................
     (CondType = attr_in_interval, CondArity = 2) ->
        arg(1, Cond, MinInterval),                                                           % get minimum value for the start of the interval
        arg(2, Cond, MaxInterval),                                                           % get maximum value for the end of the interval
        ((integer(MinInterval),
          integer(MaxInterval),
          MinInterval < MaxInterval) ->
            CondCst  in MinInterval..MaxInterval,                                            % create variable corresponding to interval start
            CondCoef in MinInterval..MaxInterval,                                            % create variable corresponding to interval end
            CondCst #< CondCoef,                                                             % interval start strictly before interval end
                                                                                             % (strictly as otherwise this would be an equality)
            LCondAttr      = [CondAttr1],                                                    % list of index of attribute variables used
            LCondExtraVars = [CondCst,CondCoef],                                             % list of additional variables
            CondAttr1 in 1..LenA,                                                            % variable of the first attribute
            CondAttr2 = 0,                                                                   % no second attribute
            CondAttr3 = 0,                                                                   % no third  attribute
            get_set_of_values_for_each_mandatory_attribute(MandatoryAttr, 0, allequal, 0,
                                                           attr_leq_coef, ColSetsBool,
                                                           MinInterval, MaxInterval,
                                                           [_|CoefValuesList]),              % first element of the output list is only useful
                                                                                             % for Boolean formulae search
            table([[CondAttr1,CondCst]], CoefValuesList),                                    % restrict to compatible pairs
            table([[CondAttr1,CondCoef]],CoefValuesList),                                    % restrict to compatible pairs
            Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),                           % constraint for getting value of Attr1
            %Ctr2 = (LogCondTightBool1 #<=> (LogAttr1 #= LogCondCst )),                      % at least one entry = interval start
            %Ctr3 = (LogCondTightBool2 #<=> (LogAttr1 #= LogCondCoef)),                      % at least one entry = interval end
            %Ctr4 = (LogCondTightBool3 #<=> (LogAttr1 #< LogCondCst )),                      % at least one entry < interval start
            %Ctr5 = (LogCondTightBool4 #<=> (LogAttr1 #> LogCondCoef)),                      % at least one entry > interval end            
            Ctr6 = (LogCondBool #<=> (LogCondCst #=< LogAttr1 #/\ LogAttr1 #=< LogCondCoef)),% the main constraint
            %SourceCtrs = [Ctr1,Ctr2,Ctr3,Ctr4,Ctr5,Ctr6],                                   % put together all constraints
            SourceCtrs = [Ctr1,Ctr6],                                                        % put together all constraints
            append([LogCondBool,/*LogCondTightBool1,LogCondTightBool2,                       % put together all local source variables
                                LogCondTightBool3,LogCondTightBool4,*/LogAttr1], LogAttributes, SourceVarsLocal),
            SourceVarsGlobal = [LogCondAttr1,LogCondCst,LogCondCoef],                        % put together all global source variables
            TargetVarsGlobal = [CondAttr1,CondCst,CondCoef],                                 % put together all global target variables
            NewCondType      = CondType,                                                     % type is just attr_in_interval
            LCondCostVars    = [CondCoef],                                                   % cost is the upper bound of the interval
            CondCheckOrder   = 0
        ;
            write(formula_generator_condition3), nl, halt
        )
    ;

    % cond(attr_leq_unary([min(CondCstMin,CondCstMax),...])): CondAttr1 =< CondU(CondAttr2,CondCst)
    % cond(unary_leq_attr([min(CondCstMin,CondCstMax),...])): CondU(CondAttr1,CondCst) =< CondAttr2
    %.....................................................................................................................................
     (memberchk(CondType,[attr_leq_unary,                                                    % COMPARING AN ATTRIBUTE AND A UNARY TERM, OR
                          unary_leq_attr]), CondArity = 1) ->                                %           A UNARY TERM AND AN ATTRIBUTE
        arg(1, Cond, LCondUnary),                                                            % get list of possible unary terms
        LCondAttr      = [CondAttr1,CondAttr2],                                              % list of index of attribute variables used
        CondAttr3      = 0,                                                                  % no third attribute (as just use two attributes)
        LCondExtraVars = [CondCst],                                                          % list of additional variables
        domain(LCondAttr, 1, LenA),                                                          % variables of the first and second attributes
        CondAttr1 #\= CondAttr2,                                                             % attributes should be distinct
        (CondType = attr_leq_unary ->
            ForbiddenCmps = [leq,eq,lt]                                                      % should NOT ALLWAYS BE true ('=<','=','<')
        ;
            ForbiddenCmps = [geq,eq,gt]                                                      % should NOT ALLWAYS BE true ('>=','=','>')
        ),
        gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, ForbiddenCmps, 0, LCondAttr),
        CondCoef = 0,                                                                        % no coefficient (as compare two attributes)
        (is_list(LCondUnary) ->                                                              % check that we have a list
            member(CondUnary, LCondUnary),                                                   % TRY OUT ALL THE POSSIBLE UNARY TERMS
            ((compound(CondUnary),                                                           % we should have a term
              functor(CondUnary, CondU, 2),                                                  % get unary term
              memberchk(CondU, [prod]),                                                      % we just use the prod unary term
              arg(1, CondUnary, CondCstMin),                                                 % get min value of constant used in unary term
              arg(2, CondUnary, CondCstMax),                                                 % get max value of constant used in unary term
              integer(CondCstMin), integer(CondCstMax), CondCstMin =< CondCstMax) ->         % check that we have a proper interval
                CondCst in CondCstMin..CondCstMax,                                           % create constant
                CondCst #> 1,
                get_set_of_unaryf_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0, CondType, prod,
                                                                               ColSetsBool, CondCstMin, CondCstMax,
                                                                               [_|CstValuesList]),
                                                                                             % first element of the output list is only useful
                                                                                             % for Boolean formulae search
                CstValuesList = [_|_],
                table([[CondAttr1,CondAttr2,CondCst]],CstValuesList),                        % restrict to compatible triples
                Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),                       % constraint for getting value of Attr1
                Ctr2 = element(LogCondAttr2, LogAttributes, LogAttr2),                       % constraint for getting value of Attr2
                (CondType = attr_leq_unary ->
                    Ctr3 = (LogCondBool      #<=> (LogAttr1 #=< LogAttr2 * LogCondCst)),
                    %Ctr4 = (LogCondTightBool #<=> (LogAttr1 #=  LogAttr2 * LogCondCst)),
                    NewCondType = attr_leq_unary(prod)
                ;
                    Ctr3 = (LogCondBool      #<=> (LogAttr1 * LogCondCst #=< LogAttr2)),
                    %Ctr4 = (LogCondTightBool #<=> (LogAttr1 * LogCondCst #=  LogAttr2)),
                    NewCondType = unary_leq_attr(prod)
                ),
                append([LogCondBool,/*LogCondTightBool,*/LogAttr1,LogAttr2], LogAttributes, SourceVarsLocal),
                SourceCtrs = [Ctr1,Ctr2,Ctr3/*,Ctr4*/],
                CondCheckOrder = 0,
                SourceVarsGlobal = [LogCondAttr1, LogCondAttr2, LogCondCst],
                TargetVarsGlobal = [   CondAttr1,    CondAttr2,    CondCst],
                LCondCostVars    = []
            ;
                write(formula_generator_condition4), nl, halt
            )
        ;
            write(formula_generator_condition5), nl, halt
        )
    ;

    % cond( unary_term_eq_coef([min(CondCstMin,CondCstMax),...], coef(CondCoefMin,CondCoefMax))): CondU(Attr1,CondCst) =  CondCoef
    % cond(unary_term_leq_coef([min(CondCstMin,CondCstMax),...], coef(CondCoefMin,CondCoefMax))): CondU(Attr1,CondCst) =< CondCoef
    %.....................................................................................................................................
     (memberchk(CondType,[unary_term_eq_coef,unary_term_leq_coef]), CondArity = 2) ->        % COMPARING A UNARY TERM WITH A COEFFICIENT
        arg(1, Cond, LCondUnary),                                                            % get list of possible unary terms
        ((arg(2, Cond, coef(CondCoefMin,CondCoefMax)),                                       % get range of coefficient
          integer(CondCoefMin), integer(CondCoefMax), CondCoefMin =< CondCoefMax) ->         % check that we have a proper interval
            CondCoef in CondCoefMin..CondCoefMax                                             % create coefficient variable
        ;
            write(formula_generator_condition6), nl, halt
        ),
        LCondAttr      = [CondAttr1],                                                        % list of index of attribute variables used
        LCondExtraVars = [CondCoef,CondCst],                                                 % list of additional variables
        CondAttr1 in 1..LenA,                                                                % variable of the first attribute
        CondAttr2 = 0,                                                                       % no second attribute (as just use one attribute)
        CondAttr3 = 0,                                                                       % no third  attribute (as just use one attribute)
        (is_list(LCondUnary) ->                                                              % check that we have a list
            member(CondUnary, LCondUnary),                                                   % TRY OUT ALL THE POSSIBLE UNARY TERMS
            ((compound(CondUnary),                                                           % we should have a term
              functor(CondUnary, CondU, 2),                                                  % get unary term
              CondU = mod,                                                                   % allow only mod, as other functions can be reformulated
              arg(1, CondUnary, CondCstMin),                                                 % get min value of constant used in unary term
              arg(2, CondUnary, CondCstMax),                                                 % get max value of constant used in unary term
              integer(CondCstMin), integer(CondCstMax), CondCstMin =< CondCstMax) ->         % check that we have a proper interval
                CondCst in CondCstMin..CondCstMax,                                           % create constant
                tab_get_mins_attr_names(MandatoryAttr, MinAttrs),                            % get list of minimum values of the mandatory attributes
                tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs),                            % get list of maximum values of the mandatory attributes
                element(CondAttr1, MinAttrs, MinAttr1),                                      % get smallest value of attribute CondAttr1
                element(CondAttr1, MaxAttrs, MaxAttr1),                                      % get largest  value of attribute CondAttr1
                CondCst #> 1,
                CondCst #=< MaxAttr1 - MinAttr1,                                             % force that at least one value occurs twice in
                get_set_of_unary_term_values_for_each_mandatory_attribute(MandatoryAttr, 0,
                                                                          allequal, 0,
                                                                          CondType, CondUnary,
                                                                          ColSetsBool,
                                                                          CondCstMin, CondCstMax,
                                                                          CondCoefMin, CondCoefMax,
                                                                          [_|CoefValuesList]),
                                                                                             % first element of the output list is only useful
                                                                                             % for Boolean formulae search
                table([[CondAttr1,CondCst,CondCoef]],CoefValuesList),                        % restrict compatible triples
                Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),                       % constraint for getting value of Attr1
                (CondType = unary_term_eq_coef ->
                    Ctr2 = (LogCondBool #<=> ((LogAttr1 mod LogCondCst) #=  LogCondCoef)),
                    append([LogCondBool,LogAttr1], LogAttributes, SourceVarsLocal),
                    SourceCtrs = [Ctr1,Ctr2],
                    NewCondType = unary_term_eq_coef(mod)
                ;
                    Ctr2 = (LogCondBool      #<=> ((LogAttr1 mod LogCondCst) #=< LogCondCoef)),
                    %Ctr3 = (LogCondTightBool #<=> ((LogAttr1 mod LogCondCst) #=  LogCondCoef)),
                    append([LogCondBool,/*LogCondTightBool,*/LogAttr1], LogAttributes, SourceVarsLocal),
                    SourceCtrs = [Ctr1,Ctr2/*,Ctr3*/],
                    NewCondType = unary_term_leq_coef(mod)
                ),
                CondCheckOrder = 0,
                SourceVarsGlobal = [LogCondAttr1,LogCondCst,LogCondCoef],
                TargetVarsGlobal = [CondAttr1,CondCst,CondCoef],
                LCondCostVars    = [CondCst]
            ;
                write(formula_generator_condition7), nl, halt
            )
        ;
            write(formula_generator_condition8), nl, halt
        )
    ;

    % cond( binary_term_eq_coef([min,...], coef(CondCoefMin,CondCoefMax)): CondB(CondAttr1,CondAttr2) =  CondCoef
    % cond(binary_term_leq_coef([min,...], coef(CondCoefMin,CondCoefMax)): CondB(CondAttr1,CondAttr2) =< CondCoef
    %.....................................................................................................................................
     (memberchk(CondType,[binary_term_eq_coef,binary_term_leq_coef]), CondArity = 2) ->      % COMPARING A BINARY TERM WITH A COEFFICIENT
        arg(1, Cond, LCondBinary),                                                           % get list of possible binary terms
        ((arg(2, Cond, coef(CondCoefMin,CondCoefMax)),                                       % get range of coefficient
          integer(CondCoefMin), integer(CondCoefMax), CondCoefMin =< CondCoefMax) ->         % check that we have a proper interval
            CondCoef in CondCoefMin..CondCoefMax                                             % create coefficient variable
        ;
            write(formula_generator_condition9), nl, halt
        ),         
        LCondAttr      = [CondAttr1,CondAttr2],                                              % list of index of attribute variables used
        CondAttr3      = 0,                                                                  % no third  attribute (as just use two attributes)
        LCondExtraVars = [CondCoef],                                                         % list of additional variables
        domain(LCondAttr, 1, LenA),                                                          % variables of the first and second attributes
        CondCst = 0,                                                                         % no constant
        (is_list(LCondBinary) ->                                                             % check that we have a list
            member(CondB, LCondBinary),                                                      % TRY OUT ALL THE POSSIBLE BINARY TERMS
            ((all_options(binary, BinaryFunctions),                                          % get the set of possible binary functions
              memberchk(CondB, BinaryFunctions)) ->                                          % we should have a binary term
                (memberchk(CondB, [plus,abs,min,max,prod]) ->                                % if use a commutative binary term
                    CondAttr1 #<  CondAttr2                                                  % break symmetry as can permutte attributes
                ;                                                                            % if does not use a commutative binary term
                    CondAttr1 #\= CondAttr2                                                  % attributes should be distinct
                ),
                (memberchk(CondB, [floor,mfloor,ceil,mod,dmod,fmod]) ->                      % min value of Attr2 should be > 0
                    forbid_cols_which_can_be_leq0(LenA, MandatoryAttr, 0, CondAttr2)
                ;
                 CondB = cmod ->                                                             % min value of Attr1 should be > 0
                    forbid_cols_which_can_be_leq0(LenA, MandatoryAttr, 0, CondAttr1)
                ;
                    true                                                                     % otherwise no restriction
                ),
                Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),
                Ctr2 = element(LogCondAttr2, LogAttributes, LogAttr2),
                (CondB = plus ->
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, plus,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                             % first element of the output list is only useful
                                                                                             % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> (LogAttr1+LogAttr2 #= LogCondCoef)),
                        NewCondType = binary_term_eq_coef(plus)
                    ;
                        Ctr3 = (LogCondBool      #<=> (LogAttr1+LogAttr2 #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> (LogAttr1+LogAttr2 #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(plus)
                    )
                ;
                 CondB = minus ->
                    gen_table_ctr_wrt_authorised_cmp(LenA, 2, MandatoryAttr, [geq,gt], 0, LCondAttr), % arg1 should always be geq or gt than arg2
                    CondCoef #> 0,                                                                 % as want a positive result
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, minus,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search



                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> (LogAttr1-LogAttr2 #= LogCondCoef)),
                        NewCondType = binary_term_eq_coef(minus)
                    ;
                        Ctr3 = (LogCondBool      #<=> (LogAttr1-LogAttr2 #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> (LogAttr1-LogAttr2 #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(minus)
                    )
                ;
                 CondB = abs ->
                    CondCoef #> 0,                                                                          % |x1-x2| = 0 is equivalent to x1=x2
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt,eq,geq,gt], 0, LCondAttr), % no order between the two attributes
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, abs,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> (abs(LogAttr1-LogAttr2) #= LogCondCoef)),
                        NewCondType = binary_term_eq_coef(minus)
                    ;
                        Ctr3 = (LogCondBool      #<=> (abs(LogAttr1-LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> (abs(LogAttr1-LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(abs)
                    )
                ;
                 CondB = min ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr1, MinAttrs, MinAttr1),
                    element(CondAttr2, MinAttrs, MinAttr2),
                    element(CondAttr1, MaxAttrs, MaxAttr1),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt,eq,geq,gt], 0, LCondAttr), % no order between the two attributes
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, min,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        CondCoef #>= max(MinAttr1,MinAttr2),
                        CondCoef #=< min(MaxAttr1,MaxAttr2),
                        Ctr3 = (LogCondBool #<=> (min(LogAttr1,LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(min)
                    ;
                        CondCoef #>  max(MinAttr1,MinAttr2),
                        CondCoef #<  min(MaxAttr1,MaxAttr2),
                        Ctr3 = (LogCondBool      #<=> (min(LogAttr1,LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> (min(LogAttr1,LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(min)
                    )
                ;
                 CondB = max ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr1, MinAttrs, MinAttr1),
                    element(CondAttr2, MinAttrs, MinAttr2),
                    element(CondAttr1, MaxAttrs, MaxAttr1),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt,eq,geq,gt], 0, LCondAttr), % no order between the two attributes
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, max,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        CondCoef #>= max(MinAttr1,MinAttr2),
                        CondCoef #=< min(MaxAttr1,MaxAttr2),
                        Ctr3 = (LogCondBool #<=> (max(LogAttr1,LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(max)
                    ;
                        CondCoef #>  max(MinAttr1,MinAttr2),
                        CondCoef #<  min(MaxAttr1,MaxAttr2),
                        Ctr3 = (LogCondBool      #<=> (max(LogAttr1,LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> (max(LogAttr1,LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(max)
                    )
                ;
                 CondB = floor ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr2, MinAttrs, MinAttr2),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    (MinAttr2 #> 0 #\/ MaxAttr2 #< 0),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 0, LCondAttr), % num should not always be leq,lt than den
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, floor,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(floor)
                    ;
                        Ctr3 = (LogCondBool      #<=> ((LogAttr1 div LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> ((LogAttr1 div LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(floor)
                    )
                ;
                 CondB = mfloor ->
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 0, LCondAttr), % num should not always be leq,lt than den
                                                                                                  % (in theory should not be forbidden)
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr2, MinAttrs, MinAttr2),
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, mfloor,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 div max(LogAttr2-LogMinAttr2,1)) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(mfloor,MinAttr2)
                    ;
                        Ctr3 = (LogCondBool      #<=> ((LogAttr1 div max(LogAttr2-LogMinAttr2,1)) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> ((LogAttr1 div max(LogAttr2-LogMinAttr2,1)) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(mfloor,MinAttr2)
                    )
                ;
                 CondB = mod ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr2, MinAttrs, MinAttr2),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    (MinAttr2 #> 0 #\/ MaxAttr2 #< 0),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 0, LCondAttr), % arg1 should not always be leq,lt than arg2
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, mod,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 mod LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(mod)
                    ;
                        Ctr3 = (LogCondBool      #<=> ((LogAttr1 mod LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> ((LogAttr1 mod LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(mod)
                    )
                ;
                 CondB = prod ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr1, MinAttrs, MinAttr1),
                    element(CondAttr2, MinAttrs, MinAttr2),
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr1, MaxAttrs, MaxAttr1),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    LogCondCoef #= 0 #=> (MinAttr1 #=< 0 #/\ MinAttr2 #=< 0 #/\ MaxAttr1 #>= 0 #/\ MaxAttr2 #>= 0),
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, prod,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 * LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(prod)
                    ;
                        Ctr3 = (LogCondBool      #<=> ((LogAttr1 * LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> ((LogAttr1 * LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(prod)
                    )
                ;
                 CondB = ceil ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr2, MinAttrs, MinAttr2),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    (MinAttr2 #> 0 #\/ MaxAttr2 #< 0),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 0, LCondAttr), % num should not always be leq,lt than den
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, ceil,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> (((LogAttr1+LogAttr2-1) div LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(ceil)
                    ;
                        Ctr3 = (LogCondBool      #<=> (((LogAttr1+LogAttr2-1) div LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> (((LogAttr1+LogAttr2-1) div LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(ceil)
                    )
                ;
                 CondB = cmod ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr1, MinAttrs, MinAttr1),
                    element(CondAttr1, MaxAttrs, MaxAttr1),
                    (MinAttr1 #> 0 #\/ MaxAttr1 #< 0),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [geq,gt], 0, LCondAttr), % arg1 should not allways be gt,geq than arg2
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, cmod,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr2 mod LogAttr1)) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(cmod)
                    ;
                        Ctr3 = (LogCondBool      #<=> ((LogAttr1 - (LogAttr2 mod LogAttr1)) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> ((LogAttr1 - (LogAttr2 mod LogAttr1)) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(cmod)
                    )
                ;
                 CondB = dmod ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr2, MinAttrs, MinAttr2),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    (MinAttr2 #> 0 #\/ MaxAttr2 #< 0),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 0, LCondAttr), % arg1 should not always be leq,lt than arg2
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, dmod,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 - (LogAttr1 mod LogAttr2)) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(dmod)
                    ;
                        Ctr3 = (LogCondBool      #<=> ((LogAttr1 - (LogAttr1 mod LogAttr2)) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> ((LogAttr1 - (LogAttr1 mod LogAttr2)) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(dmod)
                    )
                ;
                 CondB = fmod ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(CondAttr2, MinAttrs, MinAttr2),
                    element(CondAttr2, MaxAttrs, MaxAttr2),
                    (MinAttr2 #> 0 #\/ MaxAttr2 #< 0),
                    gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 0, LCondAttr), % arg1 should not always be leq,lt than arg2
                    get_set_of_binary_term_values_for_each_pair_of_mandatory_attributes(MandatoryAttr, 0,
                                                                                        allequal, 0,
                                                                                        CondType, fmod,
                                                                                        ColSetsBool,
                                                                                        CondCoefMin, CondCoefMax,
                                                                                        [_|ValuesAttrPairs]),
                                                                                            % first element of the output list is only useful
                                                                                            % for Boolean formulae search
                    ValuesAttrPairs = [_|_],
                    table([[CondAttr1,CondAttr2,CondCoef]],ValuesAttrPairs),
                    (CondType = binary_term_eq_coef ->
                        Ctr3 = (LogCondBool #<=> ((LogAttr1 mod LogAttr2 #= 0)*LogAttr2 +
                                                  (LogAttr1 mod LogAttr2 #> 0)*(LogAttr1 mod LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_eq_coef(fmod)
                    ;
                        Ctr3 = (LogCondBool      #<=> ((LogAttr1 mod LogAttr2 #= 0)*LogAttr2 +
                                                       (LogAttr1 mod LogAttr2 #> 0)*(LogAttr1 mod LogAttr2) #=< LogCondCoef)),
                        %Ctr4 = (LogCondTightBool #<=> ((LogAttr1 mod LogAttr2 #= 0)*LogAttr2 +
                        %                               (LogAttr1 mod LogAttr2 #> 0)*(LogAttr1 mod LogAttr2) #=  LogCondCoef)),
                        NewCondType = binary_term_leq_coef(fmod)
                    )
                ;
                    true
                ),
                (CondB = mfloor ->
                    SourceVarsGlobal = [LogCondAttr1,LogCondAttr2,LogCondCoef,LogMinAttr2],
                    TargetVarsGlobal = [CondAttr1,CondAttr2,CondCoef,MinAttr2]
                ;
                    SourceVarsGlobal = [LogCondAttr1,LogCondAttr2,LogCondCoef],
                    TargetVarsGlobal = [CondAttr1,CondAttr2,CondCoef]
                ),
                (CondType = binary_term_eq_coef ->
                    append([LogCondBool,LogAttr1,LogAttr2], LogAttributes, SourceVarsLocal),
                    SourceCtrs = [Ctr1,Ctr2,Ctr3]
                ;
                    append([LogCondBool,/*LogCondTightBool,*/LogAttr1,LogAttr2], LogAttributes, SourceVarsLocal),
                    SourceCtrs = [Ctr1,Ctr2,Ctr3/*,Ctr4*/]
                ),
                LCondCostVars = [],
                CondCheckOrder = 0
            ;
                write(formula_generator_condition10), nl, halt
            )
        ;
            write(formula_generator_condition11), nl, halt
        )
    ;

    % cond(linear_eq_coef(cst(CondCstMin,CondCstMax), coef(CondCoefMin,CondCoefMax))): CondCst*CondAttr1+CondAttr2+CondAttr3=CondCoef
    %.....................................................................................................................................
     (CondType = linear_eq_coef,                                                             % COMPARING A TERNARY TERM WITH A COEFFICIENT
      functor(Cond, linear_eq_coef, 2),
      arg(1, Cond, cst(CondCstMin,CondCstMax)),
      arg(2, Cond, coef(CondCoefMin,CondCoefMax)),
      integer(CondCstMin),                                                                   % coefficient of the first var. of the linear term
      integer(CondCstMax),
      integer(CondCoefMin),                                                                  % coefficient of the right-hand side of the equality
      integer(CondCoefMax),
      CondCstMin  =< CondCstMax,
      CondCoefMin =< CondCoefMax) ->
        CondCst  in CondCstMin..CondCstMax,                                                  % create coefficient var. of the first var. of the linear term
        CondCoef in CondCoefMin..CondCoefMax,                                                % create coefficient var. of the right-hand side of the equality
        CondCst #\= 0,                                                                       % as otherwise it would cancel out Attr1
        LCondAttr = [CondAttr1,CondAttr2,CondAttr3],                                         % list of index of attribute variables used
        LCondExtraVars = [CondCst,CondCoef],                                                 % list of additional variables
        domain(LCondAttr, 1, LenA),                                                          % variables of the first, second and third attributes
        CondAttr2 #< CondAttr3,                                                              % as Attr2 and Attr3 are symmetric in the condition
        all_distinct(LCondAttr),                                                             % all three attributes should be distinct
        Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),
        Ctr2 = element(LogCondAttr2, LogAttributes, LogAttr2),
        Ctr3 = element(LogCondAttr3, LogAttributes, LogAttr3),
        Ctr4 = (LogCondBool #<=> (LogCondCst*LogAttr1+LogAttr2+LogAttr3) #= LogCondCoef),
        SourceVarsGlobal = [LogCondAttr1,LogCondAttr2,LogCondAttr3,LogCondCst,LogCondCoef],
        TargetVarsGlobal = [   CondAttr1,   CondAttr2,   CondAttr3,   CondCst,   CondCoef],
        append([LogCondBool,    LogAttr1,   LogAttr2,    LogAttr3], LogAttributes, SourceVarsLocal),
        SourceCtrs     = [Ctr1,Ctr2,Ctr3,Ctr4],
        NewCondType    = linear_eq_coef,
        LCondCostVars  = [CondCst,CondCoef],
        CondCheckOrder = 0
    ;

    % cond(sum_leq_attr) : CondAttr1+CondAttr2                 =< CondAttr3
    % cond(minus_mod_eq0): (CondAttr3-CondAttr1) mod CondAttr2 =  0
    %.....................................................................................................................................
     (memberchk(CondType,[sum_leq_attr,minus_mod_eq0,ceil_leq_floor]),
      CondArity = 0                                  ) ->
        LCondAttr      = [CondAttr1,CondAttr2,CondAttr3],                                    % list of index of attribute variables used
        LCondExtraVars = [],                                                                 % no additional variables
        domain(LCondAttr, 1, LenA),                                                          % vars.of first, second and third attributes
        CondCoef = 0,                                                                        % no coefficient (as just use three attributes)
        CondCst  = 0,                                                                        % no constant
        % the table constraint posted by gen_table_ctr_wrt_authorised_cmp forces all three attributes to be distinct
        (CondType = ceil_leq_floor ->
             gen_table_ctr_wrt_authorised_cmp(LenA, 3,                                       % condition not used (arg1=arg2=arg3=1) or
                                              MandatoryAttr,                                 % if condition is used then:
                                              [geq,gt],                                      %  . arg1 should always be geq or gt than arg2
                                              1,                                             %  . arg2 should always be geq or gt than arg3
                                              LCondAttr)                                     % 1 as shift index by 1
        ;
             gen_table_ctr_wrt_authorised_cmp(LenA, 3,                                       % condition not used (arg1=arg2=arg3=1) or
                                              MandatoryAttr,                                 % if condition is used then:
                                              [leq,lt],                                      %  . arg1 should always be leq or lt than arg2
                                              1,                                             %  . arg2 should always be leq or lt than arg3
                                              LCondAttr)                                     % 1 as shift index by 1
        ),
        get_set_of_ternary_triplets_of_mandatory_attributes(MandatoryAttr, 0, CondType, ColSetsBool, TripletsList),
        table([LCondAttr],TripletsList),
        Ctr1 = element(LogCondAttr1, LogAttributes, LogAttr1),
        Ctr2 = element(LogCondAttr2, LogAttributes, LogAttr2),
        Ctr3 = element(LogCondAttr3, LogAttributes, LogAttr3),
        (CondType = sum_leq_attr ->
            Ctr4 = (LogCondBool #<=> (LogAttr1+LogAttr2) #=< LogAttr3)
        ;
         CondType = minus_mod_eq0 ->
            tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of min. and max. values of the mandatory attr.
            tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % as well as list of index+1 of Boolean attributes
            element(CondAttr2, [0|MinAttrs], MinAttr2),
            element(CondAttr2, [0|MaxAttrs], MaxAttr2),
            (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0),
            Ctr4 = (LogCondBool #<=> ((LogAttr3-LogAttr1) mod LogAttr2 #= 0))
        ;
            tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of min. and max. values of the mandatory attr.
            tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % as well as list of index+1 of Boolean attributes
            element(CondAttr2, [0|MinAttrs], MinAttr2),
            element(CondAttr2, [0|MaxAttrs], MaxAttr2),
            (MinAttr2 #=< 0 #/\ MaxAttr2 #>= 0),
            element(CondAttr3, [0|MinAttrs], MinAttr3),
            element(CondAttr3, [0|MaxAttrs], MaxAttr3),
            (MinAttr3 #=< 0 #/\ MaxAttr3 #>= 0),
            Ctr4 = (LogCondBool #<=> (((LogAttr1 - LogAttr3 + LogAttr2 - 1) / LogAttr2) #=< ((LogAttr1 - LogAttr2)/ LogAttr3)))
        ),
        SourceVarsGlobal = [LogCondAttr1,LogCondAttr2,LogCondAttr3],
        TargetVarsGlobal = [   CondAttr1,   CondAttr2,   CondAttr3],
        append([LogCondBool,    LogAttr1,   LogAttr2,    LogAttr3], LogAttributes, SourceVarsLocal),
        SourceCtrs     = [Ctr1,Ctr2,Ctr3,Ctr4],
        NewCondType    = CondType,
        LCondCostVars  = [],
        CondCheckOrder = 0
    ;

        write(formula_generator_condition12), nl, halt
    ).

% From the options describing a then or an else, generate the corresponding then or else (its constraints, source variables).
%
% INPUT PARAMETERS
%  LenA           : number of mandatory attributes (note that we only have mandatory attributes)
%  MandatoryAttr  : list of mandatory attributes
%  Then           : term representing the options of the then or the else
%
% OUTPUT PARAMETERS
%  NewThenType     : then or else type used (keep eventual unary/binary term, but remove the constants used to declare a domain)
%  LThenAttr       : target attribute variables indicating which attribute the then or the else uses
%  LThenExtraVars  : extra target variables used by the then or the else (label first on the attribute variables)
%  LThenCostVars   : cost variables for computing the cost of the then part
%  ThenAttr1       : index of the first attribute used by the then or by the else (between 1 and LenA)
%  ThenAttr2       : index of the eventual second attribute used by the the then or the else (0 if unused, between 1 and LenA otherwise)
%  ThenCst         : constant associated with the then or the else (0 if unused)
%  ThenCoef        : coefficient associated with the then or the else (0 if unused)
%  SourceVarsLocal : source variables that are specific for each table entry (we use the convention that the first source variable corresponds
%                    to the result of the then/else and the last sources variables correspond to the entries of the table, i.e. LogAttributes)
%  SourceVarsGlobal: source variables that are the same for each table entry
%  TargetVarsGlobal: target variables that are the same for each table entry
%  SourceCtrs      : list of constraints that will be posted on each table entry
%  ThenCheckOrder  : set to the position of the LogThenOrder variable in the list SourceVarsLocal if will have to check that both arguments of
%                    a min or max unary function contribute after posting all the constraints on all table entries, set to 0 if no check to do
%
formula_generator_then_else(LenA, MandatoryAttr, Then, NewThenType, LThenAttr, LThenExtraVars, LThenCostVars, ThenAttr1, ThenAttr2, 
                            ThenCst, ThenCoef, SourceVarsLocal, SourceVarsGlobal, TargetVarsGlobal, SourceCtrs, ThenCheckOrder) :-
    (compound(Then) -> true ;
     atom(Then)     -> true ; write(formula_generator_then_else1), nl, halt),        % fail if Then is not a term
    functor(Then, ThenType, ThenArity),                                              % get then type and number of parameters
    length(LogAttributes, LenA),                                                     % list of logical vars.for the actual values of the attributes

    % then(coef(ThenCoefMin,ThenCoefMax)):  = ThenCoef
    %.....................................................................................................................................
    ((ThenType = coef, ThenArity = 2) ->                                             % use just a coefficient
        LThenAttr      = [],                                                         % list of index of attribute variables used
        LThenExtraVars = [ThenCoef],                                                 % list of additional variables
        ThenAttr1 = 0,                                                               % no first  attribute (as just use a coef)
        ThenAttr2 = 0,                                                               % no second attribute (as just use a coef)
        ThenCst   = 0,                                                               % no constant         (as just use a coef)
        ((Then = coef(ThenCoefMin,ThenCoefMax),                                      % get range of coefficient
          integer(ThenCoefMin), integer(ThenCoefMax), ThenCoefMin =< ThenCoefMax) -> % check that we have a proper interval
            ThenCoef in ThenCoefMin..ThenCoefMax,                                    % create coefficient variable (same var. for all entries)
            Ctr1 = (LogRes #= LogThenCoef),
            append([LogRes], LogAttributes, SourceVarsLocal),
            SourceVarsGlobal = [LogThenCoef],
            TargetVarsGlobal = [ThenCoef],
            SourceCtrs = [Ctr1],
            NewThenType = ThenType,
            LThenCostVars = [ThenCoef],                                              % cost of the then will be the |ThenCoef|
            ThenCheckOrder = 0
        ;
            write(formula_generator_then_else2), nl, halt
        )
    ;

    % then(attr):  = ThenAttr1
    %.....................................................................................................................................
     (ThenType = attr, ThenArity = 0) ->                                             % use just an attribute
        LThenAttr      = [ThenAttr1],                                                % list of index of attribute variables used
        LThenExtraVars = [],                                                         % list of additional variables
        ThenAttr1 in 1..LenA,                                                        % variable of the first attribute (same var. for all entries)
        ThenAttr2 = 0,                                                               % no second attribute
        ThenCst   = 0,                                                               % no constant    (as just use an attribute)
        ThenCoef  = 0,                                                               % no coefficient (as just use an attribute)
        Ctr1 = element(LogThenAttr1, LogAttributes, LogAttr1),                       % constraint for getting value of Attr1
        append([LogAttr1], LogAttributes, SourceVarsLocal),
        SourceVarsGlobal = [LogThenAttr1],
        TargetVarsGlobal = [ThenAttr1],
        SourceCtrs = [Ctr1],                                                         % list of constraints to post on each table entry
        NewThenType = ThenType,
        LThenCostVars = [1],                                                         % cost of the then will be the +1
        ThenCheckOrder = 0
    ;

    % then(unary_term([min(ThenCstMin,ThenCstMax),...])):  = ThenU(ThenAttr1,ThenCst)
    %.....................................................................................................................................
     (ThenType = unary_term, ThenArity = 1) ->                                       % use a unary term
        LThenAttr      = [ThenAttr1],                                                % list of index of attribute variables used
        LThenExtraVars = [ThenCst],                                                  % list of additional variables
        ThenAttr1 in 1..LenA,                                                        % variable of the first attribute
        ThenAttr2 = 0,                                                               % no second attribute
        ThenCoef  = 0,                                                               % no coefficient (as just use an attribute)
        arg(1, Then, LThenUnary),                                                    % get list of possible unary terms
        (is_list(LThenUnary) ->                                                      % check that we have a list
            member(ThenUnary, LThenUnary),                                           % TRY OUT ALL THE POSSIBLE UNARY TERMS
            ((compound(ThenUnary),                                                   % we should have a term
              functor(ThenUnary, ThenU, 2),                                          % get unary term
              arg(1, ThenUnary, ThenCstMin),                                         % get min value of constant used in unary term
              arg(2, ThenUnary, ThenCstMax),                                         % get max value of constant used in unary term
              integer(ThenCstMin), integer(ThenCstMax), ThenCstMin =< ThenCstMax) -> % check that we have a proper interval
                ThenCst in ThenCstMin..ThenCstMax,                                   % create constant
                (memberchk(ThenU, [prod])                            -> ThenCst #> 1 ;
                 memberchk(ThenU, [plus,minus,floor,ceil,floor_min]) -> ThenCst #> 0 ;
                 memberchk(ThenU, [min,max]) ->
                    tab_get_mins_attr_names(MandatoryAttr,MinAttrs),
                    tab_get_maxs_attr_names(MandatoryAttr,MaxAttrs),
                    element(ThenAttr1, MinAttrs, MinAttr1),
                    element(ThenAttr1, MaxAttrs, MaxAttr1),
                    ThenCst #< MaxAttr1,                                             % if x = 1..10 then min(x,1) <=> 1, min(x,10) <=> x,
                    ThenCst #> MinAttr1                                              %                   max(x,1) <=> x, max(x,10) <=> 10
                ;
                    memberchk(ThenU, [mod]) ->
                    tab_get_mins_attr_names(MandatoryAttr,MinAttrs),
                    tab_get_maxs_attr_names(MandatoryAttr,MaxAttrs),
                    element(ThenAttr1, MinAttrs, MinAttr1),
                    element(ThenAttr1, MaxAttrs, MaxAttr1),
                    ThenCst #>= 2,
                    ThenCst #=< (MaxAttr1 - MinAttr1)                                % otherwise attr mod cst <=> attr
                ;
                    true
                ),
                (memberchk(ThenU, [min_min,max_min]) ->
                    tab_get_mins_attr_names(MandatoryAttr,MinAttrs),
                    tab_get_maxs_attr_names(MandatoryAttr,MaxAttrs),
                    element(ThenAttr1, MinAttrs, MinAttr1),
                    element(ThenAttr1, MaxAttrs, MaxAttr1),
                    ThenCst #< MaxAttr1 - MinAttr1,                                   % if x = 1..10 then min(x-1,0) <=> 0, min(x-1,9) <=> x,
                    ThenCst #> 0                                                      %                   max(x-1,0) <=> x, max(x-1,9) <=> 9
                ;
                    true),
                (memberchk(ThenU, [max_min,floor_min,min_min]) ->
                    tab_get_mins_attr_names(MandatoryAttr,MinAttrs),
                    element(ThenAttr1, MinAttrs, MinAttr1),                          % MinAttr1 is the min.value of the attributes used in unary term
                    MinAttr1 #> 0
                ;
                    true
                ),
                Ctr1 = element(LogThenAttr1, LogAttributes, LogAttr1),               % constraint for getting value of Attr1
                (ThenU = plus      -> Ctr2 = (LogRes #= LogAttr1+LogThenCst)                          ;
                 ThenU = minus     -> Ctr2 = (LogRes #= LogAttr1-LogThenCst)                          ;
                 ThenU = prod      -> Ctr2 = (LogRes #= LogAttr1*LogThenCst)                          ;
                 ThenU = min       -> Ctr2 = (LogRes #= min(LogAttr1,LogThenCst))                     ;
                 ThenU = min_min   -> Ctr2 = (LogRes #= min(LogAttr1-LogMinAttr1,LogThenCst))         ;
                 ThenU = max       -> Ctr2 = (LogRes #= max(LogAttr1,LogThenCst))                     ;
                 ThenU = max_min   -> Ctr2 = (LogRes #= max(LogAttr1-LogMinAttr1,LogThenCst))         ;
                 ThenU = floor     -> Ctr2 = (LogRes #= (LogAttr1 div LogThenCst))                    ;
                 ThenU = floor_min -> Ctr2 = (LogRes #= ((LogAttr1 - LogMinAttr1) div LogThenCst))    ;
                 ThenU = ceil      -> Ctr2 = (LogRes #= ((LogAttr1 + LogThenCst - 1) div LogThenCst)) ;
                 ThenU = mod       -> Ctr2 = (LogRes #= (LogAttr1 mod LogThenCst))                    ;
                    write(formula_generator_then_else3), nl, halt                    % do not accept geq, in, power, sum_consec
                ),
                (memberchk(ThenU, [min,max]) ->
                    Ctr3 = (LogThenOrder #= 0 #<=> LogAttr1 #< LogThenCst),
                    Ctr4 = (LogThenOrder #= 1 #<=> LogAttr1 #= LogThenCst),          % be careful: order of the Log variables matters
                    Ctr5 = (LogThenOrder #= 2 #<=> LogAttr1 #> LogThenCst),          % as it is used in gen_formula when posting ctrs
                    append([LogRes,LogThenOrder,LogAttr1], LogAttributes, SourceVarsLocal),
                    SourceCtrs = [Ctr1,Ctr2,Ctr3,Ctr4,Ctr5],
                    ThenCheckOrder = 2                                               % position of LogThenOrder in list SourceVarsLocal
                ;
                 memberchk(ThenU, [min_min,max_min]) ->
                    Ctr3 = (LogThenOrder #= 0 #<=> LogAttr1-LogMinAttr1 #< LogThenCst),
                    Ctr4 = (LogThenOrder #= 1 #<=> LogAttr1-LogMinAttr1 #= LogThenCst),
                    Ctr5 = (LogThenOrder #= 2 #<=> LogAttr1-LogMinAttr1 #> LogThenCst),
                    append([LogRes,LogThenOrder,LogAttr1], LogAttributes, SourceVarsLocal),
                    SourceCtrs = [Ctr1,Ctr2,Ctr3,Ctr4,Ctr5],
                    ThenCheckOrder = 2                                               % position of LogThenOrder in list SourceVarsLocal
                ;
                    append([LogRes,LogAttr1], LogAttributes, SourceVarsLocal),
                    SourceCtrs = [Ctr1,Ctr2],
                    ThenCheckOrder = 0
                ),
                (memberchk(ThenU, [max_min,min_min,floor_min]) ->
                    SourceVarsGlobal = [LogThenAttr1,LogThenCst,LogMinAttr1],
                    TargetVarsGlobal = [ThenAttr1,ThenCst,MinAttr1],
                    NewThenType = unary_term(ThenU,MinAttr1)
                ;
                    SourceVarsGlobal = [LogThenAttr1,LogThenCst],
                    TargetVarsGlobal = [ThenAttr1,ThenCst],
                    NewThenType = unary_term(ThenU)
                ),
                LThenCostVars = [ThenCst]                                            % cost of the then will be the |ThenCst|
            ;
                write(formula_generator_then_else4), nl, halt
            )
        ;
            write(formula_generator_then_else5), nl, halt
        )
    ;

    % then(binary_term([min,...])):  = ThenB(ThenAttr1,ThenAttr2)
    %.....................................................................................................................................
     (ThenType = binary_term, ThenArity = 1) ->                                      % use a binary term
        LThenAttr = [ThenAttr1,ThenAttr2],                                           % list of index of attribute variables used
        domain(LThenAttr, 1, LenA),                                                  % variables of the first and second attributes
        arg(1, Then,  LThenBinary),                                                  % get list of possible binary terms
        (is_list(LThenBinary) ->                                                     % check that we have a list
            member(ThenB, LThenBinary),                                              % TRY OUT ALL THE POSSIBLE BINARY TERMS
            (memberchk(ThenB, [plus,minus,abs,min,max,prod,floor,mfloor,ceil,mod,cmod,dmod,fmod]) ->
                ThenCst  = 0,                                                        % no constant (as just use two attributes)
                ThenCoef = 0,                                                        % no coefficient (as just use two attributes)
                LThenExtraVars = []                                                  % list of additional variables
            ;
             (functor(ThenB, FThenB, 2),                                             % ThenB = linear(_,_)     or plus_min(_,_) or
              memberchk(FThenB, [linear,plus_min,plus_floor]),                       %         plus_floor(_,_)
              arg(1, ThenB, ThenCstMin),                                             % get min value of constant used in binary term
              arg(2, ThenB, ThenCstMax),                                             % get max value of constant used in binary term
              integer(ThenCstMin), integer(ThenCstMax), ThenCstMin =< ThenCstMax) -> % check that we have a proper interval
                ThenCst in ThenCstMin..ThenCstMax,
                ThenCoef = 0,                                                        % no coefficient (as just use two attributes)
                LThenExtraVars = [ThenCst],                                          % list of additional variables
                (ThenB = linear(_,_) ->
                    ThenCst #\= -1, ThenCst #\= 0, ThenCst #\= 1                     % as otherwise linear would be same as plus or minus
                ;
                 ThenB = plus_min(_,_) ->
                    tab_get_mins_attr_names(MandatoryAttr,MinAttrs),
                    tab_get_maxs_attr_names(MandatoryAttr,MaxAttrs),
                    element(ThenAttr2, MinAttrs, MinAttr2),                          % MinAttr2 is the min.value of attribute ThenAttr2
                    element(ThenAttr2, MaxAttrs, MaxAttr2),                          % MaxAttr2 is the max.value of attribute ThenAttr2
                    ThenCst #> 0,                                                    % as want to decrease Attr1
                    ThenCst #> MinAttr2,                                             % as Attr1+Attr2-ThenCst < Attr1 should be feasible
                    ThenCst #< MaxAttr2                                              % as Attr1+Attr2-ThenCst > Attr1 should be feasible
                ;
                 ThenB = plus_floor(_,_) ->                                          % as otherwise plus_floor would be same as ceil
                    ThenCst #\= -1, ThenCst #\= 0,
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(ThenAttr2, MinAttrs, MinAttr2),
                    element(ThenAttr2, MaxAttrs, MaxAttr2),
                    (MinAttr2 #> 0 #\/ MaxAttr2 #< 0)
                ;
                 memberchk(ThenB, [floor, ceil, mod, dmod, fmod]) ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(ThenAttr2, MinAttrs, MinAttr2),
                    element(ThenAttr2, MaxAttrs, MaxAttr2),
                    (MinAttr2 #> 0 #\/ MaxAttr2 #< 0)
                ;
                 ThenB = cmod ->
                    tab_get_mins_attr_names(MandatoryAttr, MinAttrs), % get list of minimum values of the mandatory attributes
                    tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs), % get list of minimum values of the mandatory attributes
                    element(ThenAttr1, MinAttrs, MinAttr1),
                    element(ThenAttr1, MaxAttrs, MaxAttr1),
                    (MinAttr1 #> 0 #\/ MaxAttr1 #< 0)
                ;
                    true
                )
            ;
             (functor(ThenB, FThenB, 4),                                             % NEW
              FThenB = minus_floor,
              arg(1, ThenB, ThenCstMin),                                             % get min value of constant used in the numerator
              arg(2, ThenB, ThenCstMax),                                             % get max value of constant used in the numerator
              arg(3, ThenB, ThenCoefMin),                                            % get min value of constant used in the denominator
              arg(4, ThenB, ThenCoefMax),                                            % get max value of constant used in the denominator
              integer(ThenCstMin),
              integer(ThenCstMax),
              ThenCstMin =< ThenCstMax,                                              % check that we have a proper interval for ThenCst
              integer(ThenCoefMin),
              integer(ThenCoefMax),
              ThenCoefMin =< ThenCoefMax) ->                                         % check that we have a proper interval for ThenCoef
                ThenCoefMin1 is max(ThenCoefMin,2),
                ThenCst  in   ThenCstMin..ThenCstMax,                                % declare the constant that is added in the numerator
                ThenCoef in ThenCoefMin1..ThenCoefMax,                               % declare the constant corresponding to the denominator
                LThenExtraVars = [ThenCoef,ThenCst]                                  % list of additional variables (enum.first on the den.)
            ;
                write(formula_generator_then_else6), nl, halt
            ),
            (memberchk(ThenB, [plus,abs,min,max,prod]) ->                            % if use a commutative binary term
                ThenAttr1 #<  ThenAttr2                                              % break symmetry as can permutte attributes
            ;                                                                        % if does not use a commutative binary term
                ThenAttr1 #\= ThenAttr2                                              % attributes should be distinct
            ),
            (memberchk(ThenB, [floor,mfloor,ceil,mod,dmod,fmod]) ->                  % min value of Attr2 should be > 0
                forbid_cols_which_can_be_leq0(LenA, MandatoryAttr, 0, ThenAttr2)
            ;
            ThenB = cmod ->                                                          % min value of Attr1 should be > 0
                forbid_cols_which_can_be_leq0(LenA, MandatoryAttr, 0, ThenAttr1)
            ;
                true                                                                 % otherwise no restriction
            ),
            (memberchk(ThenB, [abs,min,max]) ->                                      % no order between the two attributes
                gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt,eq,geq,gt], 0, LThenAttr)
            ;
             memberchk(ThenB, [floor,mfloor,ceil, % NEW                              % num should not always be leq,lt than den
                               minus_floor,mod,dmod,fmod]) ->                        % arg1 should not always be leq,lt than arg2
                gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [leq,lt], 0, LThenAttr)
            ;
             ThenB = cmod ->                                                         % arg1 should not allways be gt,geq than arg2
                gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [geq,gt], 0, LThenAttr)
            ;
             ThenB = minus ->                                                        % arg1 should not allways be gt than arg2
                gen_table_ctr_wrt_forbidden_cmp(LenA, MandatoryAttr, [gt], 0, LThenAttr)
            ;
                true
            ),
            Ctr1 = element(LogThenAttr1, LogAttributes, LogAttr1),                   % constraint for getting value of Attr1
            Ctr2 = element(LogThenAttr2, LogAttributes, LogAttr2),                   % constraint for getting value of Attr2
            (ThenB = plus                 -> Ctr3 = (LogRes #= LogAttr1+LogAttr2)                                      ;
             ThenB = minus                -> Ctr3 = (LogRes #= LogAttr1-LogAttr2)                                      ;
             ThenB = linear(_,_)          -> Ctr3 = (LogRes #= (LogAttr1+LogThenCst*LogAttr2))                         ;
             ThenB = abs                  -> Ctr3 = (LogRes #= abs(LogAttr1-LogAttr2))                                 ;
             ThenB = min                  -> Ctr3 = (LogRes #= min(LogAttr1,LogAttr2))                                 ;
             ThenB = max                  -> Ctr3 = (LogRes #= max(LogAttr1,LogAttr2))                                 ;
             ThenB = plus_min(_,_)        -> Ctr3 = (LogRes #= min(LogAttr1+LogAttr2-LogThenCst,LogAttr1))             ;
             ThenB = prod                 -> Ctr3 = (LogRes #= (LogAttr1 * LogAttr2))                                  ;
             ThenB = floor                -> Ctr3 = (LogRes #= (LogAttr1 div LogAttr2))                                ;
             ThenB = mfloor               -> tab_get_mins_attr_names(MandatoryAttr,MinAttrs),% get min.values of attributes used in the formula
                                             element(ThenAttr2, MinAttrs, MinAttr2),         % MinAttr2: the min.val.of attr.used in unary term
                                             Ctr3 = (LogRes #= (LogAttr1 div max(LogAttr2-LogMinAttr2,1)))             ;
             ThenB = ceil                 -> Ctr3 = (LogRes #= ((LogAttr1+LogAttr2-1) div LogAttr2))                   ;
             ThenB = plus_floor(_,_)      -> Ctr3 = (LogRes #= ((LogAttr1+LogAttr2+LogThenCst) div LogAttr1))          ;
             ThenB = minus_floor(_,_,_,_) -> Ctr3 = (LogRes #= ((LogAttr1-LogAttr2+LogThenCst) div LogThenCoef))       ; % NEW
             ThenB = mod                  -> Ctr3 = (LogRes #= (LogAttr1 mod LogAttr2))                                ;
             ThenB = cmod                 -> Ctr3 = (LogRes #= (LogAttr1 - (LogAttr2 mod LogAttr1)))                   ;
             ThenB = dmod                 -> Ctr3 = (LogRes #= (LogAttr1 - (LogAttr1 mod LogAttr2)))                   ;
             ThenB = fmod                 -> Ctr3 = (LogRes #= ((LogAttr1 mod LogAttr2 #= 0)*LogAttr2 +
                                                                (LogAttr1 mod LogAttr2 #> 0)*(LogAttr1 mod LogAttr2))) ;
                                             true
            ),
            append([LogRes,LogAttr1,LogAttr2], LogAttributes, SourceVarsLocal),      % these variables will be replaced by values on a row
            (ThenB = mfloor ->
                SourceVarsGlobal = [LogThenAttr1,LogThenAttr2,LogMinAttr2],
                TargetVarsGlobal = [ThenAttr1,ThenAttr2,MinAttr2],
                NewThenType      = binary_term(ThenB,MinAttr2),
                LThenCostVars    = [1]                                               % cost of the then will be the 1
            ;
             memberchk(ThenB,[linear(_,_),plus_min(_,_),plus_floor(_,_)]) ->
                SourceVarsGlobal = [LogThenAttr1,LogThenAttr2,LogThenCst],
                TargetVarsGlobal = [ThenAttr1,ThenAttr2,ThenCst],
                LThenCostVars    = [ThenCst],                                        % cost of the then will be the |ThenCst|
                (ThenB = linear(_,_)     -> NewThenType = binary_term(linear)     ;
                 ThenB = plus_min(_,_)   -> NewThenType = binary_term(plus_min)   ;
                                            NewThenType = binary_term(plus_floor) )
            ;
             ThenB = minus_floor(_,_,_,_) ->                                         % NEW
                SourceVarsGlobal = [LogThenAttr1,LogThenAttr2,LogThenCst,LogThenCoef],
                TargetVarsGlobal = [   ThenAttr1,   ThenAttr2,   ThenCst,   ThenCoef],  
                LThenCostVars    = [ThenCst, ThenCoef],                              % cost of the then will be |ThenCst|+|ThenCoef|
                NewThenType = binary_term(minus_floor)
            ;
                SourceVarsGlobal = [LogThenAttr1,LogThenAttr2],
                TargetVarsGlobal = [ThenAttr1,ThenAttr2],
                NewThenType      = binary_term(ThenB),
                (memberchk(ThenB, [cmod,dmod,fmod]) ->
                    LThenCostVars = [2]                                              % cost of the then will be 2 (penalize complex mod functions)
                ;
                    LThenCostVars = [1]                                              % cost of the then will be the 1
                )
            ),
            SourceCtrs = [Ctr1,Ctr2,Ctr3],                                           % list of constraints to post on each table entry
            ThenCheckOrder = 0
        ;
            write(formula_generator_then_else7), nl, halt
        )
    ;
        write(formula_generator_then_else8(Then)), nl, halt
    ).

% avoid to use the same unary/binary function with the same attributes both in the condition part and in the then part
avoid_same_func_in_cond_and_then(NewCondType, CondAttr1, CondAttr2, NewThenType, ThenAttr1, ThenAttr2) :-
    functor(NewCondType, CondType, _),
    functor(NewThenType, ThenType, _),
    ((CondType = unary_term_eq_coef, ThenType = unary_term) ->
        arg(1, NewCondType, CondFunc),
        arg(1, NewThenType, ThenFunc),
        (CondFunc = ThenFunc ->
            CondAttr1 #\= ThenAttr1
        ;
            true
        )
    ;
     (CondType = binary_term_eq_coef, ThenType = binary_term) ->
        arg(1, NewCondType, CondFunc),
        arg(1, NewThenType, ThenFunc),
        (CondFunc = ThenFunc ->
            CondAttr1 #\= ThenAttr1 #\/ CondAttr2 #\= ThenAttr2
        ;
            true
        )
        
    ;
        true
    ).

% limit attribute usage for then depending on the CondType
avoid_same_condattr_in_then(MandatoryAttr, NewCondType, LCondAttr, LThenAttr, CondCoef) :-
    (NewCondType = attr_eq_attr -> % no need to use one of the attributes for THEN
        LCondAttr = [_,CondAttr],
        (foreach(ThenAttr,LThenAttr), param(CondAttr) do CondAttr #\= ThenAttr)
    ;
     NewCondType = attr_eq_coef -> % no need to use the attribute for THEN
        LCondAttr = [CondAttr],
        (foreach(ThenAttr,LThenAttr), param(CondAttr) do CondAttr #\= ThenAttr)
    ;
     NewCondType = binary_term_eq_coef(plus) -> % no need to use the attribute for THEN
        LCondAttr = [CondAttr1, CondAttr2],
        tab_get_mins_attr_names(MandatoryAttr,MinAttrs),
        tab_get_maxs_attr_names(MandatoryAttr,MaxAttrs),
        element(CondAttr1, MinAttrs, MinAttr1),
        element(CondAttr2, MinAttrs, MinAttr2),
        element(CondAttr1, MaxAttrs, MaxAttr1),
        element(CondAttr2, MaxAttrs, MaxAttr2),
        (foreach(ThenAttr,LThenAttr), param(CondAttr1), param(CondAttr2),
         param(MinAttr1), param(MinAttr2), param(MaxAttr1), param(MaxAttr2), param(CondCoef) do
                (CondCoef #= (MinAttr1 + MinAttr2)) #=> ((CondAttr1 #\= ThenAttr) #/\ (CondAttr2 #\= ThenAttr)),
                (CondCoef #= (MaxAttr1 + MaxAttr2)) #=> ((CondAttr1 #\= ThenAttr) #/\ (CondAttr2 #\= ThenAttr))
        )
    ;
     NewCondType = binary_term_eq_coef(prod) -> % no need to use the attribute for THEN
        LCondAttr = [CondAttr1, CondAttr2],
        tab_get_mins_attr_names(MandatoryAttr,MinAttrs),
        tab_get_maxs_attr_names(MandatoryAttr,MaxAttrs),
        element(CondAttr1, MinAttrs, MinAttr1),
        element(CondAttr2, MinAttrs, MinAttr2),
        element(CondAttr1, MaxAttrs, MaxAttr1),
        element(CondAttr2, MaxAttrs, MaxAttr2),
        (foreach(ThenAttr,LThenAttr), param(CondAttr1), param(CondAttr2),
         param(MinAttr1), param(MinAttr2), param(MaxAttr1), param(MaxAttr2), param(CondCoef) do
                (CondCoef #= (MinAttr1 * MinAttr2)) #=> ((CondAttr1 #\= ThenAttr) #/\ (CondAttr2 #\= ThenAttr)),
                (CondCoef #= (MaxAttr1 * MaxAttr2)) #=> ((CondAttr1 #\= ThenAttr) #/\ (CondAttr2 #\= ThenAttr))
        )
    ;
        true
    ).

% Same but for else in cases like this:
% X in [1..10]
% CondType = attr_leq_coef, CondCoef = 9 meaning that ElseCond is basically attr_eq_coef
avoid_same_condattr_in_else(MandatoryAttr, NewCondType, LCondAttr, LElseAttr, CondCoef) :-
    (NewCondType = attr_leq_coef -> % no need to use the attribute for THEN
        LCondAttr = [CondAttr],
        tab_get_maxs_attr_names(MandatoryAttr, MaxAttrs),                           % get list of maximum values of the mandatory attributes
        element(CondAttr, MaxAttrs, MaxAttr1),                                      % get largest value of attribute CondAttr1
        (foreach(ElseAttr,LElseAttr), param(CondAttr), param(MaxAttr1), param(CondCoef) do
                (MaxAttr1 #= CondCoef + 1) #=> CondAttr #\= ElseAttr
        )
    ;
     NewCondType = attr_geq_coef -> % no need to use the attribute for THEN
        LCondAttr = [CondAttr],
        tab_get_mins_attr_names(MandatoryAttr, MinAttrs),                           % get list of maximum values of the mandatory attributes
        element(CondAttr, MinAttrs, MinAttr1),                                      % get largest value of attribute CondAttr1
        (foreach(ElseAttr,LElseAttr), param(CondAttr), param(MinAttr1), param(CondCoef) do
                (MinAttr1 #= CondCoef - 1) #=> CondAttr #\= ElseAttr
        )
    ;
        true
    ).

% One case:
%  - avoid the generation of formula in up_v_c_s_minc:
%    rminc   = (v mod c = 0 ? 0 : (v mod c = 0 ? c : v mod c))
% can be expanded to include more cases that don't intersect with avoid_same_func_in_cond_and_then
avoid_simplifiable_conditional(_MandatoryAttr,
                               NewCondType, NewThenType, NewElseType,
                               LCondAttr,  _LThenAttr,   LElseAttr,
                               CondCoef,    ThenCoef,   _ElseCoef) :-
    ((NewCondType = binary_term_eq_coef(mod),
      NewThenType = coef,
      NewElseType = binary_term(fmod)) ->
        LCondAttr = [A1, A2],
        LElseAttr = [A3, A4],
        ((A1 #= A3) #/\ (A2 #= A4)) #=> (CondCoef #\= ThenCoef)
    ;
        true).


forbid_cols_that_cant_intersect_min_max(LenA, AttrNames, MinVar, MaxVar, Var) :-
    findall(I,
            (I in 1..LenA,
             indomain(I),                            % go through the attributes we can have in the formula
             nth1(I, AttrNames, AttrNameI),          % get the different attributes names
             tab_get_min_max(AttrNameI, MinI, MaxI), % get minimum and maximum value of attribute I
             (MaxI < MinVar -> true ;                % catch the case when there is no intersection
              MinI > MaxVar -> true ;
                               false)),
            ListI),
    var_diff_from_values(ListI, Var).                % remove all values of ListI from variable Var

forbid_cols_where_min_gt_up_or_max_leq_low(LenA, AttrNames, Low, Up, Var) :-
    findall(I,
            (I in 1..LenA,
             indomain(I),                            % go through the attributes we can have in the formula
             nth1(I, AttrNames, AttrNameI),          % get the different attributes names
             tab_get_min_max(AttrNameI, MinI, MaxI), % get minimum and maximum value of attribute I
             (MinI  > Up  -> true ;                  % catch cases when MinI > Up or MaxI =< Low
              MaxI =< Low -> true ;
                             false)),
            ListI),
    var_diff_from_values(ListI, Var).                % remove all values of ListI from variable Var

% Set up an automaton constraint to enforce the fact that the then (resp. else) part
% return at least two distinct values wrt the different table entries for which the
% condition holds (resp. does not hold).
% BoolCond   : list of Boolean result variables of the condition of each table entry;
% ResThenElse: list of result variables of the then or the else part
% PosThen    : 1 if use the Then, 0 if use the Else
no_unique_value_for_res_then_or_res_else(BoolCond, ResThenElse, PosThen) :-
        NegThen is 1-PosThen,
        automaton(ResThenElse, Ri, BoolCond,
                  [source(r), sink(t)],
                  [arc(r, NegThen, r, [T]),
                   arc(r, PosThen, s, [Ri]),
                   arc(s, NegThen, s, [T]),
                   arc(s, PosThen, s, (Ri #=  T -> [T])),
                   arc(s, PosThen, t, (Ri #\= T -> [T])),
                   arc(t, NegThen, t, [T]),
                   arc(t, PosThen, t, [T])],
                   [T], [0], [_]).

gen_then_else_min_max_ctr([], _, _, 0) :- !.
gen_then_else_min_max_ctr([BoolCond-Order|R], BVal, OVal, (BoolCond #= BVal #/\ Order #= OVal) #\/ S) :-
    gen_then_else_min_max_ctr(R, BVal, OVal, S).
