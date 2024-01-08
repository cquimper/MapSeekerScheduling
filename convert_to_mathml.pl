% Purpose: convert saved conjectures to Content MathML format.
% Authors: Ramiz Gindullin, IMT Atlantique

:- use_module(library(lists)).

% The convertation tested on Firefox, Chrome and Safari
% For Firefox it is recommended to use extension "MathML Fonts" for better rendering:
%    link: https://addons.mozilla.org/en-US/firefox/addon/mathml-fonts/
% For Chrome it is recommended to use extension "FMath 'HTML + MathML' Solution" for better rendering:
%    link: https://chrome.google.com/webstore/detail/fmath-html-%2B-mathml-solut/emdjdpchbjipnjhkfljbcapgfecmnglm
% No additional software is needed, except an internet access
top:-
    convert_conjectures_to_mathml('results version 2022 06 13/found_conjectures_all.pl', 'mathml_low_c.html').

top(1 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_low_c.pl',    'results version 2022 05 17/found_conjectures_low_c.html'   ).
top(2 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_low_maxc.pl', 'results version 2022 05 17/found_conjectures_low_maxc.html').
top(3 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_low_maxs.pl', 'results version 2022 05 17/found_conjectures_low_maxs.html').
top(4 ) :- convert_conjectures_to_mathml('results version 2022 05 23/found_conjectures_low_mina.pl', 'results version 2022 05 23/found_conjectures_low_mina.html').
top(5 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_low_mins.pl', 'results version 2022 05 17/found_conjectures_low_mins.html').
top(6 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_low_minc.pl', 'results version 2022 05 17/found_conjectures_low_minc.html').
top(7 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_low_s.pl',    'results version 2022 05 17/found_conjectures_low_s.html'   ).
top(8 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_low_v.pl',    'results version 2022 05 17/found_conjectures_low_v.html'   ).
top(9 ) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_c.pl',     'results version 2022 05 17/found_conjectures_up_c.html'    ).
top(10) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_maxa.pl',  'results version 2022 05 17/found_conjectures_up_maxa.html' ).
top(11) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_maxc.pl',  'results version 2022 05 17/found_conjectures_up_maxc.html' ).
top(12) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_maxs.pl',  'results version 2022 05 17/found_conjectures_up_maxs.html' ).
top(13) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_minc.pl',  'results version 2022 05 17/found_conjectures_up_minc.html' ).
top(14) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_mins.pl',  'results version 2022 05 17/found_conjectures_up_mins.html' ).
top(15) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_s.pl',     'results version 2022 05 17/found_conjectures_up_s.html'    ).
top(16) :- convert_conjectures_to_mathml('results version 2022 05 17/found_conjectures_up_v.pl',     'results version 2022 05 17/found_conjectures_up_v.html'    ).

% Loads conjectures from a file, creates a simple html document with conjectures converted in Content MathML format
convert_conjectures_to_mathml(ConjectureFile, MathMLFile):-
    write('convert file: '), write(ConjectureFile),nl,
    consult(ConjectureFile),
    findall(conjecture(Type,OutputCol,OutputName,MaxN,Cost,Formula)      ,
            conjecture(_,Type,OutputCol,OutputName,MaxN,Cost,Formula,_,_), % cmodif
            ConjectureMathMLList                                         ),
    open(MathMLFile, write, SOut),
    % Create header of the html document
    format(SOut, '<!DOCTYPE html>~n<html lang = "en">~n<head>~n<meta charset="utf-8">~n<title>MathML convertation</title>~n', []),
    format(SOut, '<script>window.MathJax = { MathML: { extensions: ["mml3.js", "content-mathml.js"]}};</script>~n', []),
    format(SOut,
           '<script type="text/javascript" async src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=MML_HTMLorMML"></script>~n',
           []),
    format(SOut, '</head>~n<body>~n', []),
    write(start_conversion),nl,
    convert_to_mathml(ConjectureMathMLList, SOut),      % convert all conjectures in MathML format and write them in the file
    write(end_conversion),nl,
    format(SOut, '</body>~n</html>~n', []),             % write the end of the document
    close(SOut),
    write('finish '),
    retractall(conjecture(_,_,_,_,_,_,_,_,_)),          % facts of the conjecture file must be declared dynamic for this to work % cmodif
    write('conversion'),nl.

% Go though the list of conjectures, convert each conjecture in MathML format and write it into the file stream SOut
convert_to_mathml([], _) :- !.
convert_to_mathml([ConjectureInit|R], SOut) :-
    ConjectureInit = conjecture(Type,col(TableName,_OutputCol),OutputName, MaxN, Cost,
                                t(_InputCols, InputNames, InputVars, Formula)),
    (foreach(Name,InputNames), foreach(Var,InputVars) do Name = Var),                              % unify variables with its names
%   format(SOut, '<math xmlns="http://www.w3.org/1998/Math/MathML">~n', []),                       % start the MathML tag, old version
    format(SOut, '<math>~n', []),    % Start the MathML tag
    format(SOut, '<mtext>~w of ~w, &emsp; Cost = ~w: &emsp; </mtext>~n',[TableName, MaxN, Cost]),  % write explanatory text
    (Type = secondary -> format(SOut, '<apply>~n<eq/>~n   <ci>~w</ci>~n',  [OutputName])        ;  % depending on the type of the conjecture
     Type = low       -> format(SOut, '<apply>~n<geq/>~n   <ci>~w</ci>~n', [OutputName])        ;  % select the relation between the output column and the formula
                         format(SOut, '<apply>~n<leq/>~n   <ci>~w</ci>~n', [OutputName])        ),
    convert_term(Formula, SOut),                                                                   % convert the conjecture and write it in the file stream SOut
    format(SOut, '</apply>~n</math>~n<br>~n', []),                                                 % finish the MathML tag
    !,
    convert_to_mathml(R, SOut).                                                                    % recursive call
convert_to_mathml([ConjectureInit|_], _) :-
    write(ConjectureInit),nl,halt.


convert_term(Term, _) :-       % first catch the case when Term was incorrectly generated
    \+ ground(Term),           % in order to avoid getting in an infinite recursive loop
    !,
    write(convert_term(Term)), nl, halt.

% Convert conditional formula in MathML
convert_term(if(Cond, Term1, Term2),SOut) :-
    !,
    format(SOut, '   <piecewise>~n   <piece>~n', []),
    convert_term(Term1, SOut),
    convert_term(Cond, SOut),
    format(SOut, '   </piece>~n   <otherwise>~n', []),
    convert_term(Term2, SOut),
    format(SOut, '   </otherwise>~n   </piecewise>~n', []).

% Convert Boolean formula in MathML
% Negated = 0: no negation
% Negated = 1:    negation 
convert_term(bool(Negated, ShiftBool, Oplus, NbTerms, Conds), SOut) :-
    !,
    format(SOut, '   <apply>~n', []),
    ((Negated = 1, Oplus = sum) -> ShiftBoolNew is ShiftBool + NbTerms  ;
     Negated = 1                -> ShiftBoolNew is ShiftBool + 1        ;
                                   ShiftBoolNew is ShiftBool            ),
    (Negated = 0 ->
        format(SOut, '   <plus/>~n',  []),
        (ShiftBoolNew = 0 -> true  ; format(SOut, '   <cn>~w</cn>~n', [ShiftBoolNew]))
    ;
     (Negated = 1, Oplus = sum) ->
        format(SOut, '   <minus/>~n', []),
        (ShiftBoolNew = 0 -> true  ; format(SOut, '   <cn>~w</cn>~n', [ShiftBoolNew]))
    ;
     Negated = 1 ->
        format(SOut, '   <plus/>~n',  []),
        (ShiftBoolNew = 0 -> true  ; format(SOut, '   <cn>~w</cn>~n', [ShiftBoolNew])),
        format(SOut, '   <apply>~n   <not/>~n', [])
    ;
        false
    ),
     (Oplus = and       ->      format(SOut, '   <apply>~n   <and/>~n', [])                             ;
      Oplus = or        ->      format(SOut, '   <apply>~n   <or/>~n', [])                              ;
      Oplus = sum       ->      format(SOut, '   <apply>~n   <plus/>~n', [])                            ;
      Oplus = allequal  ->      format(SOut, '   <apply>~n   <or/>~n   <apply>~n   <and/>~n', [])       ;
      Oplus = xor       ->      format(SOut, '   <apply>~n   <xor/>~n', [])                             ;
      % voting and card1 aren't tested yet
      Oplus = voting    ->      format(SOut, '   <apply>~n   <geq/>~n   <apply>~n   <plus/>~n', [])     ;
      Oplus = card1     ->      format(SOut, '   <apply>~n   <eq/>~n   <apply>~n   <plus/>~n', [])     ;
                                false                                                                   ),
    (foreach(Term,Conds), param(SOut) do convert_term(Term, SOut)),
    (Oplus = allequal  -> %allequal case must look like (Cond1 AND Cond2 AND Cond3) OR (not Cond1 AND not Cond2 AND not Cond3)
        format(SOut, '   </apply>~n   <apply>~n   <and/>~n', []),
        (foreach(Term,Conds), param(SOut) do convert_term(not(Term), SOut)),
        format(SOut, '   </apply>~n', [])
    ;
     Oplus = voting  ->
        length(Conds, LConds),
        LConds1 is (LConds div 2 + 1),
        format(SOut, '   </apply>~n   <cn>~w</cn>~n', [LConds1])
    ;
     Oplus = card1  ->
        format(SOut, '   </apply>~n   <cn>1</cn>~n', [])
    ;
        true),
    
    ((Negated = 1, Oplus \== sum) ->
        format(SOut, '   </apply>~n', [])
    ;
        true
    ),
    format(SOut, '   </apply>~n   </apply>~n', []).

convert_term(Term, SOut) :-     % convert a nonnegative number
    integer(Term), Term >= 0, !,
    format(SOut, '   <cn>~w</cn>~n', [Term]).

convert_term(Term, SOut) :-     % convert a negative number
    integer(Term), Term < 0, !,
    Term1 is -Term,
    convert_term(minus(Term1), SOut).

convert_term(Term, SOut) :-     % convert an atom (variable)
    atom(Term), !,
    format(SOut, '   <ci>~w</ci>~n', [Term]).

convert_term(geq0(Term),SOut) :-
    !, convert_term(geq(Term,0), SOut).

convert_term(not(not(Term)), SOut) :-
    !, convert_term(Term, SOut).

convert_term(not(Term), SOut) :-
    !,
    format(SOut, '   <apply>~n   <not/>~n', []),
    convert_term(Term, SOut),
    format(SOut, '   </apply>~n', []).

convert_term(minus(Term), SOut) :-
    !,
    format(SOut, '   <apply>~n   <minus/>~n',  []),
    convert_term(Term, SOut),
    format(SOut, '   </apply>~n',  []).

convert_term(Term, SOut) :-
    functor(Term, Type, 2),
    memberchk(Type, [in,eq,geq,leq,plus,minus,div,min,max]),
    !,
    arg(1,Term,Term1),
    arg(2,Term,Term2),
    format(SOut, '   <apply>~n', []),
    (Type = in          ->      format(SOut, '   <in/>~n',     [])      ;
     Type = eq          ->      format(SOut, '   <eq/>~n',     [])      ;
     Type = geq         ->      format(SOut, '   <geq/>~n',    [])      ;
     Type = leq         ->      format(SOut, '   <leq/>~n',    [])      ;
     Type = plus        ->      format(SOut, '   <plus/>~n',   [])      ;
     Type = minus       ->      format(SOut, '   <minus/>~n',  [])      ;
     Type = div         ->      format(SOut, '   <divide/>~n', [])      ;
     Type = min         ->      format(SOut, '   <min/>~n',    [])      ;
     Type = max         ->      format(SOut, '   <max/>~n',    [])      ),
    convert_term(Term1, SOut),
    convert_term(Term2, SOut),
    format(SOut, '   </apply>~n', []).

convert_term(prod(Term1, Term2), SOut) :-
    !,
    (Term1 = 1          ->      convert_term(Term2, SOut)               ;
     Term2 = 1          ->      convert_term(Term1, SOut)               ;
     Term1 = -1         ->      convert_term(minus(Term2), SOut)        ;
     Term2 = -1         ->      convert_term(minus(Term1), SOut)        ;
     integer(Term2)     ->      convert_term(prod1(Term2, Term1), SOut) ;
                                convert_term(prod1(Term1, Term2), SOut) ).

convert_term(prod1(Term1, Term2), SOut) :-
    !,
    ((integer(Term1), Term1 =< -1) ->
        Term10 is -Term1, convert_term(minus(prod1(Term10, Term2)), SOut)
    ;
        format(SOut, '   <apply>~n   <times/>~n', []),
        convert_term(Term1, SOut),
        convert_term(Term2, SOut),
        format(SOut, '   </apply>~n', [])
    ).

convert_term(power(Term1, Term2), SOut) :-
    !,
    (Term2 = 1  ->      % if Power = 1 no need to apply the tag
        convert_term(Term1, SOut)
    ;
        format(SOut, '   <apply>~n   <power/>~n', []),
        convert_term(Term1, SOut),
        convert_term(Term2, SOut),
        format(SOut, '   </apply>~n', [])
    ).

convert_term(mod(Term1, Term2), SOut) :-
    !,
    format(SOut, '   <apply>~n   <rem/>~n', []),
    convert_term(Term1, SOut),
    convert_term(Term2, SOut),
    format(SOut, '   </apply>~n', []).

convert_term(abs(Term1, Term2), SOut) :-
    !,
    format(SOut, '   <apply>~n   <abs/>~n', []),
    convert_term(minus(Term1, Term2), SOut),
    format(SOut, '   </apply>~n', []).

convert_term(ceil(Term1, Term2), SOut) :-
    !,
    format(SOut, '   <apply>~n   <ceiling/>~n', []),
    convert_term(div(Term1, Term2), SOut),
    format(SOut, '   </apply>~n', []).

convert_term(floor(Term1, Term2), SOut) :-
    !,
    format(SOut, '   <apply>~n   <floor/>~n', []),
    convert_term(div(Term1, Term2), SOut),
    format(SOut, '   </apply>~n', []).

convert_term(cmod(Term1, Term2), SOut) :-
    !, convert_term(minus(Term1, mod(Term2,Term1)), SOut).

convert_term(dmod(Term1, Term2), SOut) :-
    !, convert_term(minus(Term1, mod(Term1,Term2)), SOut).

convert_term(fmod(Term1, Term2), SOut) :-
    !, convert_term(max(prod(Term2, eq(mod(Term1,Term2), 0)),
                        prod(mod(Term1,Term2), geq(mod(Term1,Term2), 1))), SOut).

convert_term(interval(Term1, Term2), SOut) :-
    !,
    format(SOut, '   <interval>~n', []),
    convert_term(Term1, SOut),
    convert_term(Term2, SOut),
    format(SOut, '   </interval>~n', []).

convert_term(sum_consec(Term),SOut) :-
    !, convert_term(div(prod(Term,plus(Term,1)),2), SOut).

convert_term(Term, SOut) :-
    functor(Term, Type, 3),
    memberchk(Type, [in,plus,min_min,max_min,floor_min,mfloor,linear,plus_min,plus_floor,sum_leq_attr,minus_mod_eq0]),
    !,
    arg(1,Term,Term1),
    arg(2,Term,Term2),
    arg(3,Term,Term3),
    (Type = in                  ->      convert_term(in(Term1,interval(Term2,Term3)), SOut)             ;
     Type = plus                ->      convert_term(plus(Term1,plus(Term2,Term3)), SOut)               ;
     Type = min_min             ->      convert_term(min(minus(Term1,Term3),Term2), SOut)               ;
     Type = max_min             ->      convert_term(max(minus(Term1,Term3),Term2), SOut)               ;
     Type = floor_min           ->      convert_term(floor(minus(Term1,Term3),Term2), SOut)             ;
     Type = mfloor              ->      convert_term(floor(Term1,max(minus(Term2,Term3),1)), SOut)      ;
     Type = linear              ->      convert_term(plus(Term1,prod(Term2,Term3)), SOut)               ;
     Type = plus_min            ->      convert_term(min(minus(plus(Term1,Term2),Term3),Term1), SOut)   ;
     Type = plus_floor          ->      convert_term(floor(plus(Term1,Term2,Term3),Term1),SOut)         ;
     Type = sum_leq_attr        ->      convert_term(leq(plus(Term1,Term2),Term3),SOut)                 ;
     Type = minus_mod_eq0       ->      convert_term(eq(mod(minus(Term3,Term1),Term2),0),SOut)          ).

convert_term(linear(Term1,Term2,Term3,Term4),SOut) :-
    !, convert_term(plus(prod(Term4,Term1),Term2,Term3), SOut).

convert_term(minus_floor(Term1,Term2,Term3,Term4),SOut) :-
    !, convert_term(div(minus(plus(Term1,Term3),Term2),Term4), SOut).

convert_term(polynom(Terms),SOut) :-
    !,
    format(SOut, '   <apply>~n   <plus/>~n', []),
    (foreach(Term,Terms), param(SOut) do convert_term(Term, SOut)),
    format(SOut, '   </apply>~n', []).

convert_term(monome([], Coef), SOut):-
    !, convert_term(Coef, SOut).

convert_term(monome([t(Term,Degree)],Coef), SOut):-
    !, convert_term(prod(Coef,power(Term,Degree)), SOut).

convert_term(monome(Terms,Coef), SOut):-
    !,
    (Coef = 0   -> true                                                                         ;
     Coef = 1   -> format(SOut, '   <apply>~n   <times/>~n', [])                                ; % don't write coefficients +1 and -1 in multiplication
     Coef = -1  -> format(SOut, '   <apply>~n   <minus/>~n   <apply>~n   <times/>~n', [])       ; % if Coef is negative use <minus/> tag first
     Coef =< -2 -> format(SOut, '   <apply>~n   <minus/>~n   <apply>~n   <times/>~n', []),
                   Coef1 is -Coef, convert_term(Coef1, SOut)                                    ;
                   format(SOut, '   <apply>~n   <times/>~n', []), convert_term(Coef, SOut)      ),
    (foreach(t(Term,Degree),Terms), param(SOut) do convert_term(power(Term,Degree), SOut)),
    (Coef =< -1 -> format(SOut, '   </apply>~n', []) ; true),
    format(SOut, '   </apply>~n', []).

convert_term(cases(TermsList),SOut) :-
    !,
    format(SOut, '   <piecewise>~n', []),
    (foreach(Term, TermsList), param(SOut)
    do
     convert_term(Term, SOut)
    ),
    format(SOut, '   </piecewise>~n', []),
    true.

convert_term(if_then(CondTerm,ThenTerm),SOut) :-
    !,
    format(SOut, '   <piece>~n', []),
    convert_term(ThenTerm, SOut),
    convert_term(CondTerm, SOut),
    format(SOut, '   </piece>~n', []),
    true.

convert_term(otherwise(ElseTerm),SOut) :-
    !,
    format(SOut, '   <otherwise>~n', []),
    convert_term(ElseTerm, SOut),
    format(SOut, '   </otherwise>~n', []).

convert_term(Term,_) :- write(Term), nl. % if it is an unknown type of Term, write it on the console for further investigation
