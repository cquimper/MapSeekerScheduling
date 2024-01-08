% Purpose: primitives for instrumenting the code in order to collect statistics on the execution
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(instrument_code,[record_trace/1,
                           instrument_code/2,
                           instrument_code/3]).

:- use_module(utility).

:- dynamic instrument_in_search_formula/3.
:- dynamic instrument_found_trivial/3.
:- dynamic instrument_found_conjecture/4.
:- dynamic instrument_fail_without_time_out/3.
:- dynamic instrument_time_out/3.
:- dynamic instrument_in_formula_generator/3.

record_trace(Tout) :-                                          % record in a text file all the prolog facts corresponding to the trace
    findall(instrument_in_search_formula(Map, Call, Cpt),
            instrument_in_search_formula(Map, Call, Cpt),
            List1),
    write_list(List1, Tout),
    format(Tout, '~n', []),
    findall(instrument_found_trivial(Map, Call, Cpt),
            instrument_found_trivial(Map, Call, Cpt),
            List2),
    write_list(List2, Tout),
    format(Tout, '~n', []),
    findall(instrument_found_conjecture(Map, Call, Cpt, Cost),
            instrument_found_conjecture(Map, Call, Cpt, Cost),
            List3),
    write_list(List3, Tout),
    format(Tout, '~n', []),
    findall(instrument_fail_without_time_out(Map, Call, Cpt),
            instrument_fail_without_time_out(Map, Call, Cpt),
            List4),
    write_list(List4, Tout),
    format(Tout, '~n', []),
    findall(instrument_time_out(Map, Call, Cpt),
            instrument_time_out(Map, Call, Cpt),
            List5),
    write_list(List5, Tout),
    format(Tout, '~n', []),
    findall(instrument_in_formula_generator(Map, Call, Cpt),
            instrument_in_formula_generator(Map, Call, Cpt),
            List6),
    write_list(List6, Tout),  
    format(Tout, '~n', []).

instrument_code(Functor, Call-Map) :-
    functor(Term, Functor, 3),    % create a term corresponding to current trace
    arg(1, Term, Map),            % first argument corresponding the map where we currently are
    arg(2, Term, Call),           % second argument corresponding to type of call (primary or secondary)
    (call(Term) ->
        arg(3, Term, N),          % get number of calls
        retract(Term)             % remove previous term
    ;
        N = 0
    ),
    N1 is N+1,                    % increment it
    functor(NewTerm, Functor, 3), % create a new term
    arg(1, NewTerm, Map),         % set its map
    arg(2, NewTerm, Call),        % set its type of call (primary or secondary)
    arg(3, NewTerm, N1),          % set its number of calls
    assert(NewTerm),              % record new term
    !.
instrument_code(Functor, CallMap) :-
    write(instrument_code(Functor, CallMap)), nl, halt.

instrument_code(Functor, Call-Map, Cost) :-
    functor(Term, Functor, 4),    % create a term corresponding to current trace
    arg(1, Term, Map),            % first argument corresponding the map where we currently are
    arg(2, Term, Call),           % second argument corresponding to type of call (primary or secondary)
    arg(3, Term, Cost),           % third argument corresponding to the cost
    (call(Term) ->
        arg(4, Term, N),          % get number of calls
        retract(Term)             % remove previous term
    ;
        N = 0
    ),
    N1 is N+1,                    % increment it
    functor(NewTerm, Functor, 4), % create a new term
    arg(1, NewTerm, Map),         % set its map
    arg(2, NewTerm, Call),        % set its type of call (primary or secondary)
    arg(3, NewTerm, Cost),        % set its cost
    arg(4, NewTerm, N1),          % set its number of calls
    assert(NewTerm),              % record new term
    !.    
instrument_code(Functor, CallMap, Cost) :-
    write(instrument_code(Functor, CallMap, Cost)), nl, halt.
