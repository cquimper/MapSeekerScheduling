% Purpose: CHECK THE MONOTONICITY OF EACH BOUND WRT THE BIGGEST GENERATED SIZE
%  This sanity check is done as limiting ourself to a given maximum number of vertices may produce some tables where
%  some entries contain wrong lower/upper bounds, specially when the number of vertices is not part of the input parameters.
%  (normally gen_data is supposed to filter such entries, see valid_optimal/5 in gen_data.pl)

% Author : Nicolas Beldiceanu, IMT Atlantique

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(tables).
:- use_module(utility).

top(KindCombi) :-
    max_size_combi(KindCombi, N),
    atoms_concat(['data/',KindCombi,'/found_problems_',KindCombi,'.tex'], FileName),
    open(FileName, write, Sout),
    top(KindCombi, N, Sout),
    findall(Functor, found_problem(Functor), FunctorNames),
    sort(FunctorNames, SortedFunctorNames),
    print_name_of_tables_with_a_problem(SortedFunctorNames, Sout),
    close(Sout).

print_name_of_tables_with_a_problem([], _) :- !.
print_name_of_tables_with_a_problem([Functor|R], Sout) :-
    (R = [_|_] ->
        format(Sout, '~w,~n', [Functor])
    ;
        format(Sout, '~w.~n', [Functor])
    ),
    print_name_of_tables_with_a_problem(R, Sout).

top(KindCombi, N, Sout) :-                                                          % N is the largest size for which generate tables
    get_tables(KindCombi, _, _, _, Tables),                                         % get all the tables
    sort(Tables, SortedTables),                                                     % remove duplicates !!!
    member(Functor, SortedTables),                                                  % get next table
    N1 is N-1,                                                                      % last size we will check again maximum size
    M in 2..N1,                                                                     % size we will check
    indomain(M),                                                                    % get current size to check
    consult_data_and_get_nb_cols(KindCombi, Functor, N, NbColumnsN, IndIn, IndOut), % consult table associated with maximum size
    consult_data_and_get_nb_cols(KindCombi, Functor, M, NbColumnsM, IndIn, IndOut), % consult table associated with current size only if same number of columns
    check_table(NbColumnsN, NbColumnsM, IndIn, IndOut, Functor, M, Sout),           % check current table wrt table associated with maximum size
    retractall(user:signature(_,_,_)),                                              % remove signature facts
    functor(TableTermN, Functor, NbColumnsN),                                       % remove all entries of table associated with maximum size
    retractall(user:TableTermN),
    (NbColumnsN = NbColumnsM -> true                                     ;
                                functor(TableTermM, Functor, NbColumnsM),           % if the two tables do not have the same number of columns
                                retractall(user:TableTermM)              ),         % then remove all entries of table associated with current size
    false.                                                                          % enforce backtracking to check the next table
top(_, _).

check_table(NbColumns, NbColumns, IndIn, IndOut, Functor, M, Sout) :- !, % check only tables which have same number of columns as the
    functor(Term, Functor, NbColumns),                                   % table associated with maximum size
    findall(Term, call(Term), Terms),                                    % get all rows of the two tables
    length(IndIn, NParam),                                               % number of input parameters
    project_terms(Terms, NParam, IndIn, IndOut, ProjectedTerms),         % project all rows wrt the input parameters and the output parameter
    keysort(ProjectedTerms, SortedTerms),                                % sort projections wrt input parameters
    check_table1(SortedTerms, Functor, M, Sout).                         % check that for each set of input parameters we have one single bound
check_table(_, _, _, _, _, _, _).                                        % do not check two tables that do not have the same number of columns

project_terms([], _, _, _, []) :- !.
project_terms([Source|R], NParam, IndIn, IndOut, [Target-Val|S]) :-
    functor(Target, t, NParam),                 % create the projected term with the appropriate arity
    project_term(IndIn, Source, Target),        % project current term wrt input parameters
    arg(IndOut, Source, Val),                   % get the value of the corresponding output parameter
    project_terms(R, NParam, IndIn, IndOut, S).

project_term([], _, _) :- !.
project_term([I|R], Term, ProjectedTerm) :-
    arg(I, Term, Val),
    arg(I, ProjectedTerm, Val),
    project_term(R, Term, ProjectedTerm).

check_table1([], _, _, _) :- !.
check_table1([_], _, _, _) :- !.
check_table1([T-V,T-V|R], Functor, M, Sout) :- !,
    check_table1([T-V|R], Functor, M, Sout).
check_table1([T-U,T-V|_], Functor, M, Sout) :- !,
    assert(found_problem(Functor)),
    format(Sout, 'PROBLEM WITH: ~w(~w) ~w-~w ~w-~w~n', [Functor,M,T,U,T,V]).
check_table1([_,T-V|R], Functor, M, Sout) :-
    check_table1([T-V|R], Functor, M, Sout).

consult_data_and_get_nb_cols(KindCombi, Functor, Size, NbColumns, IndIn, IndOut) :-
    number_codes(Size, CodesSize),                         % convert Size to an atom in order to create
    atom_codes(AtomSize, CodesSize),                       % the name of the subfolder that will contain
    atoms_concat(['data/',KindCombi,                       % all metadata files corresponding to
                  '/data',AtomSize,                        % combinatorial objects that have up to
                  '/',                                     % Size elements
                  Functor,                                 % add the name of the table to the file name
                  '.pl'], FileName),                       % add the suffix to indicate a Prolog file
    consult(FileName),                                     % read signature and table facts
    signature(Functor, Size, Args),                        % get description of the different columns
    functor(Args, t, NbColumns),                           % create a term with the appropriate number of columns
    findall(I, (I in 1..NbColumns, indomain(I),            % get positions of input parameters
                arg(I, Args, t(_,primary,input))), IndIn),
    findall(I, (I in 1..NbColumns, indomain(I),            % get position of the output parameter (the bound)
                arg(I, Args, t(_,_,output))), [IndOut]).
