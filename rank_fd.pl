:- module(rank_fd, [rank_fds/6]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(statistics)).
:- use_module(tables).
:- use_module(table_access).
:- use_module(utility).
:- use_module(stat_utility).

% Input
%  Table    : name of the table for which rank functional dependencies
%  Arity    : total number of columns in the table Table
%  NbRows   : total number of rows in the table Table
%  OutputCol: index of the result column for which we rank its functional dependencies
%  FdsI     : list of functional dependencies attached to column OutputCol
%
% Output
%  RankedFds: list of ranked functional dependencies attached to column OutputCol
%
rank_fds(Table, Arity, NbRows, OutputCol, FdsI, RankedFds) :-    
    findall(Fd, (member(X, FdsI), sort(X, Fd)), Fds),       % make sure that list of columns within each fd is sorted
    critical_vals_for_pearson_correlation_coef(NbRows, RCrit),
    rows_retrieved([OutputCol], Table, Arity, RowsO1),      % project all entries of the table Table onto the output column OutputCol    
    append(RowsO1, RowsO),                                  % extract list of output values of table Table by flattifying RowsO1
    findall([Distincts,CorrsS,N]-Fd,                        % create for each fd Fd a triple used to rank it, where:
                                                            %  . Distincts is the number of distinct vectors within Fd
                                                            %  . CorrsS    is the maximum correlaton coefficient for Fd
                                                            %  . N         is the number of columns for Fd
            (member(Fd, Fds),                               % get current functional dependency for which we will compute the rank
             findall(Col, member(col(_,Col), Fd), FdCols),  % extract the column numbers of the current functional dependency
             length(FdCols, N),                             % project the different rows of the table wrt the columns of the current fd
             rows_retrieved(FdCols, Table, Arity, Rows),    % as a list of sublists
             sort(Rows, RowsS),
             length(RowsS, Distincts),                      % compute the number of distinct rows after projecting on the columns of the fd

               % IF THE FUNCTIONAL DEPENDENCY Fd DEPENDs ON ONE SINGLE COLUMN
               %........................................................................................................................
               (FdCols = [_] ->

                    % calculate correlation coefficient between Fd column and output column to catch linear formulae of type a1.X+a2
                    append(Rows, RowsVal), 
                    protected_correlation(RowsVal, RowsO, CorrVal1),
                    get_correlation_wrt_test(CorrVal1, RCrit, CorrVal),
                    
                    % calculate correlation coefficient between inverse value of Fd column and output column to catch formulae of type (a1/X)+a2
                    findall(Div1, (J in 1..NbRows,          
                                   indomain(J), 
                                   nth1(J, Rows, [Row1]),
                                   (Row1 = 0 -> Div1 is 100000 ; Div1 is 1 / Row1)
                                  ), RowsDiv1), 
                    protected_correlation(RowsDiv1, RowsO, CorrDiv1),
                    get_correlation_wrt_test(CorrDiv1, RCrit, CorrDiv),
                    CorrsA = [CorrVal,CorrDiv]              % record computed correlations
               ;

               % IF THE FUNCTIONAL DEPENDENCY Fd DEPENDS ON TWO COLUMNS
               %........................................................................................................................
                FdCols = [_,_] ->

                    % calculate adjusted correlation coefficient between input columns and the output column
                    % to catch linear formulae of type a1.X+a2.Y+a3
                    adjusted_multiple_correlation(RowsO, Rows, MultiCorrAdj10, R2),
                    f_multiple_correlation(R2, NbRows, 2, F),
                    DF2 is NbRows - 2 - 1,                  % secondary degree of freedom for the F-test (2 is the number of columns)
                    f_distribution(N, DF2, FCrit),
                    (F > FCrit -> MultiCorrAdj is -abs(MultiCorrAdj10) ; MultiCorrAdj is 0),
                
                    % calculate linear correlation coefficients between the sum of values of input columns and the output column
                    % to catch formulae of types X+Y, X-Y, X*Y, X div Y, Y div X, min(X,Y), max(X,Y)
                    findall([Sum,Diff,Prod,Div1,Div2,Min,Max], (J in 1..NbRows,
                                                                indomain(J),
                                                                nth1(J, Rows, [RowI1,RowI2]),
                                                                Sum  is RowI1 + RowI2,
                                                                Diff is RowI1 - RowI2,
                                                                Prod is RowI1 * RowI2,
                                                                (RowI2 = 0 -> Div1 is RowI1 * 100000 ; Div1 is RowI1 / RowI2),
                                                                (RowI1 = 0 -> Div2 is RowI2 * 100000 ; Div2 is RowI2 / RowI1),
                                                                min_member(Min, [RowI1, RowI2]),
                                                                max_member(Max, [RowI1, RowI2])
                                                                ), RowsFunctions),
                    transpose(RowsFunctions, ColumnsFunctions),
                    findall(Corr,(memberchk(RowsC, ColumnsFunctions),
                                  protected_correlation(RowsC, RowsO, Corr1),
                                  get_correlation_wrt_test(Corr1, RCrit, Corr)), CorrsA0),

                    CorrsA = [MultiCorrAdj|CorrsA0]         % record computed correlations

               ;

               % IF THE FUNCTIONAL DEPENDENCY Fd DEPENDS ON MORE THAN TWO COLUMNS
               %........................................................................................................................
                    length(Coeffs, N),                      % generate all [1,-1] coefficient vectors starting with 1
                    Coeffs = [1|_],
                    findall(Coeffs, gen_combinations_of_values(Coeffs, [1,-1]), CoeffsAll),
                    
                    % calculate adjusted multiple correlation coefficient between input columns and the output column
                    % to catch formulae of type sum(ai.Xi) + a0
                    adjusted_multiple_correlation(RowsO, Rows, MultiCorrAdj10, R2),
                    f_multiple_correlation(R2, NbRows, N, F),
                    DF2 is NbRows - N - 1,                  % secondary degree of freedom for the F-test
                    f_distribution(N, DF2, FCrit),
                    (F > FCrit -> MultiCorrAdj is -abs(MultiCorrAdj10) ; MultiCorrAdj is 0),
                
                    % calculate linear correlation coefficients between the output column and all combinations of sums/substractions
                    % of input columns to catch formulae of type sum((-1)^ai.Xi) + a0
                    findall(CorrSP, (NbRows =< 1000,           % as it is costly, do not perform it for tables with large numbers of entries
                                     member(Coeffs, CoeffsAll),
                                     findall(ScalarProduct, (J in 1..NbRows,
                                                             indomain(J),
                                                             nth1(J, Rows, RowJ),
                                                             scalar_prod(RowJ, Coeffs, ScalarProduct)
                                                            ), RowsSP),
                                     protected_correlation(RowsSP, RowsO, CorrSP1),
                                     get_correlation_wrt_test(CorrSP1, RCrit, CorrSP)), CorrsSum),

                    % calculate linear correlation coefficients between the output column and all combinations of sums/substractions
                    % of input columns to catch formulae of type sum_{k\neq i}((-1)^ak.Xk) + (-1)^ai.Xi^2 + a0
                    findall(CorrSq, (NbRows =< 1000,            % as it is costly, do not perform it for tables with large numbers of entries
                                     I in 1..N,
                                     indomain(I),
                                     findall(Sq, (J in 1..NbRows,
                                                  indomain(J),
                                                  nth1(J, Rows, RowJ),
                                                  nth1(I, RowJ, Val1),
                                                  Val is Val1 * Val1,
                                                  findall(RowK, (K in 1..N,
                                                                 indomain(K),
                                                                 K =\= I, 
                                                                 nth1(K, RowJ, RowK)), ValsK),
                                                  Sq = [Val|ValsK]
                                                 ), RowsSq),
                                     adjusted_multiple_correlation(RowsO, RowsSq, CorrSq10, R2),
                                     f_multiple_correlation(R2, NbRows, N, F),
                                     DF2 is NbRows - N - 1,
                                     f_distribution(N, DF2, FCrit),
                                     (F > FCrit -> CorrSq1 is CorrSq10 ; CorrSq1 is 0),
                                     CorrSq is -abs(CorrSq1)), CorrsSq),
                    append([[MultiCorrAdj], CorrsSum, CorrsSq], CorrsA)                
               ), 
               min_member(CorrsS, CorrsA)
            ), 
            ResList),
    sort(ResList, RankedFds).

% Input
%  . Columns  : set of columns (in the form of a list of integers) on which we will project all rows of table TableName
%  . TableName: considered table
%  . NbColumns: number of columns of table TableName
%
% Output
%  . Rows     : list of all rows of table TableName projected on the subset of columns Columns
%
rows_retrieved(Columns, TableName, NbColumns, Rows) :- % retrieve all rows from the table (with selected column), as a list of lists
    findall(X, (functor(Term,TableName,NbColumns),
                user:call(Term),                       % select the Term
                row_retrieved(Columns, Term, X)        % extract the row from the Term
               ),
            Rows).

row_retrieved([], _, []) :- !.
row_retrieved(Columns, Term, Row) :- % retrieve a row from the term (with selected column), as a list
    Columns = [H|T],
    arg(H, Term, X),                 % retrive an element from the Term
    Row = [X|Row1],
    row_retrieved(T, Term, Row1).    % move to the next element from the Term
