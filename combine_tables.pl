% Purpose: Merge two (or potentially more) data tables into one file using the information from primary and foreign keys of these tables
% Author : Ramiz Gindullin, IMT Atlantique

:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(table_access).

% for test purposes
% combine_tables(fktesttable2, fktesttable1, 1, TableNew, _).
% combine_tables(techreviewtab1, techreviewtab2, 0, TableNew, _).
% findall(X, (functor(X, fktesttable2_fktesttable1, 7), call(X)), Y), write(Y).
top:-
    combine_tables(use_case_tasks_sc1, use_case_machines, 1, _, _),
    combine_tables(use_case_tasks_sc2, use_case_machines, 1, _, _),
    combine_tables(use_case_tasks_sc3, use_case_machines, 1, _, _).

% INPUTS:
%       TableChild      - the name of the child table
%       TableParent     - the name of the parent table
%       IncludeFK       - a flag that determines if we include (IncludeFK = 1) or exclude (IncludeFK /= 1) FK columns from the new table; . 
% OUTPUTS (optional):
%       TableNew        - the name of the combined table, in the format of TableChild_TableParent
%       ColList         - the list that stores information about where each column of the table TableNew is taken from (tableChild or tableParent)
%
% Assumptions:
% ------------
% . all columns of the parent tables should be input columns
% . at least one column of the child table should be an output
combine_tables(TableChild, TableParent, IncludeFK, TableNew, ColList):-
    % load tables TableChild and TableParent with metadata
    PrefixName = 'data/model_seeker/data0/',
    ColsInfoFactName = merge_table_info,
    atom_concat(PrefixName, TableChild, ChildPrefixNameFunctor), atom_concat(PrefixName, TableParent, ParentPrefixNameFunctor),
    atom_concat(ChildPrefixNameFunctor, '.pl', ChildConsultFile), atom_concat(ParentPrefixNameFunctor, '.pl', ParentConsultFile),
    atom_concat(ChildPrefixNameFunctor, '_metadata.pl', ChildMetadataFile), atom_concat(ParentPrefixNameFunctor, '_metadata.pl', ParentMetadataFile),
    consult(ChildConsultFile), consult(ParentConsultFile),
    consult(ChildMetadataFile), consult(ParentMetadataFile),
    
    % Obtain relevant metadata from child and parent tables
    tab_get_arity(col(TableChild,_), NChild),                   % get the number of columns of the table TableChild 
    tab_get_arity(col(TableParent,_), NParent),                 % get the number of columns of the table TableParent
    tab_get_pks(col(TableChild,_), [_-Pk|_]),                   % get the primary key of the table TableChild
    tab_get_fks(col(TableChild,_), AllFks),                     % get the all foreign keys of the table TableChild
                                                                % IMPORTANT: the main limitation of the current pipeline - it is assumed,
                                                                % that all foreign keys will consist of only 1 column. Yes, we can combine two tables with
                                                                % compound foreign keys, this step is trivial, but during coversion of the metadata to MiniZinc
                                                                % model it can only recognise single column foreign keys.
                                                                % the support for multi-column foreign keys is too complex for the potential usability of such case 
    memberchk([ind(_,pk(TableParent,Fk1),fk(Fk2))|_], AllFks),  % select only the foreign key that connects tables TableChild and TableParent
    length(Fk1, FKLength),                                      % get the length of said foreign key

    ((IncludeFK = 1) ->
        findall(col(TableChild,I),
                between(1,NChild,I), ChildList)                 % generate the list ChildList of columns from the table TableChild,
    ;
        findall(col(TableChild,I), (between(1,NChild,I),        % generate the list ChildList of columns from the table TableChild,
                nonmember(I, Fk2)), ChildList)                  % except columns that are part of the foreign key Fk2
    ),
    findall(col(TableParent,I), (between(1,NParent,I),          % generate the list ParentList of columns from the table TableParent,
                nonmember(I, Fk1)), ParentList),                % except columns that are part of the primary key (Fk1) of the table TableParent
    
    append(ChildList,ParentList,ColList),                       % The list ColList stores the information about the table TableNew, where
    %write(collist-ColList), nl,                                % for each element of the list stores the name of the source table and the source column number
                                                                % of the source table; later it could be used for looking up the relevant metadata
    ((IncludeFK = 1) ->               
        N is NChild + NParent - FKLength                        % calculate the number of the columns in the table TableNew
    ;
        N is NChild + NParent - 2 * FKLength                    % calculate the number of the columns in the table TableNew; columns from the foreign key are excluded
    ),
    N1 is N + 1,
    
                           
    atom_concat(TableChild, '_', TableNew0),                    % generate the name of the table TableNew in the format TableChild_TableParent
    atom_concat(TableNew0, TableParent, TableNew),
    atom_concat(TableNew, '.pl', TableNewFileWOPrefix),
    atom_concat(PrefixName, TableNewFileWOPrefix, TableNewFile),
    open(TableNewFile, write, SOut),
    format(SOut, ':- multifile signature/3.~n:- multifile ~w/~w.~n:- multifile ~w/~w.~n:- dynamic signature/3.~n:- dynamic ~w/~w.~n:- dynamic ~w/~w.~n~n',
           [TableNew,N,ColsInfoFactName,N1,TableNew,N,ColsInfoFactName,N1]),
    
    functor(Signature, signature, 3),                           % generate signature for table TableNew
    arg(1, Signature, TableNew),                                % record name of the table TableNew
    arg(2, Signature, 0),                                       % record the fact that it will be used for the ASSISTANT
    functor(ColumnTerms,t,N),                                   % create a term for storing information about columns
    (foreach(col(TableSignature,ColSignature), ColList), for(ISignature,1,N),
     param(ColumnTerms), param(TableChild)                      % fill the term ColumnTerms with information about columns
     do (functor(ColumnTerm,t,3),                               % create a term for storing information about column I 
         tab_get_name(col(TableSignature,ColSignature), Name),  % get the name of column I
         arg(1, ColumnTerm, Name),                              % record the name of column I
         (TableSignature = TableChild ->                        % if the column I is taken from table TableChild
                tab_get_kind(col(TableSignature,ColSignature),
                              Kind),                            % get the kind (primary/secondary) of the column I
                tab_get_inout(col(TableSignature,ColSignature),
                              InOut)                            % get the type (input/output) of the column  I
         ;
          Kind = primary,                                       % otherwise a column from a parent table will always be 
          InOut = input),                                       % a primary column and input column (i.e. can not be an output)
         arg(2, ColumnTerm, Kind),                              % record the kind of the column I
         arg(3, ColumnTerm, InOut),                             % record the type of the column I
         arg(ISignature, ColumnTerms, ColumnTerm))),            % place the term ColumnTerm with the term ColumnTerms
    arg(3, Signature, ColumnTerms),                             % record the term ColumnTerms within the therm Singature
    format(SOut, '~w.~n~n', Signature),
    %write(Signature),nl,
/*
    Fact ColsInfo stores metadata information we need to put on a merged table:
        ------------------------------------------------------
        Information for each column:
        ----------------------------
        . table     : table name
        . type_table: type of table from which the column comes from: parent or child
                      (focus on one child and one or more parents)
        . type_column: primary | % primary key of a child  table (e.g. task id)
                       foreign | % primary key of a parent table (e.g. machine id)
                       data      % neither primary nor foreign
        . column     : position of original column
        
        t(table,type_table,type_column,column)
        
        Assumptions:
        ------------
        . all columns of the parent tables should be input columns
        . at least one column of the child table should be an output
*/
    functor(ColsInfo, ColsInfoFactName, N1),
    arg(1,ColsInfo,TableNew),
    (foreach(col(TableInfo, ColIn), ColList), for(IInfo,2,N1), param(ColsInfo), param([TableChild,TableParent,Pk,Fk2, Fk1])
    do
     arg(IInfo, ColsInfo, ColInfo),
     (TableInfo = TableChild ->
        TypeTable = child,
        (memberchk(ColIn,Pk)  -> TypeColumn = primary                   ;
         memberchk(ColIn,Fk2) -> TypeColumn = foreign(TableParent,Fk1)  ;
                                 TypeColumn = data                      )
     ;
        TypeTable = parent,
        TypeColumn = data
     ),
     ColInfo = t(TableInfo,TypeTable,TypeColumn,ColIn)
    ),
    %write(ColsInfo),nl,
    format(SOut, '~w.~n~n', ColsInfo),


    findall(RowNew, (functor(RowChild, TableChild, NChild), call(RowChild),           % select a row RowChild from the table TableChild
                     functor(RowParent, TableParent, NParent), call(RowParent),       % select a row RowParent from the table TableParent
                     ( foreach(I1, Fk2), foreach(I2, Fk1), param(RowChild), param(RowParent)
                       do (arg(I1, RowChild, Value),                                  % check all the foreign key columns of tables TableChild and TableParent
                           arg(I2, RowParent, Value)                                  % and make sure that they contain same values within selected rows
                          )                                                           % RowChild and RowParent
                     ),
                     functor(RowNew,TableNew,N),                                      % after corresponding rows from TableChild and TableParent are found
                                                                                      % generate a new row RowNew for the TableNew
                     (foreach(col(Table,Col), ColList),for(I,1,N),                    % find a source table name Table from the list ColList 
                      param(RowNew), param(RowChild), param(RowParent)
                      %TODO: add support for several parent tables
                      do ((Table = TableChild -> arg(Col, RowChild,  Value);          % if source column from TableChild, select the corresponding value from RowChild 
                                                 arg(Col, RowParent, Value)),         % if source column from TableParent, select the corresponding value from RowParent
                          arg(I, RowNew, Value)                                       % write the selected value into RowNew
                         )
                     ),
                     %write(RowNew),nl,
                     format(SOut, '~w.~n', RowNew),
                     %assert(RowNew),                                                 % after the row RowNew is generated, assert it for further use
                     true), _),
        close(SOut),
        write(table(TableNew,0,N,0,[pk(Pk),no_fk])),write('.'),nl,
        remove_signature_and_table_facts(TableChild, TableParent).


% copied from [main.pl] and enhanced for two tables
remove_signature_and_table_facts(TableChild, TableParent) :-
    signature(TableChild, _, ArgsChild),                                % from the signature fact of Table get information on the columns of the table
    functor(ArgsChild, t, NbColumnsChild),                              % get number of columns of the table
    functor(TermChild, TableChild, NbColumnsChild),                     % create a term whose functor is the table name and whose arity is NbColumns
    signature(TableParent, _, ArgsParent),                              % from the signature fact of Table get information on the columns of the table
    functor(ArgsParent, t, NbColumnsParent),                            % get number of columns of the table
    functor(TermParent, TableParent, NbColumnsParent),                  % create a term whose functor is the table name and whose arity is NbColumns
    retractall(signature(_,_,_)),                                       % remove the signature fact as finish to deal with current table
    retractall(TermChild),
    retractall(TermParent).                                             % remove all rows of the current table as finish to deal with current table
