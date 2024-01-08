% Purpose: Tables giving the lower/upper bound of a partition characteristics wrt a set of partition characteristics
% Warning: should be added within the tables.pl and tables_list.pl file containing all the tables of all combinatorial objects.
%          do not forget to add both the max_size_combi/2 in tables.pl and the table/5 in tables_list.pl facts.
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(tables,[attributes_combi/2,
                  low_up_attributes_combi/2,
                  max_size_combi/2,
                  get_tables/5,
                  get_table/5,
                  table/6,
                  extract_index_names_from_table_signature/3]).

:- use_module(tables_list).

attributes_combi(graph,      [v,c,minc,maxc,s,mins,maxs,mina,maxa]).                                 % digraph (partition of partition)
attributes_combi(forest,     [v,t,f,minf,maxf,mind,maxd,minp,maxp,mint,maxt]).                       % forest
attributes_combi(forest0,    [v,t,f,minf,maxf,mind,maxd,minp,maxp,mint,maxt]).                       % forest without isolated vertex
attributes_combi(tree,       [v,f,mind,maxd,minp,maxp]).                                             % one single tree
attributes_combi(partition,  [n,nval,min,max,range,sum_squares]).                                    % partition (extend nvalue)
attributes_combi(partition0, [n,m,nval,min,max,range,sum_squares]).                                  % partition (extend nvalue but do not consider unsued values)
attributes_combi(group,      [n,ng,nv,smin,smax,srange,ssum_squares,dmin,dmax,drange,dsum_squares]). % group constraint
attributes_combi(cgroup,     [n,ng,nv,smin,smax,srange,ssum_squares,dmin,dmax,drange,dsum_squares]). % cyclic group constraint

low_up_attributes_combi(graph,      [low-v,low-c,low-minc,low-maxc,low-s,low-mins,low-maxs,low-mina,
                                     up-v, up-c, up-minc, up-maxc, up-s, up-mins, up-maxs, up-maxa]).
low_up_attributes_combi(forest,     [low-t,low-f,low-minf,low-maxf,low-mind,low-maxd,low-minp,low-maxp,low-mint,low-maxt,
                                     up-t, up-f, up-minf, up-maxf, up-mind, up-maxd, up-minp, up-maxp, up-mint, up-maxt]).
low_up_attributes_combi(forest0,     [low-t,low-f,low-minf,low-maxf,low-mind,low-maxd,low-minp,low-maxp,low-mint,low-maxt,
                                     up-t, up-f, up-minf, up-maxf, up-mind, up-maxd, up-minp, up-maxp, up-mint, up-maxt]).
low_up_attributes_combi(tree,       [low-f,low-mind,low-maxd,low-minp,low-maxp,
                                     up-f, up-mind, up-maxd, up-minp, up-maxp]).
low_up_attributes_combi(partition,  [low-nval,low-min,low-max,low-range,low-sum_squares,
                                     up-nval, up-min, up-max, up-range, up-sum_squares]).
low_up_attributes_combi(partition0, [low-nval,low-min,low-max,low-range,low-sum_squares,
                                     up-nval, up-min, up-max, up-range, up-sum_squares]).
low_up_attributes_combi(group,      [low-ng,low-nv,low-smin,low-smax,low-srange,low-ssum_squares,low-dmin,low-dmax,low-drange,low-dsum_squares,
                                     up-ng, up-nv, up-smin, up-smax, up-srange, up-ssum_squares, up-dmin, up-dmax, up-drange, up-dsum_squares]).
low_up_attributes_combi(cgroup,     [low-ng,low-nv,low-smin,low-smax,low-srange,low-ssum_squares,low-dmin,low-dmax,low-drange,low-dsum_squares,
                                     up-ng, up-nv, up-smin, up-smax, up-srange, up-ssum_squares, up-dmin, up-dmax, up-drange, up-dsum_squares]).

max_size_combi(model_seeker,  0). % no nodes when in model_seeker mode
max_size_combi(graph,        26). % maximum number of nodes for which we generate graphs
max_size_combi(forest,       18). % maximum number of nodes for which we generate forests
max_size_combi(forest0,      18). % maximum number of nodes for which we generate forests
max_size_combi(tree,         18). % maximum number of nodes for which we generate trees
max_size_combi(partition,    30). % maximum number of elements for which we generate partitions
max_size_combi(partition0,   30). % maximum number of elements for which we generate partitions0
max_size_combi(group,        30). % maximum number of elements for which we generate groups
max_size_combi(cgroup,       30). % maximum number of elements for which we generate circular groups

get_tables(KindCombi, MaxN, NParam, BoundOrOptions, TableNames) :-
    findall(Name, table(KindCombi,Name,MaxN,_,NParam,BoundOrOptions), TableNames).

get_table(KindCombi, MaxN, NParam, BoundOrOptions, TableName) :-
    table(KindCombi, TableName, MaxN, _, NParam, BoundOrOptions),
    !. % normally table names should be distinct, but who knows

% Each table has the following arguments:
%  . the type of combinatorial object (graph forest, forest0, tree, partition, partition0, group, cgroup) or the fact that we use the model seeker (model_seeker)
%  . the name of the table
%  . the list of pairs of the form MaxN-NCol where
%     - MaxN is the maximum number of elements used to generate a table
%     - NCol is the corresponding number of columns of the same table
%  . the number of parameters of the lower/upper bound
%  . the lower/upper bound considered
table(KindCombi, Name, MaxN, NCol, NParam, BoundOrOptions) :-
    table(KindCombi, Name, ListMaxNNCol, NParam, BoundOrOptions),
    member(MaxN-NCol, ListMaxNNCol).

extract_index_names_from_table_signature(Args, Names, Indices) :-
    functor(Args, _, NbCols),
    extract_index_name_from_signatures(Names, NbCols, Args, Indices).

extract_index_name_from_signatures([], _, _, []) :- !.
extract_index_name_from_signatures([Name|R], NbCols, Args, [Index|S]) :-
    extract_index_name_from_signature(Name, 1, NbCols, Args, Index),
    extract_index_name_from_signatures(R, NbCols, Args, S).

extract_index_name_from_signature(Name, Index, NbCols, Args, Index) :-
    Index =< NbCols,
    arg(Index, Args, SubTerm),
    arg(1, SubTerm, Name), % assumes name is the first argument
    !.
extract_index_name_from_signature(Name, I, NbCols, Args, Index) :-
    (I = NbCols ->
        write(extract_index_name_from_signature(Name)), nl, halt
    ;
        I1 is I+1,
        extract_index_name_from_signature(Name, I1, NbCols, Args, Index)
    ).
