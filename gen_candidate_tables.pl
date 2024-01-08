% Purpose: Test program
% Author : Nicolas Beldiceanu, IMT Atlantique

:- module(gen_candidate_tables,[gen_candidate_tables/5,
                                gen_table_and_metadata_file_names/5,
                                remove_signature_and_table_facts/1,
                                remove_metadata_facts/1]).

:- use_module(library(clpfd)).
:- use_module(tables).
:- use_module(table_access).
:- use_module(utility).

% only used when find conjectures for combinatorial objects: on backtrack produce table names for which we will search conjectures
gen_candidate_tables(Table, KindCombi, KindBound, Bound, NParam) :-
    (KindCombi = graph ->
        CorruptedTables = [low_c_minc_maxc_mins_maxs,
                           low_c_minc_mins_maxs,
                           low_maxc_mins_maxs,
                           low_maxc_mins_maxs_minc,
                           low_maxc_mins_minc,
                           low_maxc_s_mins_maxs_mina,
                           low_maxc_s_mins_maxs_minc,
                           low_maxc_s_mins_minc,
                           low_minc_maxc_maxs_mins,
                           low_minc_maxc_mins_maxs,
                           low_minc_maxc_s_mins_maxs_mina,
                           low_minc_mins_maxs,
                           low_minc_mins_maxs_maxc,
                           low_minc_s_maxc,
                           low_minc_s_maxs_maxc,
                           low_minc_s_mins_maxc,
                           low_minc_s_mins_maxs_maxc,
                           up_c_maxs_mins,
                           up_c_minc_maxs_mins,
                           up_maxc_maxs_mins,
                           up_maxc_mins_maxs,
                           up_maxc_mins_maxs_minc,
                           up_maxc_s_c,
                           up_maxc_s_maxs,
                           up_maxc_s_minc,
                           up_maxc_s_mins,
                           up_maxc_s_mins_c,
                           up_maxc_s_mins_maxs,
                           up_maxc_s_mins_minc,
                           up_minc_maxc_maxs_mins,
                           up_minc_maxc_mins_maxs,
                           up_minc_maxc_s_c,
                           up_minc_maxc_s_maxs,
                           up_minc_maxc_s_mins,
                           up_minc_maxc_s_mins_c,
                           up_minc_maxc_s_mins_maxs,
                           up_minc_maxs_mins,
                           up_minc_s_c,
                           up_minc_s_mins_c]
    ;
     memberchk(KindCombi, [forest,forest0,tree,partition,partition0,group,cgroup]) ->
        CorruptedTables = []
    ;
        write(gen_candidate_tables(KindCombi)), nl, halt
    ),
    (fd_var(NParam) -> indomain(NParam) ; true),                 % first enumerate on the nber.of input params.of the table if NParam is an fd.var.
    attributes_combi(KindCombi, KindBounds),                     % select the kind of bound for which search a formula
    member(KindBound, KindBounds),
    low_up_attributes_combi(KindCombi, ListPairsBoundAttribute),
    ((memberchk(low-KindBound, ListPairsBoundAttribute), \+memberchk(up-KindBound,  ListPairsBoundAttribute)) -> Bound = low(_)                ;
     (memberchk(up-KindBound,  ListPairsBoundAttribute), \+memberchk(low-KindBound, ListPairsBoundAttribute)) -> Bound = up(_)                 ;
                                                                                                                 member(Bound, [low(_), up(_)])),

% member(Table, [up_v_maxc_mins_maxa, up_v_maxc_s_maxa, up_v_minc_maxc_maxa]),
% Table = low_c_mins_maxs_maxc, % check this table later, after adding new conditionals
% Table = low_v_mins_mina,
% Table = low_v_maxp_mint_minf,
% Table = low_c_minc_mins_s,
% Table = up_maxc_s_maxs_c,
% Table = low_c_maxc_mina,
% Table = low_n_min_max, % TO REMOVE
% Table = low_n_min_sum_squares_max,
% Table = up_n_ng_dmin_nv,
% Table = up_n_ng_dmax_nv,
% Table = up_n_dmin_dsum_squares_nv,
% Table = low_n_nval_min_max_sum_squares,

    get_tables(KindCombi, 2, NParam, Bound, TableNames),         % get all the table names (using the smallest size 2)
    member(Table, TableNames),                                   % select one of the tables
    atom_proper_suffix_add_underscore(Table, KindBound),         % check that the table is about a bound of type KindBound
    (memberchk(Table, CorruptedTables) -> false ; true).         % no sense to try a generated formula for a known corrupted table

gen_table_and_metadata_file_names(Directory, MaxN, Table, TableFile, MetadataFile) :-
	number_codes(MaxN, CodesMaxN),                               % convert MaxN to an atom
	atom_codes(AtomMaxN, CodesMaxN),                             % as it will be part of the name of the directory where to access data
	atom_concat(Directory, AtomMaxN, PrefixName0),               % prefix of the directory where data and metadata are recorded (include size)
	atom_concat(PrefixName0, '/', PrefixName),                   % concatenate the
	atom_concat(PrefixName, Table, PrefixNameTable),             % table name
	atom_concat(PrefixNameTable, '.pl', TableFile),              % path to the file containing the table
	atom_concat(PrefixNameTable, '_metadata.pl', MetadataFile).  % path to the file containing the metadata about the table

remove_signature_and_table_facts(Table) :-
    user:signature(Table, _, Args),                              % from the signature fact of Table get information on the columns of the table
    functor(Args, t, NbColumns),                                 % get number of columns of the table
    functor(Term, Table, NbColumns),                             % create a term whose functor is the table name and whose arity is NbColumns
    retractall(user:signature(_,_,_)),                           % remove the signature fact as finish to deal with current table
    retractall(user:Term).                                       % remove all rows of the current table as finish to deal with current table

remove_metadata_facts(Table) :-                                  % remove current metadata fact to reclaim space
    get_metadata_arity(Arity),                                   % get arity of metadata fact
    functor(TermMetadata, table_metadata, Arity),                % construct a term with variables for the metadata fact
    get_metadata_attributes(ListIndAttr),                        % get list of pairs of index and attribute name
    memberchk(I-table_name, ListIndAttr),                        % extract index of table name attribute
    arg(I, TermMetadata, Table),                                 % set argument corresponding to the table name to the proper table name
    retractall(user:TermMetadata).                               % remove metadata fact of table Table to reclaim space
