% Purpose: Main program to run to generate conjectures (in the context of combinatorial objects) or
%          to acquire constraints (in the context of ASSISTANT)
% Author : Nicolas Beldiceanu, IMT Atlantique

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(timeout)).
:- use_module(tables).
:- use_module(table_access).
:- use_module(utility).
:- use_module(stat_utility).
:- use_module(gen_bool_formula).
:- use_module(gen_formula).
:- use_module(gen_formula_term).
:- use_module(gen_formula_write).
:- use_module(gen_candidate_tables).
:- use_module(gen_options_for_formulae).
:- use_module(eval_formula).
:- use_module(instrument_code).

:- multifile table_metadata/31.
:- dynamic table_metadata/31.

:- multifile max_cost/1.
:- dynamic max_cost/1.

%set_prolog_flag(informational,off).

% use to compute average size of tables used in the experiments wrt a given combinatorial object
average_size(KindCombi) :-
        atoms_concat(['data/',KindCombi,'/meta_metadata_',KindCombi,'.pl'], FileName),
        consult(FileName),                                              % read proper file containing size_to_compute_conjectures facts
        findall(MaxN, (gen_candidate_tables(Table, KindCombi, _, _, _),
                       write(Table), nl,
                       size_to_compute_conjectures(Table,_,MaxN,_)), LSizes),
        sumlist(LSizes, Sum),
        length(LSizes, Len),
        Av is Sum / Len,
        write(t(sum,Sum,len,Len,av,Av)), nl,
        retractall(size_to_compute_conjectures(_,_,_,_)).

top(0) :-
    !,
    top(model_seeker, assistant, 0, 0, 0, 1, 1).
top(1) :-
    g8.                              % by default search conjectures for the graphs for mina with 1 and 2 input parameters


ms :- ms(1,1).
% Model seeker mode, e.g. ms(1000,1) -- ms(1000,1000)
ms(SplitDiv, SplitMod)  :- top(model_seeker, assistant, 0, 0, 0, SplitDiv, SplitMod).

g1  :- top(graph,  v,    low(v),    1, 2, 1, 1).
g2  :- top(graph,  c,    low(c),    1, 2, 1, 1).
g3  :- top(graph,  minc, low(minc), 1, 2, 1, 1).
g4  :- top(graph,  maxc, low(maxc), 1, 2, 1, 1).
g5  :- top(graph,  s,    low(s),    1, 2, 1, 1).
g6  :- top(graph,  mins, low(mins), 1, 2, 1, 1).
g7  :- top(graph,  maxs, low(maxs), 1, 2, 1, 1).
g8  :- top(graph,  mina, low(mina), 1, 2, 1, 1).
g9  :- top(graph,  v,    up(v),     1, 2, 1, 1).
g10 :- top(graph,  c,    up(c),     1, 2, 1, 1).
g11 :- top(graph,  minc, up(minc),  1, 2, 1, 1).
g12 :- top(graph,  maxc, up(maxc),  1, 2, 1, 1).
g13 :- top(graph,  s,    up(s),     1, 2, 1, 1).
g14 :- top(graph,  mins, up(mins),  1, 2, 1, 1).
g15 :- top(graph,  maxs, up(maxs),  1, 2, 1, 1).
g16 :- top(graph,  maxa, up(maxa),  1, 2, 1, 1).

g1(NParam)  :- top(graph,  v,    low(v),    NParam, NParam, 1, 1).
g2(NParam)  :- top(graph,  c,    low(c),    NParam, NParam, 1, 1).
g3(NParam)  :- top(graph,  minc, low(minc), NParam, NParam, 1, 1).
g4(NParam)  :- top(graph,  maxc, low(maxc), NParam, NParam, 1, 1).
g5(NParam)  :- top(graph,  s,    low(s),    NParam, NParam, 1, 1).
g6(NParam)  :- top(graph,  mins, low(mins), NParam, NParam, 1, 1).
g7(NParam)  :- top(graph,  maxs, low(maxs), NParam, NParam, 1, 1).
g8(NParam)  :- top(graph,  mina, low(mina), NParam, NParam, 1, 1).
g9(NParam)  :- top(graph,  v,    up(v),     NParam, NParam, 1, 1).
g10(NParam) :- top(graph,  c,    up(c),     NParam, NParam, 1, 1).
g11(NParam) :- top(graph,  minc, up(minc),  NParam, NParam, 1, 1).
g12(NParam) :- top(graph,  maxc, up(maxc),  NParam, NParam, 1, 1).
g13(NParam) :- top(graph,  s,    up(s),     NParam, NParam, 1, 1).
g14(NParam) :- top(graph,  mins, up(mins),  NParam, NParam, 1, 1).
g15(NParam) :- top(graph,  maxs, up(maxs),  NParam, NParam, 1, 1).
g16(NParam) :- top(graph,  maxa, up(maxa),  NParam, NParam, 1, 1).

g1(MinNParam, MaxNParam)  :- top(graph,  v,    low(v),    MinNParam, MaxNParam, 1, 1).
g2(MinNParam, MaxNParam)  :- top(graph,  c,    low(c),    MinNParam, MaxNParam, 1, 1).
g3(MinNParam, MaxNParam)  :- top(graph,  minc, low(minc), MinNParam, MaxNParam, 1, 1).
g4(MinNParam, MaxNParam)  :- top(graph,  maxc, low(maxc), MinNParam, MaxNParam, 1, 1).
g5(MinNParam, MaxNParam)  :- top(graph,  s,    low(s),    MinNParam, MaxNParam, 1, 1).
g6(MinNParam, MaxNParam)  :- top(graph,  mins, low(mins), MinNParam, MaxNParam, 1, 1).
g7(MinNParam, MaxNParam)  :- top(graph,  maxs, low(maxs), MinNParam, MaxNParam, 1, 1).
g8(MinNParam, MaxNParam)  :- top(graph,  mina, low(mina), MinNParam, MaxNParam, 1, 1).
g9(MinNParam, MaxNParam)  :- top(graph,  v,    up(v),     MinNParam, MaxNParam, 1, 1).
g10(MinNParam, MaxNParam) :- top(graph,  c,    up(c),     MinNParam, MaxNParam, 1, 1).
g11(MinNParam, MaxNParam) :- top(graph,  minc, up(minc),  MinNParam, MaxNParam, 1, 1).
g12(MinNParam, MaxNParam) :- top(graph,  maxc, up(maxc),  MinNParam, MaxNParam, 1, 1).
g13(MinNParam, MaxNParam) :- top(graph,  s,    up(s),     MinNParam, MaxNParam, 1, 1).
g14(MinNParam, MaxNParam) :- top(graph,  mins, up(mins),  MinNParam, MaxNParam, 1, 1).
g15(MinNParam, MaxNParam) :- top(graph,  maxs, up(maxs),  MinNParam, MaxNParam, 1, 1).
g16(MinNParam, MaxNParam) :- top(graph,  maxa, up(maxa),  MinNParam, MaxNParam, 1, 1).

g1(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  v,    low(v),    MinNParam, MaxNParam, SplitDiv, SplitMod).
g2(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  c,    low(c),    MinNParam, MaxNParam, SplitDiv, SplitMod).
g3(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  minc, low(minc), MinNParam, MaxNParam, SplitDiv, SplitMod).
g4(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  maxc, low(maxc), MinNParam, MaxNParam, SplitDiv, SplitMod).
g5(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  s,    low(s),    MinNParam, MaxNParam, SplitDiv, SplitMod).
g6(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  mins, low(mins), MinNParam, MaxNParam, SplitDiv, SplitMod).
g7(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  maxs, low(maxs), MinNParam, MaxNParam, SplitDiv, SplitMod).
g8(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  mina, low(mina), MinNParam, MaxNParam, SplitDiv, SplitMod).
g9(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(graph,  v,    up(v),     MinNParam, MaxNParam, SplitDiv, SplitMod).
g10(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(graph,  c,    up(c),     MinNParam, MaxNParam, SplitDiv, SplitMod).
g11(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(graph,  minc, up(minc),  MinNParam, MaxNParam, SplitDiv, SplitMod).
g12(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(graph,  maxc, up(maxc),  MinNParam, MaxNParam, SplitDiv, SplitMod).
g13(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(graph,  s,    up(s),     MinNParam, MaxNParam, SplitDiv, SplitMod).
g14(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(graph,  mins, up(mins),  MinNParam, MaxNParam, SplitDiv, SplitMod).
g15(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(graph,  maxs, up(maxs),  MinNParam, MaxNParam, SplitDiv, SplitMod).
g16(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(graph,  maxa, up(maxa),  MinNParam, MaxNParam, SplitDiv, SplitMod).

f1  :- top(forest, t,    low(t),    1, 2, 1, 1).
f2  :- top(forest, f,    low(f),    1, 2, 1, 1).
f3  :- top(forest, minf, low(minf), 1, 2, 1, 1).
f4  :- top(forest, maxf, low(maxf), 1, 2, 1, 1).
f5  :- top(forest, mind, low(mind), 1, 2, 1, 1).
f6  :- top(forest, maxd, low(maxd), 1, 2, 1, 1).
f7  :- top(forest, minp, low(minp), 1, 2, 1, 1).
f8  :- top(forest, maxp, low(maxp), 1, 2, 1, 1).
f9  :- top(forest, mint, low(mint), 1, 2, 1, 1).
f10 :- top(forest, maxt, low(maxt), 1, 2, 1, 1).
f11 :- top(forest, t,    up(t),     1, 2, 1, 1).
f12 :- top(forest, f,    up(f),     1, 2, 1, 1).
f13 :- top(forest, minf, up(minf),  1, 2, 1, 1).
f14 :- top(forest, maxf, up(maxf),  1, 2, 1, 1).
f15 :- top(forest, mind, up(mind),  1, 2, 1, 1).
f16 :- top(forest, maxd, up(maxd),  1, 2, 1, 1).
f17 :- top(forest, minp, up(minp),  1, 2, 1, 1).
f18 :- top(forest, maxp, up(maxp),  1, 2, 1, 1).
f19 :- top(forest, mint, up(mint),  1, 2, 1, 1).
f20 :- top(forest, maxt, up(maxt),  1, 2, 1, 1).

f1(NParam)  :- top(forest, t,    low(t),    NParam, NParam, 1, 1).
f2(NParam)  :- top(forest, f,    low(f),    NParam, NParam, 1, 1).
f3(NParam)  :- top(forest, minf, low(minf), NParam, NParam, 1, 1).
f4(NParam)  :- top(forest, maxf, low(maxf), NParam, NParam, 1, 1).
f5(NParam)  :- top(forest, mind, low(mind), NParam, NParam, 1, 1).
f6(NParam)  :- top(forest, maxd, low(maxd), NParam, NParam, 1, 1).
f7(NParam)  :- top(forest, minp, low(minp), NParam, NParam, 1, 1).
f8(NParam)  :- top(forest, maxp, low(maxp), NParam, NParam, 1, 1).
f9(NParam)  :- top(forest, mint, low(mint), NParam, NParam, 1, 1).
f10(NParam) :- top(forest, maxt, low(maxt), NParam, NParam, 1, 1).
f11(NParam) :- top(forest, t,    up(t),     NParam, NParam, 1, 1).
f12(NParam) :- top(forest, f,    up(f),     NParam, NParam, 1, 1).
f13(NParam) :- top(forest, minf, up(minf),  NParam, NParam, 1, 1).
f14(NParam) :- top(forest, maxf, up(maxf),  NParam, NParam, 1, 1).
f15(NParam) :- top(forest, mind, up(mind),  NParam, NParam, 1, 1).
f16(NParam) :- top(forest, maxd, up(maxd),  NParam, NParam, 1, 1).
f17(NParam) :- top(forest, minp, up(minp),  NParam, NParam, 1, 1).
f18(NParam) :- top(forest, maxp, up(maxp),  NParam, NParam, 1, 1).
f19(NParam) :- top(forest, mint, up(mint),  NParam, NParam, 1, 1).
f20(NParam) :- top(forest, maxt, up(maxt),  NParam, NParam, 1, 1).

f1(MinNParam, MaxNParam)  :- top(forest, t,    low(t),    MinNParam, MaxNParam, 1, 1).
f2(MinNParam, MaxNParam)  :- top(forest, f,    low(f),    MinNParam, MaxNParam, 1, 1).
f3(MinNParam, MaxNParam)  :- top(forest, minf, low(minf), MinNParam, MaxNParam, 1, 1).
f4(MinNParam, MaxNParam)  :- top(forest, maxf, low(maxf), MinNParam, MaxNParam, 1, 1).
f5(MinNParam, MaxNParam)  :- top(forest, mind, low(mind), MinNParam, MaxNParam, 1, 1).
f6(MinNParam, MaxNParam)  :- top(forest, maxd, low(maxd), MinNParam, MaxNParam, 1, 1).
f7(MinNParam, MaxNParam)  :- top(forest, minp, low(minp), MinNParam, MaxNParam, 1, 1).
f8(MinNParam, MaxNParam)  :- top(forest, maxp, low(maxp), MinNParam, MaxNParam, 1, 1).
f9(MinNParam, MaxNParam)  :- top(forest, mint, low(mint), MinNParam, MaxNParam, 1, 1).
f10(MinNParam, MaxNParam) :- top(forest, maxt, low(maxt), MinNParam, MaxNParam, 1, 1).
f11(MinNParam, MaxNParam) :- top(forest, t,    up(t),     MinNParam, MaxNParam, 1, 1).
f12(MinNParam, MaxNParam) :- top(forest, f,    up(f),     MinNParam, MaxNParam, 1, 1).
f13(MinNParam, MaxNParam) :- top(forest, minf, up(minf),  MinNParam, MaxNParam, 1, 1).
f14(MinNParam, MaxNParam) :- top(forest, maxf, up(maxf),  MinNParam, MaxNParam, 1, 1).
f15(MinNParam, MaxNParam) :- top(forest, mind, up(mind),  MinNParam, MaxNParam, 1, 1).
f16(MinNParam, MaxNParam) :- top(forest, maxd, up(maxd),  MinNParam, MaxNParam, 1, 1).
f17(MinNParam, MaxNParam) :- top(forest, minp, up(minp),  MinNParam, MaxNParam, 1, 1).
f18(MinNParam, MaxNParam) :- top(forest, maxp, up(maxp),  MinNParam, MaxNParam, 1, 1).
f19(MinNParam, MaxNParam) :- top(forest, mint, up(mint),  MinNParam, MaxNParam, 1, 1).
f20(MinNParam, MaxNParam) :- top(forest, maxt, up(maxt),  MinNParam, MaxNParam, 1, 1).

f1(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, t,    low(t),    MinNParam, MaxNParam, SplitDiv, SplitMod).
f2(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, f,    low(f),    MinNParam, MaxNParam, SplitDiv, SplitMod).
f3(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, minf, low(minf), MinNParam, MaxNParam, SplitDiv, SplitMod).
f4(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, maxf, low(maxf), MinNParam, MaxNParam, SplitDiv, SplitMod).
f5(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, mind, low(mind), MinNParam, MaxNParam, SplitDiv, SplitMod).
f6(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, maxd, low(maxd), MinNParam, MaxNParam, SplitDiv, SplitMod).
f7(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, minp, low(minp), MinNParam, MaxNParam, SplitDiv, SplitMod).
f8(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, maxp, low(maxp), MinNParam, MaxNParam, SplitDiv, SplitMod).
f9(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest, mint, low(mint), MinNParam, MaxNParam, SplitDiv, SplitMod).
f10(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, maxt, low(maxt), MinNParam, MaxNParam, SplitDiv, SplitMod).
f11(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, t,    up(t),     MinNParam, MaxNParam, SplitDiv, SplitMod).
f12(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, f,    up(f),     MinNParam, MaxNParam, SplitDiv, SplitMod).
f13(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, minf, up(minf),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f14(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, maxf, up(maxf),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f15(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, mind, up(mind),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f16(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, maxd, up(maxd),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f17(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, minp, up(minp),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f18(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, maxp, up(maxp),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f19(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, mint, up(mint),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f20(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest, maxt, up(maxt),  MinNParam, MaxNParam, SplitDiv, SplitMod).

f01  :- top(forest0, t,    low(t),    1, 2, 1, 1).
f02  :- top(forest0, f,    low(f),    1, 2, 1, 1).
f03  :- top(forest0, minf, low(minf), 1, 2, 1, 1).
f04  :- top(forest0, maxf, low(maxf), 1, 2, 1, 1).
f05  :- top(forest0, mind, low(mind), 1, 2, 1, 1).
f06  :- top(forest0, maxd, low(maxd), 1, 2, 1, 1).
f07  :- top(forest0, minp, low(minp), 1, 2, 1, 1).
f08  :- top(forest0, maxp, low(maxp), 1, 2, 1, 1).
f09  :- top(forest0, mint, low(mint), 1, 2, 1, 1).
f010 :- top(forest0, maxt, low(maxt), 1, 2, 1, 1).
f011 :- top(forest0, t,    up(t),     1, 2, 1, 1).
f012 :- top(forest0, f,    up(f),     1, 2, 1, 1).
f013 :- top(forest0, minf, up(minf),  1, 2, 1, 1).
f014 :- top(forest0, maxf, up(maxf),  1, 2, 1, 1).
f015 :- top(forest0, mind, up(mind),  1, 2, 1, 1).
f016 :- top(forest0, maxd, up(maxd),  1, 2, 1, 1).
f017 :- top(forest0, minp, up(minp),  1, 2, 1, 1).
f018 :- top(forest0, maxp, up(maxp),  1, 2, 1, 1).
f019 :- top(forest0, mint, up(mint),  1, 2, 1, 1).
f020 :- top(forest0, maxt, up(maxt),  1, 2, 1, 1).

f01(NParam)  :- top(forest0, t,    low(t),    NParam, NParam, 1, 1).
f02(NParam)  :- top(forest0, f,    low(f),    NParam, NParam, 1, 1).
f03(NParam)  :- top(forest0, minf, low(minf), NParam, NParam, 1, 1).
f04(NParam)  :- top(forest0, maxf, low(maxf), NParam, NParam, 1, 1).
f05(NParam)  :- top(forest0, mind, low(mind), NParam, NParam, 1, 1).
f06(NParam)  :- top(forest0, maxd, low(maxd), NParam, NParam, 1, 1).
f07(NParam)  :- top(forest0, minp, low(minp), NParam, NParam, 1, 1).
f08(NParam)  :- top(forest0, maxp, low(maxp), NParam, NParam, 1, 1).
f09(NParam)  :- top(forest0, mint, low(mint), NParam, NParam, 1, 1).
f010(NParam) :- top(forest0, maxt, low(maxt), NParam, NParam, 1, 1).
f011(NParam) :- top(forest0, t,    up(t),     NParam, NParam, 1, 1).
f012(NParam) :- top(forest0, f,    up(f),     NParam, NParam, 1, 1).
f013(NParam) :- top(forest0, minf, up(minf),  NParam, NParam, 1, 1).
f014(NParam) :- top(forest0, maxf, up(maxf),  NParam, NParam, 1, 1).
f015(NParam) :- top(forest0, mind, up(mind),  NParam, NParam, 1, 1).
f016(NParam) :- top(forest0, maxd, up(maxd),  NParam, NParam, 1, 1).
f017(NParam) :- top(forest0, minp, up(minp),  NParam, NParam, 1, 1).
f018(NParam) :- top(forest0, maxp, up(maxp),  NParam, NParam, 1, 1).
f019(NParam) :- top(forest0, mint, up(mint),  NParam, NParam, 1, 1).
f020(NParam) :- top(forest0, maxt, up(maxt),  NParam, NParam, 1, 1).

f01(MinNParam, MaxNParam)  :- top(forest0, t,    low(t),    MinNParam, MaxNParam, 1, 1).
f02(MinNParam, MaxNParam)  :- top(forest0, f,    low(f),    MinNParam, MaxNParam, 1, 1).
f03(MinNParam, MaxNParam)  :- top(forest0, minf, low(minf), MinNParam, MaxNParam, 1, 1).
f04(MinNParam, MaxNParam)  :- top(forest0, maxf, low(maxf), MinNParam, MaxNParam, 1, 1).
f05(MinNParam, MaxNParam)  :- top(forest0, mind, low(mind), MinNParam, MaxNParam, 1, 1).
f06(MinNParam, MaxNParam)  :- top(forest0, maxd, low(maxd), MinNParam, MaxNParam, 1, 1).
f07(MinNParam, MaxNParam)  :- top(forest0, minp, low(minp), MinNParam, MaxNParam, 1, 1).
f08(MinNParam, MaxNParam)  :- top(forest0, maxp, low(maxp), MinNParam, MaxNParam, 1, 1).
f09(MinNParam, MaxNParam)  :- top(forest0, mint, low(mint), MinNParam, MaxNParam, 1, 1).
f010(MinNParam, MaxNParam) :- top(forest0, maxt, low(maxt), MinNParam, MaxNParam, 1, 1).
f011(MinNParam, MaxNParam) :- top(forest0, t,    up(t),     MinNParam, MaxNParam, 1, 1).
f012(MinNParam, MaxNParam) :- top(forest0, f,    up(f),     MinNParam, MaxNParam, 1, 1).
f013(MinNParam, MaxNParam) :- top(forest0, minf, up(minf),  MinNParam, MaxNParam, 1, 1).
f014(MinNParam, MaxNParam) :- top(forest0, maxf, up(maxf),  MinNParam, MaxNParam, 1, 1).
f015(MinNParam, MaxNParam) :- top(forest0, mind, up(mind),  MinNParam, MaxNParam, 1, 1).
f016(MinNParam, MaxNParam) :- top(forest0, maxd, up(maxd),  MinNParam, MaxNParam, 1, 1).
f017(MinNParam, MaxNParam) :- top(forest0, minp, up(minp),  MinNParam, MaxNParam, 1, 1).
f018(MinNParam, MaxNParam) :- top(forest0, maxp, up(maxp),  MinNParam, MaxNParam, 1, 1).
f019(MinNParam, MaxNParam) :- top(forest0, mint, up(mint),  MinNParam, MaxNParam, 1, 1).
f020(MinNParam, MaxNParam) :- top(forest0, maxt, up(maxt),  MinNParam, MaxNParam, 1, 1).

f01(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, t,    low(t),    MinNParam, MaxNParam, SplitDiv, SplitMod).
f02(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, f,    low(f),    MinNParam, MaxNParam, SplitDiv, SplitMod).
f03(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, minf, low(minf), MinNParam, MaxNParam, SplitDiv, SplitMod).
f04(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, maxf, low(maxf), MinNParam, MaxNParam, SplitDiv, SplitMod).
f05(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, mind, low(mind), MinNParam, MaxNParam, SplitDiv, SplitMod).
f06(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, maxd, low(maxd), MinNParam, MaxNParam, SplitDiv, SplitMod).
f07(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, minp, low(minp), MinNParam, MaxNParam, SplitDiv, SplitMod).
f08(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, maxp, low(maxp), MinNParam, MaxNParam, SplitDiv, SplitMod).
f09(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(forest0, mint, low(mint), MinNParam, MaxNParam, SplitDiv, SplitMod).
f010(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, maxt, low(maxt), MinNParam, MaxNParam, SplitDiv, SplitMod).
f011(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, t,    up(t),     MinNParam, MaxNParam, SplitDiv, SplitMod).
f012(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, f,    up(f),     MinNParam, MaxNParam, SplitDiv, SplitMod).
f013(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, minf, up(minf),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f014(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, maxf, up(maxf),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f015(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, mind, up(mind),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f016(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, maxd, up(maxd),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f017(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, minp, up(minp),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f018(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, maxp, up(maxp),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f019(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, mint, up(mint),  MinNParam, MaxNParam, SplitDiv, SplitMod).
f020(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(forest0, maxt, up(maxt),  MinNParam, MaxNParam, SplitDiv, SplitMod).

t1  :- top(tree, f,    low(f),    1, 2, 1, 1).
t2  :- top(tree, mind, low(mind), 1, 2, 1, 1).
t3  :- top(tree, maxd, low(maxd), 1, 2, 1, 1).
t4  :- top(tree, minp, low(minp), 1, 2, 1, 1).
t5  :- top(tree, maxp, low(maxp), 1, 2, 1, 1).
t6  :- top(tree, f,    up(f),     1, 2, 1, 1).
t7  :- top(tree, mind, up(mind),  1, 2, 1, 1).
t8  :- top(tree, maxd, up(maxd),  1, 2, 1, 1).
t9  :- top(tree, minp, up(minp),  1, 2, 1, 1).
t10 :- top(tree, maxp, up(maxp),  1, 2, 1, 1).

t1(NParam)  :- top(tree, f,    low(f),    NParam, NParam, 1, 1).
t2(NParam)  :- top(tree, mind, low(mind), NParam, NParam, 1, 1).
t3(NParam)  :- top(tree, maxd, low(maxd), NParam, NParam, 1, 1).
t4(NParam)  :- top(tree, minp, low(minp), NParam, NParam, 1, 1).
t5(NParam)  :- top(tree, maxp, low(maxp), NParam, NParam, 1, 1).
t6(NParam)  :- top(tree, f,    up(f),     NParam, NParam, 1, 1).
t7(NParam)  :- top(tree, mind, up(mind),  NParam, NParam, 1, 1).
t8(NParam)  :- top(tree, maxd, up(maxd),  NParam, NParam, 1, 1).
t9(NParam)  :- top(tree, minp, up(minp),  NParam, NParam, 1, 1).
t10(NParam) :- top(tree, maxp, up(maxp),  NParam, NParam, 1, 1).

t1(MinNParam, MaxNParam)  :- top(tree, f,    low(f),    MinNParam, MaxNParam, 1, 1).
t2(MinNParam, MaxNParam)  :- top(tree, mind, low(mind), MinNParam, MaxNParam, 1, 1).
t3(MinNParam, MaxNParam)  :- top(tree, maxd, low(maxd), MinNParam, MaxNParam, 1, 1).
t4(MinNParam, MaxNParam)  :- top(tree, minp, low(minp), MinNParam, MaxNParam, 1, 1).
t5(MinNParam, MaxNParam)  :- top(tree, maxp, low(maxp), MinNParam, MaxNParam, 1, 1).
t6(MinNParam, MaxNParam)  :- top(tree, f,    up(f),     MinNParam, MaxNParam, 1, 1).
t7(MinNParam, MaxNParam)  :- top(tree, mind, up(mind),  MinNParam, MaxNParam, 1, 1).
t8(MinNParam, MaxNParam)  :- top(tree, maxd, up(maxd),  MinNParam, MaxNParam, 1, 1).
t9(MinNParam, MaxNParam)  :- top(tree, minp, up(minp),  MinNParam, MaxNParam, 1, 1).
t10(MinNParam, MaxNParam) :- top(tree, maxp, up(maxp),  MinNParam, MaxNParam, 1, 1).

t1(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, f,    low(f),    MinNParam, MaxNParam, SplitDiv, SplitMod).
t2(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, mind, low(mind), MinNParam, MaxNParam, SplitDiv, SplitMod).
t3(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, maxd, low(maxd), MinNParam, MaxNParam, SplitDiv, SplitMod).
t4(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, minp, low(minp), MinNParam, MaxNParam, SplitDiv, SplitMod).
t5(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, maxp, low(maxp), MinNParam, MaxNParam, SplitDiv, SplitMod).
t6(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, f,    up(f),     MinNParam, MaxNParam, SplitDiv, SplitMod).
t7(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, mind, up(mind),  MinNParam, MaxNParam, SplitDiv, SplitMod).
t8(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, maxd, up(maxd),  MinNParam, MaxNParam, SplitDiv, SplitMod).
t9(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(tree, minp, up(minp),  MinNParam, MaxNParam, SplitDiv, SplitMod).
t10(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(tree, maxp, up(maxp),  MinNParam, MaxNParam, SplitDiv, SplitMod).

p1  :- top(partition, nval,        low(nval),        1, 2, 1, 1).
p2  :- top(partition, min,         low(min),         1, 2, 1, 1).
p3  :- top(partition, max,         low(max),         1, 2, 1, 1).
p4  :- top(partition, range,       low(range),       1, 2, 1, 1).
p5  :- top(partition, sum_squares, low(sum_squares), 1, 2, 1, 1).
p6  :- top(partition, nval,        up(nval),         1, 2, 1, 1).
p7  :- top(partition, min,         up(min),          1, 2, 1, 1).
p8  :- top(partition, max,         up(max),          1, 2, 1, 1).
p9  :- top(partition, range,       up(range),        1, 2, 1, 1).
p10 :- top(partition, sum_squares, up(sum_squares),  1, 2, 1, 1).

p1(NParam)  :- top(partition, nval,        low(nval),        NParam, NParam, 1, 1).
p2(NParam)  :- top(partition, min,         low(min),         NParam, NParam, 1, 1).
p3(NParam)  :- top(partition, max,         low(max),         NParam, NParam, 1, 1).
p4(NParam)  :- top(partition, range,       low(range),       NParam, NParam, 1, 1).
p5(NParam)  :- top(partition, sum_squares, low(sum_squares), NParam, NParam, 1, 1).
p6(NParam)  :- top(partition, nval,        up(nval),         NParam, NParam, 1, 1).
p7(NParam)  :- top(partition, min,         up(min),          NParam, NParam, 1, 1).
p8(NParam)  :- top(partition, max,         up(max),          NParam, NParam, 1, 1).
p9(NParam)  :- top(partition, range,       up(range),        NParam, NParam, 1, 1).
p10(NParam) :- top(partition, sum_squares, up(sum_squares),  NParam, NParam, 1, 1).

p1(MinNParam, MaxNParam)  :- top(partition, nval,        low(nval),        MinNParam, MaxNParam, 1, 1).
p2(MinNParam, MaxNParam)  :- top(partition, min,         low(min),         MinNParam, MaxNParam, 1, 1).
p3(MinNParam, MaxNParam)  :- top(partition, max,         low(max),         MinNParam, MaxNParam, 1, 1).
p4(MinNParam, MaxNParam)  :- top(partition, range,       low(range),       MinNParam, MaxNParam, 1, 1).
p5(MinNParam, MaxNParam)  :- top(partition, sum_squares, low(sum_squares), MinNParam, MaxNParam, 1, 1).
p6(MinNParam, MaxNParam)  :- top(partition, nval,        up(nval),         MinNParam, MaxNParam, 1, 1).
p7(MinNParam, MaxNParam)  :- top(partition, min,         up(min),          MinNParam, MaxNParam, 1, 1).
p8(MinNParam, MaxNParam)  :- top(partition, max,         up(max),          MinNParam, MaxNParam, 1, 1).
p9(MinNParam, MaxNParam)  :- top(partition, range,       up(range),        MinNParam, MaxNParam, 1, 1).
p10(MinNParam, MaxNParam) :- top(partition, sum_squares, up(sum_squares),  MinNParam, MaxNParam, 1, 1).

p1(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, nval,        low(nval),        MinNParam, MaxNParam, SplitDiv, SplitMod).
p2(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, min,         low(min),         MinNParam, MaxNParam, SplitDiv, SplitMod).
p3(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, max,         low(max),         MinNParam, MaxNParam, SplitDiv, SplitMod).
p4(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, range,       low(range),       MinNParam, MaxNParam, SplitDiv, SplitMod).
p5(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, sum_squares, low(sum_squares), MinNParam, MaxNParam, SplitDiv, SplitMod).
p6(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, nval,        up(nval),         MinNParam, MaxNParam, SplitDiv, SplitMod).
p7(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, min,         up(min),          MinNParam, MaxNParam, SplitDiv, SplitMod).
p8(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, max,         up(max),          MinNParam, MaxNParam, SplitDiv, SplitMod).
p9(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition, range,       up(range),        MinNParam, MaxNParam, SplitDiv, SplitMod).
p10(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(partition, sum_squares, up(sum_squares),  MinNParam, MaxNParam, SplitDiv, SplitMod).

p01  :- top(partition0, nval,        low(nval),        1, 2, 1, 1).
p02  :- top(partition0, min,         low(min),         1, 2, 1, 1).
p03  :- top(partition0, max,         low(max),         1, 2, 1, 1).
p04  :- top(partition0, range,       low(range),       1, 2, 1, 1).
p05  :- top(partition0, sum_squares, low(sum_squares), 1, 2, 1, 1).
p06  :- top(partition0, nval,        up(nval),         1, 2, 1, 1).
p07  :- top(partition0, min,         up(min),          1, 2, 1, 1).
p08  :- top(partition0, max,         up(max),          1, 2, 1, 1).
p09  :- top(partition0, range,       up(range),        1, 2, 1, 1).
p010 :- top(partition0, sum_squares, up(sum_squares),  1, 2, 1, 1).

p01(NParam)  :- top(partition0, nval,        low(nval),        NParam, NParam, 1, 1).
p02(NParam)  :- top(partition0, min,         low(min),         NParam, NParam, 1, 1).
p03(NParam)  :- top(partition0, max,         low(max),         NParam, NParam, 1, 1).
p04(NParam)  :- top(partition0, range,       low(range),       NParam, NParam, 1, 1).
p05(NParam)  :- top(partition0, sum_squares, low(sum_squares), NParam, NParam, 1, 1).
p06(NParam)  :- top(partition0, nval,        up(nval),         NParam, NParam, 1, 1).
p07(NParam)  :- top(partition0, min,         up(min),          NParam, NParam, 1, 1).
p08(NParam)  :- top(partition0, max,         up(max),          NParam, NParam, 1, 1).
p09(NParam)  :- top(partition0, range,       up(range),        NParam, NParam, 1, 1).
p010(NParam) :- top(partition0, sum_squares, up(sum_squares),  NParam, NParam, 1, 1).

p01(MinNParam, MaxNParam)  :- top(partition0, nval,        low(nval),        MinNParam, MaxNParam, 1, 1).
p02(MinNParam, MaxNParam)  :- top(partition0, min,         low(min),         MinNParam, MaxNParam, 1, 1).
p03(MinNParam, MaxNParam)  :- top(partition0, max,         low(max),         MinNParam, MaxNParam, 1, 1).
p04(MinNParam, MaxNParam)  :- top(partition0, range,       low(range),       MinNParam, MaxNParam, 1, 1).
p05(MinNParam, MaxNParam)  :- top(partition0, sum_squares, low(sum_squares), MinNParam, MaxNParam, 1, 1).
p06(MinNParam, MaxNParam)  :- top(partition0, nval,        up(nval),         MinNParam, MaxNParam, 1, 1).
p07(MinNParam, MaxNParam)  :- top(partition0, min,         up(min),          MinNParam, MaxNParam, 1, 1).
p08(MinNParam, MaxNParam)  :- top(partition0, max,         up(max),          MinNParam, MaxNParam, 1, 1).
p09(MinNParam, MaxNParam)  :- top(partition0, range,       up(range),        MinNParam, MaxNParam, 1, 1).
p010(MinNParam, MaxNParam) :- top(partition0, sum_squares, up(sum_squares),  MinNParam, MaxNParam, 1, 1).

p01(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, nval,        low(nval),        MinNParam, MaxNParam, SplitDiv, SplitMod).
p02(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, min,         low(min),         MinNParam, MaxNParam, SplitDiv, SplitMod).
p03(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, max,         low(max),         MinNParam, MaxNParam, SplitDiv, SplitMod).
p04(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, range,       low(range),       MinNParam, MaxNParam, SplitDiv, SplitMod).
p05(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, sum_squares, low(sum_squares), MinNParam, MaxNParam, SplitDiv, SplitMod).
p06(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, nval,        up(nval),         MinNParam, MaxNParam, SplitDiv, SplitMod).
p07(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, min,         up(min),          MinNParam, MaxNParam, SplitDiv, SplitMod).
p08(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, max,         up(max),          MinNParam, MaxNParam, SplitDiv, SplitMod).
p09(MinNParam, MaxNParam, SplitDiv, SplitMod)  :- top(partition0, range,       up(range),        MinNParam, MaxNParam, SplitDiv, SplitMod).
p010(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(partition0, sum_squares, up(sum_squares),  MinNParam, MaxNParam, SplitDiv, SplitMod).

group01 :- top(group, ng,           low(ng),           1, 2, 1, 1).
group02 :- top(group, nv,           low(nv),           1, 2, 1, 1).
group03 :- top(group, smin,         low(smin),         1, 2, 1, 1).
group04 :- top(group, smax,         low(smax),         1, 2, 1, 1).
group05 :- top(group, srange,       low(srange),       1, 2, 1, 1).
group06 :- top(group, ssum_squares, low(ssum_squares), 1, 2, 1, 1).
group07 :- top(group, dmin,         low(dmin),         1, 2, 1, 1).
group08 :- top(group, dmax,         low(dmax),         1, 2, 1, 1).
group09 :- top(group, drange,       low(drange),       1, 2, 1, 1).
group10 :- top(group, dsum_squares, low(dsum_squares), 1, 2, 1, 1).
group11 :- top(group, ng,           up(ng),            1, 2, 1, 1).
group12 :- top(group, nv,           up(nv),            1, 2, 1, 1).
group13 :- top(group, smin,         up(smin),          1, 2, 1, 1).
group14 :- top(group, smax,         up(smax),          1, 2, 1, 1).
group15 :- top(group, srange,       up(srange),        1, 2, 1, 1).
group16 :- top(group, ssum_squares, up(ssum_squares),  1, 2, 1, 1).
group17 :- top(group, dmin,         up(dmin),          1, 2, 1, 1).
group18 :- top(group, dmax,         up(dmax),          1, 2, 1, 1).
group19 :- top(group, drange,       up(drange),        1, 2, 1, 1).
group20 :- top(group, dsum_squares, up(dsum_squares),  1, 2, 1, 1).

group01(NParam) :- top(group, ng,           low(ng),           NParam, NParam, 1, 1).
group02(NParam) :- top(group, nv,           low(nv),           NParam, NParam, 1, 1).
group03(NParam) :- top(group, smin,         low(smin),         NParam, NParam, 1, 1).
group04(NParam) :- top(group, smax,         low(smax),         NParam, NParam, 1, 1).
group05(NParam) :- top(group, srange,       low(srange),       NParam, NParam, 1, 1).
group06(NParam) :- top(group, ssum_squares, low(ssum_squares), NParam, NParam, 1, 1).
group07(NParam) :- top(group, dmin,         low(dmin),         NParam, NParam, 1, 1).
group08(NParam) :- top(group, dmax,         low(dmax),         NParam, NParam, 1, 1).
group09(NParam) :- top(group, drange,       low(drange),       NParam, NParam, 1, 1).
group10(NParam) :- top(group, dsum_squares, low(dsum_squares), NParam, NParam, 1, 1).
group11(NParam) :- top(group, ng,           up(ng),            NParam, NParam, 1, 1).
group12(NParam) :- top(group, nv,           up(nv),            NParam, NParam, 1, 1).
group13(NParam) :- top(group, smin,         up(smin),          NParam, NParam, 1, 1).
group14(NParam) :- top(group, smax,         up(smax),          NParam, NParam, 1, 1).
group15(NParam) :- top(group, srange,       up(srange),        NParam, NParam, 1, 1).
group16(NParam) :- top(group, ssum_squares, up(ssum_squares),  NParam, NParam, 1, 1).
group17(NParam) :- top(group, dmin,         up(dmin),          NParam, NParam, 1, 1).
group18(NParam) :- top(group, dmax,         up(dmax),          NParam, NParam, 1, 1).
group19(NParam) :- top(group, drange,       up(drange),        NParam, NParam, 1, 1).
group20(NParam) :- top(group, dsum_squares, up(dsum_squares),  NParam, NParam, 1, 1).

group01(MinNParam, MaxNParam) :- top(group, ng,           low(ng),           MinNParam, MaxNParam, 1, 1).
group02(MinNParam, MaxNParam) :- top(group, nv,           low(nv),           MinNParam, MaxNParam, 1, 1).
group03(MinNParam, MaxNParam) :- top(group, smin,         low(smin),         MinNParam, MaxNParam, 1, 1).
group04(MinNParam, MaxNParam) :- top(group, smax,         low(smax),         MinNParam, MaxNParam, 1, 1).
group05(MinNParam, MaxNParam) :- top(group, srange,       low(srange),       MinNParam, MaxNParam, 1, 1).
group06(MinNParam, MaxNParam) :- top(group, ssum_squares, low(ssum_squares), MinNParam, MaxNParam, 1, 1).
group07(MinNParam, MaxNParam) :- top(group, dmin,         low(dmin),         MinNParam, MaxNParam, 1, 1).
group08(MinNParam, MaxNParam) :- top(group, dmax,         low(dmax),         MinNParam, MaxNParam, 1, 1).
group09(MinNParam, MaxNParam) :- top(group, drange,       low(drange),       MinNParam, MaxNParam, 1, 1).
group10(MinNParam, MaxNParam) :- top(group, dsum_squares, low(dsum_squares), MinNParam, MaxNParam, 1, 1).
group11(MinNParam, MaxNParam) :- top(group, ng,           up(ng),            MinNParam, MaxNParam, 1, 1).
group12(MinNParam, MaxNParam) :- top(group, nv,           up(nv),            MinNParam, MaxNParam, 1, 1).
group13(MinNParam, MaxNParam) :- top(group, smin,         up(smin),          MinNParam, MaxNParam, 1, 1).
group14(MinNParam, MaxNParam) :- top(group, smax,         up(smax),          MinNParam, MaxNParam, 1, 1).
group15(MinNParam, MaxNParam) :- top(group, srange,       up(srange),        MinNParam, MaxNParam, 1, 1).
group16(MinNParam, MaxNParam) :- top(group, ssum_squares, up(ssum_squares),  MinNParam, MaxNParam, 1, 1).
group17(MinNParam, MaxNParam) :- top(group, dmin,         up(dmin),          MinNParam, MaxNParam, 1, 1).
group18(MinNParam, MaxNParam) :- top(group, dmax,         up(dmax),          MinNParam, MaxNParam, 1, 1).
group19(MinNParam, MaxNParam) :- top(group, drange,       up(drange),        MinNParam, MaxNParam, 1, 1).
group20(MinNParam, MaxNParam) :- top(group, dsum_squares, up(dsum_squares),  MinNParam, MaxNParam, 1, 1).

group01(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, ng,           low(ng),           MinNParam, MaxNParam, SplitDiv, SplitMod).
group02(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, nv,           low(nv),           MinNParam, MaxNParam, SplitDiv, SplitMod).
group03(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, smin,         low(smin),         MinNParam, MaxNParam, SplitDiv, SplitMod).
group04(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, smax,         low(smax),         MinNParam, MaxNParam, SplitDiv, SplitMod).
group05(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, srange,       low(srange),       MinNParam, MaxNParam, SplitDiv, SplitMod).
group06(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, ssum_squares, low(ssum_squares), MinNParam, MaxNParam, SplitDiv, SplitMod).
group07(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, dmin,         low(dmin),         MinNParam, MaxNParam, SplitDiv, SplitMod).
group08(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, dmax,         low(dmax),         MinNParam, MaxNParam, SplitDiv, SplitMod).
group09(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, drange,       low(drange),       MinNParam, MaxNParam, SplitDiv, SplitMod).
group10(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, dsum_squares, low(dsum_squares), MinNParam, MaxNParam, SplitDiv, SplitMod).
group11(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, ng,           up(ng),            MinNParam, MaxNParam, SplitDiv, SplitMod).
group12(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, nv,           up(nv),            MinNParam, MaxNParam, SplitDiv, SplitMod).
group13(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, smin,         up(smin),          MinNParam, MaxNParam, SplitDiv, SplitMod).
group14(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, smax,         up(smax),          MinNParam, MaxNParam, SplitDiv, SplitMod).
group15(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, srange,       up(srange),        MinNParam, MaxNParam, SplitDiv, SplitMod).
group16(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, ssum_squares, up(ssum_squares),  MinNParam, MaxNParam, SplitDiv, SplitMod).
group17(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, dmin,         up(dmin),          MinNParam, MaxNParam, SplitDiv, SplitMod).
group18(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, dmax,         up(dmax),          MinNParam, MaxNParam, SplitDiv, SplitMod).
group19(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, drange,       up(drange),        MinNParam, MaxNParam, SplitDiv, SplitMod).
group20(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(group, dsum_squares, up(dsum_squares),  MinNParam, MaxNParam, SplitDiv, SplitMod).

cgroup01 :- top(cgroup, ng,           low(ng),           1, 2, 1, 1).
cgroup02 :- top(cgroup, nv,           low(nv),           1, 2, 1, 1).
cgroup03 :- top(cgroup, smin,         low(smin),         1, 2, 1, 1).
cgroup04 :- top(cgroup, smax,         low(smax),         1, 2, 1, 1).
cgroup05 :- top(cgroup, srange,       low(srange),       1, 2, 1, 1).
cgroup06 :- top(cgroup, ssum_squares, low(ssum_squares), 1, 2, 1, 1).
cgroup07 :- top(cgroup, dmin,         low(dmin),         1, 2, 1, 1).
cgroup08 :- top(cgroup, dmax,         low(dmax),         1, 2, 1, 1).
cgroup09 :- top(cgroup, drange,       low(drange),       1, 2, 1, 1).
cgroup10 :- top(cgroup, dsum_squares, low(dsum_squares), 1, 2, 1, 1).
cgroup11 :- top(cgroup, ng,           up(ng),            1, 2, 1, 1).
cgroup12 :- top(cgroup, nv,           up(nv),            1, 2, 1, 1).
cgroup13 :- top(cgroup, smin,         up(smin),          1, 2, 1, 1).
cgroup14 :- top(cgroup, smax,         up(smax),          1, 2, 1, 1).
cgroup15 :- top(cgroup, srange,       up(srange),        1, 2, 1, 1).
cgroup16 :- top(cgroup, ssum_squares, up(ssum_squares),  1, 2, 1, 1).
cgroup17 :- top(cgroup, dmin,         up(dmin),          1, 2, 1, 1).
cgroup18 :- top(cgroup, dmax,         up(dmax),          1, 2, 1, 1).
cgroup19 :- top(cgroup, drange,       up(drange),        1, 2, 1, 1).
cgroup20 :- top(cgroup, dsum_squares, up(dsum_squares),  1, 2, 1, 1).

cgroup01(NParam) :- top(cgroup, ng,           low(ng),           NParam, NParam, 1, 1).
cgroup02(NParam) :- top(cgroup, nv,           low(nv),           NParam, NParam, 1, 1).
cgroup03(NParam) :- top(cgroup, smin,         low(smin),         NParam, NParam, 1, 1).
cgroup04(NParam) :- top(cgroup, smax,         low(smax),         NParam, NParam, 1, 1).
cgroup05(NParam) :- top(cgroup, srange,       low(srange),       NParam, NParam, 1, 1).
cgroup06(NParam) :- top(cgroup, ssum_squares, low(ssum_squares), NParam, NParam, 1, 1).
cgroup07(NParam) :- top(cgroup, dmin,         low(dmin),         NParam, NParam, 1, 1).
cgroup08(NParam) :- top(cgroup, dmax,         low(dmax),         NParam, NParam, 1, 1).
cgroup09(NParam) :- top(cgroup, drange,       low(drange),       NParam, NParam, 1, 1).
cgroup10(NParam) :- top(cgroup, dsum_squares, low(dsum_squares), NParam, NParam, 1, 1).
cgroup11(NParam) :- top(cgroup, ng,           up(ng),            NParam, NParam, 1, 1).
cgroup12(NParam) :- top(cgroup, nv,           up(nv),            NParam, NParam, 1, 1).
cgroup13(NParam) :- top(cgroup, smin,         up(smin),          NParam, NParam, 1, 1).
cgroup14(NParam) :- top(cgroup, smax,         up(smax),          NParam, NParam, 1, 1).
cgroup15(NParam) :- top(cgroup, srange,       up(srange),        NParam, NParam, 1, 1).
cgroup16(NParam) :- top(cgroup, ssum_squares, up(ssum_squares),  NParam, NParam, 1, 1).
cgroup17(NParam) :- top(cgroup, dmin,         up(dmin),          NParam, NParam, 1, 1).
cgroup18(NParam) :- top(cgroup, dmax,         up(dmax),          NParam, NParam, 1, 1).
cgroup19(NParam) :- top(cgroup, drange,       up(drange),        NParam, NParam, 1, 1).
cgroup20(NParam) :- top(cgroup, dsum_squares, up(dsum_squares),  NParam, NParam, 1, 1).

cgroup01(MinNParam, MaxNParam) :- top(cgroup, ng,           low(ng),           MinNParam, MaxNParam, 1, 1).
cgroup02(MinNParam, MaxNParam) :- top(cgroup, nv,           low(nv),           MinNParam, MaxNParam, 1, 1).
cgroup03(MinNParam, MaxNParam) :- top(cgroup, smin,         low(smin),         MinNParam, MaxNParam, 1, 1).
cgroup04(MinNParam, MaxNParam) :- top(cgroup, smax,         low(smax),         MinNParam, MaxNParam, 1, 1).
cgroup05(MinNParam, MaxNParam) :- top(cgroup, srange,       low(srange),       MinNParam, MaxNParam, 1, 1).
cgroup06(MinNParam, MaxNParam) :- top(cgroup, ssum_squares, low(ssum_squares), MinNParam, MaxNParam, 1, 1).
cgroup07(MinNParam, MaxNParam) :- top(cgroup, dmin,         low(dmin),         MinNParam, MaxNParam, 1, 1).
cgroup08(MinNParam, MaxNParam) :- top(cgroup, dmax,         low(dmax),         MinNParam, MaxNParam, 1, 1).
cgroup09(MinNParam, MaxNParam) :- top(cgroup, drange,       low(drange),       MinNParam, MaxNParam, 1, 1).
cgroup10(MinNParam, MaxNParam) :- top(cgroup, dsum_squares, low(dsum_squares), MinNParam, MaxNParam, 1, 1).
cgroup11(MinNParam, MaxNParam) :- top(cgroup, ng,           up(ng),            MinNParam, MaxNParam, 1, 1).
cgroup12(MinNParam, MaxNParam) :- top(cgroup, nv,           up(nv),            MinNParam, MaxNParam, 1, 1).
cgroup13(MinNParam, MaxNParam) :- top(cgroup, smin,         up(smin),          MinNParam, MaxNParam, 1, 1).
cgroup14(MinNParam, MaxNParam) :- top(cgroup, smax,         up(smax),          MinNParam, MaxNParam, 1, 1).
cgroup15(MinNParam, MaxNParam) :- top(cgroup, srange,       up(srange),        MinNParam, MaxNParam, 1, 1).
cgroup16(MinNParam, MaxNParam) :- top(cgroup, ssum_squares, up(ssum_squares),  MinNParam, MaxNParam, 1, 1).
cgroup17(MinNParam, MaxNParam) :- top(cgroup, dmin,         up(dmin),          MinNParam, MaxNParam, 1, 1).
cgroup18(MinNParam, MaxNParam) :- top(cgroup, dmax,         up(dmax),          MinNParam, MaxNParam, 1, 1).
cgroup19(MinNParam, MaxNParam) :- top(cgroup, drange,       up(drange),        MinNParam, MaxNParam, 1, 1).
cgroup20(MinNParam, MaxNParam) :- top(cgroup, dsum_squares, up(dsum_squares),  MinNParam, MaxNParam, 1, 1).

cgroup01(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, ng,           low(ng),           MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup02(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, nv,           low(nv),           MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup03(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, smin,         low(smin),         MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup04(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, smax,         low(smax),         MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup05(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, srange,       low(srange),       MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup06(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, ssum_squares, low(ssum_squares), MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup07(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, dmin,         low(dmin),         MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup08(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, dmax,         low(dmax),         MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup09(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, drange,       low(drange),       MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup10(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, dsum_squares, low(dsum_squares), MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup11(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, ng,           up(ng),            MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup12(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, nv,           up(nv),            MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup13(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, smin,         up(smin),          MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup14(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, smax,         up(smax),          MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup15(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, srange,       up(srange),        MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup16(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, ssum_squares, up(ssum_squares),  MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup17(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, dmin,         up(dmin),          MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup18(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, dmax,         up(dmax),          MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup19(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, drange,       up(drange),        MinNParam, MaxNParam, SplitDiv, SplitMod).
cgroup20(MinNParam, MaxNParam, SplitDiv, SplitMod) :- top(cgroup, dsum_squares, up(dsum_squares),  MinNParam, MaxNParam, SplitDiv, SplitMod).

% KindCombi<>model_seeker, Bound=low(_), MinNParam>0, MaxNParam>0: combinatorial object, lower bound
% KindCombi<>model_seeker, Bound=up(_),  MinNParam>0, MaxNParam>0: combinatorial object, upper bound
% KindCombi<>model_seeker, Bound=0,      MinNParam>0, MaxNParam>0: combinatorial object, (feasibility TODO when update exp.pl)
% KindCombi= model_seeker, Bound=0,      MinNParam=0, MaxNParam=0: model seeker
%
% . KindCombi: kind of combinatorial object          if combinatorial object or model_seeker if model seeker
% . KindBound: name of the bound                     if combinatorial object or assistant    if model seeker
% . Bound    : low(KindBound) or up(KindBound)       if combinatorial object or 0            if model seeker
% . MinNParam: minimum number of input parameters    if combinatorial object or 0            if model seeker
% . MaxNParam: maximum number of input parameters    if combinatorial object or 0            if model seeker
% . SplitDiv : division factor to split a map        if combinatorial object or 0            if model seeker
% . SplitId  : split identifier (from 1 to SplitDiv) if combinatorial object or 0            if model seeker
top(KindCombi, KindBound, Bound, MinNParam, MaxNParam, SplitDiv, SplitMod) :-
    retractall(missing(_,_,_)), nl,                                % remove facts giving the formulae that cound not be generated
    init_cpt(conjecture_id, 0),                                    % assert id label for the conjectures that will be generated % cmodif
    (KindCombi \== model_seeker ->
        functor(Bound, LowUp, 1),
        atoms_concat([KindCombi,'_',LowUp,'_',KindBound,'_',MinNParam,'_',MaxNParam,'_',SplitDiv,'_',SplitMod], FilesOutSuffix), % modif
        atoms_concat(['data/',KindCombi,'/found_conjectures_',FilesOutSuffix,'.pl'], ConjecturesFileName),
        atoms_concat(['data/',KindCombi,'/meta_metadata_',KindCombi,'.pl'], FileName),
        consult(FileName)                                          % read proper file containing size_to_compute_conjectures facts
    ;
        FilesOutSuffix = found_model,
        atoms_concat(['data/',KindCombi,'/',FilesOutSuffix,'_',SplitDiv,'_',SplitMod,'.pl'], ConjecturesFileName)
    ),
    open(ConjecturesFileName, write, Sout),                        % file used for storing acquired conjectures (store internal representation)
    write_conjecture_file_header(Sout),
    top_search(KindCombi, KindBound, Bound, MinNParam, MaxNParam, SplitDiv, SplitMod, Sout), % modif
    add_characteristics_for_which_could_not_find_any_formula_to_conjecture_file(Sout), % modif
    close(Sout),
    atoms_concat(['data/',KindCombi,'/trace_',FilesOutSuffix,'.pl'], TraceFileName),
    open(TraceFileName, write, Tout),
    record_trace(Tout),
    close(Tout),
    (KindCombi \== model_seeker ->
        get_number_of_found_bounds(KindCombi, ConjecturesFileName), % compute and write statistics on acquired bounds
        check_conjectures(KindCombi, ConjecturesFileName),          % check acquired conjectures wrt largest available table
        retractall(size_to_compute_conjectures(_,_,_,_))
    ;
        true
    ).

write_conjecture_file_header(Sout) :-
    format(Sout, '% Contains all found conjectures generated by main, where each conjecture is a conjecture\\9 fact of the form:~n', []), % cmodif
    format(Sout, '%  . id(KindCombi, MapId, ConjId): conjecture identifier,~n', []),  % cmodif
    format(Sout, '%  . Kind                        : low (if lower bound); up (if upper bound); primary, secondary (if equality),~n', []),
    format(Sout, '%  . col(Table,Col)              : table and column for which the conjecture holds (left-hand side of the conjecture),~n', []),
    format(Sout, '%  . Name                        : name corresponding to col(Table,Col),~n', []),
    format(Sout, '%  . MaxN                        : size that was used for generating the conjecture,~n', []),
    format(Sout, '%  . Cost                        : cost associated with the conjecture,~n', []),
    format(Sout, '%  . Formula                     : formula associated with the conjecture (right-hand side of the conjecture),~n', []),
    format(Sout, '%  . ConjFamily                  : family to which belong the conjecture,~n', []), % cmodif
    format(Sout, '%  . Option                      : option that was used to generate the conjecture.~n', []), % cmodif
    format(Sout, '% Contains also all characteritics for which could not find any formula, see missing\\3 facts.~n~n', []), % modif
    format(Sout, ':- multifile conjecture/9.~n', []),  % cmodif
    format(Sout, ':- multifile missing/3.~n',    []),  % modif
    format(Sout, ':- dynamic conjecture/9.~n',   []),  % cmodif
    format(Sout, ':- dynamic missing/3.~n~n',    []).  % modif

add_characteristics_for_which_could_not_find_any_formula_to_conjecture_file(Sout) :-                                              % modif
    findall(missing(Table,Kind,CharacteristicName), missing(Table,Kind,CharacteristicName), Missings),                            % modif
    (Missings = [_|_] -> format(Sout, '~n', []) ; true),                                                                          % modif
    add_characteristics_for_which_could_not_find_any_formula_to_conjecture_file1(Missings, Sout).                                 % modif

add_characteristics_for_which_could_not_find_any_formula_to_conjecture_file1([], _) :- !.                                         % modif
add_characteristics_for_which_could_not_find_any_formula_to_conjecture_file1([missing(Table,Kind,CharacteristicName)|R], Sout) :- % modif
    format(Sout, 'missing(~w,~w,~w).~n', [Table,Kind,CharacteristicName]),                                                        % modif
    add_characteristics_for_which_could_not_find_any_formula_to_conjecture_file1(R, Sout).                                        % modif

top_search(KindCombi, KindBound, Bound, MinNParam, MaxNParam, SplitDiv, SplitMod, Sout) :- % modif
    gen_options_bool(KindCombi, OptionsBool),                              % get options of Boolean     formulae once and for all
    gen_options_cond(KindCombi, OptionsCond, MinNParam),                   % get options of conditional formulae once and for all
    OptionsBoolCond = OptionsBool-OptionsCond,                             % put together Boolean and conditional options
    (KindCombi \== model_seeker ->                                         % if find conjectures for combinatorial objects
        NParam in MinNParam..MaxNParam,                                    % get all corresponding tables
        findall(NParam-Table,                                              % return also NParam as NParam not fixed after the findall
                gen_candidate_tables(Table, KindCombi, KindBound, Bound, NParam), Tables),
        member_split(Tables, SplitDiv, SplitMod, NPara-Table),             % iterate over the tables corresponding to parameters SplitDiv and SplitMod
        size_to_compute_conjectures(Table, _, MaxN, _),                    % get data size from which we will compute the conjectures
        Map = t(KindBound, NPara, Table)                                   % used to instrument the code: known in which map we currently are (used fixed NPara)
    ;                                                                      % if handle standalone tables
        get_tables(KindCombi, 0, _, _, TableNames),                        % get all the table names (using size 0 as only size)
        MaxN = 0,                                                          % tables are in data/KindCombi/data0 directory
        member_split(TableNames, SplitDiv, SplitMod, Table),               % iterate over the tables corresponding to parameters SplitDiv and SplitMod
        %member(Table, TableNames),                                        % select one of the tables
        Map = 0
    ),
    retractall(max_cost(_)),                                               % used to record best cost found so far for a formula
    nl,
    write('--------------------------------------------------------------------------------'), nl,
    write('Handle table '), write(Table), write(' of '), write(MaxN), nl,
    write('--------------------------------------------------------------------------------'), nl,
%   statistics,
    trimcore,
%   statistics,
    atoms_concat(['data/',KindCombi,'/data'], DirData),
    gen_table_and_metadata_file_names(DirData, MaxN, Table, TableFile, MetadataFile),
    consult(TableFile),                                                    % read the main table
    consult(MetadataFile),                                                 % read the metadata of the main table
    write('--> consult '), write(Table), nl,
    clear_type_found_formula,                                              % as we did not yet found any formula for current table
    (KindCombi \== model_seeker ->                                         % if find conjectures for combinatorial objects
        tab_get_primaries(Table, Primaries),                               % get the list of primary characteristics, i.e. initial input parameters
 %      search_primaries(Primaries, OptionsBoolCond),                      % search Boolean formulae for the feasibility of the input params NewF
        tab_get_secondaries(Table, Secondaries),                           % get the list of secondary characteristics which have at least one fd
        search_secondaries(KindCombi, Bound, Secondaries, OptionsBoolCond, % search a formula that explains each secondary characteristics % cmodif
                           [], Primaries, MaxN, 2-Map, Sout, FoundParams),
        nl,
        tab_get_bound_col(Table,Col),                                      % identify the col.of the main table corresponding to a lower/upper bound
        (tab_get_type(col(Table,Col), cst) ->                              % if bound is equal to a constant
            write_found_constant(KindCombi, Bound, Table, Col, MaxN, 1-Map, Sout), % cmodif
            assert(max_cost(0)),                                           % assert to indicate that a formula was found
            assert(max_cost(col(Table,Col), 0))                            % max_cost\2 used by search_secondaries1
        ;
         (tab_get_equal(col(Table,Col), PrimaryCol),                       % if bound is equal to a primary char.
          memberchk(col(Table,PrimaryCol), Primaries)) ->
            write_found_identical_column(KindCombi, Bound, Table, Col, PrimaryCol, MaxN, 1-Map, Sout), % cmodif
            assert(max_cost(0)),                                           % assert to indicate that a formula was found
            assert(max_cost(col(Table,Col), 0))                            % max_cost\2 used by search_secondaries1
        ;                                                                  % if bound is = to a secondary char.for which we already found a formula
         get_first_found_secondary_equal_to_bound(FoundParams, Table, Col, SecondaryCol) ->
            write_found_identical_column(KindCombi, Bound, Table, Col, SecondaryCol, MaxN, 1-Map, Sout), % cmodif
            assert(max_cost(0)),                                           % assert to indicate that a formula was found
            assert(max_cost(col(Table,Col), 0))                            % max_cost\2 used by search_secondaries1
        ;
            append(Primaries, Secondaries, AllParams),
            length(Primaries, LenPrimaries),
            length(AllParams, LenAllParams),
            length(FoundParams, LenFoundParams),
            remove_cst_columns(FoundParams, FoundParamsWithoutCst),        % do not want to use constant columns as an input parameter
            remove_cst_columns(AllParams, AllParamsWithoutCst),            % do not want to use constant columns as an input parameter
                                                                           %  . first search a formula by just using the primary char.or a subset
            tab_get_fd(col(Table,Col), FD),                                %    get functional dependencies determining Col
            search_with_small_fd_primaries(KindCombi, Bound, FD, LenPrimaries, Primaries, OptionsBoolCond, MaxN, Table, Col, 1-Map, Sout), % cmodif
            (max_cost(MaxCost1) -> CurMaxCost2 is MaxCost1+1 ; CurMaxCost2 = 100000),
            (LenFoundParams = LenPrimaries -> true                         %  . second search a formula by also using the secondary char.
            ;                                                              %    for which a formula was found
                search_a_formula(CurMaxCost2, KindCombi, Bound, OptionsBoolCond, Primaries, FoundParamsWithoutCst, 0, MaxN, Table, Col, 1-Map, Sout) % cmodif
                
            ),
            (max_cost(_) ->                                                                                                            % modif
                true                                                                                                                   % modif
            ;                                                              % record characteristics for which could not find a formula % modif
                functor(Bound, Func, 1), arg(1, Bound, Char), assert(missing(Table, Func, Char))                                       % modif
            ),                                                                                                                         % modif
            (LenAllParams = LenFoundParams -> true                         %  . third search a formula by also using the secondary char.
            ;                                                              %    for which no formula was found (to identify unifying formulae)
                (max_cost(MaxCost2) -> CurMaxCost3 is MaxCost2+1 ; CurMaxCost3 = CurMaxCost2),
                search_a_formula(CurMaxCost3, KindCombi, Bound, OptionsBoolCond, FoundParamsWithoutCst, AllParamsWithoutCst, 0, MaxN, Table, Col, 1-Map, Sout) % cmodif
            )
        ),
        remove_metadata_facts(Table)                                       % remove current metadata fact to reclaim space
    ;                                                                      % if we are in the context of ASSISTANT
        tab_get_secondaries(Table, Secondaries),                           % try to find a formula for every secondary column by using
        nl,                                                                % input columns as input parameters
        search_secondaries(Secondaries, OptionsBoolCond, 0, 1-0, Sout)
    ),
    write('<-- reclaim '), write(Table), nl,
    remove_signature_and_table_facts(Table),                               % remove current table to reclaim space as finish to generate its conjectures
    false.
top_search(_, _, _, _, _, _, _, _).

get_first_found_secondary_equal_to_bound([col(Table,SecondaryCol)|_], Table, BoundCol, SecondaryCol) :-
    tab_get_kind(col(Table,SecondaryCol), secondary),
    tab_get_equal(col(Table,SecondaryCol), BoundCol),
    !.
get_first_found_secondary_equal_to_bound([_|R], Table, BoundCol, FoundSecondary) :-
    get_first_found_secondary_equal_to_bound(R, Table, BoundCol, FoundSecondary).

remove_cst_columns(Cols, ColsWithoutCst) :-
    findall(Col, (member(Col, Cols), tab_get_type(Col, Type), Type \== cst), ColsWithoutCst).

%search_primaries(Primaries, OptionsBoolCond) :-                   % search Boolean formulae explaining the feasibility of the input parameters
%   instrument_code(instrument_in_search_primaries, CallMap),
%    retractall(max_cost(_)),                                      % to avoid considering the cost of the previous problem!
%    true.

% INPUT PARAMETERS (used only in the context of combinatorial objects)
%  KindCombi      : kind of combinatorial object
%  Bound          : considered map (a term low(Characteristics), up(Characteristics) for combinatorial object, or 0 for the model seeker)
%  Secondaries    : list of secondary characteristics for which try to find a formula
%  OptionsBoolCond: list of options for Boolean and conditionals that parametrise the type of formula we are looking for
%                   (but not which input parameter we will use)
%  PrevPrimaries  : previous list of primary characteristics (used to avoid searching twice a formula with a same attributes)
%  Primaries      : list of primary characteristics that will be input parameters for searching for a formula
%                   (before also incorporating in this list secondary characteristics for which we could find a formula)
%  MaxN           : 0 if ASSISTANT, size of combinatorial objects used to acquire formulae otherwise
%  CallMap        : use for generating a trace
%  Sout           : output where to print the founded formulae
%
% OUTPUT PARAMETERS
%  FoundParams  : list of parameters for which found a formula
search_secondaries(KindCombi, Bound, Secondaries, OptionsBoolCond, PrevPrimaries, Primaries, MaxN, CallMap, Sout, FoundParams) :- % cmodif
    retractall(max_cost(_,_)),                            % before entering the first call to search_secondaries1 remove max cost
                                                          % found for the different secondary char.: the second call to search_secondaries1
                                                          % will use the recorded maximum cost in order to try to find simpler formulae
                                                          % (i.e. formulae with a smaller cost) which also use the secondary char. found by
                                                          % the first call to search_secondaries1
    search_secondaries1(Secondaries, KindCombi, Bound, OptionsBoolCond, PrevPrimaries, Primaries, MaxN, CallMap, Sout, SecNotFound, NewInputParams), % cmodif
    (NewInputParams = [_|_] ->
        write('----------'), nl,
        append(Primaries, NewInputParams, CurPrimaries),  % try to find simpler formulae by using the secondary char.for which we just found a formula
        search_secondaries1(NewInputParams, KindCombi, Bound, OptionsBoolCond, Primaries, CurPrimaries, MaxN, CallMap, Sout, _, NewNewInputParams), % cmodif
        (NewNewInputParams = [_|_] ->
            write('----------'), nl
        ;
            true
        )
    ;
        true
    ),
    ((SecNotFound    = [_|_],                             % if there is still a secondary characteristic for which did not find a formula
      NewInputParams = [_|_]) ->                          % if there is at least one new input parameter since the last iteration
        append(Primaries, NewInputParams, NewPrimaries),  % build list of new input parameters including the secondary char. for which just find a formula
        search_secondaries(KindCombi, Bound, SecNotFound, % try to find a formula for the left over secondary char. % cmodif
                           OptionsBoolCond, Primaries, NewPrimaries, MaxN, CallMap, Sout, FoundParams)
    ;                                                     % stop searching formulae as:
     SecNotFound = [_|_] ->                               %  . either we do not have any new input parameters since the last iteration
        FoundParams = Primaries,
        write('could not find a formula for '), write_col_names(SecNotFound)
    ;                                                     %  . either we could find a formula for every secondary char.
        append(Primaries, NewInputParams, FoundParams)
    ).

get_smallest_set_included_in([], _, _, [], []) :- !.
get_smallest_set_included_in([Set|RemainingSets], _, List, Set, RemainingSets) :-
    included_in_set(Set, List), !.
get_smallest_set_included_in([_|R], LenList, List, Set, RemainingSets) :-
    get_smallest_set_included_in(R, LenList, List, Set, RemainingSets).

included_in_set([], _) :- !.
included_in_set([V|R], [V|S]) :- !,
    included_in_set(R, S).
included_in_set([col(Table,V)|R], [col(Table,U)|S]) :-
    U < V,
    included_in_set([col(Table,V)|R], S).

search_with_small_fd_primaries(KindCombi, Bound, FD, LenInputParams, InputParams, OptionsBoolCond, MaxN, Table, Col, CallMap, Sout) :- % cmodif
    get_smallest_set_included_in(FD, LenInputParams, InputParams, SmallestFD, RemainingFD),            % extract smallest fd included in input params
    (SmallestFD = [] ->                                                                                % if such smallest fd does not exist
        (max_cost(_) ->                                                                                % if already found a formula for col
            true                                                                                       % then stop the search of a formula
        ;                                                                                              % otherwise use all input parameters
            search_a_formula(100000, KindCombi, Bound, OptionsBoolCond, [], InputParams, 0,            % to try to find a formula explaining Col % cmodif
                             MaxN, Table, Col, CallMap, Sout)
        )
    ;                                                                                                  % if a small fd exists then use it to try % cmodif
        search_a_formula(100000, KindCombi, Bound, OptionsBoolCond, [], SmallestFD, 1,                 % to find a formula explaining Col
                         MaxN, Table, Col, CallMap, Sout),
        (max_cost(_) ->                                                                                % if could find wrt a small fd
            true                                                                                       % then stop the search of a formula
        ;                                                                                              % otherwise continue the search by trying
            search_with_small_fd_primaries(KindCombi, Bound, RemainingFD, LenInputParams, InputParams, % to use other small fd before trying % cmodif
                                           OptionsBoolCond, MaxN, Table, Col, CallMap, Sout)           % to use all input parameters
        )
    ).

search_secondaries1([], _, _, _, _, _, _, _, _, [], []) :- !. % cmodif
search_secondaries1([col(Table,Col)|R], KindCombi, Bound, OptionsBoolCond, PrevInputParams, InputParams, MaxN, CallMap, Sout, SecNotFound, NewInputParams) :- % cmodif
(OptionsBoolCond = [] -> write('option bool cond is empty') ; true),
    (PrevInputParams = [] ->                                                            % if on first call (no previous input parameters)
        retractall(max_cost(_)),                                                        % to avoid considering the cost of the previous problem!
        length(InputParams, LenInputParams),                                            % get number of input parameters
        tab_get_fd(col(Table,Col), FD),                                                 % get functional dependencies determining Col
        search_with_small_fd_primaries(KindCombi, Bound, FD, LenInputParams, InputParams, OptionsBoolCond, MaxN, Table, Col, CallMap, Sout) % cmodif
    ;
        delete(InputParams, col(Table,Col), FilteredInputParams),                       % when try to find simpler formulae with secondary
        (max_cost(col(Table,Col),MaxCostCol) ->                                         % char. for which we just found a formula,
            true                                                                        % then InputParams will contain col(Table,Col),
        ;                                                                               % so remove it
            MaxCostCol = 100000                                                         % use appropriate cost when on 2nd call to search
        ),                                                                              % formulae that minimize the cost
        search_a_formula(MaxCostCol, KindCombi, Bound, OptionsBoolCond, PrevInputParams, FilteredInputParams, 0, MaxN, Table, Col, CallMap, Sout) % cmodif
    ),
    (max_cost(_) ->                                                                     % if found at least one formula for col(Table,Col)
        SecNotFound = NotFound,                                                         % then record the secondary characteristics as a new
        NewInputParams = [col(Table,Col)|NewInParams]                                   % input parameter (since found a formula for it)
    ;                                                                                   % otherwise record current secondary characteristics in
        SecNotFound    = [col(Table,Col)|NotFound],                                     % the list of secondary characteristics for which we
        NewInputParams = NewInParams                                                    % still need to find a formula
    ),                                                                                  % input parameters are kept the same
    search_secondaries1(R, KindCombi, Bound, OptionsBoolCond, PrevInputParams, InputParams, MaxN, CallMap, Sout, NotFound, NewInParams).

% used only in the context of ASSISTANT
search_secondaries([], _, _, _, _) :- !.
search_secondaries([col(Table,Col)|R], OptionsBoolCond, MaxN, CallMap, Sout) :-
   tab_get_name(col(Table,Col),Name),
   tab_get_inputs(Table, Inputs),     % get the list of all inputs
   findall(Input, (member(Input,Inputs),
                   tab_get_type(Input, Type),
                   memberchk(Type, [int,bool])),                                        % ignore input columns of types set and string
            InputsFiltered),
   write('Search formula for column '), write(Col), write(': '), write(Name), nl,
   statistics(total_runtime,[Start|_]),
   tab_get_nb_rows(col(Table,_), NbRows),
   (NbRows =< 10 ->
        TimeOut is 6000
   ;
    NbRows =< 100 ->
        TimeOut is 12000
   ;
    NbRows =< 1000 ->
        TimeOut is 20000
   ;
        TimeOut is 50000
   ),
   time_out(search_a_formula(100000, model_seeker, 0, OptionsBoolCond, [], InputsFiltered, 0, MaxN, Table, Col, CallMap, Sout), % cmodif
            TimeOut,
           _),
   statistics(total_runtime,[Stop|_]),
   Runtime is Stop - Start,
   write(Runtime), write('ms'), nl,
   search_secondaries(R, OptionsBoolCond, MaxN, CallMap, Sout).

write_col_names([]) :- !, nl.
write_col_names([col(Table,Col)|R]) :-
    tab_get_name(col(Table,Col), Name),
    write(Name),
    assert(missing(Table, col, Name)), % record characteristics for which could not find any formula as it will be put at the end of the conjecture file % modif
    (R = [_|_] ->
        (R = [_] -> write(' and ') ; write(', '))
    ;
        true
    ),
    write_col_names(R).

% search a formula for a given column Col of the table Table (KindCombi<>model_seeker means we search a conjecture for combinatorial objects,
%                                                             KindCombi=model_seeker if in ASSISTANT);
% when KindCombi<>model_seeker the formula we search for should only use characteristics that belong to InputParams;
% to know whether at least one formula was found or not, the caller can check whether the fact max_cost(Cost) was asserted or not.
search_a_formula(CurMaxCost, KindCombi, Bound, OptionsBoolCond, PrevInputParams, InputParams, ForceToUseOnlySmallFd, % cmodif
                 MaxN, Table, Col, CallMap, Sout) :-
    instrument_code(instrument_in_search_formula, CallMap),
    retractall(max_cost(_)),                                                    % to avoid considering the cost of the previous problem!
    CurMaxCost > 0,                                                             % otherwise found already a formula with the smallest possible cost
    tab_get_equal(col(Table,Col), Eq),                                          % (test CurMaxCost > 0 has to be done after retractall(max_cost(_)))
    tab_get_type(col(Table,Col), Type),
    tab_get_kind(col(Table,Col), Kind),
    tab_get_name(col(Table,Col), Name),
    tab_get_max_name_length(Table, secondary, MaxNameLength),
    atom_codes(Name, Codes),
    length(Codes, NameLength),
    NbWhiteSpaces is MaxNameLength-NameLength,                                  % number of additional white spaces
    (KindCombi \== model_seeker ->                                              % PrintEqCst is set in order to avoid to print more than
        (Kind = secondary ->                                                    % once trivial formulae of type Eq and Cst in the context
            PrintEqCst = 1                                                      % of a bound
        ;
        PrevInputParams = [] ->
            PrintEqCst = 1
        ;
            PrintEqCst = 0
        )
    ;
        PrintEqCst = 1
    ),
    (Eq > 0 ->
        get_first_equal_column(KindCombi, Table, Col, FirstCol),
        (Col = FirstCol -> FirstEq = 0 ; FirstEq = FirstCol)
    ;
        FirstEq = 0
    ),    
    (FirstEq > 0 ->                                                             % if column is equal to some previous column that is not a bound
        (PrintEqCst = 1 ->
            (memberchk(col(Table,FirstEq), InputParams) ->                      % is clearer if we always refer to the first column
                FoundEqual = 1                                                  % check that this first column belongs to the input parameters
            ;
                FoundEqual = 0
            ),
            (FoundEqual = 1 ->
                write_found_identical_column(KindCombi, Bound, Table, Col, FirstEq, MaxN, CallMap, Sout), % cmodif
                assert(max_cost(0)),                                            % assert to indicate that a formula was found
                assert(max_cost(col(Table,Col), 0))                             % max_cost\2 used by search_secondaries1
            ;
                false
            )
        ;
            true
        )
    ;
     Type = cst ->                                                              % if all values in the column are the same
        (PrintEqCst = 1 ->                                                      % then print the constant
            write_found_constant(KindCombi, Bound, Table, Col, MaxN, CallMap, Sout), % cmodif
            assert(max_cost(0)),                                                % assert to indicate that a formula was found
            assert(max_cost(col(Table,Col), 0))                                 % max_cost\2 used by search_secondaries1
        ;
            true
        )
    ;
        keep_conditional_formula(Kind, Col),                                    % if already found Boolean formula with small cost for Col do nothing
                                                                                % construct list of options (type of formula & formula params)
        get_table_entries(InputParams, col(Table,Col), ParamsOutput, TableEntries),
        Mode = main(ParamsOutput),                                              % the formula search is called from the main level
        gen_options_lists(KindCombi, col(Table,Col), Kind, OptionsBoolCond,     % when KindCombi<>model_seeker the options should only use char. that
                          PrevInputParams, InputParams, ForceToUseOnlySmallFd,  % belong to InputParams
                          TableEntries, Mode, ListsOptions, ColSetsBool),
        tab_get_min(col(Table,Col),                                             % get minimum value, as need to record it for
                        ValCol),                                                % Boolean formulae which are shifted by the min.value
        formula_generator(CallMap, KindCombi, OptionsBoolCond, ListsOptions, TableEntries, Mode, ColSetsBool, ValCol, Kind, Table, Col, CurMaxCost,
                          FormulaPattern, FormulaFamily, OptionUsedToGenerateConjecture, Cost, Result), % cmodif
        (Result = time_out ->
%           write('.'),
            instrument_code(instrument_time_out, CallMap),
            false                                                               % enforce backtrack to find a formula with some other option
        ;
            record_type_found_formula(FormulaPattern, Col, Cost),               % remove type prev.formula found for Col and record new one wit Cost
            retractall(max_cost(_)),
            retractall(max_cost(col(Table,Col), _)),
%           write('found formula: '), nl, write(FormulaPattern), nl, nl,
            NewMaxCost is Cost-1,
%           NewMaxCost is Cost+10,                                              % used to show all solutions: to catch symmetries
            assert(max_cost(NewMaxCost)),
            assert(max_cost(col(Table,Col), NewMaxCost)),                       % max_cost\2 used by search_secondaries1
            write('Cost = '),
            (Cost < 10 -> write(' ') ; true),
            write(Cost), write(':  '),
            write(Name),
            (Kind = secondary -> write_spaces(NbWhiteSpaces), write(' = ')  ;
             Kind = low       ->                              write(' >= ') ;
                                                              write(' =< ') ),
            ((formula_pattern_write(FormulaPattern),
              (FormulaFamily = decomp ->write(' % NEW DECOMP') ; true), nl,
              formula_pattern_create_term(FormulaPattern, FormulaTerm)) ->
                  true
             ;
                  write(error_output(FormulaPattern)),nl,nl, halt
            ),
            ((increment_cpt(conjecture_id, NextConjId),
              format(Sout,
                     'conjecture(~w,~w,~w,~w,~w,~w,~w,~w,~w).~n~n',
                     [id(KindCombi,Bound,NextConjId),
                     Kind,col(Table,Col),Name,MaxN,Cost,FormulaTerm,FormulaFamily,OptionUsedToGenerateConjecture]),
              instrument_code(instrument_found_conjecture, CallMap, Cost)) ->
                 true
            ;
                 nl,nl,nl,write('impossible to convert and store FormulaPattern below:'),nl,
                 write(FormulaPattern),nl,nl
            )
        )
    ),
    formula_generator_if_cost_reach_0,
    !.
search_a_formula(_, _, _, _, _, _, _, _, _, _, _, _).                           % succeed after trying the different options

formula_generator_if_cost_reach_0 :- max_cost(C), C =< 0, !.
formula_generator_if_cost_reach_0 :- false.

% KindCombi    : kind of combinatorial object % cmodif
% Bound        : considered map (a term low(Characteristics), up(Characteristics) for combinatorial object, or 0 for the model seeker)
% Table        : considered table
% Col          : column for which print a formula
% MaxN         : size that was used for generating the conjecture (0 if model seeker)
% CallMap      : information for the trace
% Sout         : output file to which write conjectures
write_found_constant(KindCombi, Bound, Table, Col, MaxN, CallMap, Sout) :- % cmodif
    tab_get_kind(col(Table,Col), KindCol),
    tab_get_name(col(Table,Col), NameCol),
    tab_get_max_name_length(Table, secondary, MaxNameLength),
    atom_codes(NameCol, Codes),
    length(Codes, NameLength),
    NbWhiteSpaces is MaxNameLength-NameLength,                            % number of additional white spaces
    tab_get_min(col(Table,Col), ValCol), write('Cost =  0:  '),
    write(NameCol),
    (KindCol = secondary -> write_spaces(NbWhiteSpaces), write(' = ')  ;
     KindCol = low       ->                              write(' >= ') ;
                                                         write(' =< ') ),
    write(ValCol), nl,
    increment_cpt(conjecture_id, NextConjId),                         % cmodif
    format(Sout,
           'conjecture(~w,~w,~w,~w,~w,~w,~w,~w,~w).~n~n',             % cmodif
           [id(KindCombi,Bound,NextConjId),                           % cmodif
            KindCol,col(Table,Col),NameCol,MaxN,0,t([],[],[],ValCol), % cmodif
            cst,[]]),                                                 % cmodif
    instrument_code(instrument_found_trivial, CallMap).

% KindCombi    : kind of combinatorial object % cmodif
% Bound        : considered map (a term low(Characteristics), up(Characteristics) for combinatorial object, or 0 for the model seeker)
% Table        : considered table
% Col          : column for which print a formula
% FirstEq      : reference to a column which is identical to column Col
% MaxN         : size that was used for generating the conjecture (0 if model seeker)
% CallMap      : information for the trace
% Sout         : output file to which write conjectures
write_found_identical_column(KindCombi, Bound, Table, Col, FirstEq, MaxN, CallMap, Sout) :- % cmodif
    tab_get_kind(col(Table,Col), KindCol),
    tab_get_name(col(Table,Col), NameCol),
    tab_get_max_name_length(Table, secondary, MaxNameLength),
    atom_codes(NameCol, Codes),
    length(Codes, NameLength),
    NbWhiteSpaces is MaxNameLength-NameLength,                            % number of additional white spaces
    tab_get_name(col(Table,FirstEq), NameFirstEq), write('Cost =  0:  '),
    write(NameCol),
    (KindCol = secondary -> write_spaces(NbWhiteSpaces), write(' = ')  ;
     KindCol = low       ->                              write(' >= ') ;
                                                         write(' =< ') ),
    write(NameFirstEq), nl,
    increment_cpt(conjecture_id, NextConjId),                                                % cmodif
    format(Sout,
           'conjecture(~w,~w,~w,~w,~w,~w,~w,~w,~w).~n~n',                                    % cmodif
           [id(KindCombi,Bound,NextConjId),                                                  % cmodif
            KindCol,                                                                         % cmodif
            col(Table,Col),                                                                  % cmodif 
            NameCol,                                                                         % cmodif 
            MaxN,                                                                            % cmodif 
            1,                                                                               % cmodif 
            t([col(Table,FirstEq)],[NameFirstEq],[VarEq],polynom([monome([t(VarEq,1)],1)])), % cmodif
            formula,                                                                         % cmodif
            []]),                                                                            % cmodif
    instrument_code(instrument_found_trivial, CallMap).

get_first_equal_column(model_seeker, Table, Col, FirstEqCol) :- % get leftmost column that is equal to col(Table,Col) and that is an input
    tab_get_equal(col(Table,Col), EqCol),
    EqCol > 0,                                                  % column EqCol is equal to column Col
    tab_get_inout(col(Table,EqCol), output),                    % in model_seeker mode if the equal column is excluded from potential inputs
    !,                                                          % we discard it
    FirstEqCol = Col.
get_first_equal_column(KindCombi, Table, Col, FirstEqCol) :-    % get leftmost column that is equal to col(Table,Col) and that is an input
    tab_get_equal(col(Table,Col), EqCol),
    EqCol > 0,                                                  % column EqCol is equal to column Col
    (tab_get_inout(col(Table,EqCol), input) ->
        true                                                    % check that column EqCol in also an input column
    ;
        tab_get_equal(col(Table,EqCol), EqEqCol),               % if column EqCol is not an input column
        (EqEqCol > 0 -> true ; false)                           % then check that column EqCol is also equal to some other column located at its left
    ),
    !,
    get_first_equal_column(KindCombi, Table, EqCol, FirstEqCol). % identify first column that is equal to colum EqCol
get_first_equal_column(_, _, FirstEqCol, FirstEqCol).    % stop the search as no previous column was both equal to col(Table,Col) and an input column

%--------------------------------------------------------------------------------------------------------------------------------------------
% put here as has to consult tables outside a module

get_number_of_found_bounds(KindCombi, FileName) :-                   % compute the number of bounds for which we found at least one formula
    consult(FileName),                                               % consult file containing all acquired conjectures
    findall(Params, conjecture(_, low, Params, _, _, _, _, _, _), LowParams), % cmodif
    sort(LowParams, SortedLowParams),
    length(SortedLowParams, LenSortedLowParams),
    nl,
    write('number of lower bounds for which found at least one formula: '),
    write(LenSortedLowParams), nl, nl,
    findall(Params, conjecture(_,  up, Params, _, _, _, _, _, _),  UpParams), % cmodif
    sort(UpParams, SortedUpParams),
    length(SortedUpParams, LenSortedUpParams),
    write('number of upper bounds for which found at least one formula: '),
    write(LenSortedUpParams), nl, nl,
    get_tables(KindCombi, 2, 1, low(_), TablesLow1),
    get_tables(KindCombi, 2, 2, low(_), TablesLow2),
    get_tables(KindCombi, 2, 3, low(_), TablesLow3),
    length(TablesLow1, LenTablesLow1),
    length(TablesLow2, LenTablesLow2),
    length(TablesLow3, LenTablesLow3),
    LenTablesLow is LenTablesLow1+LenTablesLow2+LenTablesLow3,
    write('number of lower bounds for which try to find a formula: '),
    write(LenTablesLow), nl,
    write('dispatched by number of parameters: '),
    write(LenTablesLow1), write('  '),
    write(LenTablesLow2), write('  '),
    write(LenTablesLow3), nl, nl,
    get_tables(KindCombi, 2, 1, up(_), TablesUp1),
    get_tables(KindCombi, 2, 2, up(_), TablesUp2),
    get_tables(KindCombi, 2, 3, up(_), TablesUp3),
    length(TablesUp1, LenTablesUp1),
    length(TablesUp2, LenTablesUp2),
    length(TablesUp3, LenTablesUp3),
    LenTablesUp is LenTablesUp1+LenTablesUp2+LenTablesUp3,
    write('number of upper bounds for which try to find a formula: '),
    write(LenTablesUp), nl,
    write('dispatched by number of parameters: '),
    write(LenTablesUp1), write('  '),
    write(LenTablesUp2), write('  '),
    write(LenTablesUp3), nl, nl.

check_conjectures(KindCombi, FileName) :-                            % check if the acquired conjectures still hold for the largest availabel size
    Zeros = t(0,0,0,0,0,0,0),                                        % counters associated with the different primary parameters (at most 7)
    low_up_attributes_combi(KindCombi, Bound_Attribute_Pair_List),
    assert_map_stats(Bound_Attribute_Pair_List, Zeros),
    (memberchk(KindCombi, [graph,
                           forest, forest0, tree,
                           partition, partition0,
                           group, cgroup]) ->
        true
    ;
        write(check_conjectures(KindCombi)), nl, halt
    ),    
    max_size_combi(KindCombi, MaxMaxN),                              % largest availabel size for which we generate data
    consult(FileName),                                               % consult file containing all acquired conjectures
    findall(conjecture(Id,Kind,col(Table,Col),Name,MaxN,Cost,FormulaTerm,ConjFamily,OptionUsed), % cmodif
            conjecture(Id,Kind,col(Table,Col),Name,MaxN,Cost,FormulaTerm,ConjFamily,OptionUsed), % cmodif
            ConjecturesToCheck),
    open(FileName, write, Sout),                                     % open current conjecture file to keep the checked conjectures
    write_conjecture_file_header(Sout),
    check_conjectures1(ConjecturesToCheck, KindCombi, MaxMaxN, Sout),% check conjectures one by one
    add_characteristics_for_which_could_not_find_any_formula_to_conjecture_file(Sout), % modif
    close(Sout),                                                     % close conjecture file that was rewritten
    findall(t(MapType, MapName, Type, Cpts), map_stat(MapType, MapName, Type, Cpts), Statistics),
    write_list(Statistics),
    findall(Cpts, map_stat(low, _, low,       Cpts), LowLowCpts), sum_list_terms(LowLowCpts, LowLowSums),
    findall(Cpts, map_stat(low, _, secondary, Cpts), LowSecCpts), sum_list_terms(LowSecCpts, LowSecSums),
    findall(Cpts, map_stat(up,  _, up,        Cpts), UpUpCpts),   sum_list_terms(UpUpCpts,   UpUpSums),
    findall(Cpts, map_stat(up,  _, secondary, Cpts), UpSecCpts),  sum_list_terms(UpSecCpts,  UpSecSums),
    write('Number of lower bounds:                                             '), write(LowLowSums), nl,
    write('Number of secondary characteristics associated with a lower bound:  '), write(LowSecSums), nl,
    write('Number of upper bounds:                                             '), write(UpUpSums),   nl,
    write('Number of secondary characteristics associated with an upper bound: '), write(UpSecSums),  nl.

check_conjectures1([], _, _, _)  :- !.
check_conjectures1([ConjectureToCheck|R], KindCombi, MaxMaxN, Sout) :-
    ConjectureToCheck = conjecture(Id,Kind,col(Table,Col),Name,MaxN,Cost,FormulaTerm,ConjFamily,OptionUsed), % cmodif
    check_conjecture(ConjectureToCheck, KindCombi, MaxN, MaxMaxN, ' is buggy', FoundError),      % check conjecture wrt table of size MaxN
    ((var(FoundError), MaxN < MaxMaxN) ->
        check_conjecture(ConjectureToCheck, KindCombi, MaxMaxN, MaxMaxN, ' failed', FoundError), % check conjecture wrt table of size MaxMaxN (largest size)
        (var(FoundError) ->                                                                      % record conjecture in conjecture file as both checks did not find any error
            format(Sout,                                                                         % cmodif
                   'conjecture(~w,~w,~w,~w,~w,~w,~w,~w,~w).~n~n',                                % cmodif
                   [Id,Kind,col(Table,Col),Name,MaxN,Cost,FormulaTerm,ConjFamily,OptionUsed])    % cmodif
        ;
            true
        )
    ;
     (var(FoundError), MaxN = MaxMaxN) ->                                                        % record conjecture in conjecture file as check did not find any error
        format(Sout,                                                                             % cmodif
               'conjecture(~w,~w,~w,~w,~w,~w,~w,~w,~w).~n~n',                                    % cmodif
               [Id,Kind,col(Table,Col),Name,MaxN,Cost,FormulaTerm,ConjFamily,OptionUsed])        % cmodif
    ;
        true
    ),
    check_conjectures1(R, KindCombi, MaxMaxN, Sout).

check_conjecture(ConjectureToCheck, KindCombi, MaxN, MaxMaxN, ErrorTypeToPrint, FoundError) :-
    ConjectureToCheck = conjecture(_Id,_Kind,col(Table,Col),_Name,_MaxNConj,_Cost,FormulaTerm,_ConjFamily,_OptionUsed), % get current conjecture % cmodif
    atoms_concat(['data/',KindCombi,'/data'], DirData),
    gen_table_and_metadata_file_names(DirData, MaxN, Table,          % construct name of file containing the table mentionned
                                      TableFile, _),                 % by the current conjecture
    consult(TableFile),                                              % load the table
    check_conjecture_wrt_table_entries(0, col(Table,Col),            % check current conjecture wrt all table entries
                                       MaxN, FormulaTerm,            % (0 as in checking mode)
                                       ErrorTypeToPrint, FoundError),% FoundError get fixed to 1 if the check did fail
    ((MaxN = MaxMaxN, var(FoundError)) ->                            % update statistics on the current map only for largest size
        signature(Table, _, Types),                                  % as no error at all for current check conjecture
        arg(Col, Types, t(_,Type,_)),
        functor(Types, t, NCol),
        findall(TypeI-NameI, (I in 1..NCol,
                              indomain(I),
                              arg(I, Types, t(NameI,TypeI,_)),
                              memberchk(TypeI, [low,up])
                             ),                                [MapType-MapName]),
        (Type = low       -> (MapType = low -> true ; write(check_conjecture_wrt_table_entry), nl, halt) ;
         Type = up        -> (MapType = up  -> true ; write(check_conjecture_wrt_table_entry), nl, halt) ;
         Type = secondary -> true                                                                        ;
                             write(check_conjecture_wrt_table_entry), nl, halt                           ),
        findall(Kind, (I in 1..NCol,
                       indomain(I),
                       arg(I, Types, t(_,Kind,_)),
                       Kind=primary
                      ),                           LPrimaries),      % get number of primaries arguments
        length(LPrimaries, NbPrimaries),                             % in order to increment the appropriate entry
        map_stat(MapType, MapName, Type, Cpts),                      % get counters of current map (one counter for each parameter)
        functor(Cpts, t, Arity),                                     % get maximum number of parameters of the different maps
        functor(NewCpts, t, Arity),                                  % create a new set of counters and update
        update_stat_counters(1, Arity, NbPrimaries, Cpts, NewCpts),  % it by copying all, except one, counters, and incrementing
        retractall(map_stat(MapType, MapName, Type, _)),             % the counter associated with position NbPrimaries
        assert(map_stat(MapType, MapName, Type, NewCpts))
    ;
        true
    ),
    remove_signature_and_table_facts(Table).                         % remove current table as finish to check current conjecture

assert_map_stats([], _) :- !.
assert_map_stats([Bound-Attribute|R], Zeros) :-
    assert(map_stat(Bound, Attribute, Bound,     Zeros)),
    assert(map_stat(Bound, Attribute, secondary, Zeros)),            % all input params are also secondary char.
    assert_map_stats(R, Zeros).

% Transfer=0 if in mode where we check a conjecture
% Transfer=1 if in mode where we try to transfer a conjecture
check_conjecture_wrt_table_entries(Transfer, col(Table,Col), MaxN, FormulaTerm, ErrorTypeToPrint, FoundError) :-
    signature(Table, _, Args),                                       % from the signature fact of Table get information on the columns of the table
    functor(Args, t, NbColumns),                                     % get number of columns of the table
    functor(Row, Table, NbColumns),                                  % create a term whose functor is the table name and whose arity is NbColumns
    check_conjecture_wrt_table_entries1(Transfer, col(Table,Col),    % check if conjecture is valid wrt the different rows of Table
                                        MaxN, FormulaTerm, Row, ErrorTypeToPrint, FoundError).

check_conjecture_wrt_table_entries1(Transfer, col(Table,Col), MaxN, FormulaTerm, Row, ErrorTypeToPrint, FoundError) :-
    call(Row),
    (check_conjecture_wrt_table_entry(col(Table,Col), MaxN, FormulaTerm,
                                      Row, ErrorTypeToPrint,         % if conjecture holds for current table entry
                                      FoundError) ->                 % then fail in order to get next table entry
        false
    ;                                                                % if conjecture does not hold for current table entry
        true                                                         % conjecture was wrong
    ),
    !,                                                               % if conjecture did not hold on one table entry cut away choice point
    (Transfer = 0 ->                                                 % if in mode where we check a conjecture
        true                                                         % then succeed (as conjecture is not valid for a table entry)
    ;                                                                % if in mode where we transfer a conjecture
        false                                                        % then fail (as the conjecture could not be transfered)
    ).                                       
check_conjecture_wrt_table_entries1(_, _, _, _, _, _, _).            % if conjecture holds for all rows of the table then succeed

check_conjecture_wrt_table_entry(col(Table,Col), MaxN, FormulaTerm, Row, ErrorTypeToPrint, FoundAnError) :-
    arg(Col, Row, ResultValue),
    FormulaTerm = t(ListOfParemeters, _ListOfNames, ListOfSourceVariables, SourceTerm),
    findall(Value, (member(col(Table,I),ListOfParemeters),arg(I,Row,Value)), ParametersValues),
    copy_term(ListOfSourceVariables-SourceTerm, ParametersValues-TargetTerm),
    eval_formula_term(TargetTerm, ValueTargetTerm),
    signature(Table, _, Types),
    arg(Col, Types, t(Name,Type,_)),
    (ResultValue = ValueTargetTerm ->
        true                           % FoundAnError remains free as did not find an error
    ;
        FoundAnError = 1,              % FoundAnError is fixed to 1 to indicate to the caller that we found an error
        write('conjecture '),
        write(FormulaTerm),
        write(' of type '),
        write(Type),
        write(' and name '),
        write(Name),
        write(ErrorTypeToPrint),
        write(' on row '),
        write(Row),
        write(' as expected value '),
        write(ResultValue),
        write(' is different from value computed by the conjecture, namely value '),
        write(ValueTargetTerm),
        write(' on size '),
        write(MaxN), nl,
        write('(maybe this row is missing from the table used to compute the invariant)'), nl,
        false
    ).

sum_list_terms([], t) :- !.
sum_list_terms(ListTerms, Res) :-
    ListTerms = [Term|_],
    functor(Term,  Functor, Arity),
    functor(Zeros, Functor, Arity),
    set_term_to_zero(Zeros),
    sum_list_terms(ListTerms, Zeros, Res).

set_term_to_zero(Term) :-
    functor(Term, _, Arity),
    set_term_to_zero(1, Arity, Term).

set_term_to_zero(I, Arity, _) :-
    I > Arity, !.
set_term_to_zero(I, Arity, Term) :-
    I =< Arity,
    arg(I, Term, 0),
    I1 is I+1,
    set_term_to_zero(I1, Arity, Term).

sum_list_terms([], ResCpts, ResCpts) :- !.
sum_list_terms([Cpts|R], CurCpts, ResCpts) :-
    sum_terms(Cpts, CurCpts, NewCpts),
    sum_list_terms(R, NewCpts, ResCpts).

sum_terms(Cpts, CurCpts, NewCpts) :-
    functor(Cpts, t, Arity),
    functor(NewCpts, t, Arity),
    sum_terms(1, Arity, Cpts, CurCpts, NewCpts).

sum_terms(I, N, _, _, _) :-
    I > N, !.
sum_terms(I, N, Cpts, CurCpts, NewCpts) :-
    I =< N,
    !,
    arg(I, Cpts, Cpt),
    arg(I, CurCpts, CurCpt),
    NewCpt is Cpt+CurCpt,
    arg(I, NewCpts, NewCpt),
    I1 is I+1,
    sum_terms(I1, N, Cpts, CurCpts, NewCpts).

update_stat_counters(I, N, _, _, _) :-
    I > N, !.
update_stat_counters(I, N, J, Cpts, NewCpts) :-
    I =< N,
    arg(I, Cpts, Cpt),
    (I = J -> Cpt1 is Cpt+1 ; Cpt1 = Cpt),
    arg(I, NewCpts, Cpt1),
    I1 is I+1,
    update_stat_counters(I1, N, J, Cpts, NewCpts).

%  for each combinaison C(IB,BC) (with input characteristic IC and bound characteristic BC) for which did not find a bound do
%    for each found conjecture C of smallest cost associated with the bound characteristics BC
%      check if conjecture C also hold for C(IB,BC)
%----------------------------------------------------------------------------------------------------------------------------
transfer_learning(KindCombi, KindBound, Bound) :-                                           % modif
    transfer_learning(KindCombi, KindBound, Bound, 1, 2, 1, 1).                             % modif

transfer_learning(KindCombi, KindBound, Bound, NParam) :-                                   % modif
    transfer_learning(KindCombi, KindBound, Bound, NParam, NParam, 1, 1).                   % modif

transfer_learning(KindCombi, KindBound, Bound, MinNParam, MaxNParam, SplitDiv, SplitMod) :- % modif
    functor(Bound, LowUp, 1),
    atoms_concat([KindCombi,'_',LowUp,'_',KindBound,'_',MinNParam,'_',MaxNParam,'_',SplitDiv,'_',SplitMod], FilesOutSuffix), % modif
    atoms_concat(['data/',KindCombi,'/found_conjectures_',FilesOutSuffix,'.pl'], ConjecturesFileName),
    consult(ConjecturesFileName),                                      % consult file containing all acquired conjectures
    NParam in MinNParam..MaxNParam,                                    % get all tables whose number of input parameters are in MinNParam..MaxNParam
    findall(Table,                                                     % (remark: NParam not fixed after the findall)
            gen_candidate_tables(Table, KindCombi, KindBound, Bound, NParam), Tables),
    member_split(Tables, SplitDiv, SplitMod, Table),                   % iterate over the tables corresponding to the parameters SplitDiv and SplitMod
    functor(Bound, FunctorBound, 1),                                   % get low or up
    (conjecture(_, FunctorBound, col(Table, _), _, _, _, _, _, _) ->   % if found a conjecture explaining the bound characteristic of Table % cmodif
        true                                                           % then do nothing
    ;                                                                  % if did not find a conjecture then try to transfer an acquired conjecture
        write('did not find a bound conjecture for '),
        write(Table), write(', so try to transfer an existing conjecture'), nl,
        max_size_combi(KindCombi,  MaxMaxN),
        MaxMaxN_1 is MaxMaxN-1,
        atoms_concat(['data/',KindCombi,'/datatransfer'], DirData),        
        gen_table_and_metadata_file_names(DirData, MaxMaxN_1, Table, TableFile, _),
        write(consult(TableFile)), nl,                                 % construct name of file containing the table Table
        consult(TableFile),                                            % load the table (where kept secondary char. with no fd)
        write(consulted), nl,
        findall(Formula-Col,
                conjecture(_,FunctorBound,Col,KindBound,_,_,Formula,_,_), % cmodif % get all formulae which may be transfered for explaining Table
                Candidates),                                           % keep only one candidate for each unique combination
        remove_duplicated_formulae(Candidates, CandidatesToLookAt),    % parameters and formula
        transfer_conjectures(CandidatesToLookAt, KindCombi, Table),    % check if one of the candidate conjecture actually applies for
        remove_signature_and_table_facts(Table)                        % the table for which did not find any bound
    ),
    false.
transfer_learning(_, _, _, _, _, _, _). % modif

remove_duplicated_formulae([], []) :- !.
remove_duplicated_formulae([F-_|R], Res) :-     % do not copy formula F is it occur in the rest of the list of formulae
    \+ \+ duplicated_formula(R, F), !,          % use double negation to cancel out the effect of unification in duplicated_formula
    remove_duplicated_formulae(R, Res).
remove_duplicated_formulae([F-C|R], [F-C|S]) :- % copy formula F as it does not occur in the rest of the list of formulae
    remove_duplicated_formulae(R, S).

duplicated_formula([F2-_|_], F1) :- % two formulae are identical iff, both
    F2 = t(_,Parameters,_,Formula), %  . they are parameterised by the set set of primary characteristics, and
    F1 = t(_,Parameters,_,Formula), %  . the formulae are identical
    !.
duplicated_formula([_|R], F) :-
    duplicated_formula(R, F).

transfer_conjectures([], _, _) :- !.
transfer_conjectures([Conjecture|_], KindCombi, Table) :- % search in the list of candidate conjectures
    write(try(Conjecture)), nl,
    transfer_conjecture(KindCombi, Conjecture, Table),    % a conjecture which could also hold for table Table
    !.
transfer_conjectures([_|R], KindCombi, Table) :-
    transfer_conjectures(R, KindCombi, Table).

transfer_conjecture(KindCombi, Conjecture, Table) :-
    Conjecture = FormulaTerm-_,
    FormulaTerm = t(ListOfParemeters, ListOfNames, ListOfSourceVariables, SourceTerm),
    add_key(ListOfNames, KeyListOfNames),
    signature(Table, _, Types),
    functor(Types, t, NCol),
    findall(Key-Ind, (member(Key-Name,KeyListOfNames),Ind in 1..NCol,indomain(Ind),arg(Ind,Types,t(Name,_,_))), KeyInd),    
    findall(I, (I in 1..NCol,indomain(I),arg(I,Types,t(_,Bound,_)),memberchk(Bound,[low,up])), [Col]),
    substitute_parameters(ListOfParemeters, Table, KeyInd, NewListOfParemeters),
    NewFormulaTerm = t(NewListOfParemeters, ListOfNames, ListOfSourceVariables, SourceTerm),
    max_size_combi(KindCombi,  MaxMaxN),
    MaxMaxN_1 is MaxMaxN-1,
    transfer_conjecture_wrt_table_entries(NewFormulaTerm, col(Table,Col), MaxMaxN_1, NewFormulaTerm).

substitute_parameters([], _, _, []) :- !.
substitute_parameters([col(_,Col)|R], Table, KeyInd, [col(Table,NewCol)|S]) :-
    memberchk(Col-NewCol, KeyInd),
    substitute_parameters(R, Table, KeyInd, S).

transfer_conjecture_wrt_table_entries(Conjecture, col(Table,Col), N, NewFormulaTerm) :-
    check_conjecture_wrt_table_entries(1, col(Table,Col), N, NewFormulaTerm, 'transfer', _), % 1 as in transfer mode
    !,
    write('--------------------------------------------------'), nl,
    write('transfer '), write(Conjecture), write(' to '), write(Table), nl, nl.

% compare conjectures found when
% (1) using only polynomials
% (2) using polynomials together with Boolean and conditionals
%-------------------------------------------------------------
cmp(KindCombi) :-
    low_up_attributes_combi(KindCombi, ListPairsBoundAttr),
    cmp1(ListPairsBoundAttr).

cmp1([]) :- !.
cmp1([Bound-Attribute|R]) :-
    write('----------------------- '),
    write(Bound),
    write(' '),
    write(Attribute),
    write(' -----------------------'),
    nl,
    atoms_concat(['conjectures_',Bound,'_',Attribute], Tempo),
    atom_concat(Tempo, '_bool.pl', Bool ),                     % conjectures_Bound_Attribute_bool.pl
    atom_concat(Tempo, '_poly.pl', Poly ),                     % conjectures_Bound_Attribute_poly.pl
    cmp_print(Bool, 'BoolCondPoly', Bound),
    cmp_print(Poly, 'Poly',         Bound),
    cmp1(R).

cmp_print(FileConjecture, Kind, BoundType) :-
    consult(FileConjecture),
    write(Kind), nl,
    get_type_simplest_formulae(BoundType,       _),
    get_type_simplest_formulae(secondary, _),
    retractall(conjecture(_,_,_,_,_,_,_,_,_)), % cmodif
    nl.

comp(KindCombi) :-
    findall(T, (bs(KindCombi, _, bool, _, C1, C2, C3), T is C1+C2+C3), TotalBool),
    findall(T, (bs(KindCombi, _, poly, _, C1, C2, C3), T is C1+C2+C3), TotalPoly),
    sumlist(TotalBool, NbBool),
    sumlist(TotalPoly, NbPoly),
    IncBool is ((NbBool-NbPoly)*100)/NbPoly,
    write(IncBool), nl,
    findall(T,  (bs(KindCombi, _, bool, _, C1, C2, _ ), T is C1+C2), TotalBool12),
    sumlist(TotalBool12, NbBool12),
    nl, nl, write(NbBool12), write('  '), write(NbBool), nl, nl,
    IncB is (100*NbBool12)/NbPoly,
    write(IncB), nl.

% result for the graph (TODO: add results for the forest, forest0, tree, partition, partition0, group and cgroup when have the corresponding maps)
bs(graph, low_c,    bool, bound,  13,  0,  23).
bs(graph, low_c,    bool, second, 34, 25, 171).
bs(graph, low_c,    poly, bound,   0,  0,  36).
bs(graph, low_c,    poly, second,  0,  0, 223).
bs(graph, low_maxc, bool, bound,   0,  4,  21).
bs(graph, low_maxc, bool, second,  0,  9,  99).
bs(graph, low_maxc, poly, bound,   0,  0,  25).
bs(graph, low_maxc, poly, second,  0,  0, 104).
bs(graph, low_maxs, bool, bound,   0,  1,  24).
bs(graph, low_maxs, bool, second,  0,  7,  66).
bs(graph, low_maxs, poly, bound,   0,  0,  24).
bs(graph, low_maxs, poly, second,  0,  0,  73).
bs(graph, low_mina, bool, bound,   0,  7,  43).
bs(graph, low_mina, bool, second, 38, 95, 212).
bs(graph, low_mina, poly, bound,   0,  0,  50).
bs(graph, low_mina, poly, second,  0,  0, 317).
bs(graph, low_minc, bool, bound,   0, 17,  20).
bs(graph, low_minc, bool, second, 10, 10,   7).
bs(graph, low_minc, poly, bound,   0,  0,  36).
bs(graph, low_minc, poly, second,  0,  0,  27).
bs(graph, low_mins, bool, bound,   0, 13,  24).
bs(graph, low_mins, bool, second,  0,  5,   1).
bs(graph, low_mins, poly, bound,   0,  0,  37).
bs(graph, low_mins, poly, second,  0,  0,   6).
bs(graph, low_s,    bool, bound,   5,  4,  26).
bs(graph, low_s,    bool, second, 32, 27, 172).
bs(graph, low_s,    poly, bound,   0,  0,  35).
bs(graph, low_s,    poly, second,  0,  0, 228).
bs(graph, low_v,    bool, bound,   0, 11,  26).
bs(graph, low_v,    bool, second, 30, 64, 179).
bs(graph, low_v,    poly, bound,   0,  0,  36).
bs(graph, low_v,    poly, second,  0,  0, 258).
bs(graph, up_c,     bool, bound,   0,  1,  20).
bs(graph, up_c,     bool, second,  9, 16,  57).
bs(graph, up_c,     poly, bound,   0,  0,  21).
bs(graph, up_c,     poly, second,  0,  0,  79).
bs(graph, up_maxc,  bool, bound,   0,  4,  12).
bs(graph, up_maxc,  bool, second, 32, 27,  82).
bs(graph, up_maxc,  poly, bound,   0,  0,  16).
bs(graph, up_maxc,  poly, second,  0,  0, 137).
bs(graph, up_maxs,  bool, bound,   0,  2,  16).
bs(graph, up_maxs,  bool, second, 15, 29,  44).
bs(graph, up_maxs,  poly, bound,   0,  0,  18).
bs(graph, up_maxs,  poly, second,  0,  0,  84).
bs(graph, up_maxa,  bool, bound,   0,  0,  20).
bs(graph, up_maxa,  bool, second, 46, 84, 254).
bs(graph, up_maxa,  poly, bound,   0,  0,  20).
bs(graph, up_maxa,  poly, second,  0,  0, 364).
bs(graph, up_minc,  bool, bound,   0, 10,  17).
bs(graph, up_minc,  bool, second, 16,  4,  85).
bs(graph, up_minc,  poly, bound,   0,  0,  27).
bs(graph, up_minc,  poly, second,  0,  0, 103).
bs(graph, up_mins,  bool, bound,   0,  2,  14).
bs(graph, up_mins,  bool, second,  1,  6,  26).
bs(graph, up_mins,  poly, bound,   0,  0,  16).
bs(graph, up_mins,  poly, second,  0,  0,  32).
bs(graph, up_s,     bool, bound,   0,  2,  18).
bs(graph, up_s,     bool, second,  5, 15,  55).
bs(graph, up_s,     poly, bound,   0,  0,  20).
bs(graph, up_s,     poly, second,  0,  0,  73).
bs(graph, up_v,     bool, bound,   0,  0,  11).
bs(graph, up_v,     bool, second,  0,  2,   1).
bs(graph, up_v,     poly, bound,   0,  0,  11).
bs(graph, up_v,     poly, second,  0,  0,   3).

get_type_simplest_formulae(Kind, Types) :-
    findall(Params, conjecture(_,Kind,Params,_,_,_,_,_,_), LParams), % cmodif
    sort(LParams, SortedLParams),
    get_all_conjectures_of_each_param(SortedLParams, Kind, LPConj),
    get_type_simplest_formula(LPConj, Types),
    findall(bool, member(_-bool, Types), LBool), length(LBool, NBool),
    findall(cond, member(_-cond, Types), LCond), length(LCond, NCond),
    findall(cond, member(_-poly, Types), LPoly), length(LPoly, NPoly),
    (Kind = secondary -> write('Secondary: ') ; write('Bound:     ')),
    write('nbool='), (NBool < 10 -> write('   ') ; NBool < 100 -> write('  ') ; write(' ')), write(NBool), write('  '),
    write('ncond='), (NCond < 10 -> write('   ') ; NCond < 100 -> write('  ') ; write(' ')), write(NCond), write('  '),
    write('npoly='), (NPoly < 10 -> write('   ') ; NPoly < 100 -> write('  ') ; write(' ')), write(NPoly), write('  '),
    Total is NBool+NCond+NPoly,
    write('('), (Total < 10 -> write('  ') ; Total < 100 -> write(' ') ; true), write(Total), write(')'), nl.

get_all_conjectures_of_each_param([], _, []) :- !.
get_all_conjectures_of_each_param([P|R], Kind, [P-LConj|S]) :-
    findall(Conj, conjecture(_,Kind,P,_,_,_,t(_,_,_,Conj),_,_), LConj), % cmodif
    get_all_conjectures_of_each_param(R, Kind, S).

get_type_simplest_formula([], []) :- !.
get_type_simplest_formula([P-LConj|R], [P-Type|S]) :-
    get_type_simplest_conjectures(LConj, poly, Type),
    get_type_simplest_formula(R, S).

get_type_simplest_conjectures([], Type, Type) :- !.
get_type_simplest_conjectures([Conj|R], BestType, Type) :-
    functor(Conj, Functor, _),
    (Functor = bool -> Type = bool                                      ;
     Functor = not  -> Type = bool                                      ;
     Functor = if   -> get_type_simplest_conjectures(R, cond,     Type) ;
                       get_type_simplest_conjectures(R, BestType, Type) ).

% used only on combinatorial objects to generate the projection links of a map
%-----------------------------------------------------------------------------
% for handling upper bounds set LowerBound to 0, for handling lower bounds set LowerBound to 1
gen_links(KindCombi, KindBound, LowerBound) :-
    MinParam = 1, MaxParam = 3,
    gen_links(MinParam, MaxParam, KindCombi, KindBound, LowerBound).

gen_links(MinParam, MaxParam, KindCombi, KindBound, LowerBound) :-
    load_metadata(MinParam, MaxParam, KindCombi, KindBound, LowerBound, TablesByNParam),
    write('metadata loaded'), nl,
    write_list(TablesByNParam), nl,
    gen_links_to_sources(MinParam, MaxParam, TablesByNParam).

load_metadata(MinParam, MaxParam, _, _, _, []) :-
    MinParam > MaxParam,
    !.
load_metadata(MinParam, MaxParam, KindCombi, KindBound, LowerBound, [FilteredTableNames|R]) :-
    MinParam =< MaxParam,
    low_up_attributes_combi(KindCombi, ListPairsBoundAttribute),
    ((LowerBound = 1, memberchk(low-KindBound, ListPairsBoundAttribute)) -> Bound = low(_)                ;
     (LowerBound = 0, memberchk(up-KindBound,  ListPairsBoundAttribute)) -> Bound = up(_)                 ;
                                                                            write(load_metadata), nl, halt),
    NbVertices = 2,                                                               % use the smallest size NbVertices = 2
    get_tables(KindCombi, NbVertices, MinParam, Bound, TableNames),               % get all the table names
    filter_table_names_wrt_kind_bound(TableNames, KindBound, FilteredTableNames),
    load_metadata(KindCombi, FilteredTableNames, NbVertices),
    MinParam1 is MinParam+1,
    load_metadata(MinParam1, MaxParam, KindCombi, KindBound, LowerBound, R).

load_metadata(KindCombi, TableNames, NbVertices) :-
    member(Functor, TableNames),
    write('load '), write(Functor), nl,
    consult_metadata_of_functor(KindCombi, NbVertices, NbVertices, Functor),
    false.
load_metadata(_, _, _).

filter_table_names_wrt_kind_bound([], _, []) :- !.
filter_table_names_wrt_kind_bound([TableName|R], KindBound, [TableName|S]) :-
    atom_proper_suffix_add_underscore(TableName, KindBound),
    !,
    filter_table_names_wrt_kind_bound(R, KindBound, S).
filter_table_names_wrt_kind_bound([_|R], KindBound, S) :-
    filter_table_names_wrt_kind_bound(R, KindBound, S).

gen_links_to_sources(MaxParam, MaxParam, _) :- !.
gen_links_to_sources(MinParam, MaxParam, [_SourceTableNames|R]) :-
    MinParam < MaxParam,
    MinParam1 is MinParam+1,
    gen_links_to_sources(MinParam1, MaxParam, R).

consult_metadata_of_functor(KindCombi, MinNbVertices, MaxNbVertices, Functor) :- % put in the main as cannot consult from module
    MaxN in MinNbVertices..MaxNbVertices,
    indomain(MaxN),                                                              % go through all the table sizes
    number_codes(MaxN, CodesMaxN),                                               % convert MaxN to an atom in order to create
    atom_codes(AtomMaxN, CodesMaxN),                                             % the name of the subfolder that will contain
    atom_concat('data/', KindCombi, DirData0),                                   % create path to the directory that
    atom_concat(DirData0, '/data', DirData),                                     % corresponds to the combinatorial object KindCombi
    atom_concat(DirData, AtomMaxN, PrefixName0),                                 % all metadata files corresponding to KindCombi
    atom_concat(PrefixName0, '/', PrefixName),                                   % that have up to MaxN vertices
    atom_concat(PrefixName, Functor, PrefixNameFunctor),                         % directory name + table name
    atom_concat(PrefixNameFunctor, '_metadata.pl', ConsultFile),                 % name of metadata file
    consult(ConsultFile),
    false.
consult_metadata_of_functor(_, _, _, _).
