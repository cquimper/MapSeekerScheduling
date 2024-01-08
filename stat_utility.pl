% Purpose: Utilities for computing correlations and statistical criteria for evaluating
%          the statistical significance of correlations;
%          used for ranking functional dependencies when generating metadata for each input table.
% Author : Ramiz Gindullin, IMT Atlantique

:- module(stat_utility,[protected_correlation/3,
                        get_correlation_wrt_test/3,
                        f_multiple_correlation/4,
                        adjusted_multiple_correlation/4,
                        f_distribution/3,
                        critical_vals_for_pearson_correlation_coef/2]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(statistics)).
:- use_module(utility).

protected_correlation(Vector1, Vector2, Corr) :- % REMARK: catch a SICStus bug in correlation/3
    ((more_than_one_unique_value(Vector1),
      more_than_one_unique_value(Vector2)) ->
        correlation(Vector1, Vector2, Corr)
    ;
        Corr is 0
    ).

get_correlation_wrt_test(CalculatedCorrelationCoef, CriticalValueOfCorrelationCoef, ResultingCorrelationCoef) :-
    Abs is abs(CalculatedCorrelationCoef),
    (Abs > CriticalValueOfCorrelationCoef ->        % if a correlation passes the test,
        ResultingCorrelationCoef is -Abs            % then calculated coefficient value is taken,
    ;                                               % otherwise correlation coefficient is equal zero
        ResultingCorrelationCoef is 0
    ).

% Inputs
%  R2: squared unadjusted multiple correlation coefficient
%  N : number of rows in the table
%  K : number of independant values (number of columns in the functional dependency in our context)
% Output:
%  F : value of the F-test for the multiple correlation coefficient
%      (taken from https://www.jstor.org/stable/2683460?seq=1#metadata_info_tab_contents)
f_multiple_correlation(R2, N, K, F) :-
    ((R2 >= 0.99999, R2 < 1.00001) ->
        F is 1000                                % has to be greater than the maximum value of f_table
    ;
        Div is N - K - 1,
        (Div =\= 0 ->
                F is (R2 / K) / ((1 - R2) / (N - K - 1))
        ; 
                F is 0                           % In case of division by zero for super small tables
        )
    ).

% Input
%  OutputValues:                         list of values of ouput columns
%  IntputValues:                         list of sublists where each sublist corresponds to the input values used to compute the corresponding output value,
%                                        e.g. in the example [2,-1] are the two input values to which the output value 5 is associated.
% Output
%  AdjustedMultipleCorrelation:          calculated adjusted multiple correlation coefficient
%                                        (taken from https://www.jstor.org/stable/2683460?seq=1#metadata_info_tab_contents)
%  UnadjustedSquaredMultipleCorrelation: calculated unadjusted squared multiple correlation coefficient (R2 in the literature)
%
% Example: ?- adjusted_multiple_correlation([0,5,1], [[1,1],[2,-1],[2,1]], MultipleCorrelation, UnadjustedSquaredMultipleCorrelation).
adjusted_multiple_correlation(OutputValues, IntputValues, AdjustedMultipleCorrelation, UnadjustedSquaredMultipleCorrelation) :-
    multiple_correlation(OutputValues, IntputValues, MultipleCorrelation),
    UnadjustedSquaredMultipleCorrelation is MultipleCorrelation*MultipleCorrelation, 
    IntputValues = [Row|_],
    length(Row, K),
    length(OutputValues, N),
    Den is N - K - 1,
    (Den = 0 ->
        AdjustedSquaredMultipleCorrelation is 0
    ;
        AdjustedSquaredMultipleCorrelation is 1 - (((N - 1) * (1 - UnadjustedSquaredMultipleCorrelation)) / Den)
    ),
    (AdjustedSquaredMultipleCorrelation =< 0 ->
        AdjustedMultipleCorrelation = 0
    ;
        AdjustedMultipleCorrelation is sqrt(AdjustedSquaredMultipleCorrelation)
    ).

% Return the critical value of F-criteria for the significance level 0.05
% (data is from http://socr.ucla.edu/Applets.dir/F_Table.html )
f_distribution(DegreesOfFreedomCol, DegreesOfFreedomRow, FCriteria) :-
    (DegreesOfFreedomCol =<  10 -> Col is max(1, DegreesOfFreedomCol) ;
     DegreesOfFreedomCol =<  12 -> Col is 11                          ;
     DegreesOfFreedomCol =<  15 -> Col is 12                          ;
     DegreesOfFreedomCol =<  20 -> Col is 13                          ;
     DegreesOfFreedomCol =<  24 -> Col is 14                          ;
     DegreesOfFreedomCol =<  30 -> Col is 15                          ;
     DegreesOfFreedomCol =<  40 -> Col is 16                          ;
     DegreesOfFreedomCol =<  60 -> Col is 17                          ;
     DegreesOfFreedomCol =< 120 -> Col is 18                          ;
                                   Col is 19                          ),
    (DegreesOfFreedomRow =<  30 -> Row is max(1, DegreesOfFreedomRow) ;
     DegreesOfFreedomRow =<  40 -> Row is 31                          ;
     DegreesOfFreedomRow =<  60 -> Row is 32                          ;
     DegreesOfFreedomRow =< 120 -> Row is 33                          ;
                                   Row is 34                          ),
    f_table(F_Table),
    nth1(Row, F_Table, F_Vals),
    nth1(Col, F_Vals,  FCriteria).

% calculates critical value for linear (or Pearson's) correlation coefficient
% significance level - 0.05
% N - number of rows (number of entries)
% (taken http://site.iugaza.edu.ps/nbarakat/files/2010/02/correlation-table-pearson.pdf)
critical_vals_for_pearson_correlation_coef(N, RCrit) :-
    D is min(max(1, N-2), 100),
    r_table(R_Table),
    memberchk([D, RCrit], R_Table).

% Input
%  OutputValues   list of values of ouput columns
%  IntputValues: list of sublists where each sublist corresponds to the input values used to compute the corresponding output value;
%
% Output
%  MultipleCorrelation: calculated unadjusted multiple correlation coefficient
%                       (taken from https://encyclopediaofmath.org/wiki/Multiple-correlation_coefficient).
multiple_correlation(OutputValues, InputValues, MultipleCorrelation) :-
    OutputValues = [_|_],                                                         % multiple_correlation/3 fail on an empty list
    same_length(OutputValues, InputValues),
    transpose(InputValues, TInputValues),                                         % transpose set of rows to columns
    OutputInputValues = [OutputValues|TInputValues],                              % combine output values with input values
    length(OutputInputValues, K),                                                 % number of variables

    % GENERATE CORRELATION MATRIX: Step 1 - Generate upper half-matrix 
    findall(CorrRow, (I in 1..K, indomain(I),
                      length(CorrRow, K), nth1(I, CorrMatrix0, CorrRow),
                      findall(Corr, (J in 1..K, indomain(J),
                                     (I = J -> Corr is 1                        ; % if it's the same variable - no need to calculate correlation
                                      J > I -> nth1(I, OutputInputValues, Var1),  % if it is not, calculate correlation between them
                                               nth1(J, OutputInputValues, Var2),  % but only for pair (I,J), not for (J,I_)
                                               protected_correlation(Var1, Var2, Corr)
                                    ;
                                     Corr = _                                   ) % no need to calculate as symmetric
                                    ), CorrRow)), CorrMatrix0),

    % GENERATE CORRELATION MATRIX: Step 2 - Convert upper half-matrix to full symmetrical matrix
    findall(CorrRow, (I in 1..K, indomain(I),
                      length(CorrRow, K), nth1(I, CorrMatrix, CorrRow),
                      findall(Corr, (J in 1..K, indomain(J),
                                     (I =< J ->
                                      nth1(I, CorrMatrix0, RowI),
                                      nth1(J, RowI, Corr)
                                    ;
                                      nth1(J, CorrMatrix0, RowI),
                                      nth1(I, RowI, Corr))), CorrRow)), CorrMatrix),

    % CALCULATE THE DETERMINANT OF THE FULL CORRELATION MATRIX
    determinant(CorrMatrix, DetAll),

    % CALCULATE THE DETERMINANT OF THE FULL CORRELATION MATRIX WITHOUT CONSIDERING THE OUTPUT VECTOR (i.e. without the first column and the first row)
    co_factor(1, 1, CorrMatrix, CF11),
    determinant(CF11, Det11),

    % CALCULATE THE UNADJUSTED MULTIPLE CORRELATION COEFFICIENT R (as sqrt of R2)
    ((Det11 >= -0.00001, Det11 =< 0.00001) ->
        MultipleCorrelation = 0
    ;
        MultipleCorrelation is min(1,sqrt(1 - (DetAll / Det11)))
    ).

% calculates the determinant of a squared matrix
determinant([[Elem]], Elem) :- !.                       % trivial case - determinant of a 1x1 matrix is equal to the sole element of the matrix
determinant(Matrix, Det) :-
    Matrix = [_,_|_],                                   % we are not on a 1x1 squared matrix
    length(Matrix, N),                                  % matrix dimensions: N x N
    Matrix = [Row|_],                                   % take the first row of the matrix to use it for the calculations
    switch_signs(Row, Vector),                          % every element of the first row is multiplied by (-1)^(1 + j), where j is column number
    findall(Det0, (I in 1..N, indomain(I),              % caclulate determinants of co_factors
                   co_factor(1, I, Matrix, CoFactor),   % and put them in a vector 
                   determinant(CoFactor, Det0)), Dets),
    scalar_prod(Dets, Vector, Det).                     % determinant is a scalar product of first row (after (-1)^(1 + j)) and co_factor determinants vector

% generates the co-factor (matrix) for a given squared matrix Matrix wrt row I and column J
co_factor(I, J, Matrix, CoFactor) :-
    length(Matrix, N),
    findall(Row, (I1 in 1..N, indomain(I1),                                     
                  I1 =\= I, nth1(I1, Matrix, Row0),                             
                  findall(Elem, (J1 in 1..N, indomain(J1),                      
                                 J1 =\= J, nth1(J1, Row0, Elem)), Row)          
                  ), CoFactor).

% multiplies elements of a given vector to elements of vector of [1, -1, 1, -1, 1, ...], accordingly
switch_signs([], []) :- !.
switch_signs([Elem|R], [Elem|S]) :-
    switch_signs1(R, S).

switch_signs1([], []) :- !.
switch_signs1([Elem|R], [Elem1|S]) :-
    Elem1 is -Elem,
    switch_signs(R, S).

f_table([[161.4476,199.5000,215.7073,224.5832,230.1619,233.9860,236.7684,238.8827,240.5433,241.8817,243.9060,245.9499,248.0131,249.0518,250.0951,251.1432,252.1957,253.2529,254.3144],
    [18.5128,19.0000,19.1643,19.2468,19.2964,19.3295,19.3532,19.3710,19.3848,19.3959,19.4125,19.4291,19.4458,19.4541,19.4624,19.4707,19.4791,19.4874,19.4957],
    [10.1280,9.5521,9.2766,9.1172,9.0135,8.9406,8.8867,8.8452,8.8123,8.7855,8.7446,8.7029,8.6602,8.6385,8.6166,8.5944,8.5720,8.5494,8.5264],
    [7.7086,6.9443,6.5914,6.3882,6.2561,6.1631,6.0942,6.0410,5.9988,5.9644,5.9117,5.8578,5.8025,5.7744,5.7459,5.7170,5.6877,5.6581,5.6281],
    [6.6079,5.7861,5.4095,5.1922,5.0503,4.9503,4.8759,4.8183,4.7725,4.7351,4.6777,4.6188,4.5581,4.5272,4.4957,4.4638,4.4314,4.3985,4.3650],
    [5.9874,5.1433,4.7571,4.5337,4.3874,4.2839,4.2067,4.1468,4.0990,4.0600,3.9999,3.9381,3.8742,3.8415,3.8082,3.7743,3.7398,3.7047,3.6689],
    [5.5914,4.7374,4.3468,4.1203,3.9715,3.8660,3.7870,3.7257,3.6767,3.6365,3.5747,3.5107,3.4445,3.4105,3.3758,3.3404,3.3043,3.2674,3.2298],
    [5.3177,4.4590,4.0662,3.8379,3.6875,3.5806,3.5005,3.4381,3.3881,3.3472,3.2839,3.2184,3.1503,3.1152,3.0794,3.0428,3.0053,2.9669,2.9276],
    [5.1174,4.2565,3.8625,3.6331,3.4817,3.3738,3.2927,3.2296,3.1789,3.1373,3.0729,3.0061,2.9365,2.9005,2.8637,2.8259,2.7872,2.7475,2.7067],
    [4.9646,4.1028,3.7083,3.4780,3.3258,3.2172,3.1355,3.0717,3.0204,2.9782,2.9130,2.8450,2.7740,2.7372,2.6996,2.6609,2.6211,2.5801,2.5379],
    [4.8443,3.9823,3.5874,3.3567,3.2039,3.0946,3.0123,2.9480,2.8962,2.8536,2.7876,2.7186,2.6464,2.6090,2.5705,2.5309,2.4901,2.4480,2.4045],
    [4.7472,3.8853,3.4903,3.2592,3.1059,2.9961,2.9134,2.8486,2.7964,2.7534,2.6866,2.6169,2.5436,2.5055,2.4663,2.4259,2.3842,2.3410,2.2962],
    [4.6672,3.8056,3.4105,3.1791,3.0254,2.9153,2.8321,2.7669,2.7144,2.6710,2.6037,2.5331,2.4589,2.4202,2.3803,2.3392,2.2966,2.2524,2.2064],
    [4.6001,3.7389,3.3439,3.1122,2.9582,2.8477,2.7642,2.6987,2.6458,2.6022,2.5342,2.4630,2.3879,2.3487,2.3082,2.2664,2.2229,2.1778,2.1307],
    [4.5431,3.6823,3.2874,3.0556,2.9013,2.7905,2.7066,2.6408,2.5876,2.5437,2.4753,2.4034,2.3275,2.2878,2.2468,2.2043,2.1601,2.1141,2.0658],
    [4.4940,3.6337,3.2389,3.0069,2.8524,2.7413,2.6572,2.5911,2.5377,2.4935,2.4247,2.3522,2.2756,2.2354,2.1938,2.1507,2.1058,2.0589,2.0096],
    [4.4513,3.5915,3.1968,2.9647,2.8100,2.6987,2.6143,2.5480,2.4943,2.4499,2.3807,2.3077,2.2304,2.1898,2.1477,2.1040,2.0584,2.0107,1.9604],
    [4.4139,3.5546,3.1599,2.9277,2.7729,2.6613,2.5767,2.5102,2.4563,2.4117,2.3421,2.2686,2.1906,2.1497,2.1071,2.0629,2.0166,1.9681,1.9168],
    [4.3807,3.5219,3.1274,2.8951,2.7401,2.6283,2.5435,2.4768,2.4227,2.3779,2.3080,2.2341,2.1555,2.1141,2.0712,2.0264,1.9795,1.9302,1.8780],
    [4.3512,3.4928,3.0984,2.8661,2.7109,2.5990,2.5140,2.4471,2.3928,2.3479,2.2776,2.2033,2.1242,2.0825,2.0391,1.9938,1.9464,1.8963,1.8432],
    [4.3248,3.4668,3.0725,2.8401,2.6848,2.5727,2.4876,2.4205,2.3660,2.3210,2.2504,2.1757,2.0960,2.0540,2.0102,1.9645,1.9165,1.8657,1.8117],
    [4.3009,3.4434,3.0491,2.8167,2.6613,2.5491,2.4638,2.3965,2.3419,2.2967,2.2258,2.1508,2.0707,2.0283,1.9842,1.9380,1.8894,1.8380,1.7831],
    [4.2793,3.4221,3.0280,2.7955,2.6400,2.5277,2.4422,2.3748,2.3201,2.2747,2.2036,2.1282,2.0476,2.0050,1.9605,1.9139,1.8648,1.8128,1.7570],
    [4.2597,3.4028,3.0088,2.7763,2.6207,2.5082,2.4226,2.3551,2.3002,2.2547,2.1834,2.1077,2.0267,1.9838,1.9390,1.8920,1.8424,1.7896,1.7330],
    [4.2417,3.3852,2.9912,2.7587,2.6030,2.4904,2.4047,2.3371,2.2821,2.2365,2.1649,2.0889,2.0075,1.9643,1.9192,1.8718,1.8217,1.7684,1.7110],
    [4.2252,3.3690,2.9752,2.7426,2.5868,2.4741,2.3883,2.3205,2.2655,2.2197,2.1479,2.0716,1.9898,1.9464,1.9010,1.8533,1.8027,1.7488,1.6906],
    [4.2100,3.3541,2.9604,2.7278,2.5719,2.4591,2.3732,2.3053,2.2501,2.2043,2.1323,2.0558,1.9736,1.9299,1.8842,1.8361,1.7851,1.7306,1.6717],
    [4.1960,3.3404,2.9467,2.7141,2.5581,2.4453,2.3593,2.2913,2.2360,2.1900,2.1179,2.0411,1.9586,1.9147,1.8687,1.8203,1.7689,1.7138,1.6541],
    [4.1830,3.3277,2.9340,2.7014,2.5454,2.4324,2.3463,2.2783,2.2229,2.1768,2.1045,2.0275,1.9446,1.9005,1.8543,1.8055,1.7537,1.6981,1.6376],
    [4.1709,3.3158,2.9223,2.6896,2.5336,2.4205,2.3343,2.2662,2.2107,2.1646,2.0921,2.0148,1.9317,1.8874,1.8409,1.7918,1.7396,1.6835,1.6223],
    [4.0847,3.2317,2.8387,2.6060,2.4495,2.3359,2.2490,2.1802,2.1240,2.0772,2.0035,1.9245,1.8389,1.7929,1.7444,1.6928,1.6373,1.5766,1.5089],
    [4.0012,3.1504,2.7581,2.5252,2.3683,2.2541,2.1665,2.0970,2.0401,1.9926,1.9174,1.8364,1.7480,1.7001,1.6491,1.5943,1.5343,1.4673,1.3893],
    [3.9201,3.0718,2.6802,2.4472,2.2899,2.1750,2.0868,2.0164,1.9588,1.9105,1.8337,1.7505,1.6587,1.6084,1.5543,1.4952,1.4290,1.3519,1.2539],
    [3.8415,2.9957,2.6049,2.3719,2.2141,2.0986,2.0096,1.9384,1.8799,1.8307,1.7522,1.6664,1.5705,1.5173,1.4591,1.3940,1.3180,1.2214,1.0000]]).

r_table([[ 1,0.9969], [ 2,0.95],   [ 3,0.8783], [ 4,0.8114], [ 5,0.7545], [ 6,0.7067], [ 7,0.6664], [ 8,0.6319], [ 9,0.6021], [ 10,0.576],
         [11,0.5529], [12,0.5324], [13,0.514],  [14,0.4973], [15,0.4821], [16,0.4683], [17,0.4555], [18,0.4438], [19,0.4329], [ 20,0.4227],
         [21,0.4132], [22,0.4044], [23,0.3961], [24,0.3882], [25,0.3809], [26,0.3739], [27,0.3673], [28,0.361],  [29,0.355],  [ 30,0.3494],
         [31,0.344],  [32,0.3388], [33,0.3338], [34,0.3291], [35,0.3246], [36,0.3202], [37,0.316],  [38,0.312],  [39,0.3081], [ 40,0.3044],
         [41,0.3008], [42,0.2973], [43,0.294],  [44,0.2907], [45,0.2876], [46,0.2845], [47,0.2816], [48,0.2787], [49,0.2759], [ 50,0.2732],
         [51,0.2706], [52,0.2681], [53,0.2656], [54,0.2632], [55,0.2609], [56,0.2586], [57,0.2564], [58,0.2542], [59,0.2521], [ 60,0.25],
         [61,0.248],  [62,0.2461], [63,0.2441], [64,0.2423], [65,0.2404], [66,0.2387], [67,0.2369], [68,0.2352], [69,0.2335], [ 70,0.2319],
         [71,0.2303], [72,0.2287], [73,0.2272], [74,0.2257], [75,0.2242], [76,0.2227], [77,0.2213], [78,0.2199], [79,0.2185], [ 80,0.2172],
         [81,0.2159], [82,0.2146], [83,0.2133], [84,0.212],  [85,0.2108], [86,0.2096], [87,0.2084], [88,0.2072], [89,0.2061], [ 90,0.205],
         [91,0.2039], [92,0.2028], [93,0.2017], [94,0.2006], [95,0.1996], [96,0.1986], [97,0.1975], [98,0.1966], [99,0.1956], [100,0.1946]]).
