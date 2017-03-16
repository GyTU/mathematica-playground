(* ::Package:: *)

f0[x_, L_] := Sin[Pi / L * x];
SolveWaveEquation = Compile[{ {L, _Real}, {T, _Real}, {c, _Real} },
  sol = NDSolve[{
    D[u[x, t], t, t] == c^2*D[u[x, t], x, x],
    u[x, 0] == f0[x,L],
    u[0, t] == 0,
    u[L, t] == 0
    }, u, {t, 0, T}, {x, 0, L}, AccuracyGoal -> 30, PrecisionGoal -> 30
  ];
  0, CompilationTarget->"C"
];


L = 1;
T = 100;
c = 1.0;
Print["Timing:"];
(AbsoluteTiming@SolveWaveEquation[L,T,c])[[1]]


Manipulate[
  Plot[
    Evaluate[u[x,t]/.sol], {x, 0, L},
    PlotRange -> { {0, 1},{-1, 1} }
  ], {t, 0, T}
]
