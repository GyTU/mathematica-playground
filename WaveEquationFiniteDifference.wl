(* ::Package:: *)

f0[x_, L_] := Sin[Pi / L * x];
SolveWaveEquation = Compile[{ {L, _Real}, {Nx, _Integer}, {T, _Real}, {Nt, _Integer}, {c, _Real} },
  Module[{dx, dt, C, x, n},
    dx = L / Nx;
    dt = T / Nt;
    C = c * dt / dx;
    x = Range[0, L, dx];
    u = Table[0, {Nt + 1}, {Nx + 1}];

    u[[1]] = f0[x, L];
    u[[All, 1]] = u[[All, Nx + 1]] = 0;
    u[[2, 2 ;; Nx]] = u[[1, 2 ;; Nx]] + 0.5 * C^2 * (u[[1, 3 ;; Nx + 1]] - 2*u[[1, 2 ;; Nx]] + u[[1, 1 ;; Nx - 1]]);

    Do[
      u[[n + 1, 2 ;; Nx]] = (
        2 * u[[n, 2 ;; Nx]] - u[[n - 1, 2 ;; Nx]] + C^2 * (u[[n, 3 ;; Nx + 1]] - 2*u[[n, 2 ;; Nx]] + u[[n, 1 ;; Nx - 1]])
      ), {n, 2, Nt}];
    0
  ], CompilationTarget -> "C"
];


L = 1;
Nx = 100;
T = 1;
Nt = 100;
c = 1.0;
Print["Timing:"];
(SolveWaveEquation[L, Nx, T, Nt, c] // AbsoluteTiming)[[1]]


Manipulate[
  Module[{dx, x, minY, maxY},
  dx = L / Nx;
  x = Range[0, L, dx];
  minY = Min[u];
  maxY = Max[u];

  ListPlot[
    Transpose[{x, u[[t]]}], Joined -> True,
    PlotRange -> { {0, L}, {minY, maxY} }]
  ], {t, 1, Nt + 1, 1}
]
