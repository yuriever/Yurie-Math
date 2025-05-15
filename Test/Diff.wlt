

(*Diff.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Diff.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Diff.nb"
]

VerificationTest[
    expr = D[f[x, y, z, w], {x, 1}, {y, 1}]*D[g[x, y, z, w], {z, 1}, {w, 1}]; 
    IBP[f][expr]
    ,
    f[x, y, z, w]*Derivative[1, 1, 1, 1][g][x, y, z, w]
    ,
    TestID->"2-Diff.nb"
]

VerificationTest[
    IBP[f, x][expr]
    ,
    (-Derivative[0, 1, 0, 0][f][x, y, z, w])*Derivative[1, 0, 1, 1][g][x, y, z, w]
    ,
    TestID->"3-Diff.nb"
]

VerificationTest[
    IBP[f, x, y][expr]
    ,
    f[x, y, z, w]*Derivative[1, 1, 1, 1][g][x, y, z, w]
    ,
    TestID->"4-Diff.nb"
]

VerificationTest[
    expr = f[x]*D[f[x], {x, 1}]; 
    IBP[f][expr]
    ,
    (-f[x])*Derivative[1][f][x]
    ,
    TestID->"5-Diff.nb"
]

VerificationTest[
    integrateChange[t^a, {t == 1 - x}, {t}, {x}]
    ,
    -(1 - x)^a
    ,
    TestID->"6-Diff.nb"
]

VerificationTest[
    integrateChange[t^a, {t == 1 - x}, {t}, {x}, -1]
    ,
    (1 - x)^a
    ,
    TestID->"7-Diff.nb"
]

VerificationTest[
    integrateChange[{t == 1 - x}, {t}, {x}, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"8-Diff.nb"
]

VerificationTest[
    integrateChange[{t == 1 - x}, {t}, {x}][t^a]
    ,
    -(1 - x)^a
    ,
    TestID->"9-Diff.nb"
]

VerificationTest[
    integrateChange[t^a, {t -> 1 - x}, {t}, {x}]
    ,
    -(1 - x)^a
    ,
    TestID->"10-Diff.nb"
]

VerificationTest[
    integrateChange[t^a, {t :> 1 - x}, {t}, {x}]
    ,
    -(1 - x)^a
    ,
    TestID->"11-Diff.nb"
]

VerificationTest[
    integrateChange[t, {x == t^2}, {t}, {x}]
    ,
    1/2
    ,
    TestID->"12-Diff.nb"
]

VerificationTest[
    integrateChange[t, {x == t^2}, {t}, {x}, "Solution" -> All]
    ,
    {1/2, 1/2}
    ,
    TestID->"13-Diff.nb"
]

VerificationTest[
    integrateChange[t, {x == t^2}, {t}, {x}, "Solution" -> {1, 2}]
    ,
    {1/2, 1/2}
    ,
    TestID->"14-Diff.nb"
]

VerificationTest[
    integrateChange[t, {x == t^2}, {t}, {x}, "Solution" -> 1 ;; 2]
    ,
    {1/2, 1/2}
    ,
    TestID->"15-Diff.nb"
]

VerificationTest[
    integrateChange[t, {x == t^2}, {t}, {x}, "Solution" -> 3]
    ,
    Quiet[{{t -> -Sqrt[x]}, {t -> Sqrt[x]}}]
    ,
    {Yurie`Math`Diff`Private`cleanSolve::argx}
    ,
    TestID->"16-Diff.nb"
]

VerificationTest[
    opts = Options[integrateChange]
    ,
    {"Solution" -> 1, "ShowSolution" -> False}
    ,
    TestID->"17-Diff.nb"
]

VerificationTest[
    SetOptions[integrateChange, "Solution" -> All]
    ,
    {"Solution" -> All, "ShowSolution" -> False}
    ,
    TestID->"18-Diff.nb"
]

VerificationTest[
    integrateChange[t, {x == t^2}, {t}, {x}]
    ,
    {1/2, 1/2}
    ,
    TestID->"19-Diff.nb"
]

VerificationTest[
    integrateChange[t, {x == t^2}, {t}, {x}, "Solution" -> 1]
    ,
    1/2
    ,
    TestID->"20-Diff.nb"
]

VerificationTest[
    SetOptions[integrateChange, opts]
    ,
    {"Solution" -> 1, "ShowSolution" -> False}
    ,
    TestID->"21-Diff.nb"
]

VerificationTest[
    integrateChange[{x == 2*t}, {x}, {t}][INT[x]*x^a]
    ,
    2^(1 + a)*t^a*INT[t]
    ,
    TestID->"22-Diff.nb"
]

VerificationTest[
    Simplify[diffChange[D[f[x, t], {t, 2}] == c^2*D[f[x, t], {x, 2}], {u == x + c*t, v == x - c*t}, {x, t}, {u, v}, {f[x, t]}]]
    ,
    c*Derivative[1, 1][f][u, v] == 0
    ,
    TestID->"23-Diff.nb"
]

VerificationTest[
    diffChange[g[t] + Derivative[1][f][t], {x == t^2}, {t}, {x}, {f[t], g[t]}]
    ,
    g[x] - 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"24-Diff.nb"
]

VerificationTest[
    diffChange[g[t] + Derivative[1][f][t], {x == t^2}, {t}, {x}, {f[t], g[t]}, "Solution" -> All]
    ,
    {g[x] - 2*Sqrt[x]*Derivative[1][f][x], g[x] + 2*Sqrt[x]*Derivative[1][f][x]}
    ,
    TestID->"25-Diff.nb"
]

VerificationTest[
    diffChange[g[t] + Derivative[1][f][t], {x -> t^2}, {t}, {x}, {f[t], g[t]}]
    ,
    g[x] - 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"26-Diff.nb"
]

VerificationTest[
    diffChange[{x -> t^2}, {t}, {x}, {f[t], g[t]}][g[t] + Derivative[1][f][t]]
    ,
    g[x] - 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"27-Diff.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Diff.nb"
]