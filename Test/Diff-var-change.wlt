

(*Diff-var-change.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Diff-var-change.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[t == 1 - x, t, x][t^a]
    ,
    -(1 - x)^a
    ,
    TestID->"2-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[{t == 1 - x}, {t}, {x}][t^a]
    ,
    -(1 - x)^a
    ,
    TestID->"3-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[t == 1 - x, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"4-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[t -> 1 - x, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"5-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[t :> 1 - x, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"6-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == t^2, t, x][t]
    ,
    1/2
    ,
    TestID->"7-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == t^2, t, x, "Solution" -> All][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"8-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == t^2, t, x, "Solution" -> {1, 2}][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"9-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == t^2, t, x, "Solution" -> 1 ;; 2][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"10-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == t^2, t, x, "Solution" -> 3][t]
    ,
    Quiet[{{t -> -Sqrt[x]}, {t -> Sqrt[x]}}]
    ,
    {Yurie`Math`solve::InvalidSolutionChoice}
    ,
    TestID->"11-Diff-var-change.nb"
]

VerificationTest[
    opts = Options[integrateChange]
    ,
    {"Solution" -> 1, "ShowSolution" -> False, "ShowJacobian" -> False}
    ,
    TestID->"12-Diff-var-change.nb"
]

VerificationTest[
    SetOptions[integrateChange, "Solution" -> All]
    ,
    {"Solution" -> All, "ShowSolution" -> False, "ShowJacobian" -> False}
    ,
    TestID->"13-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == t^2, t, x][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"14-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == t^2, t, x, "Solution" -> 1][t]
    ,
    1/2
    ,
    TestID->"15-Diff-var-change.nb"
]

VerificationTest[
    SetOptions[integrateChange, opts]
    ,
    {"Solution" -> 1, "ShowSolution" -> False, "ShowJacobian" -> False}
    ,
    TestID->"16-Diff-var-change.nb"
]

VerificationTest[
    integrateChange[x == 2*t, x, t][INT[x]*x^a]
    ,
    2^(1 + a)*t^a*INT[t]
    ,
    TestID->"17-Diff-var-change.nb"
]

VerificationTest[
    Simplify[diffChange[{u == x + c*t, v == x - c*t}, {x, t}, {u, v}, f[x, t]][D[f[x, t], {t, 2}] == c^2*D[f[x, t], {x, 2}]]]
    ,
    c*Derivative[1, 1][f][u, v] == 0
    ,
    TestID->"18-Diff-var-change.nb"
]

VerificationTest[
    diffChange[x == t^2, t, x, {f[t], g[t]}][g[t] + Derivative[1][f][t]]
    ,
    g[x] - 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"19-Diff-var-change.nb"
]

VerificationTest[
    diffChange[x == t^2, t, x, {f[t], g[t]}, "Solution" -> All][g[t] + Derivative[1][f][t]]
    ,
    {g[x] - 2*Sqrt[x]*Derivative[1][f][x], g[x] + 2*Sqrt[x]*Derivative[1][f][x]}
    ,
    TestID->"20-Diff-var-change.nb"
]

VerificationTest[
    diffChange[x -> t^2, t, x, {f[t], g[t]}][g[t] + Derivative[1][f][t]]
    ,
    g[x] - 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"21-Diff-var-change.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Diff-var-change.nb"
]