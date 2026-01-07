

(* Diff-var-change.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Diff-var-change.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Diff-var-change.nb"
]

VerificationTest[
    Simplify[diffChange[{u == x + c*t, v == x - c*t}, {x, t}, {u, v}, f[x, t]][D[f[x, t], {t, 2}] == c^2*D[f[x, t], {x, 2}]]]
    ,
    c*Derivative[1, 1][f][u, v] == 0
    ,
    TestID->"[2] Diff-var-change.nb"
]

VerificationTest[
    diffChange[x == t^2, t, x, {f[t], g[t]}][g[t] + Derivative[1][f][t]]
    ,
    g[x] - 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"[3] Diff-var-change.nb"
]

VerificationTest[
    diffChange[x == t^2, t, x, {f[t], g[t]}, "Solution" -> All][g[t] + Derivative[1][f][t]]
    ,
    {g[x] - 2*Sqrt[x]*Derivative[1][f][x], g[x] + 2*Sqrt[x]*Derivative[1][f][x]}
    ,
    TestID->"[4] Diff-var-change.nb"
]

VerificationTest[
    diffChange[x == t^2, t, x, {f[t], g[t]}, "Solution" -> {1, 1}][g[t] + Derivative[1][f][t]]
    ,
    g[x] - 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"[5] Diff-var-change.nb"
]

VerificationTest[
    diffChange[x == t^2, t, x, {f[t], g[t]}, "Solution" -> {2, 1}][g[t] + Derivative[1][f][t]]
    ,
    g[x] + 2*Sqrt[x]*Derivative[1][f][x]
    ,
    TestID->"[6] Diff-var-change.nb"
]

VerificationTest[
    Simplify[diffChange[{u[1] == x[1, 1] + c*t[1], v[1] == x[1, 1] - c*t[1]}, {x[1, 1], t[1]}, {u[1], v[1]}, {f[x[1, 1], t[1]]}][D[f[x[1, 1], t[1]], {t[1], 2}] == c^2*D[f[x[1, 1], t[1]], {x[1, 1], 2}]]]
    ,
    c*Derivative[1, 1][f][u[1], v[1]] == 0
    ,
    TestID->"[7] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[{t == 1 - x}, {t}, {x}, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"[8] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[t == 1 - x, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"[9] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[t -> 1 - x, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"[10] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[t :> 1 - x, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"[11] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x -> 1 - t, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"[12] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x :> 1 - t, t, x, -1][t^a]
    ,
    (1 - x)^a
    ,
    TestID->"[13] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == t^2, t, x][t]
    ,
    1/2
    ,
    TestID->"[14] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == t^2, t, x, "Solution" -> All][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"[15] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == t^2, t, x, "Solution" -> {1, 2}][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"[16] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == t^2, t, x, "Solution" -> 1 ;; 2][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"[17] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == t^2, t, x, "Solution" -> 3][t]
    ,
    Quiet[{{t -> -Sqrt[x]}, {t -> Sqrt[x]}}]
    ,
    {Yurie`Math`solve::InvalidSolutionChoice}
    ,
    TestID->"[18] Diff-var-change.nb"
]

VerificationTest[
    opts = Options[integrationChange]
    ,
    {"Solution" -> 1, "ShowSolution" -> False, "ShowJacobian" -> False}
    ,
    TestID->"[19] Diff-var-change.nb"
]

VerificationTest[
    SetOptions[integrationChange, "Solution" -> All]
    ,
    {"Solution" -> All, "ShowSolution" -> False, "ShowJacobian" -> False}
    ,
    TestID->"[20] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == t^2, t, x][t]
    ,
    {1/2, 1/2}
    ,
    TestID->"[21] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == t^2, t, x, "Solution" -> 1][t]
    ,
    1/2
    ,
    TestID->"[22] Diff-var-change.nb"
]

VerificationTest[
    SetOptions[integrationChange, opts]
    ,
    {"Solution" -> 1, "ShowSolution" -> False, "ShowJacobian" -> False}
    ,
    TestID->"[23] Diff-var-change.nb"
]

VerificationTest[
    integrationChange[x == 2*t, x, t][INT[x]*x^a]
    ,
    2^(1 + a)*t^a*INT[t]
    ,
    TestID->"[24] Diff-var-change.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Diff-var-change.nb"
]