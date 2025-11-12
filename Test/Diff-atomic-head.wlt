

(* Diff-atomic-head.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Diff-atomic-head.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Diff-atomic-head.nb"
]

VerificationTest[
    PD[z[1]]*PD[z[1]]
    ,
    PD[z[1], z[1]]
    ,
    TestID->"[2] Diff-atomic-head.nb"
]

VerificationTest[
    PD[z[1]]*PD[z[2]]
    ,
    PD[z[1], z[2]]
    ,
    TestID->"[3] Diff-atomic-head.nb"
]

VerificationTest[
    PD[z[1], z[2]]*PD[z[3]]^2
    ,
    PD[z[1], z[2], z[3], z[3]]
    ,
    TestID->"[4] Diff-atomic-head.nb"
]

VerificationTest[
    (ReplaceAll[PD[z[3], rest___] :> PD[rest]])[PD[z[1], z[2]]*PD[z[3]]^2]
    ,
    PD[z[1], z[2], z[3]]
    ,
    TestID->"[5] Diff-atomic-head.nb"
]

VerificationTest[
    PD[x]/PD[y]
    ,
    PD[x]/PD[y]
    ,
    TestID->"[6] Diff-atomic-head.nb"
]

VerificationTest[
    PD[x, y, z, z]/PD[z, x]
    ,
    PD[y, z]
    ,
    TestID->"[7] Diff-atomic-head.nb"
]

VerificationTest[
    PD[x, y, z, z]/PD[z, z, x, w]
    ,
    PD[y]/PD[w]
    ,
    TestID->"[8] Diff-atomic-head.nb"
]

VerificationTest[
    INT[x, y]/INT[x]
    ,
    INT[y]
    ,
    TestID->"[9] Diff-atomic-head.nb"
]

VerificationTest[
    INT[x, y, z, x]
    ,
    Quiet[INT[x, y, z]]
    ,
    {Yurie`Math`INT::Duplicate}
    ,
    TestID->"[10] Diff-atomic-head.nb"
]

VerificationTest[
    SUM[x, y]/SUM[x]
    ,
    SUM[y]
    ,
    TestID->"[11] Diff-atomic-head.nb"
]

VerificationTest[
    SUM[x, y, z, x]
    ,
    Quiet[SUM[x, y, z]]
    ,
    {Yurie`Math`SUM::Duplicate}
    ,
    TestID->"[12] Diff-atomic-head.nb"
]

VerificationTest[
    SUM[x]/SUM[x, y]
    ,
    1/SUM[y]
    ,
    TestID->"[13] Diff-atomic-head.nb"
]

VerificationTest[
    expr = PD[x, y]*f[x] + PD[x]*g[x] + h[y]
    ,
    h[y] + g[x]*PD[x] + f[x]*PD[x, y]
    ,
    TestID->"[14] Diff-atomic-head.nb"
]

VerificationTest[
    expr2 = PD[x, y]*f[x] + g[PD[x]*g[x]]
    ,
    g[g[x]*PD[x]] + f[x]*PD[x, y]
    ,
    TestID->"[15] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][expr]
    ,
    {{x} -> g[x], {x, y} -> f[x], {} -> h[y]}
    ,
    TestID->"[16] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][expr2]
    ,
    Quiet[g[g[x]*PD[x]] + f[x]*PD[x, y]]
    ,
    {Yurie`Math`PDCoefficient::nonlinear}
    ,
    TestID->"[17] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][PD[z]*a + b]
    ,
    {{z} -> a, {} -> b}
    ,
    TestID->"[18] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][PD[z]*a]
    ,
    {{z} -> a}
    ,
    TestID->"[19] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][PD[z, zb]*a]
    ,
    {{z, zb} -> a}
    ,
    TestID->"[20] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][PD[z]]
    ,
    {{z} -> 1}
    ,
    TestID->"[21] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][PD[z, zb]]
    ,
    {{z, zb} -> 1}
    ,
    TestID->"[22] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][a]
    ,
    {{} -> a}
    ,
    TestID->"[23] Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[][a*b]
    ,
    {{} -> a*b}
    ,
    TestID->"[24] Diff-atomic-head.nb"
]

VerificationTest[
    INTCancel[x, y][{f[x], f[x]*INT[x, y, z], f[x]*INT[z, w], f[x]*INT[x] + 1}]
    ,
    {f[x], f[x]*INT[z], f[x]*INT[w, z], 1 + f[x]}
    ,
    TestID->"[25] Diff-atomic-head.nb"
]

VerificationTest[
    INTCancel[a][INT[a, b]]
    ,
    INT[b]
    ,
    TestID->"[26] Diff-atomic-head.nb"
]

VerificationTest[
    INTCancel[a][ConditionalExpression[INT[a, b], condition]]
    ,
    ConditionalExpression[INT[b], condition]
    ,
    TestID->"[27] Diff-atomic-head.nb"
]

VerificationTest[
    INTCancel[a][Piecewise[{{INT[a, b1], condition1}, {INT[a, b2], condition2}}, INT[a, b]]]
    ,
    Piecewise[{{INT[b1], condition1}, {INT[b2], condition2}}, INT[b]]
    ,
    TestID->"[28] Diff-atomic-head.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Diff-atomic-head.nb"
]