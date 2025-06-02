

(*Diff-atomic-head.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Diff-atomic-head.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Diff-atomic-head.nb"
]

VerificationTest[
    PD[z[1]]*PD[z[1]]
    ,
    PD[z[1], z[1]]
    ,
    TestID->"2-Diff-atomic-head.nb"
]

VerificationTest[
    PD[z[1]]*PD[z[2]]
    ,
    PD[z[1], z[2]]
    ,
    TestID->"3-Diff-atomic-head.nb"
]

VerificationTest[
    PD[z[1], z[2]]*PD[z[3]]^2
    ,
    PD[z[1], z[2], z[3], z[3]]
    ,
    TestID->"4-Diff-atomic-head.nb"
]

VerificationTest[
    (ReplaceAll[PD[z[3], rest___] :> PD[rest]])[PD[z[1], z[2]]*PD[z[3]]^2]
    ,
    PD[z[1], z[2], z[3]]
    ,
    TestID->"5-Diff-atomic-head.nb"
]

VerificationTest[
    PD[x]/PD[y]
    ,
    PD[x]/PD[y]
    ,
    TestID->"6-Diff-atomic-head.nb"
]

VerificationTest[
    PD[x, y, z, z]/PD[z, x]
    ,
    PD[y, z]
    ,
    TestID->"7-Diff-atomic-head.nb"
]

VerificationTest[
    PD[x, y, z, z]/PD[z, z, x, w]
    ,
    PD[y]/PD[w]
    ,
    TestID->"8-Diff-atomic-head.nb"
]

VerificationTest[
    expr = PD[x, y]*f[x] + PD[x]*g[x] + h[y]
    ,
    h[y] + g[x]*PD[x] + f[x]*PD[x, y]
    ,
    TestID->"9-Diff-atomic-head.nb"
]

VerificationTest[
    expr2 = PD[x, y]*f[x] + g[PD[x]*g[x]]
    ,
    g[g[x]*PD[x]] + f[x]*PD[x, y]
    ,
    TestID->"10-Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[expr]
    ,
    {{x} -> g[x], {x, y} -> f[x], {} -> h[y]}
    ,
    TestID->"11-Diff-atomic-head.nb"
]

VerificationTest[
    PDCoefficient[expr2]
    ,
    Quiet[g[g[x]*PD[x]] + f[x]*PD[x, y]]
    ,
    {Yurie`Math`PDCoefficient::nonlinear}
    ,
    TestID->"12-Diff-atomic-head.nb"
]

VerificationTest[
    INT[x, y]/INT[x]
    ,
    INT[y]
    ,
    TestID->"13-Diff-atomic-head.nb"
]

VerificationTest[
    INT[x, y, z, x]
    ,
    Quiet[INT[x, y, z]]
    ,
    {Yurie`Math`INT::duplicate}
    ,
    TestID->"14-Diff-atomic-head.nb"
]

VerificationTest[
    SUM[x, y]/SUM[x]
    ,
    SUM[y]
    ,
    TestID->"15-Diff-atomic-head.nb"
]

VerificationTest[
    SUM[x, y, z, x]
    ,
    Quiet[SUM[x, y, z]]
    ,
    {Yurie`Math`SUM::duplicate}
    ,
    TestID->"16-Diff-atomic-head.nb"
]

VerificationTest[
    SUM[x]/SUM[x, y]
    ,
    1/SUM[y]
    ,
    TestID->"17-Diff-atomic-head.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Diff-atomic-head.nb"
]