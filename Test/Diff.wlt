

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
    integrate[{ϕ, 0, 1}][ϕ^a]
    ,
    1/(1 + a)
    ,
    TestID->"2-Diff.nb"
]

VerificationTest[
    integrate[{ϕ, 0, 1}, {η, 0, 1}][ϕ^a*η^b*INT[ϕ, κ]]
    ,
    INT[κ]/(1 + a + b + a*b)
    ,
    TestID->"3-Diff.nb"
]

VerificationTest[
    expr = D[f[x, y, z, w], {x, 1}, {y, 1}]*D[g[x, y, z, w], {z, 1}, {w, 1}]; 
    IBP[f][expr]
    ,
    f[x, y, z, w]*Derivative[1, 1, 1, 1][g][x, y, z, w]
    ,
    TestID->"4-Diff.nb"
]

VerificationTest[
    IBP[f, x][expr]
    ,
    (-Derivative[0, 1, 0, 0][f][x, y, z, w])*Derivative[1, 0, 1, 1][g][x, y, z, w]
    ,
    TestID->"5-Diff.nb"
]

VerificationTest[
    IBP[f, x, y][expr]
    ,
    f[x, y, z, w]*Derivative[1, 1, 1, 1][g][x, y, z, w]
    ,
    TestID->"6-Diff.nb"
]

VerificationTest[
    expr = f[x]*D[f[x], {x, 1}]; 
    IBP[f][expr]
    ,
    (-f[x])*Derivative[1][f][x]
    ,
    TestID->"7-Diff.nb"
]

VerificationTest[
    expr = D[f[x]*g[x], {x, 2}]
    ,
    2*Derivative[1][f][x]*Derivative[1][g][x] + g[x]*Derivative[2][f][x] + f[x]*Derivative[2][g][x]
    ,
    TestID->"8-Diff.nb"
]

VerificationTest[
    diffReplace[f -> x^2][expr]
    ,
    2*g[x] + 4*x*Derivative[1][g][x] + x^2*Derivative[2][g][x]
    ,
    TestID->"9-Diff.nb"
]

VerificationTest[
    diffReplace[f -> x^2, Inactive][expr]
    ,
    g[x]*Inactive[D][x^2, {x, 2}] + 2*Inactive[D][x^2, {x, 1}]*Derivative[1][g][x] + x^2*Derivative[2][g][x]
    ,
    TestID->"10-Diff.nb"
]

VerificationTest[
    diffReplace[f -> y^2][expr]
    ,
    y^2*Derivative[2][g][x]
    ,
    TestID->"11-Diff.nb"
]

VerificationTest[
    diffReplace[f -> y^2, Inactive][expr]
    ,
    g[x]*Inactive[D][y^2, {x, 2}] + 2*Inactive[D][y^2, {x, 1}]*Derivative[1][g][x] + y^2*Derivative[2][g][x]
    ,
    TestID->"12-Diff.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Diff.nb"
]