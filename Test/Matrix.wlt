

(*Matrix.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Matrix.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Matrix.nb"
]

VerificationTest[
    {Jx, Jy, Jz} = Table[matAngularMomentum[1][i], {i, {"x", "y", "z"}}]; 
    ,
    Null
    ,
    TestID->"2-Matrix.nb"
]

VerificationTest[
    matComm[Jx, Jy] - I*Jz
    ,
    {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}
    ,
    TestID->"3-Matrix.nb"
]

VerificationTest[
    matComm[Jy, Jz] - I*Jx
    ,
    {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}
    ,
    TestID->"4-Matrix.nb"
]

VerificationTest[
    matComm[Jz, Jx] - I*Jy
    ,
    {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}
    ,
    TestID->"5-Matrix.nb"
]

VerificationTest[
    J2 = Jx . Jx + Jy . Jy + Jz . Jz
    ,
    {{2, 0, 0}, {0, 2, 0}, {0, 0, 2}}
    ,
    TestID->"6-Matrix.nb"
]

VerificationTest[
    matAngularMomentum[1/2] /@ {"x", "y", "z"} - (1/2)*PauliMatrix /@ Range[3]
    ,
    {{{0, 0}, {0, 0}}, {{0, 0}, {0, 0}}, {{0, 0}, {0, 0}}}
    ,
    TestID->"7-Matrix.nb"
]

VerificationTest[
    PauliMatrix /@ Range[3] - matPauli /@ Range[3]
    ,
    {{{0, 0}, {0, 0}}, {{0, 0}, {0, 0}}, {{0, 0}, {0, 0}}}
    ,
    TestID->"8-Matrix.nb"
]

VerificationTest[
    Table[matPauli[i], {i, 0, 3}]
    ,
    {{{1, 0}, {0, 1}}, {{0, 1}, {1, 0}}, {{0, -I}, {I, 0}}, {{1, 0}, {0, -1}}}
    ,
    TestID->"9-Matrix.nb"
]

VerificationTest[
    Table[matPauli[i], {i, {"t", "x", "y", "z"}}]
    ,
    {{{1, 0}, {0, 1}}, {{0, 1}, {1, 0}}, {{0, -I}, {I, 0}}, {{1, 0}, {0, -1}}}
    ,
    TestID->"10-Matrix.nb"
]

VerificationTest[
    Table[matDirac[i], {i, 0, 3}]
    ,
    {{{0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}}, {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}, {{0, 0, 0, -I}, {0, 0, I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}, {{0, 0, 1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}}
    ,
    TestID->"11-Matrix.nb"
]

VerificationTest[
    matDirac[5]
    ,
    {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}
    ,
    TestID->"12-Matrix.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Matrix.nb"
]