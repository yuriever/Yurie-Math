

(*Quest.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Quest.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Quest.nb"
]

VerificationTest[
    Simplify[Sin[n*Pi], Assumptions -> isZ[n]]
    ,
    0
    ,
    TestID->"2-Quest.nb"
]

VerificationTest[
    Simplify[Sqrt[x^2], Assumptions -> isRP[x]]
    ,
    x
    ,
    TestID->"3-Quest.nb"
]

VerificationTest[
    linearQ[x, y][a^2*x - y + b^2]
    ,
    True
    ,
    TestID->"4-Quest.nb"
]

VerificationTest[
    linearQ[x, y][1/x]
    ,
    False
    ,
    TestID->"5-Quest.nb"
]

VerificationTest[
    linearQ[x, y][f[x]]
    ,
    False
    ,
    TestID->"6-Quest.nb"
]

VerificationTest[
    FreeQ[(x_) + (y_)][f[a + b]]
    ,
    False
    ,
    TestID->"7-Quest.nb"
]

VerificationTest[
    presentQ[(x_) + (y_)][f[a + b]]
    ,
    True
    ,
    TestID->"8-Quest.nb"
]

VerificationTest[
    FreeQ[_Integer][f[a + b]]
    ,
    True
    ,
    TestID->"9-Quest.nb"
]

VerificationTest[
    presentQ[_Integer][f[a + b]]
    ,
    False
    ,
    TestID->"10-Quest.nb"
]

VerificationTest[
    patternPresentQ[a + b]
    ,
    False
    ,
    TestID->"11-Quest.nb"
]

VerificationTest[
    patternPresentQ[_Times]
    ,
    True
    ,
    TestID->"12-Quest.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Quest.nb"
]