

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
    linearQ[a^2*x - y + b^2, {x, y}]
    ,
    True
    ,
    TestID->"4-Quest.nb"
]

VerificationTest[
    linearQ[x, {x, y}]
    ,
    True
    ,
    TestID->"5-Quest.nb"
]

VerificationTest[
    linearQ[y, x]
    ,
    False
    ,
    TestID->"6-Quest.nb"
]

VerificationTest[
    linearQ[y, {x}]
    ,
    False
    ,
    TestID->"7-Quest.nb"
]

VerificationTest[
    linearQ[f[x], {x, y}]
    ,
    False
    ,
    TestID->"8-Quest.nb"
]

VerificationTest[
    FreeQ[(x_) + (y_)][f[a + b]]
    ,
    False
    ,
    TestID->"9-Quest.nb"
]

VerificationTest[
    presentQ[(x_) + (y_)][f[a + b]]
    ,
    True
    ,
    TestID->"10-Quest.nb"
]

VerificationTest[
    FreeQ[_Integer][f[a + b]]
    ,
    True
    ,
    TestID->"11-Quest.nb"
]

VerificationTest[
    presentQ[_Integer][f[a + b]]
    ,
    False
    ,
    TestID->"12-Quest.nb"
]

VerificationTest[
    patternPresentQ[a + b]
    ,
    False
    ,
    TestID->"13-Quest.nb"
]

VerificationTest[
    patternPresentQ[_Times]
    ,
    True
    ,
    TestID->"14-Quest.nb"
]

VerificationTest[
    syntacticNegativeQ[0]
    ,
    False
    ,
    TestID->"15-Quest.nb"
]

VerificationTest[
    list = {1, a, a[1], a[1, 2], a[1][2], f[a + b], a + b, a*b, a/b}; 
    ,
    Null
    ,
    TestID->"16-Quest.nb"
]

VerificationTest[
    (Map[syntacticNegativeQ])[list]
    ,
    {False, False, False, False, False, False, False, False, False}
    ,
    TestID->"17-Quest.nb"
]

VerificationTest[
    (Map[syntacticNegativeQ])[-list]
    ,
    {True, True, True, True, True, True, True, True, True}
    ,
    TestID->"18-Quest.nb"
]

VerificationTest[
    syntacticNegativeQ[a - b]
    ,
    False
    ,
    TestID->"19-Quest.nb"
]

VerificationTest[
    syntacticNegativeQ[-a + b]
    ,
    True
    ,
    TestID->"20-Quest.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Quest.nb"
]