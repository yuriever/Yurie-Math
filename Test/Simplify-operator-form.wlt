

(*Simplify-operator-form.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Simplify-operator-form.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Simplify-operator-form.nb"
]

VerificationTest[
    times[y, z][x]
    ,
    x*y*z
    ,
    TestID->"2-Simplify-operator-form.nb"
]

VerificationTest[
    divide[y, z][x]
    ,
    x/(y*z)
    ,
    TestID->"3-Simplify-operator-form.nb"
]

VerificationTest[
    plus[y, z][x]
    ,
    x + y + z
    ,
    TestID->"4-Simplify-operator-form.nb"
]

VerificationTest[
    minus[y, z][x]
    ,
    x - y - z
    ,
    TestID->"5-Simplify-operator-form.nb"
]

VerificationTest[
    m = n; 
    Table[m, {n, 2}]
    ,
    {1, 2}
    ,
    TestID->"6-Simplify-operator-form.nb"
]

VerificationTest[
    modularize[Table[m, {n, 2}]]
    ,
    {n, n}
    ,
    TestID->"7-Simplify-operator-form.nb"
]

VerificationTest[
    {block[{n = 1}][m], m, n}
    ,
    {1, n, n}
    ,
    TestID->"8-Simplify-operator-form.nb"
]

VerificationTest[
    {module[{m = 1}][m], m, n}
    ,
    {1, n, n}
    ,
    TestID->"9-Simplify-operator-form.nb"
]

VerificationTest[
    ClearAll[m]; 
    ,
    Null
    ,
    TestID->"10-Simplify-operator-form.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Simplify-operator-form.nb"
]