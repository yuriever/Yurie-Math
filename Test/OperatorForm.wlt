

(*OperatorForm.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-OperatorForm.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-OperatorForm.nb"
]

VerificationTest[
    times[y, z][x]
    ,
    x*y*z
    ,
    TestID->"2-OperatorForm.nb"
]

VerificationTest[
    divide[y, z][x]
    ,
    x/(y*z)
    ,
    TestID->"3-OperatorForm.nb"
]

VerificationTest[
    plus[y, z][x]
    ,
    x + y + z
    ,
    TestID->"4-OperatorForm.nb"
]

VerificationTest[
    minus[y, z][x]
    ,
    x - y - z
    ,
    TestID->"5-OperatorForm.nb"
]

VerificationTest[
    solve[{x, y}][{x == 0, y == 0}]
    ,
    {{x -> 0, y -> 0}}
    ,
    TestID->"6-OperatorForm.nb"
]

VerificationTest[
    solve[{x, y}, 1][{x == 0, y == 0}]
    ,
    {x -> 0, y -> 0}
    ,
    TestID->"7-OperatorForm.nb"
]

VerificationTest[
    solve[{x, y}, 2][{x == 0, y == 0}]
    ,
    Quiet[{{x -> 0, y -> 0}}]
    ,
    {Yurie`Math`solve::InvalidSolutionChoice}
    ,
    TestID->"8-OperatorForm.nb"
]

VerificationTest[
    solve1[{x, y}][{x == 0, y == 0}]
    ,
    {x -> 0, y -> 0}
    ,
    TestID->"9-OperatorForm.nb"
]

VerificationTest[
    solve[x][x^2 == 1]
    ,
    {{x -> -1}, {x -> 1}}
    ,
    TestID->"10-OperatorForm.nb"
]

VerificationTest[
    solve[x, 1][x^2 == 1]
    ,
    {x -> -1}
    ,
    TestID->"11-OperatorForm.nb"
]

VerificationTest[
    solve1[{y, z}][x == 0]
    ,
    Quiet[{}]
    ,
    {Yurie`Math`solve::NoSolution}
    ,
    TestID->"12-OperatorForm.nb"
]

VerificationTest[
    m = n; 
    Table[m, {n, 2}]
    ,
    {1, 2}
    ,
    TestID->"13-OperatorForm.nb"
]

VerificationTest[
    modularize[Table[m, {n, 2}]]
    ,
    {n, n}
    ,
    TestID->"14-OperatorForm.nb"
]

VerificationTest[
    {block[{n = 1}][m], m, n}
    ,
    {1, n, n}
    ,
    TestID->"15-OperatorForm.nb"
]

VerificationTest[
    {module[{m = 1}][m], m, n}
    ,
    {1, n, n}
    ,
    TestID->"16-OperatorForm.nb"
]

VerificationTest[
    ClearAll[m]; 
    ,
    Null
    ,
    TestID->"17-OperatorForm.nb"
]

VerificationTest[
    Normal[(Series[#1, {x, 0, 0}] & )[x]]
    ,
    x
    ,
    TestID->"18-OperatorForm.nb"
]

VerificationTest[
    series[{x, 0, 0}][x]
    ,
    0
    ,
    TestID->"19-OperatorForm.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-OperatorForm.nb"
]