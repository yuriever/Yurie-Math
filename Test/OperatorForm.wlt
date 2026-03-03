

(* OperatorForm.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] OperatorForm.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] OperatorForm.nb"
]

VerificationTest[
    times[y, z][x]
    ,
    x*y*z
    ,
    TestID->"[2] OperatorForm.nb"
]

VerificationTest[
    divide[y, z][x]
    ,
    x/(y*z)
    ,
    TestID->"[3] OperatorForm.nb"
]

VerificationTest[
    plus[y, z][x]
    ,
    x + y + z
    ,
    TestID->"[4] OperatorForm.nb"
]

VerificationTest[
    minus[y, z][x]
    ,
    x - y - z
    ,
    TestID->"[5] OperatorForm.nb"
]

VerificationTest[
    times[][x]
    ,
    x
    ,
    TestID->"[6] OperatorForm.nb"
]

VerificationTest[
    divide[][x]
    ,
    x
    ,
    TestID->"[7] OperatorForm.nb"
]

VerificationTest[
    plus[][x]
    ,
    x
    ,
    TestID->"[8] OperatorForm.nb"
]

VerificationTest[
    minus[][x]
    ,
    x
    ,
    TestID->"[9] OperatorForm.nb"
]

VerificationTest[
    Hold[Evaluate[plusSafe[a, b]]]
    ,
    Hold[a + b]
    ,
    TestID->"[10] OperatorForm.nb"
]

VerificationTest[
    Hold[Evaluate[plusSafe[{a, b}, {c, d}]]]
    ,
    Hold[{a + c, b + d}]
    ,
    TestID->"[11] OperatorForm.nb"
]

VerificationTest[
    Hold[Evaluate[plusSafe[{a, {b, c}}, {d, {e, f}}]]]
    ,
    Hold[{a + d, {b + e, c + f}}]
    ,
    TestID->"[12] OperatorForm.nb"
]

VerificationTest[
    Hold[Evaluate[plusSafe[a, {b, c}]]]
    ,
    Hold[plusSafe[a, {b, c}]]
    ,
    TestID->"[13] OperatorForm.nb"
]

VerificationTest[
    Hold[Evaluate[plusSafe[{a, b}, {c, d, e}]]]
    ,
    Hold[plusSafe[{a, b}, {c, d, e}]]
    ,
    TestID->"[14] OperatorForm.nb"
]

VerificationTest[
    Hold[Evaluate[plusSafe[{a, b}, {c, {d, e}}]]]
    ,
    Hold[{a + c, plusSafe[b, {d, e}]}]
    ,
    TestID->"[15] OperatorForm.nb"
]

VerificationTest[
    Hold[Evaluate[plusSafe[{a, {b, c}}, {d, {e, f, g}}]]]
    ,
    Hold[{a + d, plusSafe[{b, c}, {e, f, g}]}]
    ,
    TestID->"[16] OperatorForm.nb"
]

VerificationTest[
    solve[{x, y}][{x == 0, y == 0}]
    ,
    {{x -> 0, y -> 0}}
    ,
    TestID->"[17] OperatorForm.nb"
]

VerificationTest[
    solve[{x, y}, 1][{x == 0, y == 0}]
    ,
    {x -> 0, y -> 0}
    ,
    TestID->"[18] OperatorForm.nb"
]

VerificationTest[
    solve[{x, y}, 2][{x == 0, y == 0}]
    ,
    Quiet[{{x -> 0, y -> 0}}]
    ,
    {Yurie`Math`solve::InvalidSolutionChoice}
    ,
    TestID->"[19] OperatorForm.nb"
]

VerificationTest[
    solve1[{x, y}][{x == 0, y == 0}]
    ,
    {x -> 0, y -> 0}
    ,
    TestID->"[20] OperatorForm.nb"
]

VerificationTest[
    solve[x][x^2 == 1]
    ,
    {{x -> -1}, {x -> 1}}
    ,
    TestID->"[21] OperatorForm.nb"
]

VerificationTest[
    solve[x, 1][x^2 == 1]
    ,
    {x -> -1}
    ,
    TestID->"[22] OperatorForm.nb"
]

VerificationTest[
    solve1[{y, z}][x == 0]
    ,
    Quiet[{}]
    ,
    {Yurie`Math`solve::NoSolution}
    ,
    TestID->"[23] OperatorForm.nb"
]

VerificationTest[
    m = n; 
    Table[m, {n, 2}]
    ,
    {1, 2}
    ,
    TestID->"[24] OperatorForm.nb"
]

VerificationTest[
    modularize[Table[m, {n, 2}]]
    ,
    {n, n}
    ,
    TestID->"[25] OperatorForm.nb"
]

VerificationTest[
    {block[{n = 1}][m], m, n}
    ,
    {1, n, n}
    ,
    TestID->"[26] OperatorForm.nb"
]

VerificationTest[
    {module[{m = 1}][m], m, n}
    ,
    {1, n, n}
    ,
    TestID->"[27] OperatorForm.nb"
]

VerificationTest[
    ClearAll[m]; 
    ,
    Null
    ,
    TestID->"[28] OperatorForm.nb"
]

VerificationTest[
    Normal[(Series[#1, {x, 0, 0}] & )[x]]
    ,
    x
    ,
    TestID->"[29] OperatorForm.nb"
]

VerificationTest[
    series[{x, 0, 0}][x]
    ,
    0
    ,
    TestID->"[30] OperatorForm.nb"
]

VerificationTest[
    rep[{{x -> y}}][x]
    ,
    y
    ,
    TestID->"[31] OperatorForm.nb"
]

VerificationTest[
    repcheck[{{x -> y}}][x]
    ,
    Quiet[y]
    ,
    {Yurie`Math`repcheck::SuspiciousRule}
    ,
    TestID->"[32] OperatorForm.nb"
]

VerificationTest[
    repcheck[{{x -> y}, z :> w}][x]
    ,
    Quiet[y]
    ,
    {Yurie`Math`repcheck::UncheckedRule,Yurie`Math`repcheck::SuspiciousRule}
    ,
    TestID->"[33] OperatorForm.nb"
]

VerificationTest[
    repcheck[Sin[x] -> Sin[2*n*Pi + x], SSA[isN[n]]][Sin[x]]
    ,
    Sin[2*n*Pi + x]
    ,
    TestID->"[34] OperatorForm.nb"
]

VerificationTest[
    rep[x -> 0][Sin[x]/x]
    ,
    Quiet[Indeterminate]
    ,
    {Power::infy,Infinity::indet}
    ,
    TestID->"[35] OperatorForm.nb"
]

VerificationTest[
    replimit[x -> 0, y :> z][(Sin[x]/x)*y]
    ,
    z
    ,
    TestID->"[36] OperatorForm.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] OperatorForm.nb"
]