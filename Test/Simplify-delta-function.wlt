

(* Simplify-delta-function.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Simplify-delta-function.nb"
]

VerificationTest[
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Simplify-delta-function.nb"
]

VerificationTest[
    expr = {x*DiracDelta[x], x^2*Derivative[4][DiracDelta][x], x*Derivative[m][DiracDelta][x], x^n*DiracDelta[x], x^n*Derivative[m][DiracDelta][x]}; 
    ,
    Null
    ,
    TestID->"[2] Simplify-delta-function.nb"
]

VerificationTest[
    deltaReduce[][expr]
    ,
    {0, 12*Derivative[2][DiracDelta][x], x*Derivative[m][DiracDelta][x], x^n*DiracDelta[x], x^n*Derivative[m][DiracDelta][x]}
    ,
    TestID->"[3] Simplify-delta-function.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, (-m)*x^(-1 + n)*Derivative[-1 + m][DiracDelta][x]}
    ,
    TestID->"[4] Simplify-delta-function.nb"
]

VerificationTest[
    AS[n >= 2 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, -((1 - m)*m*x^(-2 + n)*Derivative[-2 + m][DiracDelta][x])}
    ,
    TestID->"[5] Simplify-delta-function.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 1 && n <= m][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, ((-1)^n*m!*Derivative[m - n][DiracDelta][x])/(m - n)!}
    ,
    TestID->"[6] Simplify-delta-function.nb"
]

VerificationTest[
    expr = {x*DiracDelta[x, y], x^2*Derivative[4, 2][DiracDelta][x, y], x*Derivative[m, k][DiracDelta][x, y], x^n*DiracDelta[x, y], x^n*Derivative[m, k][DiracDelta][x, y]}; 
    ,
    Null
    ,
    TestID->"[7] Simplify-delta-function.nb"
]

VerificationTest[
    deltaReduce[][expr]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], x*Derivative[k, m][DiracDelta][y, x], x^n*DiracDelta[x, y], x^n*Derivative[k, m][DiracDelta][y, x]}
    ,
    TestID->"[8] Simplify-delta-function.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, (-m)*x^(-1 + n)*Derivative[k, -1 + m][DiracDelta][y, x]}
    ,
    TestID->"[9] Simplify-delta-function.nb"
]

VerificationTest[
    AS[n >= 2 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, -((1 - m)*m*x^(-2 + n)*Derivative[k, -2 + m][DiracDelta][y, x])}
    ,
    TestID->"[10] Simplify-delta-function.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 1 && n <= m][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, ((-1)^n*m!*Derivative[k, m - n][DiracDelta][y, x])/(m - n)!}
    ,
    TestID->"[11] Simplify-delta-function.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Simplify-delta-function.nb"
]