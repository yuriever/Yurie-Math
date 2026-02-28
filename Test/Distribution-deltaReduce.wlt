

(* Distribution-deltaReduce.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-deltaReduce.nb"
]

VerificationTest[
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-deltaReduce.nb"
]

VerificationTest[
    expr = {x*DiracDelta[x], x^2*Derivative[4][DiracDelta][x], x*Derivative[m][DiracDelta][x], x^n*DiracDelta[x], x^n*Derivative[m][DiracDelta][x]}; 
    ,
    Null
    ,
    TestID->"[2] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][expr]
    ,
    {0, 12*Derivative[2][DiracDelta][x], x*Derivative[m][DiracDelta][x], x^n*DiracDelta[x], x^n*Derivative[m][DiracDelta][x]}
    ,
    TestID->"[3] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, (-m)*x^(-1 + n)*Derivative[-1 + m][DiracDelta][x]}
    ,
    TestID->"[4] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 2 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, -((1 - m)*m*x^(-2 + n)*Derivative[-2 + m][DiracDelta][x])}
    ,
    TestID->"[5] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 1 && n <= m][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, (-1)^n*FactorialPower[m, n]*Derivative[m - n][DiracDelta][x]}
    ,
    TestID->"[6] Distribution-deltaReduce.nb"
]

VerificationTest[
    expr = {x*DiracDelta[x, y], x^2*Derivative[4, 2][DiracDelta][x, y], x*Derivative[m, k][DiracDelta][x, y], x^n*DiracDelta[x, y], x^n*Derivative[m, k][DiracDelta][x, y]}; 
    ,
    Null
    ,
    TestID->"[7] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][expr]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], x*Derivative[k, m][DiracDelta][y, x], x^n*DiracDelta[x, y], x^n*Derivative[k, m][DiracDelta][y, x]}
    ,
    TestID->"[8] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, (-m)*x^(-1 + n)*Derivative[k, -1 + m][DiracDelta][y, x]}
    ,
    TestID->"[9] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 2 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, -((1 - m)*m*x^(-2 + n)*Derivative[k, -2 + m][DiracDelta][y, x])}
    ,
    TestID->"[10] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 1 && n <= m][deltaReduce[][expr]]
    ,
    {0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, (-1)^n*FactorialPower[m, n]*Derivative[k, m - n][DiracDelta][y, x]}
    ,
    TestID->"[11] Distribution-deltaReduce.nb"
]

VerificationTest[
    expr = {x*deltaD[x], x^2*deltaD[x, 4], x*deltaD[x, m], x^n*deltaD[x], x^n*deltaD[x, m]}; 
    ,
    Null
    ,
    TestID->"[12] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][expr]
    ,
    {0, 12*deltaD[x, 2], x*deltaD[x, m], x^n*deltaD[x], x^n*deltaD[x, m]}
    ,
    TestID->"[13] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*deltaD[x, 2], (-m)*deltaD[x, -1 + m], 0, (-m)*x^(-1 + n)*deltaD[x, -1 + m]}
    ,
    TestID->"[14] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 2 && m >= 2][deltaReduce[][expr]]
    ,
    {0, 12*deltaD[x, 2], (-m)*deltaD[x, -1 + m], 0, -((1 - m)*m*x^(-2 + n)*deltaD[x, -2 + m])}
    ,
    TestID->"[15] Distribution-deltaReduce.nb"
]

VerificationTest[
    AS[n >= 1 && m >= 1 && n <= m][deltaReduce[][expr]]
    ,
    {0, 12*deltaD[x, 2], (-m)*deltaD[x, -1 + m], 0, (-1)^n*deltaD[x, m - n]*FactorialPower[m, n]}
    ,
    TestID->"[16] Distribution-deltaReduce.nb"
]

VerificationTest[
    expr = (x + y)^2*DiracDelta[x] + (y + 1)^2*DiracDelta[x]; 
    ,
    Null
    ,
    TestID->"[17] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][Expand[expr]]
    ,
    DiracDelta[x] + 2*y*DiracDelta[x] + 2*y^2*DiracDelta[x]
    ,
    TestID->"[18] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][expr]
    ,
    y^2*DiracDelta[x] + (1 + y)^2*DiracDelta[x]
    ,
    TestID->"[19] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][deltaFromDirac[expr]]
    ,
    y^2*deltaD[x] + (1 + y)^2*deltaD[x]
    ,
    TestID->"[20] Distribution-deltaReduce.nb"
]

VerificationTest[
    expr = f[x]*x*DiracDelta[x] + f[x]*x^2*DiracDelta[x]; 
    ,
    Null
    ,
    TestID->"[21] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][expr]
    ,
    x*DiracDelta[x]*f[x] + x^2*DiracDelta[x]*f[x]
    ,
    TestID->"[22] Distribution-deltaReduce.nb"
]

VerificationTest[
    deltaReduce[][deltaFromDirac[expr]]
    ,
    x*deltaD[x]*f[x] + x^2*deltaD[x]*f[x]
    ,
    TestID->"[23] Distribution-deltaReduce.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-deltaReduce.nb"
]