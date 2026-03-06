

(* Distribution-distReduce.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-distReduce.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-distReduce.nb"
]

VerificationTest[
    expr = {x*DiracDelta[x], x^2*Derivative[4][DiracDelta][x], x*Derivative[m][DiracDelta][x], x^n*DiracDelta[x], x^n*Derivative[m][DiracDelta][x]}; 
    ,
    Null
    ,
    TestID->"[2] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[distReduce[][expr]]
    ,
    TableForm[{0, 12*Derivative[2][DiracDelta][x], x*Derivative[m][DiracDelta][x], x^n*DiracDelta[x], x^n*Derivative[m][DiracDelta][x]}]
    ,
    TestID->"[3] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 1 && m >= 2][distReduce[][expr]]]
    ,
    TableForm[{0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, (-m)*x^(-1 + n)*Derivative[-1 + m][DiracDelta][x]}]
    ,
    TestID->"[4] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 2 && m >= 2][distReduce[][expr]]]
    ,
    TableForm[{0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, -((1 - m)*m*x^(-2 + n)*Derivative[-2 + m][DiracDelta][x])}]
    ,
    TestID->"[5] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 1 && m >= 1 && n <= m][distReduce[][expr]]]
    ,
    TableForm[{0, 12*Derivative[2][DiracDelta][x], (-m)*Derivative[-1 + m][DiracDelta][x], 0, (-1)^n*FactorialPower[m, n]*Derivative[m - n][DiracDelta][x]}]
    ,
    TestID->"[6] Distribution-distReduce.nb"
]

VerificationTest[
    expr = {x*DiracDelta[x, y], x^2*Derivative[4, 2][DiracDelta][x, y], x*Derivative[m, k][DiracDelta][x, y], x^n*DiracDelta[x, y], x^n*Derivative[m, k][DiracDelta][x, y]}; 
    ,
    Null
    ,
    TestID->"[7] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[distReduce[][expr]]
    ,
    TableForm[{0, 12*Derivative[2, 2][DiracDelta][x, y], x*Derivative[k, m][DiracDelta][y, x], x^n*DiracDelta[x, y], x^n*Derivative[k, m][DiracDelta][y, x]}]
    ,
    TestID->"[8] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 1 && m >= 2][distReduce[][expr]]]
    ,
    TableForm[{0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, (-m)*x^(-1 + n)*Derivative[k, -1 + m][DiracDelta][y, x]}]
    ,
    TestID->"[9] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 2 && m >= 2][distReduce[][expr]]]
    ,
    TableForm[{0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, -((1 - m)*m*x^(-2 + n)*Derivative[k, -2 + m][DiracDelta][y, x])}]
    ,
    TestID->"[10] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 1 && m >= 1 && n <= m][distReduce[][expr]]]
    ,
    TableForm[{0, 12*Derivative[2, 2][DiracDelta][x, y], (-m)*Derivative[k, -1 + m][DiracDelta][y, x], 0, (-1)^n*FactorialPower[m, n]*Derivative[k, m - n][DiracDelta][y, x]}]
    ,
    TestID->"[11] Distribution-distReduce.nb"
]

VerificationTest[
    res1 = distReduce[][expr]; 
    res2 = distReduce["DistTogether" -> False][expr]
    ,
    {0, 12*Derivative[2][DiracDelta][x]*Derivative[2][DiracDelta][y], x*Derivative[k][DiracDelta][y]*Derivative[m][DiracDelta][x], x^n*DiracDelta[x]*DiracDelta[y], x^n*Derivative[k][DiracDelta][y]*Derivative[m][DiracDelta][x]}
    ,
    TestID->"[12] Distribution-distReduce.nb"
]

VerificationTest[
    distApart[res1 - res2]
    ,
    {0, 0, 0, 0, 0}
    ,
    TestID->"[13] Distribution-distReduce.nb"
]

VerificationTest[
    expr = {x*deltaD[x], x^2*deltaD[x, 4], x*deltaD[x, m], x^n*deltaD[x], x^n*deltaD[x, m]}; 
    ,
    Null
    ,
    TestID->"[14] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[distReduce[][expr]]
    ,
    TableForm[{0, 12*dist[deltaD, {2}][x], x*dist[deltaD, {m}][x], x^n*dist[deltaD, {0}][x], x^n*dist[deltaD, {m}][x]}]
    ,
    TestID->"[15] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 1 && m >= 2][distReduce[][expr]]]
    ,
    TableForm[{0, 12*dist[deltaD, {2}][x], (-m)*dist[deltaD, {-1 + m}][x], 0, (-m)*x^(-1 + n)*dist[deltaD, {-1 + m}][x]}]
    ,
    TestID->"[16] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 2 && m >= 2][distReduce[][expr]]]
    ,
    TableForm[{0, 12*dist[deltaD, {2}][x], (-m)*dist[deltaD, {-1 + m}][x], 0, -((1 - m)*m*x^(-2 + n)*dist[deltaD, {-2 + m}][x])}]
    ,
    TestID->"[17] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[AS[n >= 1 && m >= 1 && n <= m][distReduce[][expr]]]
    ,
    TableForm[{0, 12*dist[deltaD, {2}][x], (-m)*dist[deltaD, {-1 + m}][x], 0, (-1)^n*FactorialPower[m, n]*dist[deltaD, {m - n}][x]}]
    ,
    TestID->"[18] Distribution-distReduce.nb"
]

VerificationTest[
    expr = {deltaD[-x], deltaD[-x, 1], deltaD[-x, n], deltaD[-2*x], deltaD[-2*x, 1], deltaD[-2*x, n]}; 
    ,
    Null
    ,
    TestID->"[19] Distribution-distReduce.nb"
]

VerificationTest[
    TableForm[distReduce[][expr]]
    ,
    TableForm[{dist[deltaD, {0}][x], -dist[deltaD, {1}][x], (-(-1)^(-1 - n))*dist[deltaD, {n}][x], (1/2)*dist[deltaD, {0}][x], (-(1/4))*dist[deltaD, {1}][x], (-(-2)^(-1 - n))*dist[deltaD, {n}][x]}]
    ,
    TestID->"[20] Distribution-distReduce.nb"
]

VerificationTest[
    expr = (x + z)^2*deltaD[x] + (x + z)^2*deltaD[z] + (y + 1)^2*deltaD[x, z]; 
    ,
    Null
    ,
    TestID->"[21] Distribution-distReduce.nb"
]

VerificationTest[
    res1 = distReduce[][Expand[expr]]
    ,
    z^2*dist[deltaD, {0}][x] + x^2*dist[deltaD, {0}][z] + dist[deltaD, {z}][x] + 2*y*dist[deltaD, {z}][x] + y^2*dist[deltaD, {z}][x]
    ,
    TestID->"[22] Distribution-distReduce.nb"
]

VerificationTest[
    res2 = distReduce[][expr]
    ,
    z^2*dist[deltaD, {0}][x] + x^2*dist[deltaD, {0}][z] + (1 + y)^2*dist[deltaD, {z}][x]
    ,
    TestID->"[23] Distribution-distReduce.nb"
]

VerificationTest[
    SS[res1 - res2]
    ,
    0
    ,
    TestID->"[24] Distribution-distReduce.nb"
]

VerificationTest[
    res = distReduce[x][expr]
    ,
    z^2*dist[deltaD, {0}][x] + x^2*dist[deltaD, {0}][z] + 2*x*z*dist[deltaD, {0}][z] + z^2*dist[deltaD, {0}][z] + (1 + y)^2*dist[deltaD, {z}][x]
    ,
    TestID->"[25] Distribution-distReduce.nb"
]

VerificationTest[
    collect[deltaD[_], SS][res]
    ,
    z^2*dist[deltaD, {0}][x] + (x + z)^2*dist[deltaD, {0}][z] + (1 + y)^2*dist[deltaD, {z}][x]
    ,
    TestID->"[26] Distribution-distReduce.nb"
]

VerificationTest[
    expr = Table[f[x]*x^n*deltaD[x, n + 1], {n, 0, 2}]
    ,
    {f[x]*dist[deltaD, {1}][x], x*f[x]*dist[deltaD, {2}][x], x^2*f[x]*dist[deltaD, {3}][x]}
    ,
    TestID->"[27] Distribution-distReduce.nb"
]

VerificationTest[
    distReduce[][expr]
    ,
    {f[x]*dist[deltaD, {1}][x], x*f[x]*dist[deltaD, {2}][x], x^2*f[x]*dist[deltaD, {3}][x]}
    ,
    TestID->"[28] Distribution-distReduce.nb"
]

VerificationTest[
    res1 = freeze[_f, distReduce[]][expr]
    ,
    {f[x]*dist[deltaD, {1}][x], -2*f[x]*dist[deltaD, {1}][x], 6*f[x]*dist[deltaD, {1}][x]}
    ,
    TestID->"[29] Distribution-distReduce.nb"
]

VerificationTest[
    res2 = distReduce["RegularTestFunction" -> True][expr]
    ,
    {f[x]*dist[deltaD, {1}][x], -2*f[x]*dist[deltaD, {1}][x], 6*f[x]*dist[deltaD, {1}][x]}
    ,
    TestID->"[30] Distribution-distReduce.nb"
]

VerificationTest[
    res1 - res2
    ,
    {0, 0, 0}
    ,
    TestID->"[31] Distribution-distReduce.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-distReduce.nb"
]