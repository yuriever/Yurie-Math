

(* Distribution-spower.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-spower.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-spower.nb"
]

VerificationTest[
    TableForm[(spower[#1][z, 0] & ) /@ {"+", "-", I, -I, 0, 1}]
    ,
    TableForm[{dist[step][z], 1 - dist[step][z], 1, 1, 1, -1 + 2*dist[step][z]}]
    ,
    TestID->"[2] Distribution-spower.nb"
]

VerificationTest[
    TableForm[Table[spowerlog[s][z, n, 0], {s, {"+", "-", 0, 1}}, {n, -2, 0}]]
    ,
    Quiet[TableForm[{{dist[spowerlog, "+"][z, -2, 0], dist[spowerlog, "+"][z, -1, 0], HoldComplete[dist[spowerlog, "+"][z, 0, 0]]}, {dist[spowerlog, "-"][z, -2, 0], dist[spowerlog, "-"][z, -1, 0], HoldComplete[dist[spowerlog, "-"][z, 0, 0]]}, {HoldComplete[dist[spowerlog, 0][z, -2, 0]], dist[spowerlog, 0][z, -1, 0], HoldComplete[dist[spowerlog, 0][z, 0, 0]]}, {dist[spowerlog, 1][z, -2, 0], HoldComplete[dist[spowerlog, 1][z, -1, 0]], HoldComplete[dist[spowerlog, 1][z, 0, 0]]}}]]
    ,
    {Yurie`Math`spowerlog::InvalidExponent,Yurie`Math`spowerlog::InvalidExponent,Yurie`Math`spowerlog::InvalidExponent,General::stop}
    ,
    TestID->"[3] Distribution-spower.nb"
]

VerificationTest[
    expr = (spower[#1][z, λ] & ) /@ {"+", "-", I, -I, 0, 1}; 
    ,
    Null
    ,
    TestID->"[4] Distribution-spower.nb"
]

VerificationTest[
    TableForm[D[expr, {z, 1}]]
    ,
    TableForm[{λ*dist[spower, "+"][z, -1 + λ], (-λ)*dist[spower, "-"][z, -1 + λ], λ*dist[spower, I][z, -1 + λ], λ*dist[spower, -I][z, -1 + λ], λ*dist[spower, 1][z, -1 + λ], λ*dist[spower, 0][z, -1 + λ]}]
    ,
    TestID->"[5] Distribution-spower.nb"
]

VerificationTest[
    TableForm[D[expr, {z, n}]]
    ,
    TableForm[{FactorialPower[λ, n]*dist[spower, "+"][z, -n + λ], (-1)^n*FactorialPower[λ, n]*dist[spower, "-"][z, -n + λ], FactorialPower[λ, n]*dist[spower, I][z, -n + λ], FactorialPower[λ, n]*dist[spower, -I][z, -n + λ], FactorialPower[λ, n]*dist[spower, Mod[n, 2]][z, -n + λ], FactorialPower[λ, n]*dist[spower, Mod[1 + n, 2]][z, -n + λ]}]
    ,
    TestID->"[6] Distribution-spower.nb"
]

VerificationTest[
    expr = (spowerlog[#1][z, n, 0] & ) /@ {"+", "-"}; 
    ,
    Null
    ,
    TestID->"[7] Distribution-spower.nb"
]

VerificationTest[
    res1 = D[expr, z]; 
    TableForm[res1]
    ,
    TableForm[{((-1)^n*dist[deltaD, {-n}][z])/(-n)! + n*dist[spowerlog, "+"][z, -1 + n, 0], -(dist[deltaD, {-n}][z]/(-n)!) - n*dist[spowerlog, "-"][z, -1 + n, 0]}]
    ,
    TestID->"[8] Distribution-spower.nb"
]

VerificationTest[
    res2 = Table[D[spowerlog[s][z, n, 0], z], {s, {"+", "-"}}, {n, -4, -1}]; 
    TableForm[res2]
    ,
    TableForm[{{(1/24)*dist[deltaD, {4}][z] - 4*dist[spowerlog, "+"][z, -5, 0], (-(1/6))*dist[deltaD, {3}][z] - 3*dist[spowerlog, "+"][z, -4, 0], (1/2)*dist[deltaD, {2}][z] - 2*dist[spowerlog, "+"][z, -3, 0], -dist[deltaD, {1}][z] - dist[spowerlog, "+"][z, -2, 0]}, {(-(1/24))*dist[deltaD, {4}][z] + 4*dist[spowerlog, "-"][z, -5, 0], (-(1/6))*dist[deltaD, {3}][z] + 3*dist[spowerlog, "-"][z, -4, 0], (-(1/2))*dist[deltaD, {2}][z] + 2*dist[spowerlog, "-"][z, -3, 0], -dist[deltaD, {1}][z] + dist[spowerlog, "-"][z, -2, 0]}}]
    ,
    TestID->"[9] Distribution-spower.nb"
]

VerificationTest[
    Table[res1, {n, -4, -1}] - Transpose[res2]
    ,
    {{0, 0}, {0, 0}, {0, 0}, {0, 0}}
    ,
    TestID->"[10] Distribution-spower.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-spower.nb"
]