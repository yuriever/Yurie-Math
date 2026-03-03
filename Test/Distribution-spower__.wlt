

(* Distribution-spower__.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-spower__.nb"
]

VerificationTest[
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-spower__.nb"
]

VerificationTest[
    (spower[#1][z, 0] & ) /@ {"+", "-", I, -I, 0, 1}
    ,
    {1, 1, 1, 1, 1, 1}
    ,
    TestID->"[2] Distribution-spower__.nb"
]

VerificationTest[
    TableForm[Table[spowerlog[s][z, n, 0], {s, {"+", "-", 0, 1}}, {n, -4, 2}]]
    ,
    TableForm[{{spowerlog["+"][z, -4, 0], spowerlog["+"][z, -3, 0], spowerlog["+"][z, -2, 0], spowerlog["+"][z, -1, 0], 1, spower["+"][z, 1], spower["+"][z, 2]}, {spowerlog["-"][z, -4, 0], spowerlog["-"][z, -3, 0], spowerlog["-"][z, -2, 0], spowerlog["-"][z, -1, 0], 1, spower["-"][z, 1], spower["-"][z, 2]}, {spower[0][z, -4], spowerlog[0][z, -3, 0], spower[0][z, -2], spowerlog[0][z, -1, 0], 1, spower[0][z, 1], spower[0][z, 2]}, {spowerlog[1][z, -4, 0], spower[1][z, -3], spowerlog[1][z, -2, 0], spower[1][z, -1], 1, spower[1][z, 1], spower[1][z, 2]}}]
    ,
    TestID->"[3] Distribution-spower__.nb"
]

VerificationTest[
    expr = (spower[#1][z, λ] & ) /@ {"+", "-", I, -I, 0, 1}
    ,
    {spower["+"][z, λ], spower["-"][z, λ], spower[I][z, λ], spower[-I][z, λ], spower[0][z, λ], spower[1][z, λ]}
    ,
    TestID->"[4] Distribution-spower__.nb"
]

VerificationTest[
    D[expr, {z, 1}]
    ,
    {λ*spower["+"][z, -1 + λ], (-λ)*spower["-"][z, -1 + λ], λ*spower[I][z, -1 + λ], λ*spower[-I][z, -1 + λ], λ*spower[1][z, -1 + λ], λ*spower[0][z, -1 + λ]}
    ,
    TestID->"[5] Distribution-spower__.nb"
]

VerificationTest[
    D[expr, {z, n}]
    ,
    {FactorialPower[λ, n]*spower["+"][z, -n + λ], (-1)^n*FactorialPower[λ, n]*spower["-"][z, -n + λ], FactorialPower[λ, n]*spower[I][z, -n + λ], FactorialPower[λ, n]*spower[-I][z, -n + λ], FactorialPower[λ, n]*spower[Mod[n, 2]][z, -n + λ], FactorialPower[λ, n]*spower[Mod[1 + n, 2]][z, -n + λ]}
    ,
    TestID->"[6] Distribution-spower__.nb"
]

VerificationTest[
    expr = (spowerlog[#1][z, n, 0] & ) /@ {"+", "-", 0, 1}
    ,
    {spowerlog["+"][z, n, 0], spowerlog["-"][z, n, 0], spowerlog[0][z, n, 0], spowerlog[1][z, n, 0]}
    ,
    TestID->"[7] Distribution-spower__.nb"
]

VerificationTest[
    TableForm[D[expr, {z, 1}]]
    ,
    TableForm[{((-1)^n*deltaFun["D", {-n}][z])/(-n)! + n*spowerlog["+"][z, -1 + n, 0], -(deltaFun["D", {-n}][z]/(-n)!) - n*spowerlog["-"][z, -1 + n, 0], ((-1 + (-1)^n)*deltaFun["D", {-n}][z])/(-n)! + n*spowerlog[1][z, -1 + n, 0], ((1 + (-1)^n)*deltaFun["D", {-n}][z])/(-n)! + n*spowerlog[0][z, -1 + n, 0]}]
    ,
    TestID->"[8] Distribution-spower__.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-spower__.nb"
]