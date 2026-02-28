

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
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-spower.nb"
]

VerificationTest[
    expr = (spower[#1][z, λ] & ) /@ {"+", "-", I, -I, 0, 1}
    ,
    {spower["+"][z, λ], spower["-"][z, λ], spower[I][z, λ], spower[-I][z, λ], spower[0][z, λ], spower[1][z, λ]}
    ,
    TestID->"[2] Distribution-spower.nb"
]

VerificationTest[
    D[expr, {z, 1}]
    ,
    {λ*spower["+"][z, -1 + λ], (-λ)*spower["-"][z, -1 + λ], λ*spower[I][z, -1 + λ], λ*spower[-I][z, -1 + λ], λ*spower[1][z, -1 + λ], λ*spower[0][z, -1 + λ]}
    ,
    TestID->"[3] Distribution-spower.nb"
]

VerificationTest[
    D[expr, {z, n}]
    ,
    {FactorialPower[λ, n]*spower["+"][z, -n + λ], (-1)^n*FactorialPower[λ, n]*spower["-"][z, -n + λ], FactorialPower[λ, n]*spower[I][z, -n + λ], FactorialPower[λ, n]*spower[-I][z, -n + λ], FactorialPower[λ, n]*spower[Mod[n, 2]][z, -n + λ], FactorialPower[λ, n]*spower[Mod[1 + n, 2]][z, -n + λ]}
    ,
    TestID->"[4] Distribution-spower.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-spower.nb"
]