

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
    (spower[#1][z, 0] & ) /@ {"+", "-", I, -I, 0, 1}
    ,
    {1, 1, 1, 1, 1, 1}
    ,
    TestID->"[2] Distribution-spower.nb"
]

VerificationTest[
    TableForm[Table[spowerlog[s][z, n, 0], {s, {"+", "-", 0, 1}}, {n, -4, -1}]]
    ,
    TableForm[{{dist["PowerLogS", "+"][z, -4, 0], dist["PowerLogS", "+"][z, -3, 0], dist["PowerLogS", "+"][z, -2, 0], dist["PowerLogS", "+"][z, -1, 0]}, {dist["PowerLogS", "-"][z, -4, 0], dist["PowerLogS", "-"][z, -3, 0], dist["PowerLogS", "-"][z, -2, 0], dist["PowerLogS", "-"][z, -1, 0]}, {dist["PowerS", 0][z, -4], dist["PowerLogS", 0][z, -3, 0], dist["PowerS", 0][z, -2], dist["PowerLogS", 0][z, -1, 0]}, {dist["PowerLogS", 1][z, -4, 0], dist["PowerS", 1][z, -3], dist["PowerLogS", 1][z, -2, 0], dist["PowerS", 1][z, -1]}}]
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
    TableForm[{λ*dist["PowerS", "+"][z, -1 + λ], (-λ)*dist["PowerS", "-"][z, -1 + λ], λ*dist["PowerS", I][z, -1 + λ], λ*dist["PowerS", -I][z, -1 + λ], λ*dist["PowerS", 1][z, -1 + λ], λ*dist["PowerS", 0][z, -1 + λ]}]
    ,
    TestID->"[5] Distribution-spower.nb"
]

VerificationTest[
    TableForm[D[expr, {z, n}]]
    ,
    TableForm[{FactorialPower[λ, n]*dist["PowerS", "+"][z, -n + λ], (-1)^n*FactorialPower[λ, n]*dist["PowerS", "-"][z, -n + λ], FactorialPower[λ, n]*dist["PowerS", I][z, -n + λ], FactorialPower[λ, n]*dist["PowerS", -I][z, -n + λ], FactorialPower[λ, n]*dist["PowerS", Mod[n, 2]][z, -n + λ], FactorialPower[λ, n]*dist["PowerS", Mod[1 + n, 2]][z, -n + λ]}]
    ,
    TestID->"[6] Distribution-spower.nb"
]

VerificationTest[
    expr = (spowerlog[#1][z, n, 0] & ) /@ {"+", "-", 0, 1}; 
    ,
    Null
    ,
    TestID->"[7] Distribution-spower.nb"
]

VerificationTest[
    res1 = D[expr, z]; 
    TableForm[res1]
    ,
    TableForm[{((-1)^n*dist["DeltaD", {-n}][z])/(-n)! + n*dist["PowerLogS", "+"][z, -1 + n, 0], -(dist["DeltaD", {-n}][z]/(-n)!) - n*dist["PowerLogS", "-"][z, -1 + n, 0], ((-1 + (-1)^n)*dist["DeltaD", {-n}][z])/(-n)! + n*dist["PowerLogS", 1][z, -1 + n, 0], ((1 + (-1)^n)*dist["DeltaD", {-n}][z])/(-n)! + n*dist["PowerLogS", 0][z, -1 + n, 0]}]
    ,
    TestID->"[8] Distribution-spower.nb"
]

VerificationTest[
    res2 = Table[D[spowerlog[s][z, n, 0], z], {s, {"+", "-", 0, 1}}, {n, -4, -1}]; 
    TableForm[res2]
    ,
    TableForm[{{(1/24)*dist["DeltaD", {4}][z] - 4*dist["PowerLogS", "+"][z, -5, 0], (-(1/6))*dist["DeltaD", {3}][z] - 3*dist["PowerLogS", "+"][z, -4, 0], (1/2)*dist["DeltaD", {2}][z] - 2*dist["PowerLogS", "+"][z, -3, 0], -dist["DeltaD", {1}][z] - dist["PowerLogS", "+"][z, -2, 0]}, {(-(1/24))*dist["DeltaD", {4}][z] + 4*dist["PowerLogS", "-"][z, -5, 0], (-(1/6))*dist["DeltaD", {3}][z] + 3*dist["PowerLogS", "-"][z, -4, 0], (-(1/2))*dist["DeltaD", {2}][z] + 2*dist["PowerLogS", "-"][z, -3, 0], -dist["DeltaD", {1}][z] + dist["PowerLogS", "-"][z, -2, 0]}, {-4*dist["PowerS", 1][z, -5], (-(1/3))*dist["DeltaD", {3}][z] - 3*dist["PowerLogS", 1][z, -4, 0], -2*dist["PowerS", 1][z, -3], -2*dist["DeltaD", {1}][z] - dist["PowerLogS", 1][z, -2, 0]}, {(1/12)*dist["DeltaD", {4}][z] - 4*dist["PowerLogS", 0][z, -5, 0], -3*dist["PowerS", 0][z, -4], dist["DeltaD", {2}][z] - 2*dist["PowerLogS", 0][z, -3, 0], -dist["PowerS", 0][z, -2]}}]
    ,
    TestID->"[9] Distribution-spower.nb"
]

VerificationTest[
    Table[res1, {n, -4, -1}] - Transpose[res2]
    ,
    {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}
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