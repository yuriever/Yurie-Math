

(* Simplify.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Simplify.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Simplify.nb"
]

VerificationTest[
    swap[a, b][{a, b, c, d}]
    ,
    {b, a, c, d}
    ,
    TestID->"[2] Simplify.nb"
]

VerificationTest[
    swap[{a, b}, {c, d}][{a, b, c, d}]
    ,
    {b, a, d, c}
    ,
    TestID->"[3] Simplify.nb"
]

VerificationTest[
    separate[EvenQ][Range[10]]
    ,
    {{2, 4, 6, 8, 10}, {1, 3, 5, 7, 9}}
    ,
    TestID->"[4] Simplify.nb"
]

VerificationTest[
    separate[FreeQ[s]][((-z)^s*Gamma[c]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[c + s])]
    ,
    {Gamma[c]/(Gamma[a]*Gamma[b]), ((-z)^s*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/Gamma[c + s]}
    ,
    TestID->"[5] Simplify.nb"
]

VerificationTest[
    expr = f[a^2 + b^2 + 2*a*b] + g[a^2 + b^2 + 2*a*b, 2]
    ,
    f[a^2 + 2*a*b + b^2] + g[a^2 + 2*a*b + b^2, 2]
    ,
    TestID->"[6] Simplify.nb"
]

VerificationTest[
    focus[f][expr]
    ,
    f[(a + b)^2] + g[a^2 + 2*a*b + b^2, 2]
    ,
    TestID->"[7] Simplify.nb"
]

VerificationTest[
    focus[f, Identity][expr]
    ,
    f[a^2 + 2*a*b + b^2] + g[a^2 + 2*a*b + b^2, 2]
    ,
    TestID->"[8] Simplify.nb"
]

VerificationTest[
    focus[f | g][expr]
    ,
    f[(a + b)^2] + g[(a + b)^2, 2]
    ,
    TestID->"[9] Simplify.nb"
]

VerificationTest[
    focus[f[a^2 + 2*a*b + b^2], h][expr]
    ,
    g[a^2 + 2*a*b + b^2, 2] + h[f[a^2 + 2*a*b + b^2]]
    ,
    TestID->"[10] Simplify.nb"
]

VerificationTest[
    expr = {Hold[1 + 1], a :> 1 + 1}
    ,
    {Hold[1 + 1], a :> 1 + 1}
    ,
    TestID->"[11] Simplify.nb"
]

VerificationTest[
    focus[Hold, Simplify][expr]
    ,
    {Hold[2], a :> 1 + 1}
    ,
    TestID->"[12] Simplify.nb"
]

VerificationTest[
    focus[RuleDelayed, Simplify][expr]
    ,
    {Hold[1 + 1], a :> 2}
    ,
    TestID->"[13] Simplify.nb"
]

VerificationTest[
    fracFocus[fracReduce[Simplify, Sqrt[1 - x]]][1 + Sqrt[1 + x]/Sqrt[1 - x]]
    ,
    1 + Sqrt[1 - x^2]/(1 - x)
    ,
    TestID->"[14] Simplify.nb"
]

VerificationTest[
    trigPhaseReduce[k][Sin[Pi*k + a]*Cos[2*Pi*k + b]]
    ,
    (-1)^k*Cos[b]*Sin[a]
    ,
    TestID->"[15] Simplify.nb"
]

VerificationTest[
    trigPhaseReduce[k][Table[Sin[Pi*k*n + a], {n, -2, 2}]]
    ,
    {Sin[a], (-1)^k*Sin[a], Sin[a], (-1)^k*Sin[a], Sin[a]}
    ,
    TestID->"[16] Simplify.nb"
]

VerificationTest[
    trigPhaseReduce[k][Table[Cos[Pi*k*n + a], {n, -2, 2}]]
    ,
    {Cos[a], (-1)^k*Cos[a], Cos[a], (-1)^k*Cos[a], Cos[a]}
    ,
    TestID->"[17] Simplify.nb"
]

VerificationTest[
    trigPhaseReduce[k][Table[Tan[Pi*k*n + a], {n, -2, 2}]]
    ,
    {Tan[a], Tan[a], Tan[a], Tan[a], Tan[a]}
    ,
    TestID->"[18] Simplify.nb"
]

VerificationTest[
    expr = rep[k -> 1][{E^(I*(a1 + a2 + a3 + k*n)*Pi), E^(I*(a1 + a2 + k*n)*Pi), E^(I*(a1 + k*n)*Pi)}]; 
    trigPhaseReduce[n][expr]
    ,
    {(-1)^n*E^(I*(a1 + a2 + a3)*Pi), (-1)^n*E^(I*(a1 + a2)*Pi), (-1)^n*E^(I*a1*Pi)}
    ,
    TestID->"[19] Simplify.nb"
]

VerificationTest[
    expr = rep[k -> 4][{E^((1/2)*I*(a1 + a2 + a3 + k*n)*Pi), E^((1/2)*I*(a1 + a2 + k*n)*Pi), E^((1/2)*I*(a1 + k*n)*Pi)}]; 
    trigPhaseReduce[n][expr]
    ,
    {E^((1/2)*I*(a1 + a2 + a3)*Pi), E^((1/2)*I*(a1 + a2)*Pi), E^((I*a1*Pi)/2)}
    ,
    TestID->"[20] Simplify.nb"
]

VerificationTest[
    list1 = {1, -1, I, -I, 1/2, -2^(-1), I/2, -I/2}; 
    (Map[phaseIgnore])[list1]
    ,
    {1, 1, 1, 1, 1/2, 1/2, 1/2, 1/2}
    ,
    TestID->"[21] Simplify.nb"
]

VerificationTest[
    list2 = Flatten[Outer[Times, list1, {c, C[1]}]]; 
    (Map[phaseIgnore])[list2]
    ,
    {c, C[1], c, C[1], c, C[1], c, C[1], c/2, C[1]/2, c/2, C[1]/2, c/2, C[1]/2, c/2, C[1]/2}
    ,
    TestID->"[22] Simplify.nb"
]

VerificationTest[
    list3 = {(-1)^a, (-I)^a, (-2^(-1))^a, (-I/2)^a, Exp[I*(Pi/2)*a]}; 
    (Map[phaseIgnore])[list3]
    ,
    {1, 1, 2^(-a), 2^(-a), 1}
    ,
    TestID->"[23] Simplify.nb"
]

VerificationTest[
    list4 = Flatten[Outer[Times, list3, {C[1]}]]; 
    (Map[phaseIgnore])[list4]
    ,
    {C[1], C[1], C[1]/2^a, C[1]/2^a, C[1]}
    ,
    TestID->"[24] Simplify.nb"
]

VerificationTest[
    phaseIgnore[I^h]
    ,
    1
    ,
    TestID->"[25] Simplify.nb"
]

VerificationTest[
    phaseIgnore[I^h*2^h]
    ,
    2^h
    ,
    TestID->"[26] Simplify.nb"
]

VerificationTest[
    phaseIgnore[I^(1 + h)*2^h]
    ,
    2^h
    ,
    TestID->"[27] Simplify.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Simplify.nb"
]