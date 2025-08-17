

(* Hyper-conformal-integral.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Hyper-conformal-integral.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Hyper-conformal-integral.nb"
]

VerificationTest[
    res1 = conformalIntegral2[{z[1], zb[1], z[2], zb[2]}, All][1/((z[0] - z[1])^h[1]*(z[0] - z[2])^h[2]*(zb[0] - zb[1])^hb[1]*(zb[0] - zb[2])^hb[2])]
    ,
    ConditionalExpression[((-1)^(-h[1] + hb[1])*Pi^2*DiracDelta[z[1] - z[2], zb[1] - zb[2]]*Gamma[1 - h[1]]*Gamma[1 - h[2]])/(Gamma[hb[1]]*Gamma[hb[2]]), Element[-Global`h[1] + Global`hb[1] | -Global`h[2] + Global`hb[2], Integers] && Global`h[1] + Global`h[2] == 2 && Global`hb[1] + Global`hb[2] == 2]
    ,
    TestID->"[2] Hyper-conformal-integral.nb"
]

VerificationTest[
    res2 = conformalIntegral3[{z[1], zb[1], z[2], zb[2], z[3], zb[3]}, All][1/((z[0] - z[1])^h[1]*(z[0] - z[2])^h[2]*(z[0] - z[3])^h[3]*(zb[0] - zb[1])^hb[1]*(zb[0] - zb[2])^hb[2]*(zb[0] - zb[3])^hb[3])]
    ,
    ConditionalExpression[(Pi*Gamma[1 - h[1]]*Gamma[1 - h[2]]*Gamma[1 - h[3]]*(z[1] - z[2])^(-1 + h[3])*(z[2] - z[3])^(-1 + h[1])*(-z[1] + z[3])^(-1 + h[2])*(zb[1] - zb[2])^(-1 + hb[3])*(zb[2] - zb[3])^(-1 + hb[1])*(-zb[1] + zb[3])^(-1 + hb[2]))/(Gamma[hb[1]]*Gamma[hb[2]]*Gamma[hb[3]]), Element[-Global`h[1] + Global`hb[1] | -Global`h[2] + Global`hb[2] | -Global`h[3] + Global`hb[3], Integers] && Global`h[1] + Global`h[2] + Global`h[3] == 2 && Global`hb[1] + Global`hb[2] + Global`hb[3] == 2]
    ,
    TestID->"[3] Hyper-conformal-integral.nb"
]

VerificationTest[
    res3 = conformalIntegralKLT[{z[1], zb[1], z[2], zb[2]}, All][1/((z[0] - z[1])^h[1]*(z[0] - z[2])^h[2]*(zb[0] - zb[1])^hb[1]*(zb[0] - zb[2])^hb[2])]
    ,
    ConditionalExpression[((-1)^(-h[1] + hb[1])*Pi*Gamma[-1 + h[1] + h[2]]*Gamma[1 - hb[1]]*Gamma[1 - hb[2]]*(z[1] - z[2])^(1 - h[1] - h[2])*(zb[1] - zb[2])^(1 - hb[1] - hb[2]))/(Gamma[h[1]]*Gamma[h[2]]*Gamma[2 - hb[1] - hb[2]]), Element[-h[1] + hb[1] | -h[2] + hb[2], Integers]]
    ,
    TestID->"[4] Hyper-conformal-integral.nb"
]

VerificationTest[
    res4 = (ReplaceAll[1/((z[1] - z[2])*(zb[1] - zb[2])) -> Pi*DiracDelta[z[1] - z[2], zb[1] - zb[2]]])[(ReplaceAll[{h[2] -> 2 - h[1], hb[2] -> 2 - hb[1]}])[times[Gamma[2 - hb[1] - hb[2]]][res3]]]
    ,
    ConditionalExpression[((-1)^(-h[1] + hb[1])*Pi^2*DiracDelta[z[1] - z[2], zb[1] - zb[2]]*Gamma[1 - hb[1]]*Gamma[-1 + hb[1]])/(Gamma[2 - h[1]]*Gamma[h[1]]), Element[-h[1] + hb[1] | h[1] - hb[1], Integers]]
    ,
    TestID->"[5] Hyper-conformal-integral.nb"
]

VerificationTest[
    trigPhaseReduce[J][(ReplaceAll[hb[1] -> h[1] - J])[FES[(ReplaceAll[{h[2] -> 2 - h[1], hb[2] -> 2 - hb[1]}])[Normal[res1/res4]]]]]
    ,
    1
    ,
    TestID->"[6] Hyper-conformal-integral.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Hyper-conformal-integral.nb"
]