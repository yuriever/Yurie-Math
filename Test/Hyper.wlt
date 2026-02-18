

(* Hyper.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Hyper.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Hyper.nb"
]

VerificationTest[
    expr = {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z]}; 
    ,
    Null
    ,
    TestID->"[2] Hyper.nb"
]

VerificationTest[
    res = hyperUnregularize[hyperRegularize[expr]]
    ,
    {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z]}
    ,
    TestID->"[3] Hyper.nb"
]

VerificationTest[
    res === expr
    ,
    True
    ,
    TestID->"[4] Hyper.nb"
]

VerificationTest[
    expr = {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z]}; 
    ,
    Null
    ,
    TestID->"[5] Hyper.nb"
]

VerificationTest[
    (Map[hyperToTaylor[n, Full]])[expr]
    ,
    {hyper["Taylor", n][(z^n*Gamma[d]*Gamma[e]*Gamma[a + n]*Gamma[b + n]*Gamma[c + n])/(n!*Gamma[a]*Gamma[b]*Gamma[c]*Gamma[d + n]*Gamma[e + n])], hyper["Taylor", n][(z^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(n!*Gamma[a]*Gamma[b]*Gamma[c + n])], hyper["Taylor", n][(z^n*Gamma[b]*Gamma[a + n])/(n!*Gamma[a]*Gamma[b + n])], hyper["Taylor", n][(z^n*Gamma[a])/(n!*Gamma[a + n])]}
    ,
    TestID->"[6] Hyper.nb"
]

VerificationTest[
    res1 = gammaTakeResidue[s, n, -s, -1, "ShowPole" -> False][(Map[hyperToMellinBarnes[s, Identity]])[expr]]; 
    (res2 = (Map[hyperToTaylor[n, Identity]])[expr]; )
    ,
    Null
    ,
    TestID->"[7] Hyper.nb"
]

VerificationTest[
    SSA[isN[n]][res1/res2]
    ,
    {1, 1, 1, 1}
    ,
    TestID->"[8] Hyper.nb"
]

VerificationTest[
    expr = {Hypergeometric2F1[a, b, c, z]*HeavisideTheta[1 - t], Hypergeometric1F1[a, b, z]*HeavisideTheta[1 - t], HypergeometricU[a, b, z]}; 
    ,
    Null
    ,
    TestID->"[9] Hyper.nb"
]

VerificationTest[
    (Map[hyperToEuler[t, Full]])[expr]
    ,
    {HeavisideTheta[1 - t]*hyper["Euler", t][((1 - t)^(-1 - b + c)*t^(-1 + b)*Gamma[c])/((1 - t*z)^a*(Gamma[b]*Gamma[-b + c]))], HeavisideTheta[1 - t]*hyper["Euler", t][(E^(t*z)*(1 - t)^(-1 - a + b)*t^(-1 + a)*Gamma[b])/(Gamma[a]*Gamma[-a + b])], hyper["Euler", t][(t^(-1 + a)*(1 + t)^(-1 - a + b))/(E^(t*z)*Gamma[a])]}
    ,
    TestID->"[10] Hyper.nb"
]

VerificationTest[
    res = integration[{t, 0, Infinity}][(Map[hyperToEuler[t, Identity]])[expr]]
    ,
    {Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], HypergeometricU[a, b, z]}
    ,
    TestID->"[11] Hyper.nb"
]

VerificationTest[
    (expr /. _HeavisideTheta -> 1) === res
    ,
    True
    ,
    TestID->"[12] Hyper.nb"
]

VerificationTest[
    expr = {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z], HypergeometricU[a, b, z]}; 
    ,
    Null
    ,
    TestID->"[13] Hyper.nb"
]

VerificationTest[
    (Map[hyperToMellinBarnes[s, Full]])[expr]
    ,
    {hyper["MellinBarnes", s][((-z)^s*Gamma[d]*Gamma[e]*Gamma[-s]*Gamma[a + s]*Gamma[b + s]*Gamma[c + s])/(Gamma[a]*Gamma[b]*Gamma[c]*Gamma[d + s]*Gamma[e + s])], hyper["MellinBarnes", s][((-z)^s*Gamma[c]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[c + s])], hyper["MellinBarnes", s][((-z)^s*Gamma[b]*Gamma[-s]*Gamma[a + s])/(Gamma[a]*Gamma[b + s])], hyper["MellinBarnes", s][((-z)^s*Gamma[a]*Gamma[-s])/Gamma[a + s]], hyper["MellinBarnes", s][(z^(-a - s)*Gamma[-s]*Gamma[a + s]*Gamma[1 + a - b + s])/(Gamma[a]*Gamma[1 + a - b])]}
    ,
    TestID->"[14] Hyper.nb"
]

VerificationTest[
    res = summation[{n, 0, Infinity}, Regularization -> "Borel"][gammaTakeResidue[s, n, -s, -1, "ShowPole" -> False][(Map[hyperToMellinBarnes[s, Identity]])[expr]]]
    ,
    {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z], HypergeometricU[a, b, z]}
    ,
    TestID->"[15] Hyper.nb"
]

VerificationTest[
    res === expr
    ,
    True
    ,
    TestID->"[16] Hyper.nb"
]

VerificationTest[
    expr = Hypergeometric2F1[a, b, c, z]
    ,
    Hypergeometric2F1[a, b, c, z]
    ,
    TestID->"[17] Hyper.nb"
]

VerificationTest[
    hyperToEuler2[t, Full][expr]
    ,
    hyper["Euler", t][(t^(-1 + b)*(1 + t)^(a - c)*Gamma[c])/((1 + t - t*z)^a*(Gamma[b]*Gamma[-b + c]))]
    ,
    TestID->"[18] Hyper.nb"
]

VerificationTest[
    res = FES[gammaFrom[DLMF["15.8.4"][integration[{t, 0, Infinity}][hyperToEuler2[t, Identity][expr]]]]]
    ,
    Hypergeometric2F1[a, b, c, z]
    ,
    TestID->"[19] Hyper.nb"
]

VerificationTest[
    Null
    ,
    Null
    ,
    TestID->"[20] Hyper.nb"
]

VerificationTest[
    expr = AppellF1[a, b1, b2, c, x, y]; 
    ,
    Null
    ,
    TestID->"[21] Hyper.nb"
]

VerificationTest[
    hyperFromAppellF1[n, Full][expr]
    ,
    hyper["AppellF1", n][(x^n*y^n*Gamma[c]*Gamma[a + n]*Gamma[b1 + n]*Gamma[b2 + n]*Gamma[-1 + c + n]*Gamma[-a + c + n]*Hypergeometric2F1[a + n, b1 + n, c + 2*n, x]*Hypergeometric2F1[a + n, b2 + n, c + 2*n, y])/(n!*Gamma[a]*Gamma[b1]*Gamma[b2]*Gamma[-a + c]*Gamma[-1 + c + 2*n]*Gamma[c + 2*n])]
    ,
    TestID->"[22] Hyper.nb"
]

VerificationTest[
    expr = f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2; 
    ,
    Null
    ,
    TestID->"[23] Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes[s | t][expr]
    ,
    Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"[24] Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes[s | t | u, Full][expr]
    ,
    f[hyper["MellinBarnes", s][((-z1)^s*Gamma[c1]*Gamma[-s]*Gamma[a1 + s]*Gamma[b1 + s])/(Gamma[a1]*Gamma[b1]*Gamma[c1 + s])]] + hyper["MellinBarnes", t][((-z2)^t*Gamma[c2]*Gamma[-t]*Gamma[a2 + t]*Gamma[b2 + t])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + t])]*hyper["MellinBarnes", u][((-z2)^u*Gamma[c2]*Gamma[-u]*Gamma[a2 + u]*Gamma[b2 + u])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + u])]
    ,
    TestID->"[25] Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n | m, Full][expr]
    ,
    Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"[26] Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n | m | k, Full][expr]
    ,
    f[hyper["Taylor", n][(z1^n*Gamma[c1]*Gamma[a1 + n]*Gamma[b1 + n])/(n!*Gamma[a1]*Gamma[b1]*Gamma[c1 + n])]] + hyper["Taylor", k][(z2^k*Gamma[c2]*Gamma[a2 + k]*Gamma[b2 + k])/(k!*Gamma[a2]*Gamma[b2]*Gamma[c2 + k])]*hyper["Taylor", m][(z2^m*Gamma[c2]*Gamma[a2 + m]*Gamma[b2 + m])/(m!*Gamma[a2]*Gamma[b2]*Gamma[c2 + m])]
    ,
    TestID->"[27] Hyper.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Hyper.nb"
]