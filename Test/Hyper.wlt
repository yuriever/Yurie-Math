

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
    expr === res
    ,
    True
    ,
    TestID->"[4] Hyper.nb"
]

VerificationTest[
    expr = {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z], BesselJ[a, z], BesselI[a, z]}; 
    ,
    Null
    ,
    TestID->"[5] Hyper.nb"
]

VerificationTest[
    (Map[hyperToTaylor[n, Full]])[expr]
    ,
    {hyper["Taylor", n][(z^n*Gamma[d]*Gamma[e]*Gamma[a + n]*Gamma[b + n]*Gamma[c + n])/(n!*Gamma[a]*Gamma[b]*Gamma[c]*Gamma[d + n]*Gamma[e + n])], hyper["Taylor", n][(z^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(n!*Gamma[a]*Gamma[b]*Gamma[c + n])], hyper["Taylor", n][(z^n*Gamma[b]*Gamma[a + n])/(n!*Gamma[a]*Gamma[b + n])], hyper["Taylor", n][(z^n*Gamma[a])/(n!*Gamma[a + n])], hyper["Taylor", n][((-1)^n*2^(-a - 2*n)*z^(a + 2*n))/(n!*Gamma[1 + a + n])], hyper["Taylor", n][(2^(-a - 2*n)*z^(a + 2*n))/(n!*Gamma[1 + a + n])]}
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
    {1, 1, 1, 1, 1, 1}
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
    expr = {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z], HypergeometricU[a, b, z], BesselJ[a, z], BesselI[a, z]}; 
    ,
    Null
    ,
    TestID->"[13] Hyper.nb"
]

VerificationTest[
    (Map[hyperToMellinBarnes[s, Full]])[expr]
    ,
    {hyper["MellinBarnes", s][((-z)^s*Gamma[d]*Gamma[e]*Gamma[-s]*Gamma[a + s]*Gamma[b + s]*Gamma[c + s])/(Gamma[a]*Gamma[b]*Gamma[c]*Gamma[d + s]*Gamma[e + s])], hyper["MellinBarnes", s][((-z)^s*Gamma[c]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[c + s])], hyper["MellinBarnes", s][((-z)^s*Gamma[b]*Gamma[-s]*Gamma[a + s])/(Gamma[a]*Gamma[b + s])], hyper["MellinBarnes", s][((-z)^s*Gamma[a]*Gamma[-s])/Gamma[a + s]], hyper["MellinBarnes", s][(z^(-a - s)*Gamma[-s]*Gamma[a + s]*Gamma[1 + a - b + s])/(Gamma[a]*Gamma[1 + a - b])], hyper["MellinBarnes", s][(2^(-a - 2*s)*z^(a + 2*s)*Gamma[-s])/Gamma[1 + a + s]], hyper["MellinBarnes", s][(2^(-a - 2*s)*E^(I*Pi*s)*z^(a + 2*s)*Gamma[-s])/Gamma[1 + a + s]]}
    ,
    TestID->"[14] Hyper.nb"
]

VerificationTest[
    res = summation[{n, 0, Infinity}, Regularization -> "Borel"][gammaTakeResidue[s, n, -s, -1, "ShowPole" -> False][(Map[hyperToMellinBarnes[s, Identity]])[expr]]]
    ,
    {HypergeometricPFQ[{a, b, c}, {d, e}, z], Hypergeometric2F1[a, b, c, z], Hypergeometric1F1[a, b, z], Hypergeometric0F1[a, z], HypergeometricU[a, b, z], BesselJ[a, z], BesselI[a, z]}
    ,
    TestID->"[15] Hyper.nb"
]

VerificationTest[
    expr === res
    ,
    True
    ,
    TestID->"[16] Hyper.nb"
]

VerificationTest[
    expr = AppellF1[a, b1, b2, c, x, y]; 
    ,
    Null
    ,
    TestID->"[17] Hyper.nb"
]

VerificationTest[
    hyperFromAppellF1[n, Full][expr]
    ,
    hyper["AppellF1", n][(x^n*y^n*Gamma[c]*Gamma[a + n]*Gamma[b1 + n]*Gamma[b2 + n]*Gamma[-1 + c + n]*Gamma[-a + c + n]*Hypergeometric2F1[a + n, b1 + n, c + 2*n, x]*Hypergeometric2F1[a + n, b2 + n, c + 2*n, y])/(n!*Gamma[a]*Gamma[b1]*Gamma[b2]*Gamma[-a + c]*Gamma[-1 + c + 2*n]*Gamma[c + 2*n])]
    ,
    TestID->"[18] Hyper.nb"
]

VerificationTest[
    expr = Hypergeometric2F1[a, b, c, z]
    ,
    Hypergeometric2F1[a, b, c, z]
    ,
    TestID->"[19] Hyper.nb"
]

VerificationTest[
    hyperToEuler2[t, Full][expr]
    ,
    hyper["Euler", t][(t^(-1 + b)*(1 + t)^(a - c)*Gamma[c])/((1 + t - t*z)^a*(Gamma[b]*Gamma[-b + c]))]
    ,
    TestID->"[20] Hyper.nb"
]

VerificationTest[
    res = FES[gammaFrom[DLMF["15.8.4"][integration[{t, 0, Infinity}][hyperToEuler2[t, Identity][expr]]]]]
    ,
    Hypergeometric2F1[a, b, c, z]
    ,
    TestID->"[21] Hyper.nb"
]

VerificationTest[
    expr === res
    ,
    True
    ,
    TestID->"[22] Hyper.nb"
]

VerificationTest[
    expr = Hypergeometric2F1[a, b, c, z]; 
    ,
    Null
    ,
    TestID->"[23] Hyper.nb"
]

VerificationTest[
    temp = hyperToMellinBarnes2[s, Identity][expr]
    ,
    ((1 - z)^s*Gamma[c]*Gamma[-a - b + c - s]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[-a + c]*Gamma[-b + c])
    ,
    TestID->"[24] Hyper.nb"
]

VerificationTest[
    res1 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, a + s, "ShowPole" -> False][temp]]; 
    (res2 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, b + s, "ShowPole" -> False][temp]]; )
    ,
    Null
    ,
    TestID->"[25] Hyper.nb"
]

VerificationTest[
    FS[minus[res1 + res2][DLMF["15.8.3"][expr]]]
    ,
    0
    ,
    TestID->"[26] Hyper.nb"
]

VerificationTest[
    expr = HypergeometricU[a, b, z]; 
    ,
    Null
    ,
    TestID->"[27] Hyper.nb"
]

VerificationTest[
    temp = hyperToMellinBarnes2[s, Identity][expr]
    ,
    (E^z*z^(1 - b - s)*Gamma[s]*Gamma[-1 + b + s])/Gamma[a + s]
    ,
    TestID->"[28] Hyper.nb"
]

VerificationTest[
    res1 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s, "ShowPole" -> False][temp]]; 
    (res2 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, -1 + b + s, "ShowPole" -> False][temp]]; )
    ,
    Null
    ,
    TestID->"[29] Hyper.nb"
]

VerificationTest[
    FS[minus[expr][res1 + res2]]
    ,
    0
    ,
    TestID->"[30] Hyper.nb"
]

VerificationTest[
    expr = BesselK[a, z]; 
    ,
    Null
    ,
    TestID->"[31] Hyper.nb"
]

VerificationTest[
    temp = hyperToMellinBarnes2[s, Identity][expr]
    ,
    2^(-1 + a + 2*s)*z^(-a - 2*s)*Gamma[s]*Gamma[a + s]
    ,
    TestID->"[32] Hyper.nb"
]

VerificationTest[
    res1 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s, "ShowPole" -> False][temp]]; 
    (res2 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s + a, "ShowPole" -> False][temp]]; )
    ,
    Null
    ,
    TestID->"[33] Hyper.nb"
]

VerificationTest[
    FS[minus[expr][res1 + res2]]
    ,
    0
    ,
    TestID->"[34] Hyper.nb"
]

VerificationTest[
    expr = BesselY[a, z]; 
    ,
    Null
    ,
    TestID->"[35] Hyper.nb"
]

VerificationTest[
    temp = hyperToMellinBarnes2[s, Identity][expr]
    ,
    -((2^(a + 2*s)*z^(-a - 2*s)*Gamma[s]*Gamma[a + s])/(Gamma[1/2 - s]*Gamma[1/2 + s]))
    ,
    TestID->"[36] Hyper.nb"
]

VerificationTest[
    res1 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s, "ShowPole" -> False][temp]]; 
    (res2 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s + a, "ShowPole" -> False][temp]]; )
    ,
    Null
    ,
    TestID->"[37] Hyper.nb"
]

VerificationTest[
    FS[minus[expr][res1 + res2]]
    ,
    0
    ,
    TestID->"[38] Hyper.nb"
]

VerificationTest[
    expr = HankelH1[a, z]; 
    ,
    Null
    ,
    TestID->"[39] Hyper.nb"
]

VerificationTest[
    temp = hyperToMellinBarnes2[s, Identity][expr]
    ,
    -((I*2^(a + 2*s)*E^(I*Pi*s)*z^(-a - 2*s)*Gamma[s]*Gamma[a + s])/Pi)
    ,
    TestID->"[40] Hyper.nb"
]

VerificationTest[
    res1 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s, "ShowPole" -> False][temp]]; 
    (res2 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s + a, "ShowPole" -> False][temp]]; )
    ,
    Null
    ,
    TestID->"[41] Hyper.nb"
]

VerificationTest[
    divide[HankelH1[a, z]][FS[res1 + res2]]
    ,
    1
    ,
    TestID->"[42] Hyper.nb"
]

VerificationTest[
    expr = HankelH2[a, z]; 
    ,
    Null
    ,
    TestID->"[43] Hyper.nb"
]

VerificationTest[
    temp = hyperToMellinBarnes2[s, Identity][expr]
    ,
    (I*2^(a + 2*s)*z^(-a - 2*s)*Gamma[s]*Gamma[a + s])/(E^(I*Pi*s)*Pi)
    ,
    TestID->"[44] Hyper.nb"
]

VerificationTest[
    res1 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s, "ShowPole" -> False][temp]]; 
    (res2 = summation[{n, 0, Infinity}][gammaTakeResidue[s, n, s + a, "ShowPole" -> False][temp]]; )
    ,
    Null
    ,
    TestID->"[45] Hyper.nb"
]

VerificationTest[
    FE[minus[HankelH2[a, z]][res1 + res2]]
    ,
    0
    ,
    TestID->"[46] Hyper.nb"
]

VerificationTest[
    expr = f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2; 
    ,
    Null
    ,
    TestID->"[47] Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes[s | t][expr]
    ,
    Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"[48] Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes[s | t | u, Full][expr]
    ,
    f[hyper["MellinBarnes", s][((-z1)^s*Gamma[c1]*Gamma[-s]*Gamma[a1 + s]*Gamma[b1 + s])/(Gamma[a1]*Gamma[b1]*Gamma[c1 + s])]] + hyper["MellinBarnes", t][((-z2)^t*Gamma[c2]*Gamma[-t]*Gamma[a2 + t]*Gamma[b2 + t])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + t])]*hyper["MellinBarnes", u][((-z2)^u*Gamma[c2]*Gamma[-u]*Gamma[a2 + u]*Gamma[b2 + u])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + u])]
    ,
    TestID->"[49] Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n | m, Full][expr]
    ,
    Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"[50] Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n | m | k, Full][expr]
    ,
    f[hyper["Taylor", n][(z1^n*Gamma[c1]*Gamma[a1 + n]*Gamma[b1 + n])/(n!*Gamma[a1]*Gamma[b1]*Gamma[c1 + n])]] + hyper["Taylor", k][(z2^k*Gamma[c2]*Gamma[a2 + k]*Gamma[b2 + k])/(k!*Gamma[a2]*Gamma[b2]*Gamma[c2 + k])]*hyper["Taylor", m][(z2^m*Gamma[c2]*Gamma[a2 + m]*Gamma[b2 + m])/(m!*Gamma[a2]*Gamma[b2]*Gamma[c2 + m])]
    ,
    TestID->"[51] Hyper.nb"
]

VerificationTest[
    expr = {BesselJ[a, z], BesselI[a, z], BesselK[a, z], BesselY[a, z], HankelH1[a, z], HankelH2[a, z]}
    ,
    {BesselJ[a, z], BesselI[a, z], BesselK[a, z], BesselY[a, z], HankelH1[a, z], HankelH2[a, z]}
    ,
    TestID->"[52] Hyper.nb"
]

VerificationTest[
    res = hyperFrom["Bessel"][expr]
    ,
    {(z^a*Hypergeometric0F1[1 + a, -(z^2/4)])/(2^a*Gamma[1 + a]), (z^a*Hypergeometric0F1[1 + a, z^2/4])/(2^a*Gamma[1 + a]), (2^(-1 + a)*Gamma[a]*Hypergeometric0F1[1 - a, z^2/4])/z^a + 2^(-1 - a)*z^a*Gamma[-a]*Hypergeometric0F1[1 + a, z^2/4], -((2^a*Gamma[a]*Hypergeometric0F1[1 - a, -(z^2/4)])/(z^a*Pi)) - (z^a*Cos[a*Pi]*Gamma[-a]*Hypergeometric0F1[1 + a, -(z^2/4)])/(2^a*Pi), -((I*2^a*Gamma[a]*Hypergeometric0F1[1 - a, -(z^2/4)])/(z^a*Pi)) - (I*z^a*Gamma[-a]*Hypergeometric0F1[1 + a, -(z^2/4)])/(2^a*E^(I*a*Pi)*Pi), (I*2^a*Gamma[a]*Hypergeometric0F1[1 - a, -(z^2/4)])/(z^a*Pi) + (I*E^(I*a*Pi)*z^a*Gamma[-a]*Hypergeometric0F1[1 + a, -(z^2/4)])/(2^a*Pi)}
    ,
    TestID->"[53] Hyper.nb"
]

VerificationTest[
    FS[divide[expr][res]]
    ,
    {1, 1, 1, 1, 1, 1}
    ,
    TestID->"[54] Hyper.nb"
]

VerificationTest[
    hyperFrom["InvalidType"][f[x]]
    ,
    Quiet[f[x]]
    ,
    {Yurie`Math`hyper::InvalidType}
    ,
    TestID->"[55] Hyper.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Hyper.nb"
]