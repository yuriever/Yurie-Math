

(*Hyper.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Hyper.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Hyper.nb"
]

VerificationTest[
    hyperUnregularize[hyperRegularize[HypergeometricPFQ[{a1, b1, c1}, {d1, e1}, z]]]
    ,
    HypergeometricPFQ[{a1, b1, c1}, {d1, e1}, z]
    ,
    TestID->"2-Hyper.nb"
]

VerificationTest[
    hyperUnregularize[hyperRegularize[Hypergeometric2F1[a1, b1, c1, z]]]
    ,
    Hypergeometric2F1[a1, b1, c1, z]
    ,
    TestID->"3-Hyper.nb"
]

VerificationTest[
    hyperUnregularize[hyperRegularize[Hypergeometric1F1[a1, b1, z]]]
    ,
    Hypergeometric1F1[a1, b1, z]
    ,
    TestID->"4-Hyper.nb"
]

VerificationTest[
    hyperUnregularize[hyperRegularize[Hypergeometric0F1[a1, z]]]
    ,
    Hypergeometric0F1[a1, z]
    ,
    TestID->"5-Hyper.nb"
]

VerificationTest[
    hyperRegularize[hyperUnregularize[HypergeometricPFQRegularized[{a1, b1, c1}, {d1, e1}, z]]]
    ,
    HypergeometricPFQRegularized[{a1, b1, c1}, {d1, e1}, z]
    ,
    TestID->"6-Hyper.nb"
]

VerificationTest[
    hyperRegularize[hyperUnregularize[Hypergeometric2F1Regularized[a1, b1, c1, z]]]
    ,
    Hypergeometric2F1Regularized[a1, b1, c1, z]
    ,
    TestID->"7-Hyper.nb"
]

VerificationTest[
    hyperRegularize[hyperUnregularize[Hypergeometric1F1Regularized[a1, b1, z]]]
    ,
    Hypergeometric1F1Regularized[a1, b1, z]
    ,
    TestID->"8-Hyper.nb"
]

VerificationTest[
    hyperRegularize[hyperUnregularize[Hypergeometric0F1Regularized[a1, z]]]
    ,
    Hypergeometric0F1Regularized[a1, z]
    ,
    TestID->"9-Hyper.nb"
]

VerificationTest[
    expr = f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2; 
    ,
    Null
    ,
    TestID->"10-Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes[s | t | u, Full][expr]
    ,
    f[hyper[hyperToMellinBarnes, s][((-z1)^s*Gamma[c1]*Gamma[-s]*Gamma[a1 + s]*Gamma[b1 + s])/(Gamma[a1]*Gamma[b1]*Gamma[c1 + s])]] + hyper[hyperToMellinBarnes, t][((-z2)^t*Gamma[c2]*Gamma[-t]*Gamma[a2 + t]*Gamma[b2 + t])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + t])]*hyper[hyperToMellinBarnes, u][((-z2)^u*Gamma[c2]*Gamma[-u]*Gamma[a2 + u]*Gamma[b2 + u])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + u])]
    ,
    TestID->"11-Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes[s | t][expr]
    ,
    Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"12-Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n | m | k, Full][expr]
    ,
    f[hyper[hyperToTaylor, n][(z1^n*Gamma[c1]*Gamma[a1 + n]*Gamma[b1 + n])/(Gamma[a1]*Gamma[b1]*Gamma[1 + n]*Gamma[c1 + n])]] + hyper[hyperToTaylor, k][(z2^k*Gamma[c2]*Gamma[a2 + k]*Gamma[b2 + k])/(Gamma[a2]*Gamma[b2]*Gamma[1 + k]*Gamma[c2 + k])]*hyper[hyperToTaylor, m][(z2^m*Gamma[c2]*Gamma[a2 + m]*Gamma[b2 + m])/(Gamma[a2]*Gamma[b2]*Gamma[1 + m]*Gamma[c2 + m])]
    ,
    TestID->"13-Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n | m, Full][expr]
    ,
    Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"14-Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n, Identity][Hypergeometric2F1[a, b, c, z]]
    ,
    (z^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(Gamma[a]*Gamma[b]*Gamma[1 + n]*Gamma[c + n])
    ,
    TestID->"15-Hyper.nb"
]

VerificationTest[
    SSA[isN[n] && z > 0][gammaTakeResidue[s, n, -s, "ShowPole" -> False][hyperToMellinBarnes[s, Identity][Hypergeometric2F1[a, b, c, z]]]]
    ,
    -((z^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(n!*Gamma[a]*Gamma[b]*Gamma[c + n]))
    ,
    TestID->"16-Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes2[s, Identity][Hypergeometric2F1[a, b, c, z]]
    ,
    ((1 - z)^s*Gamma[c]*Gamma[-a - b + c - s]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[-a + c]*Gamma[-b + c])
    ,
    TestID->"17-Hyper.nb"
]

VerificationTest[
    hyperToTaylor[n | m | k | l, Full][{Hypergeometric2F1[a, b, c, x], Hypergeometric1F1[a, b, x], Hypergeometric0F1[a, x], HypergeometricPFQ[{a, b}, {c}, x]}]
    ,
    {hyper[hyperToTaylor, n][(x^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(Gamma[a]*Gamma[b]*Gamma[1 + n]*Gamma[c + n])], hyper[hyperToTaylor, m][(x^m*Gamma[b]*Gamma[a + m])/(Gamma[a]*Gamma[1 + m]*Gamma[b + m])], hyper[hyperToTaylor, k][(x^k*Gamma[a])/(Gamma[1 + k]*Gamma[a + k])], hyper[hyperToTaylor, l][(x^l*Gamma[c]*Gamma[a + l]*Gamma[b + l])/(Gamma[a]*Gamma[b]*Gamma[1 + l]*Gamma[c + l])]}
    ,
    TestID->"18-Hyper.nb"
]

VerificationTest[
    hyperToMellinBarnes[s | t | u | v, Full][{Hypergeometric2F1[a, b, c, x], Hypergeometric1F1[a, b, x], Hypergeometric0F1[a, x], HypergeometricPFQ[{a, b}, {c}, x]}]
    ,
    {hyper[hyperToMellinBarnes, s][((-x)^s*Gamma[c]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[c + s])], hyper[hyperToMellinBarnes, t][((-x)^t*Gamma[b]*Gamma[-t]*Gamma[a + t])/(Gamma[a]*Gamma[b + t])], hyper[hyperToMellinBarnes, u][((-x)^u*Gamma[a]*Gamma[-u])/Gamma[a + u]], hyper[hyperToMellinBarnes, v][((-x)^v*Gamma[c]*Gamma[-v]*Gamma[a + v]*Gamma[b + v])/(Gamma[a]*Gamma[b]*Gamma[c + v])]}
    ,
    TestID->"19-Hyper.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Hyper.nb"
]