

(* Gamma.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Gamma.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Gamma.nb"
]

VerificationTest[
    expr = Flatten[{Table[Gamma[Δ + n], {n, -1, 2}], Gamma[-Δ]}]
    ,
    {Gamma[-1 + Δ], Gamma[Δ], Gamma[1 + Δ], Gamma[2 + Δ], Gamma[-Δ]}
    ,
    TestID->"[2] Gamma.nb"
]

VerificationTest[
    gammaShift[-Δ -> n][expr]
    ,
    {Gamma[-1 + Δ], Gamma[Δ], Gamma[1 + Δ], Gamma[2 + Δ], Gamma[n - Δ]/Pochhammer[-Δ, n]}
    ,
    TestID->"[3] Gamma.nb"
]

VerificationTest[
    gammaShift[Δ + 1 -> n][expr]
    ,
    {Gamma[-1 + Δ], Gamma[Δ], Gamma[1 + n + Δ]/Pochhammer[1 + Δ, n], Gamma[2 + Δ], Gamma[-Δ]}
    ,
    TestID->"[4] Gamma.nb"
]

VerificationTest[
    gammaShift[Δ + (_.) -> n][expr]
    ,
    {Gamma[-1 + n + Δ]/Pochhammer[-1 + Δ, n], Gamma[n + Δ]/Pochhammer[Δ, n], Gamma[1 + n + Δ]/Pochhammer[1 + Δ, n], Gamma[2 + n + Δ]/Pochhammer[2 + Δ, n], Gamma[-Δ]}
    ,
    TestID->"[5] Gamma.nb"
]

VerificationTest[
    gammaFrom[n!]
    ,
    Gamma[1 + n]
    ,
    TestID->"[6] Gamma.nb"
]

VerificationTest[
    gammaFrom[Sin[x]*Beta[a, b], "Transformation" -> {"Beta"}]
    ,
    (Gamma[a]*Gamma[b]*Sin[x])/Gamma[a + b]
    ,
    TestID->"[7] Gamma.nb"
]

VerificationTest[
    gammaFrom[Sin[x], "ActivateGamma" -> False]
    ,
    Pi/(Inactive[Gamma][x/Pi]*Inactive[Gamma][1 - x/Pi])
    ,
    TestID->"[8] Gamma.nb"
]

VerificationTest[
    gammaFrom[multiGamma[{a}, {b}]]
    ,
    Gamma[a]/Gamma[b]
    ,
    TestID->"[9] Gamma.nb"
]

VerificationTest[
    gammaFrom[multiGamma[{a}, {}]]
    ,
    Gamma[a]
    ,
    TestID->"[10] Gamma.nb"
]

VerificationTest[
    gammaFrom[multiGamma[{}, {b}]]
    ,
    1/Gamma[b]
    ,
    TestID->"[11] Gamma.nb"
]

VerificationTest[
    gammaFrom[multiGamma[{multiGamma[{x}, {}]}, {}]]
    ,
    Gamma[multiGamma[{x}, {}]]
    ,
    TestID->"[12] Gamma.nb"
]

VerificationTest[
    gammaFrom[FactorialPower[x, n]]
    ,
    Gamma[1 + x]/Gamma[1 - n + x]
    ,
    TestID->"[13] Gamma.nb"
]

VerificationTest[
    gammaSeparate[F[a]*Gamma[x]*Gamma[y]]
    ,
    {Gamma[x]*Gamma[y], F[a]}
    ,
    TestID->"[14] Gamma.nb"
]

VerificationTest[
    gammaSeparate[Gamma[x]*Gamma[y]*H[a]]
    ,
    {Gamma[x]*Gamma[y], H[a]}
    ,
    TestID->"[15] Gamma.nb"
]

VerificationTest[
    gammaSeparate[Gamma[x]]
    ,
    {Gamma[x], 1}
    ,
    TestID->"[16] Gamma.nb"
]

VerificationTest[
    gammaSeparate[Gamma[x] + Gamma[y]]
    ,
    {1, Gamma[x] + Gamma[y]}
    ,
    TestID->"[17] Gamma.nb"
]

VerificationTest[
    gammaSeparate[Gamma[x]^2]
    ,
    {Gamma[x]^2, 1}
    ,
    TestID->"[18] Gamma.nb"
]

VerificationTest[
    gammaSeparate[multiGamma[{x, y}, {}]]
    ,
    {Gamma[x]*Gamma[y], 1}
    ,
    TestID->"[19] Gamma.nb"
]

VerificationTest[
    gammaSeparate[x]
    ,
    {1, x}
    ,
    TestID->"[20] Gamma.nb"
]

VerificationTest[
    SetOptions[gammaTakeResidue, "ShowPole" -> False]
    ,
    {"SimplePole" -> True, "ShowPole" -> False, "PlusListable" -> False}
    ,
    TestID->"[21] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, n, -x][Gamma[x]]
    ,
    Quiet[Gamma[x]]
    ,
    {Yurie`Math`gammaTakeResidue::GammaNotInExpr}
    ,
    TestID->"[22] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, x, -x][Gamma[x]]
    ,
    Quiet[Gamma[x]]
    ,
    {Yurie`Math`gammaTakeResidue::IndexConflict}
    ,
    TestID->"[23] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, n, f[x]][Gamma[x]]
    ,
    Quiet[Gamma[x]]
    ,
    {Yurie`Math`gammaTakeResidue::NonlinearInVar}
    ,
    TestID->"[24] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[y, n, f[x]][Gamma[x]]
    ,
    Quiet[Gamma[x]]
    ,
    {Yurie`Math`gammaTakeResidue::NonlinearInVar}
    ,
    TestID->"[25] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, n, (-a)*x][Gamma[(-a)*x]*Gamma[x]]
    ,
    -(((-1)^n*Gamma[n/a])/(a*n!))
    ,
    TestID->"[26] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, n, x, "SimplePole" -> False][Gamma[x]^2]
    ,
    (2*PolyGamma[0, 1 + n])/n!^2
    ,
    TestID->"[27] Gamma.nb"
]

VerificationTest[
    Simplify[Table[SeriesCoefficient[Gamma[x]^2, {x, -n, -1}] - (2*(-1)^(2*n)*PolyGamma[0, 1 + n])/n!^2, {n, Range[4]}]]
    ,
    {0, 0, 0, 0}
    ,
    TestID->"[28] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x[1], n[1], -x[1]][Gamma[-x]]
    ,
    Quiet[Gamma[-x]]
    ,
    {Yurie`Math`gammaTakeResidue::GammaNotInExpr}
    ,
    TestID->"[29] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x[1], n[1], -x[1]][Gamma[-x[1]]]
    ,
    -((-1)^n[1]/n[1]!)
    ,
    TestID->"[30] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x[1], n[1], x[1], "SimplePole" -> False][Gamma[x[1]]^2]
    ,
    (2*PolyGamma[0, 1 + n[1]])/n[1]!^2
    ,
    TestID->"[31] Gamma.nb"
]

VerificationTest[
    expr = Gamma[x]; 
    ,
    Null
    ,
    TestID->"[32] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, n, x][expr]
    ,
    (-1)^n/n!
    ,
    TestID->"[33] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, n -> 2, x][expr]
    ,
    1/2
    ,
    TestID->"[34] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, {n, 2}, x][expr]
    ,
    1/2
    ,
    TestID->"[35] Gamma.nb"
]

VerificationTest[
    expr = Gamma[x]*INT[x, y]; 
    ,
    Null
    ,
    TestID->"[36] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[x, n, x][expr]
    ,
    ((-1)^n*INT[y])/n!
    ,
    TestID->"[37] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[s, n, -s, Right][{Gamma[-s]*s^a, Gamma[-s]*s^b}]
    ,
    {((-1)^n*n^a)/n!, ((-1)^n*n^b)/n!}
    ,
    TestID->"[38] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[s, n, -s, Right][{{Gamma[-s]*s^a, Gamma[-s]*s^b}, {Gamma[s + a]}}]
    ,
    Quiet[{{((-1)^n*n^a)/n!, ((-1)^n*n^b)/n!}, {Gamma[a + s]}}]
    ,
    {Yurie`Math`gammaTakeResidue::GammaNotInExpr}
    ,
    TestID->"[39] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[s, n, s][Gamma[-s + 1] + Gamma[s]]
    ,
    Quiet[Gamma[1 - s] + Gamma[s]]
    ,
    {Yurie`Math`gammaTakeResidue::PlusListable}
    ,
    TestID->"[40] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[s, n, s, "PlusListable" -> True][Gamma[-s + 1] + Gamma[s]]
    ,
    Quiet[(-1)^n/n! + Gamma[1 - s]]
    ,
    {Yurie`Math`gammaTakeResidue::GammaNotInExpr}
    ,
    TestID->"[41] Gamma.nb"
]

VerificationTest[
    gammaTakeResidue[s, n, s, "PlusListable" -> True][{Gamma[s], Gamma[-s + 1] + Gamma[s]}]
    ,
    Quiet[{(-1)^n/n!, (-1)^n/n! + Gamma[1 - s]}]
    ,
    {Yurie`Math`gammaTakeResidue::GammaNotInExpr}
    ,
    TestID->"[42] Gamma.nb"
]

VerificationTest[
    SetOptions[gammaTakeResidue, {"SimplePole" -> True, "ShowPole" -> True, "PlusListable" -> False}]
    ,
    {"SimplePole" -> True, "ShowPole" -> True, "PlusListable" -> False}
    ,
    TestID->"[43] Gamma.nb"
]

VerificationTest[
    multiGamma[{}, {}]
    ,
    1
    ,
    TestID->"[44] Gamma.nb"
]

VerificationTest[
    multiGamma[{x}, {x}]
    ,
    1
    ,
    TestID->"[45] Gamma.nb"
]

VerificationTest[
    multiGamma[{y, x}, {}]
    ,
    multiGamma[{x, y}, {}]
    ,
    TestID->"[46] Gamma.nb"
]

VerificationTest[
    multiGamma[{x, y}, {x}]
    ,
    multiGamma[{y}, {}]
    ,
    TestID->"[47] Gamma.nb"
]

VerificationTest[
    multiGammaFrom[Gamma[a] + Gamma[b]*Gamma[c] + f[Gamma[b]/Gamma[d]]]
    ,
    f[multiGamma[{b}, {d}]] + multiGamma[{a}, {}] + multiGamma[{b, c}, {}]
    ,
    TestID->"[48] Gamma.nb"
]

VerificationTest[
    multiGammaFrom[(1 + Gamma[a])/x]
    ,
    (1 + multiGamma[{a}, {}])/x
    ,
    TestID->"[49] Gamma.nb"
]

VerificationTest[
    multiGammaFrom[(1 + Gamma[a])/Gamma[b]^2]
    ,
    multiGamma[{}, {b, b}]*(1 + multiGamma[{a}, {}])
    ,
    TestID->"[50] Gamma.nb"
]

VerificationTest[
    multiGammaFrom[(1 + Gamma[a]^2)/Gamma[b]]
    ,
    multiGamma[{}, {b}]*(1 + multiGamma[{a, a}, {}])
    ,
    TestID->"[51] Gamma.nb"
]

VerificationTest[
    multiGammaFrom[Gamma[Gamma[x]]]
    ,
    multiGamma[{Gamma[x]}, {}]
    ,
    TestID->"[52] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c}, {d}]]
    ,
    multiGamma[{a1 + b1, a2 + b1, a1 + b2, a2 + b2, c}, {a1 + a2 + b1 + b2, d}]
    ,
    TestID->"[53] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c, s}, {d}]]
    ,
    Quiet[multiGamma[{c, a1 - s, a2 - s, s, b1 + s, b2 + s}, {d}]]
    ,
    {Yurie`Math`multiGammaReduceByBarnesLemma::NotMatch}
    ,
    TestID->"[54] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c}, {d, s}]]
    ,
    Quiet[multiGamma[{c, a1 - s, a2 - s, b1 + s, b2 + s}, {d, s}]]
    ,
    {Yurie`Math`multiGammaReduceByBarnesLemma::NotMatch}
    ,
    TestID->"[55] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, b3 + s, c}, {a1 + a2 + b1 + b2 + b3 + s, d}]]
    ,
    multiGamma[{a1 + b1, a2 + b1, a1 + b2, a2 + b2, a1 + b3, a2 + b3, c}, {a1 + a2 + b1 + b2, a1 + a2 + b1 + b3, a1 + a2 + b2 + b3, d}]
    ,
    TestID->"[56] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, b3 + s, c, s}, {a1 + a2 + b1 + b2 + b3 + s, d}]]
    ,
    Quiet[multiGamma[{c, a1 - s, a2 - s, s, b1 + s, b2 + s, b3 + s}, {d, a1 + a2 + b1 + b2 + b3 + s}]]
    ,
    {Yurie`Math`multiGammaReduceByBarnesLemma::NotMatch}
    ,
    TestID->"[57] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, b3 + s, c}, {a1 + a2 + b1 + b2 + b3 + s, d, s}]]
    ,
    Quiet[multiGamma[{c, a1 - s, a2 - s, b1 + s, b2 + s, b3 + s}, {d, s, a1 + a2 + b1 + b2 + b3 + s}]]
    ,
    {Yurie`Math`multiGammaReduceByBarnesLemma::NotMatch}
    ,
    TestID->"[58] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][t^a*multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c}, {d}]]
    ,
    t^a*multiGamma[{a1 + b1, a2 + b1, a1 + b2, a2 + b2, c}, {a1 + a2 + b1 + b2, d}]
    ,
    TestID->"[59] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][t^a*multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c, s}, {d}]]
    ,
    Quiet[t^a*multiGamma[{c, a1 - s, a2 - s, s, b1 + s, b2 + s}, {d}]]
    ,
    {Yurie`Math`multiGammaReduceByBarnesLemma::NotMatch}
    ,
    TestID->"[60] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][s^a*multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c}, {d}]]
    ,
    Quiet[s^a*multiGamma[{c, a1 - s, a2 - s, b1 + s, b2 + s}, {d}]]
    ,
    {Yurie`Math`multiGammaReduceByBarnesLemma::NotMatch}
    ,
    TestID->"[61] Gamma.nb"
]

VerificationTest[
    multiGammaReduceByBarnesLemma[s][1 + multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c}, {d}]]
    ,
    Quiet[1 + multiGamma[{c, a1 - s, a2 - s, b1 + s, b2 + s}, {d}]]
    ,
    {Yurie`Math`multiGammaReduceByBarnesLemma::NotProduct}
    ,
    TestID->"[62] Gamma.nb"
]

VerificationTest[
    (multiGammaSimplify[#1, Element[x, Integers]] & )[multiGamma[{}, {x, 1 - x}]]
    ,
    0
    ,
    TestID->"[63] Gamma.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Gamma.nb"
]