

(*Hyper-AppellF1.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Hyper-AppellF1.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (Gamma[1 + a]*Gamma[1 + b]*Inactive[AppellF1][1 + a, -c, -d, 2 + a + b, x, y])/Gamma[2 + a + b]
    ,
    TestID->"2-Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[All, Identity][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (AppellF1[1 + a, -c, -d, 2 + a + b, x, y]*Gamma[1 + a]*Gamma[1 + b])/Gamma[2 + a + b]
    ,
    TestID->"3-Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[u][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (Gamma[1 + a]*Gamma[1 + b]*Inactive[AppellF1][1 + a, -c, -d, 2 + a + b, x, y])/Gamma[2 + a + b]
    ,
    TestID->"4-Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[v][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (1 - u)^b*u^a*(1 - u*x)^c*(1 - u*y)^d
    ,
    TestID->"5-Hyper-AppellF1.nb"
]

VerificationTest[
    hyperFromAppellF1[n, Full][AppellF1[a, b1, b2, c, x, y]]
    ,
    hyper["AppellF1", n][(x^n*y^n*Hypergeometric2F1[a + n, b1 + n, c + 2*n, x]*Hypergeometric2F1[a + n, b2 + n, c + 2*n, y]*Pochhammer[a, n]*Pochhammer[b1, n]*Pochhammer[b2, n]*Pochhammer[-a + c, n])/(n!*Pochhammer[c, 2*n]*Pochhammer[-1 + c + n, n])]
    ,
    TestID->"6-Hyper-AppellF1.nb"
]

VerificationTest[
    hyperFromAppellF1[n, Full][AppellF1[a, b1, b2, c, x, y]^2]
    ,
    Quiet[AppellF1[a, b1, b2, c, x, y]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"7-Hyper-AppellF1.nb"
]

VerificationTest[
    hyperFromAppellF1[n | m, Full][AppellF1[a, b1, b2, c, x, y]^2]
    ,
    hyper["AppellF1", m][(x^m*y^m*Hypergeometric2F1[a + m, b1 + m, c + 2*m, x]*Hypergeometric2F1[a + m, b2 + m, c + 2*m, y]*Pochhammer[a, m]*Pochhammer[b1, m]*Pochhammer[b2, m]*Pochhammer[-a + c, m])/(m!*Pochhammer[c, 2*m]*Pochhammer[-1 + c + m, m])]*hyper["AppellF1", n][(x^n*y^n*Hypergeometric2F1[a + n, b1 + n, c + 2*n, x]*Hypergeometric2F1[a + n, b2 + n, c + 2*n, y]*Pochhammer[a, n]*Pochhammer[b1, n]*Pochhammer[b2, n]*Pochhammer[-a + c, n])/(n!*Pochhammer[c, 2*n]*Pochhammer[-1 + c + n, n])]
    ,
    TestID->"8-Hyper-AppellF1.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Hyper-AppellF1.nb"
]