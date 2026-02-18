

(* Hyper-AppellF1.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Hyper-AppellF1.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[All, Inactive][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (Gamma[1 + a]*Gamma[1 + b]*Inactive[AppellF1][1 + a, -c, -d, 2 + a + b, x, y])/Gamma[2 + a + b]
    ,
    TestID->"[2] Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[All][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (AppellF1[1 + a, -c, -d, 2 + a + b, x, y]*Gamma[1 + a]*Gamma[1 + b])/Gamma[2 + a + b]
    ,
    TestID->"[3] Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[u][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (AppellF1[1 + a, -c, -d, 2 + a + b, x, y]*Gamma[1 + a]*Gamma[1 + b])/Gamma[2 + a + b]
    ,
    TestID->"[4] Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[v][u^a*(1 - u)^b*(1 - u*x)^c*(1 - u*y)^d]
    ,
    (1 - u)^b*u^a*(1 - u*x)^c*(1 - u*y)^d
    ,
    TestID->"[5] Hyper-AppellF1.nb"
]

VerificationTest[
    AppellF1FromIntegral[u][u^a*(1 - u)^b*(u*x1*x2 + x1 + x2)^c*(u*y1*y2 + x1*x2)^d]
    ,
    ((x1*x2)^d*(x1 + x2)^c*AppellF1[1 + a, -c, -d, 2 + a + b, -((x1*x2)/(x1 + x2)), -((y1*y2)/(x1*x2))]*Gamma[1 + a]*Gamma[1 + b])/Gamma[2 + a + b]
    ,
    TestID->"[6] Hyper-AppellF1.nb"
]

VerificationTest[
    hyperFromAppellF1[n, Full][AppellF1[a, b1, b2, c, x, y]]
    ,
    hyper["AppellF1", n][(x^n*y^n*Gamma[c]*Gamma[a + n]*Gamma[b1 + n]*Gamma[b2 + n]*Gamma[-1 + c + n]*Gamma[-a + c + n]*Hypergeometric2F1[a + n, b1 + n, c + 2*n, x]*Hypergeometric2F1[a + n, b2 + n, c + 2*n, y])/(Gamma[a]*Gamma[b1]*Gamma[b2]*Gamma[-a + c]*Gamma[1 + n]*Gamma[-1 + c + 2*n]*Gamma[c + 2*n])]
    ,
    TestID->"[7] Hyper-AppellF1.nb"
]

VerificationTest[
    hyperFromAppellF1[n, Full][AppellF1[a, b1, b2, c, x, y]^2]
    ,
    Quiet[AppellF1[a, b1, b2, c, x, y]^2]
    ,
    {Yurie`Math`hyper::SymbolNotEnough}
    ,
    TestID->"[8] Hyper-AppellF1.nb"
]

VerificationTest[
    hyperFromAppellF1[n | m, Full][AppellF1[a, b1, b2, c, x, y]^2]
    ,
    hyper["AppellF1", m][(x^m*y^m*Gamma[c]*Gamma[a + m]*Gamma[b1 + m]*Gamma[b2 + m]*Gamma[-1 + c + m]*Gamma[-a + c + m]*Hypergeometric2F1[a + m, b1 + m, c + 2*m, x]*Hypergeometric2F1[a + m, b2 + m, c + 2*m, y])/(Gamma[a]*Gamma[b1]*Gamma[b2]*Gamma[-a + c]*Gamma[1 + m]*Gamma[-1 + c + 2*m]*Gamma[c + 2*m])]*hyper["AppellF1", n][(x^n*y^n*Gamma[c]*Gamma[a + n]*Gamma[b1 + n]*Gamma[b2 + n]*Gamma[-1 + c + n]*Gamma[-a + c + n]*Hypergeometric2F1[a + n, b1 + n, c + 2*n, x]*Hypergeometric2F1[a + n, b2 + n, c + 2*n, y])/(Gamma[a]*Gamma[b1]*Gamma[b2]*Gamma[-a + c]*Gamma[1 + n]*Gamma[-1 + c + 2*n]*Gamma[c + 2*n])]
    ,
    TestID->"[9] Hyper-AppellF1.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Hyper-AppellF1.nb"
]