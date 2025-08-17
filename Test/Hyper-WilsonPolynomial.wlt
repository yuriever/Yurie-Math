

(* Hyper-WilsonPolynomial.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    WilsonPolynomialToHyper[][WilsonPolynomial[a, b, c, d, n, x]]
    ,
    (Gamma[a + b + n]*Gamma[a + c + n]*Gamma[a + d + n]*HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*Sqrt[x], a + I*Sqrt[x]}, {a + b, a + c, a + d}, 1])/(Gamma[a + b]*Gamma[a + c]*Gamma[a + d])
    ,
    TestID->"[2] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    WilsonPolynomialToHyper[Inactive][WilsonPolynomial[a, b, c, d, n, x]]
    ,
    (Gamma[a + b + n]*Gamma[a + c + n]*Gamma[a + d + n]*Inactive[HypergeometricPFQ][{-n, -1 + a + b + c + d + n, a - I*Sqrt[x], a + I*Sqrt[x]}, {a + b, a + c, a + d}, 1])/(Gamma[a + b]*Gamma[a + c]*Gamma[a + d])
    ,
    TestID->"[3] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    WilsonPolynomialFromHyper[][WilsonPolynomialToHyper[][WilsonPolynomial[a, b, c, d, n, x]]]
    ,
    WilsonPolynomial[a, b, c, d, n, x]
    ,
    TestID->"[4] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    WilsonPolynomialFromHyper[][HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*x, a + I*x}, {a + b, a + c, a + d}, 1]]
    ,
    (Gamma[a + b]*Gamma[a + c]*Gamma[a + d]*WilsonPolynomial[a, b, c, d, n, x^2])/(Gamma[a + b + n]*Gamma[a + c + n]*Gamma[a + d + n])
    ,
    TestID->"[5] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    WilsonPolynomialFromHyper[Inactive][HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*x, a + I*x}, {a + b, a + c, a + d}, 1]]
    ,
    (Gamma[a + b]*Gamma[a + c]*Gamma[a + d]*Inactive[WilsonPolynomial][a, b, c, d, n, x^2])/(Gamma[a + b + n]*Gamma[a + c + n]*Gamma[a + d + n])
    ,
    TestID->"[6] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    WilsonPolynomialToHyper[][WilsonPolynomialFromHyper[][HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*x, a + I*x}, {a + b, a + c, a + d}, 1]]]
    ,
    HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*Sqrt[x^2], a + I*Sqrt[x^2]}, {a + b, a + c, a + d}, 1]
    ,
    TestID->"[7] Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Hyper-WilsonPolynomial.nb"
]