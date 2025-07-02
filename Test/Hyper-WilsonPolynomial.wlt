

(*Hyper-WilsonPolynomial.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    wilsonPolynomialToHyper[wilsonPolynomialFromHyper[HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*x, a + I*x}, {a + b, a + c, a + d}, 1]]]
    ,
    HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*Sqrt[x^2], a + I*Sqrt[x^2]}, {a + b, a + c, a + d}, 1]
    ,
    TestID->"2-Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    wilsonPolynomialFromHyper[wilsonPolynomialToHyper[wilsonPolynomial[a, b, c, d, n, x]]]
    ,
    wilsonPolynomial[a, b, c, d, n, x]
    ,
    TestID->"3-Hyper-WilsonPolynomial.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Hyper-WilsonPolynomial.nb"
]