

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
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Hyper-AppellF1.nb"
]