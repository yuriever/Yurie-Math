

(*Hyper-JacobiPhi.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Hyper-JacobiPhi.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Hyper-JacobiPhi.nb"
]

VerificationTest[
    SSA[z > 0][jacobiPhiFromHyper[jacobiPhiToHyper[jacobiPhi[a, b, c, z]]]]
    ,
    jacobiPhi[a, b, c, z]
    ,
    TestID->"2-Hyper-JacobiPhi.nb"
]

VerificationTest[
    jacobiPhiToHyper[jacobiPhiFromHyper[Hypergeometric2F1[a, b, c, z]]]
    ,
    Hypergeometric2F1[a, b, c, z]
    ,
    TestID->"3-Hyper-JacobiPhi.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Hyper-JacobiPhi.nb"
]