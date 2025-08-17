

(* Hyper-JacobiPhi.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Hyper-JacobiPhi.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Hyper-JacobiPhi.nb"
]

VerificationTest[
    JacobiPhiToHyper[][JacobiPhi[a, b, c, z]]
    ,
    Hypergeometric2F1[(1/2)*(1 + a + b - I*c), (1/2)*(1 + a + b + I*c), 1 + a, -Sinh[z]^2]
    ,
    TestID->"[2] Hyper-JacobiPhi.nb"
]

VerificationTest[
    JacobiPhiToHyper[Inactive][JacobiPhi[a, b, c, z]]
    ,
    Inactive[Hypergeometric2F1][(1/2)*(1 + a + b - I*c), (1/2)*(1 + a + b + I*c), 1 + a, -Sinh[z]^2]
    ,
    TestID->"[3] Hyper-JacobiPhi.nb"
]

VerificationTest[
    PowerExpand[JacobiPhiFromHyper[][JacobiPhiToHyper[][JacobiPhi[a, b, c, z]]]]
    ,
    JacobiPhi[a, b, c, z]
    ,
    TestID->"[4] Hyper-JacobiPhi.nb"
]

VerificationTest[
    JacobiPhiFromHyper[][Hypergeometric2F1[a, b, c, z]]
    ,
    JacobiPhi[-1 + c, a + b - c, I*(a - b), ArcSinh[Sqrt[-z]]]
    ,
    TestID->"[5] Hyper-JacobiPhi.nb"
]

VerificationTest[
    JacobiPhiFromHyper[Inactive][Hypergeometric2F1[a, b, c, z]]
    ,
    Inactive[JacobiPhi][-1 + c, a + b - c, I*(a - b), ArcSinh[Sqrt[-z]]]
    ,
    TestID->"[6] Hyper-JacobiPhi.nb"
]

VerificationTest[
    JacobiPhiToHyper[][JacobiPhiFromHyper[][Hypergeometric2F1[a, b, c, z]]]
    ,
    Hypergeometric2F1[a, b, c, z]
    ,
    TestID->"[7] Hyper-JacobiPhi.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Hyper-JacobiPhi.nb"
]