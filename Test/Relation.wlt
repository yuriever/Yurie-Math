

(*Relation.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Relation.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Relation.nb"
]

VerificationTest[
    (ReplaceAll[relationMellinBarnes[(a + b)^(-Δ), a, s]])[(a + b)^(-Δ)]
    ,
    a^s*b^(-s - Δ)*INT[s]*multiGamma[{-s, s + Δ}, {Δ}]
    ,
    TestID->"2-Relation.nb"
]

VerificationTest[
    (ReplaceAll[relationMellinBarnes[(b + c)^(-s - Δ), b, t]])[(ReplaceAll[relationMellinBarnes[(a + b + c)^(-Δ), a, s]])[(a + b + c)^(-Δ)]]
    ,
    a^s*b^t*c^(-s - t - Δ)*INT[s, t]*multiGamma[{-s, -t, s + t + Δ}, {Δ}]
    ,
    TestID->"3-Relation.nb"
]

VerificationTest[
    (ReplaceAll[relationMellinBarnes[(a + b)^(-Δ), a, s, Identity]])[(a + b)^(-Δ)]
    ,
    a^s*b^(-s - Δ)*multiGamma[{-s, s + Δ}, {Δ}]
    ,
    TestID->"4-Relation.nb"
]

VerificationTest[
    (ReplaceAll[relationFeynman[1/(a1^Δ1*a2^Δ2), a1, s]])[1/(a1^Δ1*a2^Δ2)]
    ,
    s^(-1 + Δ2)*(a1 + a2*s)^(-Δ1 - Δ2)*INT[s]*multiGamma[{Δ1 + Δ2}, {Δ1, Δ2}]
    ,
    TestID->"5-Relation.nb"
]

VerificationTest[
    (ReplaceAll[relationFeynman[(a1 + a2*s)^(-Δ1 - Δ2)/a3^Δ3, a1 + a2*s, t]])[(ReplaceAll[relationFeynman[1/(a1^Δ1*a2^Δ2), a1, s]])[1/(a1^Δ1*a2^Δ2*a3^Δ3)]]
    ,
    s^(-1 + Δ2)*t^(-1 + Δ3)*(a1 + a2*s + a3*t)^(-Δ1 - Δ2 - Δ3)*INT[s, t]*multiGamma[{Δ1 + Δ2 + Δ3}, {Δ1, Δ2, Δ3}]
    ,
    TestID->"6-Relation.nb"
]

VerificationTest[
    (ReplaceAll[relationFeynman[1/(a1^Δ1*a2^Δ2), a1, s, Identity]])[1/(a1^Δ1*a2^Δ2)]
    ,
    s^(-1 + Δ2)*(a1 + a2*s)^(-Δ1 - Δ2)*multiGamma[{Δ1 + Δ2}, {Δ1, Δ2}]
    ,
    TestID->"7-Relation.nb"
]

VerificationTest[
    Null
    ,
    Null
    ,
    TestID->"8-Relation.nb"
]

VerificationTest[
    Null
    ,
    Null
    ,
    TestID->"9-Relation.nb"
]

VerificationTest[
    Null
    ,
    Null
    ,
    TestID->"10-Relation.nb"
]

VerificationTest[
    Null
    ,
    Null
    ,
    TestID->"11-Relation.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Relation.nb"
]