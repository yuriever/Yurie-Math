

(* Distribution-deltaOld.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-deltaOld.nb"
]

VerificationTest[
    Get["Yurie`Base`"];
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-deltaOld.nb"
]

VerificationTest[
    deltaOld[z]
    ,
    deltaOld[z, 0]
    ,
    TestID->"[2] Distribution-deltaOld.nb"
]

VerificationTest[
    deltaOld[{z}]
    ,
    deltaOld[z, 0]
    ,
    TestID->"[3] Distribution-deltaOld.nb"
]

VerificationTest[
    deltaOld[{z, zb}]
    ,
    deltaOld[{z, zb}, {0, 0}]
    ,
    TestID->"[4] Distribution-deltaOld.nb"
]

VerificationTest[
    deltaOld[{z}, {n}]
    ,
    deltaOld[z, n]
    ,
    TestID->"[5] Distribution-deltaOld.nb"
]

VerificationTest[
    ValueQ[deltaOld[{z}, n], Method -> "TrialEvaluation"]
    ,
    False
    ,
    TestID->"[6] Distribution-deltaOld.nb"
]

VerificationTest[
    ValueQ[deltaOld[z, {n}], Method -> "TrialEvaluation"]
    ,
    False
    ,
    TestID->"[7] Distribution-deltaOld.nb"
]

VerificationTest[
    D[deltaOld[z], {z, n}]
    ,
    deltaOld[z, n]
    ,
    TestID->"[8] Distribution-deltaOld.nb"
]

VerificationTest[
    D[deltaOld[z, n], z]
    ,
    deltaOld[z, 1 + n]
    ,
    TestID->"[9] Distribution-deltaOld.nb"
]

VerificationTest[
    D[deltaOld[{z, zb}, {n, nb}], z, zb]
    ,
    deltaOld[{z, zb}, {1 + n, 1 + nb}]
    ,
    TestID->"[10] Distribution-deltaOld.nb"
]

VerificationTest[
    D[deltaOld[{z, zb}, n], z]
    ,
    deltaOld[{z, zb}, {1 + n, n}]
    ,
    TestID->"[11] Distribution-deltaOld.nb"
]

VerificationTest[
    D[deltaOld[z, {n, nb}], z]
    ,
    deltaOld[z, {1 + n, 1 + nb}]
    ,
    TestID->"[12] Distribution-deltaOld.nb"
]

VerificationTest[
    expr = {DiracDelta[z], DiracDelta[z, zb], Derivative[n][DiracDelta][z], Derivative[n, nb][DiracDelta][z, zb]};
    ,
    Null
    ,
    TestID->"[13] Distribution-deltaOld.nb"
]

VerificationTest[
    deltaFromDirac[expr]
    ,
    {deltaOld[z, 0], deltaOld[{z, zb}, {0, 0}], deltaOld[z, n], deltaOld[{z, zb}, {n, nb}]}
    ,
    TestID->"[14] Distribution-deltaOld.nb"
]

VerificationTest[
    minus[expr][deltaToDirac[deltaFromDirac[expr]]]
    ,
    {0, 0, 0, 0}
    ,
    TestID->"[15] Distribution-deltaOld.nb"
]

VerificationTest[
    expr = {DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb]};
    ,
    Null
    ,
    TestID->"[16] Distribution-deltaOld.nb"
]

VerificationTest[
    res = Yurie`Math`Distribution`Private`deltaApart[expr]
    ,
    {DiracDelta[z]*DiracDelta[zb], Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb], (a + b)*DiracDelta[z]*DiracDelta[zb], (a + b)*Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb]}
    ,
    TestID->"[17] Distribution-deltaOld.nb"
]

VerificationTest[
    Yurie`Math`Distribution`Private`deltaTogether[res]
    ,
    {DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb]}
    ,
    TestID->"[18] Distribution-deltaOld.nb"
]

VerificationTest[
    res = Yurie`Math`Distribution`Private`deltaApart[deltaFromDirac[expr]]
    ,
    {deltaOld[z, 0]*deltaOld[zb, 0], deltaOld[z, n]*deltaOld[zb, nb], (a + b)*deltaOld[z, 0]*deltaOld[zb, 0], (a + b)*deltaOld[z, n]*deltaOld[zb, nb]}
    ,
    TestID->"[19] Distribution-deltaOld.nb"
]

VerificationTest[
    Yurie`Math`Distribution`Private`deltaTogether[res]
    ,
    {deltaOld[{z, zb}, {0, 0}], deltaOld[{z, zb}, {n, nb}], (a + b)*deltaOld[{z, zb}, {0, 0}], (a + b)*deltaOld[{z, zb}, {n, nb}]}
    ,
    TestID->"[20] Distribution-deltaOld.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-deltaOld.nb"
]