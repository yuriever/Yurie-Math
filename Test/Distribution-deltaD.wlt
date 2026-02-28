

(* Distribution-deltaD.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-deltaD.nb"
]

VerificationTest[
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-deltaD.nb"
]

VerificationTest[
    deltaD[z]
    ,
    deltaD[z, 0]
    ,
    TestID->"[2] Distribution-deltaD.nb"
]

VerificationTest[
    deltaD[{z}]
    ,
    deltaD[z, 0]
    ,
    TestID->"[3] Distribution-deltaD.nb"
]

VerificationTest[
    deltaD[{z, zb}]
    ,
    deltaD[{z, zb}, {0, 0}]
    ,
    TestID->"[4] Distribution-deltaD.nb"
]

VerificationTest[
    deltaD[{z}, {n}]
    ,
    deltaD[z, n]
    ,
    TestID->"[5] Distribution-deltaD.nb"
]

VerificationTest[
    ValueQ[deltaD[{z}, n], Method -> "TrialEvaluation"]
    ,
    False
    ,
    TestID->"[6] Distribution-deltaD.nb"
]

VerificationTest[
    ValueQ[deltaD[z, {n}], Method -> "TrialEvaluation"]
    ,
    False
    ,
    TestID->"[7] Distribution-deltaD.nb"
]

VerificationTest[
    D[deltaD[z], {z, n}]
    ,
    deltaD[z, n]
    ,
    TestID->"[8] Distribution-deltaD.nb"
]

VerificationTest[
    D[deltaD[z, n], z]
    ,
    deltaD[z, 1 + n]
    ,
    TestID->"[9] Distribution-deltaD.nb"
]

VerificationTest[
    D[deltaD[{z, zb}, {n, nb}], z, zb]
    ,
    deltaD[{z, zb}, {1 + n, 1 + nb}]
    ,
    TestID->"[10] Distribution-deltaD.nb"
]

VerificationTest[
    D[deltaD[{z, zb}, n], z]
    ,
    deltaD[{z, zb}, {1 + n, n}]
    ,
    TestID->"[11] Distribution-deltaD.nb"
]

VerificationTest[
    D[deltaD[z, {n, nb}], z]
    ,
    deltaD[z, {1 + n, 1 + nb}]
    ,
    TestID->"[12] Distribution-deltaD.nb"
]

VerificationTest[
    expr = {DiracDelta[z], DiracDelta[z, zb], Derivative[n][DiracDelta][z], Derivative[n, nb][DiracDelta][z, zb]}; 
    ,
    Null
    ,
    TestID->"[13] Distribution-deltaD.nb"
]

VerificationTest[
    deltaFromDirac[expr]
    ,
    {deltaD[z, 0], deltaD[{z, zb}, {0, 0}], deltaD[z, n], deltaD[{z, zb}, {n, nb}]}
    ,
    TestID->"[14] Distribution-deltaD.nb"
]

VerificationTest[
    minus[expr][deltaToDirac[deltaFromDirac[expr]]]
    ,
    {0, 0, 0, 0}
    ,
    TestID->"[15] Distribution-deltaD.nb"
]

VerificationTest[
    expr = {DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb]}; 
    ,
    Null
    ,
    TestID->"[16] Distribution-deltaD.nb"
]

VerificationTest[
    res = Yurie`Math`Distribution`Private`deltaApart[expr]
    ,
    {DiracDelta[z]*DiracDelta[zb], Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb], (a + b)*DiracDelta[z]*DiracDelta[zb], (a + b)*Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb]}
    ,
    TestID->"[17] Distribution-deltaD.nb"
]

VerificationTest[
    Yurie`Math`Distribution`Private`deltaTogether[res]
    ,
    {DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb]}
    ,
    TestID->"[18] Distribution-deltaD.nb"
]

VerificationTest[
    res = Yurie`Math`Distribution`Private`deltaApart[deltaFromDirac[expr]]
    ,
    {deltaD[z, 0]*deltaD[zb, 0], deltaD[z, n]*deltaD[zb, nb], (a + b)*deltaD[z, 0]*deltaD[zb, 0], (a + b)*deltaD[z, n]*deltaD[zb, nb]}
    ,
    TestID->"[19] Distribution-deltaD.nb"
]

VerificationTest[
    Yurie`Math`Distribution`Private`deltaTogether[res]
    ,
    {deltaD[{z, zb}, {0, 0}], deltaD[{z, zb}, {n, nb}], (a + b)*deltaD[{z, zb}, {0, 0}], (a + b)*deltaD[{z, zb}, {n, nb}]}
    ,
    TestID->"[20] Distribution-deltaD.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-deltaD.nb"
]