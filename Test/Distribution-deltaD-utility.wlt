

(* Distribution-deltaD-utility.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-deltaD-utility.nb"
]

VerificationTest[
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-deltaD-utility.nb"
]

VerificationTest[
    expr = {DiracDelta[z], DiracDelta[z, zb], Derivative[n][DiracDelta][z], Derivative[n, nb][DiracDelta][z, zb]}; 
    ,
    Null
    ,
    TestID->"[2] Distribution-deltaD-utility.nb"
]

VerificationTest[
    deltaFromDirac[expr]
    ,
    {deltaFun["D", {0}][z], deltaFun["D", {0, 0}][z, zb], deltaFun["D", {n}][z], deltaFun["D", {n, nb}][z, zb]}
    ,
    TestID->"[3] Distribution-deltaD-utility.nb"
]

VerificationTest[
    minus[expr][deltaToDirac[deltaFromDirac[expr]]]
    ,
    {0, 0, 0, 0}
    ,
    TestID->"[4] Distribution-deltaD-utility.nb"
]

VerificationTest[
    expr = {DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb]}; 
    ,
    Null
    ,
    TestID->"[5] Distribution-deltaD-utility.nb"
]

VerificationTest[
    res = TableForm[Yurie`Math`Distribution`Private`deltaApart[expr]]
    ,
    TableForm[{DiracDelta[z]*DiracDelta[zb], Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb], (a + b)*DiracDelta[z]*DiracDelta[zb], (a + b)*Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb]}]
    ,
    TestID->"[6] Distribution-deltaD-utility.nb"
]

VerificationTest[
    TableForm[Yurie`Math`Distribution`Private`deltaTogether[res]]
    ,
    TableForm[TableForm[{DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb]}]]
    ,
    TestID->"[7] Distribution-deltaD-utility.nb"
]

VerificationTest[
    res = TableForm[Yurie`Math`Distribution`Private`deltaApart[deltaFromDirac[expr]]]
    ,
    TableForm[{deltaFun["D", {0}][z]*deltaFun["D", {0}][zb], deltaFun["D", {n}][z]*deltaFun["D", {nb}][zb], (a + b)*deltaFun["D", {0}][z]*deltaFun["D", {0}][zb], (a + b)*deltaFun["D", {n}][z]*deltaFun["D", {nb}][zb]}]
    ,
    TestID->"[8] Distribution-deltaD-utility.nb"
]

VerificationTest[
    TableForm[Yurie`Math`Distribution`Private`deltaTogether[res]]
    ,
    TableForm[TableForm[{deltaFun["D", {0, 0}][z, zb], deltaFun["D", {n, nb}][z, zb], (a + b)*deltaFun["D", {0, 0}][z, zb], (a + b)*deltaFun["D", {n, nb}][z, zb]}]]
    ,
    TestID->"[9] Distribution-deltaD-utility.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-deltaD-utility.nb"
]