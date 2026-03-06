

(* Distribution-dist-utility.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-dist-utility.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-dist-utility.nb"
]

VerificationTest[
    expr = {DiracDelta[z], DiracDelta[z, zb], Derivative[n][DiracDelta][z], Derivative[n, nb][DiracDelta][z, zb], HeavisideTheta[z], HeavisideTheta[z, zb]}; 
    ,
    Null
    ,
    TestID->"[2] Distribution-dist-utility.nb"
]

VerificationTest[
    TableForm[distFromSys[expr]]
    ,
    TableForm[{dist[deltaD, {0}][z], dist[deltaD, {0, 0}][z, zb], dist[deltaD, {n}][z], dist[deltaD, {n, nb}][z, zb], dist[step][z], dist[step][z, zb]}]
    ,
    TestID->"[3] Distribution-dist-utility.nb"
]

VerificationTest[
    minus[expr][distToSys[distFromSys[expr]]]
    ,
    {0, 0, 0, 0, 0, 0}
    ,
    TestID->"[4] Distribution-dist-utility.nb"
]

VerificationTest[
    expr = {DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb], HeavisideTheta[z, zb]}; 
    ,
    Null
    ,
    TestID->"[5] Distribution-dist-utility.nb"
]

VerificationTest[
    res = TableForm[distApart[expr]]
    ,
    TableForm[{DiracDelta[z]*DiracDelta[zb], Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb], (a + b)*DiracDelta[z]*DiracDelta[zb], (a + b)*Derivative[n][DiracDelta][z]*Derivative[nb][DiracDelta][zb], HeavisideTheta[z]*HeavisideTheta[zb]}]
    ,
    TestID->"[6] Distribution-dist-utility.nb"
]

VerificationTest[
    TableForm[distTogether[res]]
    ,
    TableForm[TableForm[{DiracDelta[z, zb], Derivative[n, nb][DiracDelta][z, zb], (a + b)*DiracDelta[z, zb], (a + b)*Derivative[n, nb][DiracDelta][z, zb], HeavisideTheta[z, zb]}]]
    ,
    TestID->"[7] Distribution-dist-utility.nb"
]

VerificationTest[
    res = TableForm[distApart[distFromSys[expr]]]
    ,
    TableForm[{dist[deltaD, {0}][z]*dist[deltaD, {0}][zb], dist[deltaD, {n}][z]*dist[deltaD, {nb}][zb], (a + b)*dist[deltaD, {0}][z]*dist[deltaD, {0}][zb], (a + b)*dist[deltaD, {n}][z]*dist[deltaD, {nb}][zb], dist[step][z]*dist[step][zb]}]
    ,
    TestID->"[8] Distribution-dist-utility.nb"
]

VerificationTest[
    TableForm[distTogether[res]]
    ,
    TableForm[TableForm[{dist[deltaD, {0, 0}][z, zb], dist[deltaD, {n, nb}][z, zb], (a + b)*dist[deltaD, {0, 0}][z, zb], (a + b)*dist[deltaD, {n, nb}][z, zb], dist[step][z, zb]}]]
    ,
    TestID->"[9] Distribution-dist-utility.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-dist-utility.nb"
]