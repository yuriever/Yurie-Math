

(* Distribution-deltaD-basic.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-deltaD-basic.nb"
]

VerificationTest[
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-deltaD-basic.nb"
]

VerificationTest[
    deltaD[z]
    ,
    deltaFun["D", {0}][z]
    ,
    TestID->"[2] Distribution-deltaD-basic.nb"
]

VerificationTest[
    deltaD[{z}]
    ,
    deltaFun["D", {0}][z]
    ,
    TestID->"[3] Distribution-deltaD-basic.nb"
]

VerificationTest[
    deltaD[{z, zb}]
    ,
    deltaFun["D", {0, 0}][z, zb]
    ,
    TestID->"[4] Distribution-deltaD-basic.nb"
]

VerificationTest[
    deltaD[{z}, {n}]
    ,
    deltaFun["D", {n}][z]
    ,
    TestID->"[5] Distribution-deltaD-basic.nb"
]

VerificationTest[
    Hold[Evaluate[deltaD[{z}, n]]]
    ,
    Hold[deltaD[{z}, n]]
    ,
    TestID->"[6] Distribution-deltaD-basic.nb"
]

VerificationTest[
    Hold[Evaluate[deltaD[z, {n}]]]
    ,
    Hold[deltaD[z, {n}]]
    ,
    TestID->"[7] Distribution-deltaD-basic.nb"
]

VerificationTest[
    D[deltaD[z], {z, n}]
    ,
    deltaFun["D", {n}][z]
    ,
    TestID->"[8] Distribution-deltaD-basic.nb"
]

VerificationTest[
    D[deltaD[z, n], z]
    ,
    deltaFun["D", {1 + n}][z]
    ,
    TestID->"[9] Distribution-deltaD-basic.nb"
]

VerificationTest[
    D[deltaD[{z, zb}, {n, nb}], z, zb]
    ,
    deltaFun["D", {1 + n, 1 + nb}][z, zb]
    ,
    TestID->"[10] Distribution-deltaD-basic.nb"
]

VerificationTest[
    D[deltaD[{z, zb}, n], z]
    ,
    Derivative[{1, 0}, 0][deltaD][{z, zb}, n]
    ,
    TestID->"[11] Distribution-deltaD-basic.nb"
]

VerificationTest[
    D[deltaD[z, {n, nb}], z]
    ,
    Derivative[1, {0, 0}][deltaD][z, {n, nb}]
    ,
    TestID->"[12] Distribution-deltaD-basic.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-deltaD-basic.nb"
]