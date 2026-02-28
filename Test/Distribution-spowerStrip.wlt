

(* Distribution-spowerStrip.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-spowerStrip.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-spowerStrip.nb"
]

VerificationTest[
    spowerStrip[{spower[I][z, n], spower[-I][z, n]}]
    ,
    {z^n, z^n}
    ,
    TestID->"[2] Distribution-spowerStrip.nb"
]

VerificationTest[
    spowerStrip[{spower["+"][z, n], spower["-"][z, n]}]
    ,
    {z^n, (-z)^n}
    ,
    TestID->"[3] Distribution-spowerStrip.nb"
]

VerificationTest[
    spowerStrip[{spower[0][z, n], spower[1][z, n]}]
    ,
    {Abs[z]^n, Abs[z]^n*Sign[z]}
    ,
    TestID->"[4] Distribution-spowerStrip.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-spowerStrip.nb"
]