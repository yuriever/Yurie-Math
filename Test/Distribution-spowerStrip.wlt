

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
    spowerStrip[{spowerlog[I][z, n, m], spowerlog[-I][z, n, m]}]
    ,
    {z^n*Log[z]^m, z^n*Log[z]^m}
    ,
    TestID->"[5] Distribution-spowerStrip.nb"
]

VerificationTest[
    spowerStrip[{spowerlog["+"][z, n, m], spowerlog["-"][z, n, m]}]
    ,
    {z^n*Log[z]^m, (-z)^n*Log[-z]^m}
    ,
    TestID->"[6] Distribution-spowerStrip.nb"
]

VerificationTest[
    spowerStrip[{spowerlog[0][z, n, m], spowerlog[1][z, n, m]}]
    ,
    {Abs[z]^n*Log[z]^m, Abs[z]^n*Log[z]^m*Sign[z]}
    ,
    TestID->"[7] Distribution-spowerStrip.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-spowerStrip.nb"
]