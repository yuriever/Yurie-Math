

(* Distribution-spowerConvert.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Distribution-spowerConvert.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Complex -> PlusMinus, PlusMinus -> Complex][{spower[I][z, n], spower[-I][z, n]}]]
    ,
    {spower[I][z, n], spower[-I][z, n]}
    ,
    TestID->"[2] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Complex -> Abs, Abs -> Complex][{spower[I][z, n], spower[-I][z, n]}]]
    ,
    {spower[I][z, n], spower[-I][z, n]}
    ,
    TestID->"[3] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[PlusMinus -> Complex, Complex -> PlusMinus][{spower["+"][z, n], spower["-"][z, n]}]]
    ,
    {spower["+"][z, n], spower["-"][z, n]}
    ,
    TestID->"[4] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[PlusMinus -> Abs, Abs -> PlusMinus][{spower["+"][z, n], spower["-"][z, n]}]]
    ,
    {spower["+"][z, n], spower["-"][z, n]}
    ,
    TestID->"[5] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Abs -> Complex, Complex -> Abs][{spower[0][z, n], spower[1][z, n]}]]
    ,
    {spower[0][z, n], spower[1][z, n]}
    ,
    TestID->"[6] Distribution-spowerConvert.nb"
]

VerificationTest[
    spowerConvert[Abs -> PlusMinus, PlusMinus -> Abs][{spower[0][z, n], spower[1][z, n]}]
    ,
    {spower[0][z, n], spower[1][z, n]}
    ,
    TestID->"[7] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Complex -> PlusMinus, PlusMinus -> Abs, Abs -> Complex][{spower[I][z, n], spower[-I][z, n]}]]
    ,
    {spower[I][z, n], spower[-I][z, n]}
    ,
    TestID->"[8] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Complex -> {Complex, Reverse}][{spower[I][z, n], spower[-I][z, n]}]]
    ,
    {E^(I*n*Pi)*spower[-I][-z, n], spower[I][-z, n]/E^(I*n*Pi)}
    ,
    TestID->"[9] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[PlusMinus -> {PlusMinus, Reverse}][{spower["+"][z, n], spower["-"][z, n]}]]
    ,
    {spower["-"][-z, n], spower["+"][-z, n]}
    ,
    TestID->"[10] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Abs -> {Abs, Reverse}][{spower[0][z, n], spower[1][z, n]}]]
    ,
    {spower[0][-z, n], -spower[1][-z, n]}
    ,
    TestID->"[11] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Complex -> {Complex, ε}][{spower[I][z, n], spower[-I][z, n]}]]
    ,
    {(z + I*ε)^n, (z - I*ε)^n}
    ,
    TestID->"[12] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[PlusMinus -> HeavisideTheta][{spower["+"][z, n], spower["-"][z, n]}]]
    ,
    {z^n*HeavisideTheta[z], (-z)^n*HeavisideTheta[-z]}
    ,
    TestID->"[13] Distribution-spowerConvert.nb"
]

VerificationTest[
    Simplify[spowerConvert[Abs -> RealAbs][{spower[0][z, n], spower[1][z, n]}]]
    ,
    {Abs[z]^n, Abs[z]^n*Sign[z]}
    ,
    TestID->"[14] Distribution-spowerConvert.nb"
]

VerificationTest[
    (Table[#1, {s, {1, -1}}] & )[spowerConvert[Complex -> {Complex, ε, Reverse}][spower[s*I][z, n]]]
    ,
    {(z + I*ε)^n, (-z + I*ε)^n/E^(I*n*Pi)}
    ,
    TestID->"[15] Distribution-spowerConvert.nb"
]

VerificationTest[
    Reverse[(Table[#1, {s, {1, -1}}] & )[spowerConvert[Complex -> {Complex, ε, Reverse}][spower[(-s)*I][z, n]]]]
    ,
    {(z + I*ε)^n, (-z + I*ε)^n/E^(I*n*Pi)}
    ,
    TestID->"[16] Distribution-spowerConvert.nb"
]

VerificationTest[
    res1 = spowerConvert[Complex -> {Abs, Reverse}][spower[s*I][z, n]]
    ,
    E^((1/2)*I*n*Pi*s)*Cos[(n*Pi*s)/2]*spower[0][-z, n] + I*E^((1/2)*I*n*Pi*s)*Sin[(n*Pi*s)/2]*spower[1][-z, n]
    ,
    TestID->"[17] Distribution-spowerConvert.nb"
]

VerificationTest[
    res2 = spowerConvert[Complex -> Abs, Abs -> {Abs, Reverse}][spower[s*I][z, n]]
    ,
    E^((1/2)*I*n*Pi*s)*Cos[(n*Pi*s)/2]*spower[0][-z, n] + I*E^((1/2)*I*n*Pi*s)*Sin[(n*Pi*s)/2]*spower[1][-z, n]
    ,
    TestID->"[18] Distribution-spowerConvert.nb"
]

VerificationTest[
    res1 - res2
    ,
    0
    ,
    TestID->"[19] Distribution-spowerConvert.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Distribution-spowerConvert.nb"
]