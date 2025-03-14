

(*Simplify.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Simplify.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Simplify.nb"
]

VerificationTest[
    swap[a, b][{a, b, c, d}]
    ,
    {b, a, c, d}
    ,
    TestID->"2-Simplify.nb"
]

VerificationTest[
    swap[{a, b}, {c, d}][{a, b, c, d}]
    ,
    {b, a, d, c}
    ,
    TestID->"3-Simplify.nb"
]

VerificationTest[
    separate[EvenQ][Range[10]]
    ,
    {{2, 4, 6, 8, 10}, {1, 3, 5, 7, 9}}
    ,
    TestID->"4-Simplify.nb"
]

VerificationTest[
    separate[FreeQ[s]][((-z)^s*Gamma[c]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[c + s])]
    ,
    {Gamma[c]/(Gamma[a]*Gamma[b]), ((-z)^s*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/Gamma[c + s]}
    ,
    TestID->"5-Simplify.nb"
]

VerificationTest[
    expr = f[a^2 + b^2 + 2*a*b] + g[a^2 + b^2 + 2*a*b, 2]
    ,
    f[a^2 + 2*a*b + b^2] + g[a^2 + 2*a*b + b^2, 2]
    ,
    TestID->"6-Simplify.nb"
]

VerificationTest[
    focus[f][expr]
    ,
    f[(a + b)^2] + g[a^2 + 2*a*b + b^2, 2]
    ,
    TestID->"7-Simplify.nb"
]

VerificationTest[
    focus[f, Identity][expr]
    ,
    f[a^2 + 2*a*b + b^2] + g[a^2 + 2*a*b + b^2, 2]
    ,
    TestID->"8-Simplify.nb"
]

VerificationTest[
    focus[f | g][expr]
    ,
    f[(a + b)^2] + g[(a + b)^2, 2]
    ,
    TestID->"9-Simplify.nb"
]

VerificationTest[
    focus[f[a^2 + 2*a*b + b^2], h][expr]
    ,
    g[a^2 + 2*a*b + b^2, 2] + h[f[a^2 + 2*a*b + b^2]]
    ,
    TestID->"10-Simplify.nb"
]

VerificationTest[
    focusPower[f][x^a]
    ,
    f[x]^f[a]
    ,
    TestID->"11-Simplify.nb"
]

VerificationTest[
    focusPowerBase[f][x^a]
    ,
    f[x]^a
    ,
    TestID->"12-Simplify.nb"
]

VerificationTest[
    focusPowerExponent[f][x^a]
    ,
    x^f[a]
    ,
    TestID->"13-Simplify.nb"
]

VerificationTest[
    trigPhaseReduce[k][Sin[(-Pi)*k + a]]
    ,
    (-1)^k*Sin[a]
    ,
    TestID->"14-Simplify.nb"
]

VerificationTest[
    trigPhaseReduce[k][Sin[3*Pi*k + a]*Cos[Pi*k + b]]
    ,
    Cos[b]*Sin[a]
    ,
    TestID->"15-Simplify.nb"
]

VerificationTest[
    trigPhaseReduce[k][Sin[3*Pi*k + a]*Cos[Pi*k + b]*Cos[Pi*k + c]]
    ,
    (-1)^k*Cos[b]*Cos[c]*Sin[a]
    ,
    TestID->"16-Simplify.nb"
]

VerificationTest[
    powerPhaseReduce[χ > 0 && χb > 0][(-χ)^a*(-χb)^b]
    ,
    E^(I*(a + b)*Pi)*χ^a*χb^b
    ,
    TestID->"17-Simplify.nb"
]

VerificationTest[
    powerPhaseReduce[χ > 0 && χb > 0, χb][(-χ)^a*(-χb)^b]
    ,
    E^(I*(a - b)*Pi)*χ^a*χb^b
    ,
    TestID->"18-Simplify.nb"
]

VerificationTest[
    powerPhaseReduce[χ > 0 && χb > 0, χ, χb][(-χ)^a*(-χb)^b]
    ,
    E^(I*(a - b)*Pi)*χ^a*χb^b
    ,
    TestID->"19-Simplify.nb"
]

VerificationTest[
    powerPhaseReduce[χ > 0 && χb > 0, {χ}, {χb}][(-χ)^a*(-χb)^b]
    ,
    E^(I*(a - b)*Pi)*χ^a*χb^b
    ,
    TestID->"20-Simplify.nb"
]

VerificationTest[
    powerPhaseReduce[χ + c > 0 && χb + c > 0, χ, χb][(-χ - c)^a*(-χb - c)^b]
    ,
    E^(I*(a - b)*Pi)*(c + χ)^a*(c + χb)^b
    ,
    TestID->"21-Simplify.nb"
]

VerificationTest[
    powerPhaseReduce[χ + c > 0 && χb + c > 0, χ | c, χb][(-χ - c)^a*(-χb - c)^b]
    ,
    E^(I*a*Pi)*(c + χ)^a*(-c - χb)^b
    ,
    TestID->"22-Simplify.nb"
]

VerificationTest[
    powerPhaseReduce[χ + c > 0 && χb + c > 0, χ, χb | c][(-χ - c)^a*(-χb - c)^b]
    ,
    ((-c - χ)^a*(c + χb)^b)/E^(I*b*Pi)
    ,
    TestID->"23-Simplify.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Simplify.nb"
]